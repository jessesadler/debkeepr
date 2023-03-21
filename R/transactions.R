## Transaction functions ##

# Note that all functions except deb_account() cast deb_lsd() columns to
# deb_decimal(). This is faster than going through the process with deb_lsd().

#' Analysis of double-entry bookkeeping
#'
#' @description
#' Family of seven related functions to analyze transactions data frames
#' that have credit, debit, and tetrapartite (lsd) or tetrapartite (lsdf)
#' columns, mimicking an account book.
#'
#' - `deb_account()` credit, debit, and current value of a single account.
#' - `deb_account_summary()` credit, debit, and current value of all accounts.
#' - `deb_credit()` total credit of each account.
#' - `deb_debit()` total debit of each account.
#' - `deb_current()` current value of each account (credit - debit).
#' - `deb_open()` current value of each account that has a positive or
#'   negative value.
#' - `deb_balance()` positive and negative value remaining in a transactions
#'   data frame.
#'
#' # Transactions data frames:
#' Transactions data frames have the structure of an account book. They
#' should have a similar arrangement to `dafforne_transactions`. Each row is
#' a transaction in the book. `credit` and `debit` columns contain the
#' account ids associated with discharging account (credit) and the receiving
#' account (debit). The `lsd` column represents the tripartite or
#' tetrapartite value of each transaction. Like `dafforne_transactions`,
#' transactions data frames can have additional columns with attributes for
#' each transaction such as id or date among others.
#'
#' @param df A data frame or tibble with at least credit, debit, and lsd
#'   columns.
#' @param account_id The id of the account to be used to calculate the
#'   credit, debit, and current values.
#' @param credit Credit column: Unquoted name of the credit column,
#'   representing the accounts that discharge the transactional values or
#'   from which the values derive. Default is `credit`.
#' @param debit Debit column: Unquoted name of the debit column,
#'   representing the accounts that receive the transactional values.
#'   Default is `debit`.
#' @param lsd Value column: Unquoted name of a column of class `deb_lsd`,
#'   `deb_decimal`, or `deb_tetra`. Default is `lsd`.
#' @param na.rm Logical. Should missing values (including `NaN`) be removed?
#'
#' @returns
#' Transaction functions return a data frame or tibble with columns for the
#' accounts in `df` and credit, debit, and/or current values in the same type
#' and `bases` as `lsd`:
#'
#' - `deb_account()`: a data frame with three rows showing the credit, debit,
#'   and current value of the given account.
#' - `deb_account_summary()` a data frame with one row for each account in
#'   `df` and credit, debit, and current value columns.
#' - `deb_credit()`: a data frame with one row for each account with the total
#'   credit of the accounts.
#' - `deb_debit()`: a data frame with one row for each account with the total
#'   debit of the accounts.
#' - `deb_current()`: a data frame with one row for each account with the
#'   current value of the accounts.
#' - `deb_open()`: a data frame with one row for each account whose current
#'   value is not `0`. If all accounts are equal to zero, a data frame with
#'   zero rows will be returned.
#' - `deb_balance()`: a data frame with two rows showing the credit and debit
#'   remaining in `df`.
#'
#' @examples
#' # Examples use dafforne_transactions data,
#' # which have default column names.
#' # See dafforne_accounts for account names.
#'
#' # Credit, debit, and current value of cash account
#' deb_account(dafforne_transactions, account_id = 1,
#'             credit = credit, debit = debit,
#'             lsd = lsd)
#'
#' # Credit, debit, and current value of profit and loss account
#' deb_account(dafforne_transactions, account_id = 23)
#'
#' # Summary of all accounts in Dafforne's ledger
#' deb_account_summary(dafforne_transactions)
#'
#' # Credit of accounts in Dafforne's ledger
#' deb_credit(dafforne_transactions)
#'
#' # Debit of accounts in Dafforne's ledger
#' deb_debit(dafforne_transactions)
#'
#' # Current value of accounts in Dafforne's ledger
#' current <- deb_current(dafforne_transactions)
#' current
#'
#' # Current value of open account in Dafforne's ledger
#' open <- deb_open(dafforne_transactions)
#' open
#'
#' # Compare the amount of rows in returned values of
#' # deb_current() vs deb_open()
#' nrow(current)
#' nrow(open)
#'
#' # Credit and debit remaining on Dafforne's ledger
#' deb_balance(dafforne_transactions)
#'
#' @name transactions
NULL


#' @rdname transactions
#' @export
deb_account <- function(df, account_id,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  # First two for transaction_check
  # Distinguish from non-quoted; not necessary but why not
  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  # need cn for base subsetting to access lsd column
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  # Checks for account_id
  rlang::check_required(account_id)

  if (is.null(account_id)) {
    cli::cli_abort(c(
      "{.var account_id} cannot be {.code NULL}.",
      "x" = "{.var account_id} must be present in `credit` and/or `debit`
          columns."))
  }
  credit_vec <- rlang::eval_tidy(credit_quo, df)
  debit_vec <- rlang::eval_tidy(debit_quo, df)

  if (rlang::is_false(account_id %in% c(credit_vec, debit_vec))) {
    cli::cli_abort(c(
      "{.var account_id} must be present in `credit` and/or `debit` columns.",
      "x" = "{.val {account_id}} is not present in either the
            `credit` or `debit` columns."))
  }
  # Need to get access to bare credit and debit vectors

  # Access data for bases and class
  deb_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(deb_vctr)

  deb_ptype_check(deb_vctr)

  # Credit
  pos <- df %>%
    dplyr::filter({{ credit }} == account_id) %>%
    dplyr::pull({{ lsd }})

  if (vec_size(pos) < 1) {
    pos <- create_debtype(deb_vctr, val = 0, n = 1)
  } else {
    pos <- sum(pos, na.rm = na.rm)
  }

  # Debit
  neg <- df %>%
    dplyr::filter({{ debit }} == account_id) %>%
    dplyr::pull({{ lsd }})

  if (vec_size(neg) < 1) {
    neg <- create_debtype(deb_vctr, val = 0, n = 1)
  } else {
    neg <- sum(neg, na.rm = na.rm)
  }

  # Current
  current <- pos - neg

  # Make data frame
  if (tibble::is_tibble(df)) {
    tibble::tibble(relation = c("credit", "debit", "current"),
                   !! cn := c(pos, neg, current))
  } else {
    data_frame(relation = c("credit", "debit", "current"),
               !! cn := c(pos, neg, current))
  }
}


#' @rdname transactions
#' @export
deb_account_summary <- function(df,
                                credit = credit, debit = debit,
                                lsd = lsd,
                                na.rm = FALSE) {

  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  deb_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(deb_vctr)

  # Turn deb_lsd or deb_tetra to deb_decimal for speed
  if (deb_is_lsd(deb_vctr) || deb_is_tetra(deb_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  pos <- df %>%
    deb_summarise(credit, data = {{ lsd }}, na.rm = na.rm) %>%
    dplyr::rename(credit = {{ lsd }})
  neg <- df %>%
    deb_summarise(debit, data = {{ lsd }}, na.rm = na.rm) %>%
    dplyr::rename(debit = {{ lsd }})

  # If statements to ensure NAs from above not turned into 0s.
  # If all accounts present in pos and neg, no NAs will be introduced
  if (all_present(pos, neg)) {
    ret <- dplyr::left_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current))
  # If no NAs, then any NAs from join should be 0s
  } else if (!anyNA(c(pos[[2]], neg[[2]]))) {
    ret <- dplyr::full_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(credit  = dplyr::coalesce(credit, 0), # replace NA with 0
                    debit   = dplyr::coalesce(debit, 0), # replace NA with 0
                    current = credit - debit,
                    # floating point problems
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current))
  # If there are NAs and NAs will be introduced by join, need to distinguish
  # between the two types. Add 0s from missing accounts then join.
  } else {
    cred <- deb_fill_missing(pos, neg)
    deb <- deb_fill_missing(neg, pos)

    ret <- dplyr::left_join(cred, deb, by = "account_id") %>%
      dplyr::mutate(current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current))
  }

  # Return deb_decimal back to deb_lsd or deb_tetra
  if (deb_is_lsd(deb_vctr)) {
    ret <- ret %>%
      dplyr::mutate(credit  = deb_as_lsd(credit),
                    debit   = deb_as_lsd(debit),
                    current = deb_as_lsd(current))
  } else if (deb_is_tetra(deb_vctr)) {
    ret <- ret %>%
      dplyr::mutate(credit  = deb_as_tetra(credit),
                    debit   = deb_as_tetra(debit),
                    current = deb_as_tetra(current))
  }
  # Arrange by id and return
  ret <- ret %>%
    dplyr::arrange(.data[["account_id"]])
  # Return
  if (tibble::is_tibble(df)) {
    ret
  } else {
    as.data.frame(ret)
  }
}


#' @rdname transactions
#' @export
deb_credit <- function(df,
                       credit = credit, debit = debit,
                       lsd = lsd,
                       na.rm = FALSE) {

  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  deb_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(deb_vctr)

  # Turn deb_lsd to deb_decimal for speed
  if (deb_is_lsd(deb_vctr) || deb_is_tetra(deb_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  pos <- deb_summarise(df, credit, data = {{ lsd }}, na.rm = na.rm)
  neg <- dplyr::distinct(df, {{ debit }})

  if (all_present(pos, neg)) {
    ret <- pos
  } else if (!anyNA(pos[[2]])) {
    # No NAs so can make missing accounts 0
    ret <- dplyr::full_join(pos, neg, by = c("account_id" = names(neg))) %>%
      dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0))
  } else {
    # Find missing accounts and make them 0
    ret <- deb_fill_missing(pos, neg)
  }

  if (deb_is_lsd(deb_vctr)) {
    ret[[cn]] <- deb_as_lsd(ret[[cn]])
  } else if (deb_is_tetra(deb_vctr)) {
    ret[[cn]] <- deb_as_tetra(ret[[cn]])
  }
  # Arrange by id and return
  ret <- ret %>%
    dplyr::arrange(.data[["account_id"]])
  # Return
  if (tibble::is_tibble(df)) {
    ret
  } else {
    as.data.frame(ret)
  }
}


#' @rdname transactions
#' @export
deb_debit <- function(df,
                       credit = credit, debit = debit,
                       lsd = lsd,
                       na.rm = FALSE) {

  credit_quo <- rlang::enquo(credit)
  debit_quo <- rlang::enquo(debit)
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)

  transaction_check(df, cn = cn,
                    credit = credit_quo, debit = debit_quo,
                    edge_columns = c(rlang::as_name(credit_quo),
                                     rlang::as_name(debit_quo)))

  deb_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(deb_vctr)

  # Turn deb_lsd to deb_decimal for speed
  if (deb_is_lsd(deb_vctr) || deb_is_tetra(deb_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  neg <- deb_summarise(df, debit, data = {{ lsd }}, na.rm = na.rm)
  pos <- dplyr::distinct(df, {{ credit }})

  if (all_present(pos, neg)) {
    ret <- neg
  } else if (!anyNA(neg[[2]])) {
    ret <- dplyr::full_join(neg, pos, c("account_id" = names(pos))) %>%
      dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0))
  } else {
    ret <- deb_fill_missing(neg, pos)
  }

  if (deb_is_lsd(deb_vctr)) {
    ret[[cn]] <- deb_as_lsd(ret[[cn]])
  } else if (deb_is_tetra(deb_vctr)) {
    ret[[cn]] <- deb_as_tetra(ret[[cn]])
  }
  # Arrange by id and return
  ret <- ret %>%
    dplyr::arrange(.data[["account_id"]])
  # Return
  if (tibble::is_tibble(df)) {
    ret
  } else {
    as.data.frame(ret)
  }
}


#' @rdname transactions
#' @export
deb_current <- function(df,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  deb_account_summary(df,
                      credit = {{ credit }},
                      debit = {{ debit }},
                      lsd = {{ lsd }},
                      na.rm = na.rm) %>%
    dplyr::select("account_id", {{ lsd }} := current)
}


#' @rdname transactions
#' @export
deb_open <- function(df,
                     credit = credit, debit = debit,
                     lsd = lsd,
                     na.rm = FALSE) {

  df %>%
    deb_account_summary(credit = {{ credit }},
                        debit = {{ debit }},
                        lsd = {{ lsd }},
                        na.rm = na.rm) %>%
    dplyr::select("account_id", {{ lsd }} := current) %>%
    # Closed accounts = 0; NA accounts still open
    dplyr::filter({{ lsd }} != 0 | is.na({{ lsd }}))
}


#' @rdname transactions
#' @export
deb_balance <- function(df,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        na.rm = FALSE) {

  current <- deb_current(df,
                         credit = {{ credit }},
                         debit = {{ debit }},
                         lsd = {{ lsd }},
                         na.rm = na.rm)

  # Putting this here ensures checks run first
  lsd_quo <- rlang::enquo(lsd)
  cn <- rlang::as_name(lsd_quo)
  # Access data for bases and class
  deb_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(deb_vctr)

  vals <- current[[2]]

  # If there are NAs
  if (anyNA(vals)) {
    balance <- create_debtype(deb_vctr, val = NA, n = 2)
  # If completely balance
  } else if (all(vals == 0)) {
    balance <- create_debtype(deb_vctr, val = 0, n = 2)
  } else {
   balance <- c(sum(vals[vals > 0]), sum(vals[vals < 0]))
  }
  # Make data frame
  if (tibble::is_tibble(df)) {
    tibble::tibble(relation = c("credit", "debit"),
                   !! cn := balance)
  } else {
   data_frame(relation = c("credit", "debit"),
              !! cn := balance)
  }
}
