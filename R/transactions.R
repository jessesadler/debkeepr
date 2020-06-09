## Transaction functions ##

#' Analysis of double-entry bookkeeping
#'
#' @description
#' Family of seven related functions to analyze transactions data frames that
#' have credit, debit, and lsd variables, mimicking an account book.
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
#' @section Transactions data frames:
#' Transactions data frames have the structure of an account book. They
#' should have a similar arrangement to `dafforne_transactions`. Each row is
#' a transaction in the book. `credit` and `debit` variables contain the
#' account ids associated with discharging account (credit) and the receiving
#' account (debit). The `lsd` variable represents the value of each
#' transaction. Like `dafforne_transactions`, transactions data frames can
#' have additional variables with attributes for each transaction such as id
#' or date.
#'
#' @param df A data frame with at least credit, debit, and lsd variables.
#' @param account_id The id of the account to be used to calculate the
#'   credit, debit, and current values.
#' @param credit Credit column: Unquoted name of the credit variable,
#'   representing the accounts that discharge the transactional values or
#'   from which the values derive. Default is `credit`.
#' @param debit Debit column: Unquoted name of the debit variable,
#'   representing the accounts that receive the transactional values.
#'   Default is `debit`.
#' @param lsd Value column: Unquoted name of a variable of class `deb_lsd`
#'   or `deb_decimal`. Default is `lsd`.
#' @param na.rm Logical. Should missing values (including `NaN`) be removed?
#'
#' @return
#' All return a tibble with variables for accounts and values in the same
#' class as `lsd`:
#'
#' - `deb_account()` a tibble with three rows showing the credit, debit, and
#'   current value of the given account.
#' - `deb_account_summary()` a tibble with one row for each account in `df`
#'   and credit, debit, and current value variables.
#' - `deb_credit()` a tibble with one row for each account with the total
#'   credit of the accounts.
#' - `deb_debit()` a tibble with one row for each account with the total
#'   debit of the accounts.
#' - `deb_current()` a tibble with one row for each account with the current
#'   value of the accounts.
#' - `deb_open()` a tibble with one row for each account whose current value
#'   is not `0`. If all accounts are equal to zero, a tibble with zero rows
#'   will be returned.
#' - `deb_balance()` a tibble with two rows showing the credit and debit
#'   remaining in `df`.
#'
#' @examples
#' # Examples with dafforne_transactions data,
#' # which uses default variable names.
#' # See dafforne_accounts for account names.
#'
#' # Credit, debit, and current value of cash account
#' deb_account(dafforne_transactions, account_id = 1,
#'             credit = credit, debit = debit)
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
#' deb_current(dafforne_transactions)
#'
#' # Current value of open account in Dafforne's ledger
#' deb_open(dafforne_transactions)
#'
#' # Compare the amount of rows in returned values of
#' # deb_current() vs deb_open()
#' nrow(deb_current(dafforne_transactions))
#' nrow(deb_open(dafforne_transactions))
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
                                     rlang::as_name(debit_quo)),
                    account_id = account_id)

  # Access data for bases and class
  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(lsd_vctr)

  deb_ptype_check(lsd_vctr)

  # deb_lsd
  if (deb_is_lsd(lsd_vctr)) {
    pos <- dplyr::filter(df, {{ credit }} == account_id)
    if (vec_size(pos) < 1) {
      cred <- deb_lsd(0, 0, 0, bases = bases)
    } else {
      cred <- sum(pos[[cn]], na.rm = na.rm)
    }

    neg <- dplyr::filter(df, {{ debit }} == account_id)
    if (vec_size(neg) < 1) {
      deb <- deb_lsd(0, 0, 0, bases = bases)
    } else {
      deb <- sum(neg[[cn]], na.rm = na.rm)
    }

    current <- cred - deb
    # deb_decimal
  } else {
    pos <- dplyr::filter(df, {{ credit }} == account_id)
    if (vec_size(pos) < 1) {
      cred <- deb_decimal(0, bases = bases)
    } else {
      cred <- sum(pos[[cn]], na.rm = na.rm)
    }

    neg <- dplyr::filter(df, {{ debit }} == account_id)
    if (vec_size(neg) < 1) {
      deb <- deb_decimal(0, bases = bases)
    } else {
      deb <- sum(neg[[cn]], na.rm = na.rm)
    }

    current <- cred - deb
    # Deal with floating point issues
    if (!is.na(current)){
      if (should_be_int(current)) {
        current <- round(current)
        }
    }
  }

  tibble::tibble(relation = c("credit", "debit", "current"),
                 !! cn := c(cred, deb, current))
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

  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(lsd_vctr)

  # Turn deb_lsd to deb_decimal until dplyr works with rcrd
  if (deb_is_lsd(lsd_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  pos <- dplyr::group_by(df, {{ credit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ credit }}, credit = {{ lsd }})
  neg <- dplyr::group_by(df, {{ debit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ debit }}, debit = {{ lsd }})

  # If statements to ensure NAs from above not turned into 0s.
  # If all accounts present in pos and neg, no NAs will be introduced
  if (all_present(pos, neg)) {
    ret <- dplyr::left_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current)) %>%
      dplyr::arrange(.data$account_id)
  # If no NAs, then any NAs from join should be 0s
  } else if (!anyNA(c(pos[[2]], neg[[2]]))) {
    ret <- dplyr::full_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(credit = dplyr::coalesce(credit, 0), # replace NA with 0
                    debit = dplyr::coalesce(debit, 0), # replace NA with 0
                    current = credit - debit,
                    # floating point problems
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current)) %>%
      dplyr::arrange(.data$account_id)
  # If there are NAs and NAs will be introduced by join, need to distinguish
  # between the two types. Add 0s from missing accounts then join.
  } else {
    pos_acc <- pos[[1]]
    neg_acc <- neg[[1]]

    pos_missing <- neg_acc[!(neg_acc %in% pos_acc)]
    neg_missing <- pos_acc[!(pos_acc %in% neg_acc)]

    pos <- tibble::tibble(account_id = c(pos_acc, pos_missing),
                          credit = c(pos$credit,
                                     rep(0, vec_size(pos_missing))))
    neg <- tibble::tibble(account_id = c(neg_acc, neg_missing),
                          debit = c(neg$debit,
                                    rep(0, vec_size(neg_missing))))

    ret <- dplyr::left_join(pos, neg, by = "account_id") %>%
      dplyr::mutate(current = credit - debit,
                    current = dplyr::if_else(should_be_int(current),
                                             round(current),
                                             current)) %>%
      dplyr::arrange(.data$account_id)
  }

  # Return deb_decimal back to deb_lsd
  if (deb_is_lsd(lsd_vctr)) {
    ret[["credit"]] <- deb_as_lsd(ret[["credit"]])
    ret[["debit"]] <- deb_as_lsd(ret[["debit"]])
    ret[["current"]] <- deb_as_lsd(ret[["current"]])
  }

  ret
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

  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(lsd_vctr)

  # Turn deb_lsd to deb_decimal until dplyr works with rcrd
  if (deb_is_lsd(lsd_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  pos <- dplyr::group_by(df, {{ credit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ credit }})
  neg <- dplyr::distinct(df, {{ debit }})

  if (all_present(pos, neg)) {
    ret <- dplyr::arrange(pos, .data$account_id)
  } else if (!anyNA(pos[[2]])) {
    ret <- dplyr::full_join(pos, neg, by = c("account_id" = names(neg))) %>%
      dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0)) %>%
      dplyr::arrange(.data$account_id)
  } else {
    pos_acc <- pos[[1]]
    neg_acc <- neg[[1]]

    pos_missing <- neg_acc[!(neg_acc %in% pos_acc)]

    ret <- tibble::tibble(account_id = c(pos_acc, pos_missing),
                          !! cn := c(pos[[2]],
                                     rep(0, vec_size(pos_missing)))) %>%
      dplyr::arrange(.data$account_id)
  }

  if (deb_is_lsd(lsd_vctr)) {
    ret[[cn]] <- deb_as_lsd(ret[[cn]])
  }

  ret
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

  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  deb_ptype_check(lsd_vctr)

  # Turn deb_lsd to deb_decimal until dplyr works with rcrd
  if (deb_is_lsd(lsd_vctr)) {
    df <- dplyr::mutate(df, !! cn := deb_as_decimal({{ lsd }}))
  }

  neg <- dplyr::group_by(df, {{ debit }}) %>%
    dplyr::summarise(!! cn := sum({{ lsd }}, na.rm = na.rm)) %>%
    dplyr::rename(account_id = {{ debit }})
  pos <- dplyr::distinct(df, {{ credit }})

  if (all_present(pos, neg)) {
    ret <- dplyr::arrange(neg, .data$account_id)
  } else if (!anyNA(neg[[2]])) {
    ret <- dplyr::full_join(neg, pos, c("account_id" = names(pos))) %>%
      dplyr::mutate(!! cn := dplyr::coalesce({{ lsd }}, 0)) %>%
      dplyr::arrange(.data$account_id)
  } else {
    neg_acc <- neg[[1]]
    pos_acc <- pos[[1]]

    neg_missing <- pos_acc[!(pos_acc %in% neg_acc)]

    ret <- tibble::tibble(account_id = c(neg_acc, neg_missing),
                          !! cn := c(neg[[2]],
                                     rep(0, vec_size(neg_missing)))) %>%
      dplyr::arrange(.data$account_id)
  }

  if (deb_is_lsd(lsd_vctr)) {
    ret[[cn]] <- deb_as_lsd(ret[[cn]])
  }

  ret
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
    dplyr::select(.data$account_id, {{ lsd }} := current)
}


#' @rdname transactions
#' @export
deb_open <- function(df,
                     credit = credit, debit = debit,
                     lsd = lsd,
                     na.rm = FALSE) {

  deb_account_summary(df,
                      credit = {{ credit }},
                      debit = {{ debit }},
                      lsd = {{ lsd }},
                      na.rm = na.rm) %>%
    dplyr::select(.data$account_id, {{ lsd }} := current) %>%
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
  lsd_vctr <- rlang::eval_tidy(lsd_quo, df)
  bases <- deb_bases(lsd_vctr)

  vals <- current[[2]]

  # If there are NAs
  if(anyNA(vals)) {
    return(tibble::tibble(relation = c("credit", "debit"),
             !! cn := vec_init(deb_lsd(bases = bases), 2L)))
  }

  # If completely balanced
  if (all(vals == 0)) {
    if (deb_is_lsd(lsd_vctr)) {
      return(tibble::tibble(relation = c("credit", "debit"),
               !! cn := deb_lsd(c(0, 0), c(0, 0), c(0, 0), bases = bases)))
    } else {
      return(tibble::tibble(relation = c("credit", "debit"),
               !! cn := deb_decimal(c(0, 0), bases = bases)))
    }
  }

  pos <- sum(vals[vals > 0])
  neg <- sum(vals[vals < 0])

  tibble::tibble(relation = c("credit", "debit"),
                 !! cn := c(pos, neg))
}
