## accounts functions with lsd list column ##

#' Calculate credit, debit, and current value of an account
#'
#' Calculate the total credit, debit, and current value of an account in a
#' transactions data frame with an lsd list column.
#'
#' `deb_account()` is similar to [deb_account_summary()], but it only returns
#' the information for one account instead of all accounts in the credit and/or
#' debit variables of a transactions data frame.
#'
#' `deb_account()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#' @param df A data frame containing transactions between accounts and values
#'   in the form of an lsd list column.
#' @param account_id The id of the account to be used to calculate the credit,
#'   debit, and current values. The value for the account must be present in
#'   the credit and/or debit variables.
#' @param credit Credit column: Unquoted name of the credit variable. This
#'   represents the accounts that discharge the transactional values or from
#'   which the values derive. Default is credit. The `credit` column must be
#'   of the same class as the `debit` column.
#' @param debit Debit column: Unquoted name of the debit variable. This
#'   represents the accounts that receive the transactional values. Default is
#'   debit. The `debit` column must be of the same class as the `credit`
#'   column.
#' @param lsd lsd list column: Unquoted name of an lsd list column of pounds,
#'   shillings, and pence values with a bases attribute.
#' @param round Round pence to specified number of decimal places. Default
#'   is 5. Rounding of the pence value takes place throughout the function and
#'   is not simply done at the end of the function call. This means that the
#'   credit, debit, and current values will add together correctly. However,
#'   the rounded values may be different than running the function without
#'   rounding and rounding pence values at the end.
#' @inheritParams deb_sum
#'
#' @return Returns a tibble with three rows corresponding to the credit,
#'   debit, and current values in an lsd list column.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Credit, debit, and current value of account "a"
#' deb_account(df = trans,
#'              account_id = "a",
#'              credit = credit,
#'              debit = debit,
#'              lsd = lsd)
#'
#' @export

deb_account <- function(df, account_id,
                         credit = credit, debit = debit,
                         lsd = lsd,
                         round = 5,
                         na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)
  column_name <- rlang::quo_name(lsd)

  # checks
  lsd_list_column_check(df, lsd)
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns, account_id)

  round_check(round)

  temp_credit <- df %>%
    dplyr::filter((!! credit) == account_id) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    .[[1]]
  temp_debit <- df %>%
    dplyr::filter((!! debit) == account_id) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    .[[1]]
  temp_current <- deb_subtract(temp_credit, temp_debit)

  tibble::tibble(relation = c("credit", "debit", "current"),
                 # Create lsd list column with simplified c.lsd
                 !! column_name := list(temp_credit, temp_debit, temp_current) %>%
                   purrr::flatten() %>%
                   to_lsd(attributes(temp_credit)$bases))
}

#' Calculate credit, debit, and current values of accounts
#'
#' Calculate the total credit, debit, and the current values for all accounts
#' in a transaction data frame with an lsd list column. Credits and debits are
#' both returned as positive numbers. If an account has more credit than debit,
#' the current value will be returned as positive values. If the debit is
#' greater, the current value will be returned as negative values.
#'
#' `deb_account_summary()` is similar to [deb_account()], but it returns the
#' values for all accounts in a transaction data frame instead of one. If you
#' only want to see the current values for single accounts, see
#' [deb_current()].
#'
#' `deb_account_summary()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with three rows for each account present in the
#'   credit and/or debit variables of `df`. This represents the total credit,
#'   debit, and current values of the accounts in an lsd list column. If an
#'   account does not have any credit or debit transactions, it will not have
#'   a summary row for that type of relation.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Credit, debit, and current values of accounts present in trans
#' deb_account_summary(df = trans,
#'                      credit = credit,
#'                      debit = debit,
#'                      lsd = lsd)
#'
#' @export

deb_account_summary <- function(df,
                                 credit = credit, debit = debit,
                                 lsd = lsd,
                                 round = 5,
                                 na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)
  column_name <- rlang::quo_name(lsd)

  # checks
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns)
  round_check(round)

  # Get bases for lsd column to use for deb_d_lsd
  lsd_bases <- rlang::eval_tidy(lsd, df)
  bases <- attributes(lsd_bases)$bases
  df_d <- dplyr::mutate(df, denarii = deb_lsd_d(!! lsd))

  temp_credits <- df_d %>%
    dplyr::group_by(!! credit) %>%
    dplyr::summarise(
      relation = "credit",
      denarii = sum(denarii, na.rm = na.rm)) %>%
    dplyr::mutate(denarii = round(denarii, round)) %>%  # to match rounding in deb_account
    dplyr::rename(account_id = !! credit)

  temp_debits <- df_d %>%
    dplyr::group_by(!! debit) %>%
    dplyr::summarise(
      relation = "debit",
      denarii = sum(denarii, na.rm = na.rm)) %>%
    dplyr::mutate(denarii = round(denarii, round)) %>%  # to match rounding in deb_account
    dplyr::rename(account_id = !! debit)

  accounts_sum <- dplyr::mutate(temp_debits, denarii = -denarii) %>%
    dplyr::bind_rows(temp_credits)

  temp_current <- accounts_sum %>%
    dplyr::group_by(.data$account_id) %>%
    dplyr::summarise(
      relation = "current",
      denarii = sum(denarii))

  dplyr::bind_rows(temp_credits, temp_debits, temp_current) %>%
    dplyr::arrange(.data$account_id) %>%
    dplyr::mutate(!! column_name := deb_d_lsd(denarii,
                                              bases = bases,
                                              round = round)) %>%
    dplyr::select(-denarii)
}

#' Calculate the total credit of accounts
#'
#' Calculate the total credit of accounts in a transactions data frame with an
#' lsd list column.
#'
#' `deb_credit()` is similar to [deb_account_summary()], but it only returns
#' the credit values for the accounts in `df`. See [deb_debit()] to return
#' the debit totals for the accounts in `df`.
#'
#' `deb_credit()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with one row for each account in `df` with the
#'   values in an lsd list column. The values represent the total value sent
#'   by each account to other accounts within `df`.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Total credit of accounts present in trans
#' deb_credit(df = trans,
#'             credit = credit,
#'             lsd = lsd)
#'
#' @export

deb_credit <- function(df,
                        credit = credit,
                        lsd = lsd,
                        round = 5,
                        na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  lsd <- rlang::enquo(lsd)

  # checks
  edge_columns <- rlang::quo_name(credit)
  credit_check(df, credit, debit = NULL, edge_columns)
  round_check(round)

  dplyr::group_by(df, !! credit) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! credit)
}

#' Calculate the total debit of accounts
#'
#' Calculate the total debit of accounts in a transactions data frame with an
#' lsd list column.
#'
#' `deb_debit()` is similar to [deb_account_summary()], but it only returns
#' the debit values for the accounts in `df`. See [deb_credit()] to return
#' the credit totals for the accounts in `df`.
#'
#' `deb_debit()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with one row for each account in `df` with the
#'   values in an lsd list column. The values represent the total value
#'   received by each account from other accounts within `df`.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Total debit of accounts present in trans
#' deb_debit(df = trans,
#'            debit = debit,
#'            lsd = lsd)
#'
#' @export

deb_debit <- function(df,
                        debit = debit,
                        lsd = lsd,
                        round = 5,
                        na.rm = FALSE) {
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)

  # checks
  edge_columns <- rlang::quo_name(debit)
  credit_check(df, credit = NULL, debit = debit, edge_columns)
  round_check(round)

  dplyr::group_by(df, !! debit) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! debit)
}

#' Calculate the current values of accounts
#'
#' Calculate the current values of accounts in a transactions data frame with
#' an lsd list column. If an account has more credit than debit, the current
#' value will be returned as positive values. If the debit is greater, the
#' current value will be returned as negative values.
#'
#' `deb_current()` is similar to [deb_account_summary()], but it only returns
#' the current values for the accounts in `df`. To see only the open accounts
#'  — only those accounts that have a current value greater than or less than
#'  zero — see [deb_open()].
#'
#' `deb_current()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account_summary
#'
#' @return Returns a tibble with one row for each account in `df` with the
#'   values in an lsd list column. The values represent the current value of
#'   the accounts in `df`.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Current values of accounts present in trans
#' deb_current(df = trans,
#'              credit = credit,
#'              debit = debit,
#'              lsd = lsd)
#' @export

deb_current <- function(df,
                         credit = credit, debit = debit,
                         lsd = lsd,
                         round = 5,
                         na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)

  deb_account_summary(df,
                       credit = !! credit,
                       debit = !! debit,
                       lsd = !! lsd,
                       round = round,
                       na.rm = na.rm) %>%
    dplyr::filter(relation == "current") %>%
    dplyr::select(-relation)
}

#' Calculate the current values of open accounts
#'
#' Calculate the current values of accounts in a transactions data frame and
#' show only those accounts that have a positive or negative balance. If an
#' account has more credit than debit, the current value will be returned as
#' positive values. If the debit is greater, the current value will be returned
#' as negative values.
#'
#' `deb_open()` is similar to [deb_current()], but it only returns current
#' values for accounts that have a value that is not zero.
#'
#' `deb_open()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account_summary
#'
#' @return Returns a tibble with one row for each account in `df` in which the
#'   current value of pounds, shillings, and pence contained in an lsd list
#'   column does not equal zero or does not have a missing value in the
#'   current value.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Current values of open accounts present in trans
#' deb_open(df = trans,
#'           credit = credit,
#'           debit = debit,
#'           lsd = lsd)
#'
#' @export

deb_open <- function(df,
                      credit = credit, debit = debit,
                      lsd = lsd,
                      round = 5,
                      na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)

  deb_current(df,
               credit = !! credit,
               debit = !! debit,
               lsd = !! lsd,
               round = round,
               na.rm = na.rm) %>%
    dplyr::filter(deb_lsd_d(!! lsd) != 0)
}

#' Calculate the balance of a transactions data frame
#'
#' Calculate the balance remaining in `df`. This shows the total credit and
#' debit remaining in the transactions data frame or account book with values
#' in an lsd list column.
#'
#' `deb_balance()` is based on [deb_open()]. The function sums the credits
#' and debits of the accounts that remain open to calculate the capital
#' remaining in the transactions data frame. The values for credit and debit
#' should be the same, as each credit also has a corresponding debit. The
#' exception is if there are missing values in `df` and `na.rm = FALSE`.
#'
#' `deb_balance()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts in an account
#' book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables contain the ids of the accounts
#' and act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account that the transactional value is
#' from, while the debit variable represents the account that receives the
#' value. Thus, from the credit account to the debit account.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account_summary
#'
#' @return Returns a tibble with two rows showing the credit and debit
#'   remaining in `df` in an lsd list column.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_as_lsd(list(c(10, 15, 6),
#'                             c(6, 3, 11),
#'                             c(4, 11, 7),
#'                             c(7, 11, 8),
#'                             c(9, 2, 11)),
#'                        bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Credit and debit remaining on trans
#' deb_balance(df = trans,
#'              credit = credit,
#'              debit = debit,
#'              lsd = lsd)
#'
#' @export

deb_balance <- function(df,
                         credit = credit, debit = debit,
                         lsd = lsd,
                         round = 5,
                         na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)
  column_name <- rlang::quo_name(lsd)

  lsd_bases <- rlang::eval_tidy(lsd, df)
  bases <- attributes(lsd_bases)$bases

  open <- deb_open(df,
                       credit = !! credit,
                       debit = !! debit,
                       lsd = !! lsd,
                       round = round,
                       na.rm = na.rm)

  temp_credit <- open %>%
    dplyr::filter(deb_lsd_d(!! lsd) > 0)
  if (nrow(temp_credit) < 1) {
    temp_credit <- to_lsd(as.numeric(c(NA, NA, NA)), bases)
  } else {
    temp_credit <- temp_credit %>%
      deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
      .[[1]]
  }

  temp_debit <- open %>%
    dplyr::filter(deb_lsd_d(!! lsd) < 0)
  if (nrow(temp_debit) < 1) {
    temp_debit <- to_lsd(as.numeric(c(NA, NA, NA)), bases)
  } else {
    temp_debit <- temp_debit %>%
      deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
      .[[1]] %>%
      purrr::map(`-`) # turn positive
  }

  tibble::tibble(relation = c("credit", "debit"),
                 # Create lsd list column with simplified c.lsd
                 !! column_name := list(temp_credit, temp_debit) %>%
                   purrr::flatten() %>%
                   to_lsd(attributes(temp_credit)$bases))
}
