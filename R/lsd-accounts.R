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
#'   is 5. Rounding to whole numbers may give results that do not add together
#'   due to rounding rules.
#' @inheritParams deb_sum
#'
#' @return Returns a tibble with three rows with the credit, debit, and current
#'   values in an lsd list column.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_lsd(l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11),
#'                     bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Credit, debit, and current value of account "a"
#' deb_account(df = trans,
#'             account_id = "a",
#'             credit = credit,
#'             debit = debit,
#'             lsd = lsd)
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

  # Get bases for lsd column to use for deb_d_lsd
  lsd_bases <- rlang::eval_tidy(lsd, df)
  bases <- attributes(lsd_bases)$bases

  temp_credit <- dplyr::filter(df, !! credit == account_id)
  if (nrow(temp_credit) < 1) {
    credit_lsd <- to_lsd(c(0, 0, 0), bases = bases)
  } else {
    credit_lsd <- deb_summarise(temp_credit, !! lsd, round = round, na.rm = na.rm) %>%
      .[[1]]
  }

  temp_debit <- dplyr::filter(df, !! debit == account_id)
  if (nrow(temp_debit) < 1) {
    debit_lsd <- to_lsd(c(0, 0, 0), bases = bases)
  } else {
    debit_lsd <- deb_summarise(temp_debit, !! lsd, round = round, na.rm = na.rm) %>%
      .[[1]]
  }

  current_lsd <- deb_subtract(credit_lsd, debit_lsd)

  tibble::tibble(relation = c("credit", "debit", "current"),
                 # Create lsd list column with simplified c.lsd
                 !! column_name := list(credit_lsd, debit_lsd, current_lsd) %>%
                   purrr::flatten() %>%
                   to_lsd(bases = bases))
}

#' Calculate credit, debit, and current values of accounts
#'
#' Calculate the total credit, debit, and the current values for all accounts
#' in a transaction data frame with an lsd list column. Credits and debits are
#' both returned as lsd list columns with positive values. If an account has
#' more credit than debit, the current value will be returned as positive
#' values. If the debit is greater, the current value will be returned as
#' negative values.
#'
#' `deb_account_summary()` is similar to [deb_account()], but it returns the
#' values for all accounts in a transaction data frame instead of one.
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
#' @return Returns a tibble with one row for each account present in `df` and
#'   three lsd list columns representing the total credit, debit, and current
#'   values of the accounts.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_lsd(l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11),
#'                     bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Credit, debit, and current values of accounts present in trans
#' deb_account_summary(df = trans,
#'                     credit = credit,
#'                     debit = debit,
#'                     lsd = lsd)
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

  # checks
  lsd_list_column_check(df, lsd)
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns)
  round_check(round)

  temp_credit <- dplyr::group_by(df, !! credit) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! credit, credit = !! lsd)
  temp_debit <- dplyr::group_by(df, !! debit) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! debit, debit = !! lsd)

  dplyr::full_join(temp_credit, temp_debit, by = "account_id") %>%
    dplyr::mutate_at(c("credit", "debit"), deb_replace_null) %>%
    dplyr::arrange(account_id) %>%
    dplyr::mutate(current = deb_subtract(lsd1 = credit, lsd2 = debit, round = round))
}

#' Calculate the total credit of accounts
#'
#' Calculate the total credit of accounts in a transactions data frame with an
#' lsd list column.
#'
#' `deb_credit()` is similar to [deb_account_summary()], but it only returns
#' the credit values for the accounts in `df`. Accounts that are not creditor
#' in any transactions appear with a value of £0. See [deb_debit()] to return
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
#' lsd_list <- deb_lsd(l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11),
#'                     bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Total credit of accounts present in trans
#' deb_credit(df = trans,
#'            credit = credit,
#'            lsd = lsd)
#'
#' @export

deb_credit <- function(df,
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
  credit_check(df, credit, debit, edge_columns)
  round_check(round)

  dplyr::group_by(df, !! credit) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! credit) %>%
    # Add any accounts that do not have a credit
    dplyr::full_join(dplyr::distinct(df, !! debit),
                     by = c("account_id" = rlang::quo_name(debit))) %>%
    dplyr::mutate(!! column_name := deb_replace_null(!! lsd, c(0, 0, 0))) %>%
    dplyr::arrange(account_id)
}

#' Calculate the total debit of accounts
#'
#' Calculate the total debit of accounts in a transactions data frame with an
#' lsd list column.
#'
#' `deb_debit()` is similar to [deb_account_summary()], but it only returns
#' the debit values for the accounts in `df`. Accounts that are not debtor in
#' any transactions appear with a value of £0. See [deb_credit()] to return the
#' credit totals for the accounts in `df`.
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
#' lsd_list <- deb_lsd(l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11),
#'                     bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Total debit of accounts present in trans
#' deb_debit(df = trans,
#'           debit = debit,
#'           lsd = lsd)
#'
#' @export

deb_debit <- function(df,
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
  credit_check(df, credit, debit, edge_columns)
  round_check(round)

  dplyr::group_by(df, !! debit) %>%
    deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! debit) %>%
    # Add any accounts that do not have a debit
    dplyr::full_join(dplyr::distinct(df, !! credit),
                     by = c("account_id" = rlang::quo_name(credit))) %>%
    dplyr::mutate(!! column_name := deb_replace_null(!! lsd, c(0, 0, 0))) %>%
    dplyr::arrange(account_id)
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
#' lsd_list <- deb_lsd(l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11),
#'                     bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Current values of accounts present in trans
#' deb_current(df = trans,
#'             credit = credit,
#'             debit = debit,
#'             lsd = lsd)
#' @export

deb_current <- function(df,
                        credit = credit, debit = debit,
                        lsd = lsd,
                        round = 5,
                        na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  lsd <- rlang::enquo(lsd)
  column_name <- rlang::quo_name(lsd)

  deb_account_summary(df,
                       credit = !! credit,
                       debit = !! debit,
                       lsd = !! lsd,
                       round = round,
                       na.rm = na.rm) %>%
    dplyr::select(account_id, !! column_name := current)
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
#' @return Returns a tibble with one row for each account in `df` where the
#'   current value in an lsd list column does not equal zero or does not have
#'   missing values in the current value. If all accounts are equal to zero or
#'   possess missing values, a tibble with zero rows will be returned.
#'
#' @examples
#' library(tibble)
#'
#' # Create a transactions data frame
#' lsd_list <- deb_lsd(l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11),
#'                     bases = c(20, 12))
#' trans <- tibble(credit = c("a", "b", "b", "a", "c"),
#'                 debit = c("b", "a", "a", "c", "a"),
#'                 lsd = lsd_list)
#'
#' # Current values of open accounts present in trans
#' deb_open(df = trans,
#'          credit = credit,
#'          debit = debit,
#'          lsd = lsd)
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
#' exceptions are if there are missing values in `df` and `na.rm = FALSE`, or
#' if the `round` argument is set to a low value, leading rounding rules to
#' alter the returned values.
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

  current <- deb_current(df,
                        credit = !! credit,
                        debit = !! debit,
                        lsd = !! lsd,
                        round = round,
                        na.rm = na.rm)
  # if all accounts are balanced
  zeros <- deb_lsd(l = rep(0, nrow(current)),
                   s = rep(0, nrow(current)),
                   d = rep(0, nrow(current)),
                   bases = bases)
  if (identical(current[[2]], zeros)) {
    tibble(relation = c("credit", "debit"),
           !! column_name := deb_as_lsd(list(c(0, 0, 0), c(0, 0, 0))))
  } else {
    # sum of credits or NA
    temp_credit <- current %>%
      dplyr::filter(deb_lsd_d(!! lsd) > 0)
    if (nrow(temp_credit) < 1) {
      temp_credit <- to_lsd(as.numeric(c(NA, NA, NA)), bases)
    } else {
      temp_credit <- temp_credit %>%
        deb_summarise(!! lsd, round = round, na.rm = na.rm) %>%
        .[[1]]
    }
    # sum of debits or NA
    temp_debit <- current %>%
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
}
