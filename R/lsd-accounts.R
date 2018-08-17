## lsd-account functions ##

#' Calculate credit, debit, and current value of an account
#'
#' Calculate the total credit, debit, and current value of a given account in
#' a transactions data frame.
#'
#' `deb_account()` is similar to [deb_account_summary()], but
#' it only returns the information for one account instead of all accounts in
#' the credit and/or debit variables of a transactions data frame.
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
#' @param df A data frame containing transactions between accounts with
#'   variables for the credit and debit accounts and values in the form of
#'   pounds, shillings, and pence variables.
#' @inheritParams deb_normalize_df
#' @inheritParams deb_sum_df
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
#' @param round Round pence to specified number of decimal places. Default
#'   is 5. Rounding of the pence value takes place throughout the function and
#'   is not simply done at the end of the function call. This means that the
#'   credit, debit, and current values will add together correctly. However,
#'   the rounded values may be different than running the function without
#'   rounding and rounding pence values at the end.
#'
#' @return Returns a data frame with three rows and four columns. The rows
#'   correspond to credit, debit, and current values in the form of
#'   pounds, shillings, and pence. The names for the pounds, shillings,
#'   and pence variables correspond to the input for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Credit, debit, and current value of account "a"
#' deb_account(df = trans, account_id = "a",
#'             credit = credit, debit = debit,
#'             l = l, s = s, d = d)
#'
#' @export

deb_account <- function(df,
                        account_id,
                        credit = credit,
                        debit = debit,
                        l = l,
                        s = s,
                        d = d,
                        bases = c(20, 12),
                        round = 5,
                        na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns, account_id)
  lsd_column_check(df, l, s, d)
  bases_check(bases)
  round_check(round)

  # Column names
  l_column <- rlang::quo_name(l)
  s_column <- rlang::quo_name(s)
  d_column <- rlang::quo_name(d)

  credit <- df %>%
    dplyr::filter((!! credit) == account_id) %>%
    deb_sum_df(!! l, !! s, !! d,
            bases = bases,
            round = round,
            na.rm = na.rm) %>%
    dplyr::mutate(denarii = decimalize_d(!! l, !! s, !! d, bases, round = round))

  debit <- df %>%
    dplyr::filter((!! debit) == account_id) %>%
    deb_sum_df(!! l, !! s, !! d,
            bases = bases,
            round = round,
            na.rm = na.rm) %>%
    dplyr::mutate(denarii = decimalize_d(!! l, !! s, !! d, bases, round = round))

  lsd <- deb_d_lsd(credit$denarii - debit$denarii, bases = bases, round = round)
  lsd <- unlist(lsd)

  current <- tibble::tibble(!! l_column := lsd[1],
                            !! s_column := lsd[2],
                            !! d_column := lsd[3])

  # Create account tibble and rename columns
  dplyr::bind_rows(credit, debit, current) %>%
    tibble::add_column(relation = c("credit", "debit", "current"), .before = 1) %>%
    dplyr::select(-.data$denarii)
}

#' Calculate credit, debit, and current values of accounts
#'
#' Calculate the total credit, debit, and the current values for all accounts
#' in a transaction data frame. Credits and debits are both returned as
#' positive numbers. If an account has more credit than debit, the current
#' value will be returned as positive values. If the debit is greater, the
#' current value will be returned as negative values.
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
#' @return Returns a tibble with five columns and three rows for each account
#'   present in the credit and/or debit variables of `df`. This represents the
#'   total credit, debit, and current values of the accounts. The names for the
#'   pounds, shillings, and pence columns correspond to the input for `l`, `s`,
#'   and `d`. If an account has zero credit or debit transactions, it will not
#'   have a summary row for that type of relation.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Credit, debit, and current values of accounts present in trans
#' deb_account_summary(df = trans,
#'                     credit = credit, debit = debit,
#'                     l = l, s = s, d = d)
#'
#' @export

deb_account_summary <- function(df,
                                credit = credit,
                                debit = debit,
                                l = l,
                                s = s,
                                d = d,
                                bases = c(20, 12),
                                round = 5,
                                na.rm = FALSE) {

  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  lsd_column_check(df, l, s, d)
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns)
  bases_check(bases)
  round_check(round)

  # Make l, s, and d NA in any row that has an NA
  if (na.rm == TRUE) {
    df <- deb_normalize_df(df, !!l, !!s, !!d, bases, replace = TRUE)
  }

  credits <- df %>%
    dplyr::group_by(!! credit) %>%
    dplyr::summarise(
      relation = "credit",
      denarii = decimalize_d(l = sum(!!l, na.rm = na.rm),
                             s = sum(!!s, na.rm = na.rm),
                             d = sum(!!d, na.rm = na.rm),
                             bases = bases,
                             round = round)) %>%
    dplyr::rename(account_id = !! credit)

  debits <- df %>%
    dplyr::group_by(!! debit) %>%
    dplyr::summarise(
      relation = "debit",
      denarii = decimalize_d(l = sum(!!l, na.rm = na.rm),
                             s = sum(!!s, na.rm = na.rm),
                             d = sum(!!d, na.rm = na.rm),
                             bases = bases,
                             round = round)) %>%
    dplyr::rename(account_id = !! debit)

  accounts_sum <- dplyr::mutate(debits, denarii = -denarii) %>%
    dplyr::bind_rows(credits)

  current <- accounts_sum %>%
    dplyr::group_by(.data$account_id) %>%
    dplyr::summarise(
      relation = "current",
      denarii = sum(denarii))

  dplyr::bind_rows(credits, debits, current) %>%
    deb_d_lsd_mutate(denarii, l_column = !! l, s_column = !! s, d_column = !! d,
                     bases = bases,
                     round = round) %>%
    dplyr::arrange(.data$account_id) %>%
    dplyr::select(-denarii)
}

#' Calculate the total credit of accounts
#'
#' Calculate the total credit of accounts in a transactions data frame.
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
#' @return Returns a tibble with four columns and one row for each account
#'   present in the transactions data frame (`df`). The values represent the
#'   total value sent by each account to other accounts within `df`. The names
#'   for the pounds, shillings, and pence variables correspond to the input
#'   for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Total credit of accounts present in trans
#' deb_credit(df = trans, credit = credit,
#'            l = l, s = s, d = d)
#'
#' @export

deb_credit <- function(df,
                       credit = credit,
                       l = l,
                       s = s,
                       d = d,
                       bases = c(20, 12),
                       round = 5,
                       na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  edge_columns <- rlang::quo_name(credit)
  credit_check(df, credit, debit = NULL, edge_columns)
  lsd_column_check(df, l, s, d)
  bases_check(bases)
  round_check(round)

  dplyr::group_by(df, !! credit) %>%
    deb_sum_df(!! l, !! s, !! d, bases = bases, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! credit)
}

#' Calculate the total debit of accounts
#'
#' Calculate the total debit of accounts in a transactions data frame.
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
#' @return Returns a tibble with four columns and one row for each account
#'   present in the transactions data frame (`df`). The values represent the
#'   total value received by each account from other accounts within `df`. The
#'   names for the pounds, shillings, and pence variables correspond to the
#'   input for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Total debit of accounts present in trans
#' deb_debit(df = trans, debit = debit,
#'           l = l, s = s, d = d)
#'
#' @export

deb_debit <- function(df,
                      debit = debit,
                      l = l,
                      s = s,
                      d = d,
                      bases = c(20, 12),
                      round = 5,
                      na.rm = FALSE) {
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  edge_columns <- rlang::quo_name(debit)
  credit_check(df, credit = NULL, debit, edge_columns)
  lsd_column_check(df, l, s, d)
  bases_check(bases)
  round_check(round)

  dplyr::group_by(df, !! debit) %>%
    deb_sum_df(!! l, !! s, !! d, bases = bases, round = round, na.rm = na.rm) %>%
    dplyr::rename(account_id = !! debit)
}

#' Calculate the current values of accounts
#'
#' Calculate the current values of accounts in a transactions data frame. If an
#' account has more credit than debit, the current value will be returned as
#' positive values. If the debit is greater, the current value will be returned
#' as negative values.
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
#' @return Returns a tibble with four columns and one row for each account
#'   present in the credit and/or debit variables of `df`. This represents the
#'   current value of the accounts in `df`. The names for the pounds,
#'   shillings, and pence variables correspond to the input for `l`, `s`, and
#'   `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Current values of accounts present in trans
#' deb_current(df = trans,
#'             credit = credit, debit = debit,
#'             l = l, s = s, d = d)
#' @export

deb_current <- function(df,
                        credit = credit,
                        debit = debit,
                        l = l,
                        s = s,
                        d = d,
                        bases = c(20, 12),
                        round = 5,
                        na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  deb_account_summary(df,
                      credit = !! credit,
                      debit = !! debit,
                      l = !! l,
                      s = !! s,
                      d = !! d,
                      bases = bases,
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
#' @return Returns a tibble with four columns and one row for each account
#'   present in the credit and/or debit variables in `df` in which the
#'   current value of pounds, shillings, and pence does not equal zero or
#'   does not have a missing value in the current value. The names for the
#'   pounds, shillings, and pence variables correspond to the input for `l`,
#'   `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Current values of open accounts present in trans
#' deb_open(df = trans,
#'          credit = credit, debit = debit,
#'          l = l, s = s, d = d)
#'
#' @export

deb_open <- function(df,
                     credit = credit,
                     debit = debit,
                     l = l,
                     s = s,
                     d = d,
                     bases = c(20, 12),
                     round = 5,
                     na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  deb_current(df,
              credit = !! credit,
              debit = !! debit,
              l = !! l,
              s = !! s,
              d = !! d,
              bases = bases,
              round = round,
              na.rm = na.rm) %>%
    dplyr::filter(dplyr::near(!! l + !! s / bases[1] + !! d / prod(bases), 0) == FALSE)
}

#' Calculate the balance of a transactions data frame
#'
#' Calculate the balance remaining in `df`. This shows the total credit and
#' debit remaining in the transactions data frame or account book.
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
#'   remaining in `df`. The names for the pounds, shillings, and pence
#'   variables correspond to the input for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "b", "a", "c"),
#'                     debit = c("b", "a", "a", "c", "a"),
#'                     l = c(10, 6, 4, 7, 9),
#'                     s = c(15, 3, 11, 11, 2),
#'                     d = c(6, 11, 7, 8, 11))
#'
#' # Credit and debit remaining on trans
#' deb_balance(df = trans,
#'             credit = credit, debit = debit,
#'             l = l, s = s, d = d)
#'
#' @export

deb_balance <- function(df,
                        credit = credit,
                        debit = debit,
                        l = l,
                        s = s,
                        d = d,
                        bases = c(20, 12),
                        round = 5,
                        na.rm = FALSE) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)
  # Column names used to mutate debit lsd to positive values
  l_column <- rlang::quo_name(l)
  s_column <- rlang::quo_name(s)
  d_column <- rlang::quo_name(d)

  open <- deb_open(df,
                   credit = !! credit,
                   debit = !! debit,
                   l = !! l,
                   s = !! s,
                   d = !! d,
                   bases = bases,
                   round = round,
                   na.rm = na.rm)
  credit <- open %>%
    dplyr::filter(!! l + !! s + !! d > 0) %>%
    deb_sum_df(l = !! l,
            s = !! s,
            d = !! d,
            bases = bases,
            round = round,
            na.rm = na.rm)

  debit <- open %>%
    dplyr::filter(!! l + !! s + !! d < 0) %>%
    deb_sum_df(l = !! l,
            s = !! s,
            d = !! d,
            bases = bases,
            round = round,
            na.rm = na.rm) %>%
    # Make lsd positive
    dplyr::mutate(!! l_column := -(!! l),
                  !! s_column := -(!! s),
                  !! d_column := -(!! d))

  dplyr::bind_rows(credit, debit) %>%
    tibble::add_column(relation = c("credit", "debit"), .before = 1)
}
