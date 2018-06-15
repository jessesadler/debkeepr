## lsd-account functions ##

#' Calculate credit, debit, and current value of an account
#'
#' Calculate the total credit, debit, and the current value of a given account
#' in the form of pounds, shillings, and pence.
#'
#' `deb_account()`` is similar to [deb_account_summary()], but
#' it only returns the information for one account instead of all accounts in
#' the credit and/or debit variables of a transactions data frame.
#'
#' `deb_account()`` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#' @param df A data frame containing transactions between accounts that has
#'   values represented in the form of pounds, shillings, and pence variables.
#'   The data frame should have two variables for accounts (credit and debit)
#'   and variables for pounds, shillings, and pence.
#' @inheritParams deb_sum
#' @param account_id The id of the account to be used to calculate the credit,
#'   debit, and current values. The value for the account should be present in
#'   the credit and/or debit variables.
#' @param credit credit column: Unquoted name of the credit variable. This is
#'   the column from which the value of the transaction is from. Default
#'   is credit. The credit column must be of the same class as the debit
#'   column.
#' @param debit debit column: Unquoted name of the debit variable. This is
#'   the column to which the value of the transaction goes. Default
#'   is debit. The debit column must be of the same class as the credit
#'   column.
#'
#' @return Returns a data frame with three rows and four columns. The rows
#'   correspond to credit, debit, and current values in the form of
#'   pounds, shillings, and pence. The names for the pounds, shillings,
#'   and pence columns correspond to the input for `l`, `s`,
#'   and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Credit, debit, and current value of account "a"
#' deb_account(trans, account_id = "a", credit, debit, l , s, d)
#'
#' @export

deb_account <- function(df,
                        account_id,
                        credit = credit,
                        debit = debit,
                        l = l,
                        s = s,
                        d = d,
                        round = 3) {
  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns, account_id)
  lsd_column_check(df, l, s, d)

  # Column names
  l_column <- rlang::quo_name(l)
  s_column <- rlang::quo_name(s)
  d_column <- rlang::quo_name(d)

  credit <- df %>%
    dplyr::filter((!! credit) == account_id) %>%
    dplyr::summarise(
      relation = "credit",
      l = deb_librae_sum(!!l, !!s, !!d),
      s = deb_solidi_sum(!!l, !!s, !!d),
      d = deb_denarii_sum(!!l, !!s, !!d, round))

  debit <- df %>%
    dplyr::filter((!! debit) == account_id) %>%
    dplyr::summarise(
      relation = "debit",
      l = deb_librae_sum(!!l, !!s, !!d),
      s = deb_solidi_sum(!!l, !!s, !!d),
      d = deb_denarii_sum(!!l, !!s, !!d, round))

  credit_d <- deb_lsd_d(c(credit$l, credit$s, credit$d))
  debit_d <- deb_lsd_d(c(debit$l, debit$s, debit$d))

  denarii <- credit_d - debit_d
  lsd <- deb_d_lsd(denarii, round)

  current <- tibble::tibble(relation = "current", l = lsd[1], s = lsd[2], d = lsd[3])

  # Create account tibble and rename columns
  dplyr::bind_rows(credit, debit, current) %>%
    dplyr::rename(!! l_column := l, !! s_column := s, !! d_column := d)
}

#' Calculate credit, debit, and current values of accounts
#'
#' Calculate the total credit, debit, and the current values in the form of
#' pounds, shillings, and pence for all accounts in `df`. Credits and
#' debits are both returned as positive numbers. If an account has more
#' credits than debits, the current value will be returned as positive
#' numbers. If the debits are greater, the current value will be returned
#' as negative numbers.
#'
#' `deb_account_summary()` is similar to [deb_account()], but
#' it returns the information for all accounts in a transaction data frame
#' instead of one. If you only want to see the current values for each
#' account, see [deb_current()].
#'
#' `deb_account_summary()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with five columns and three rows for each account
#'   present in the credit and/or debit variables of `df`. This
#'   represents the total credit, debit, and current values of the accounts
#'   in the form of pounds, shillings, and pence. The names for the pounds,
#'   shillings, and pence columns correspond to the input for `l`, `s`,
#'   and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Credit, debit, and current values of accounts present in trans
#' deb_account_summary(trans, credit, debit, l, s, d)
#'
#' @export

deb_account_summary <- function(df,
                                credit = credit,
                                debit = debit,
                                l = l,
                                s = s,
                                d = d,
                                round = 3) {

  credit <- rlang::enquo(credit)
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  lsd_column_check(df, l, s, d)
  edge_columns <- c(rlang::quo_name(credit), rlang::quo_name(debit))
  credit_check(df, credit, debit, edge_columns)

  credits <- df %>%
    dplyr::group_by(!! credit) %>%
    dplyr::summarise(
      relation = "credit",
      denarii = round(decimalize_d(sum(!!l), sum(!!s), sum(!!d)), round)) %>%
    dplyr::rename(account_id = !! credit)

  debits <- df %>%
    dplyr::group_by(!! debit) %>%
    dplyr::summarise(
      relation = "debit",
      denarii = round(decimalize_d(sum(!!l), sum(!!s), sum(!!d)), round)) %>%
    dplyr::rename(account_id = !! debit)

  accounts_sum <- dplyr::mutate(debits, denarii = -denarii) %>%
    dplyr::bind_rows(credits)

  current <- accounts_sum %>%
    dplyr::group_by(account_id) %>%
    dplyr::summarise(
      relation = "current",
      denarii = sum(denarii))

  dplyr::bind_rows(credits, debits, current) %>%
    deb_d_mutate(denarii, l_column = !! l, s_column = !! s, d_column = !! d) %>%
    dplyr::arrange(account_id) %>%
    dplyr::select(-denarii)
}

#' Calculate the total credit of accounts
#'
#' Calculate the total credit of accounts in a transactions data frame (`df`)
#' in the form of pounds, shillings, and pence.
#'
#' `deb_credit()` is similar to [deb_account_summary()], but it only returns
#' the credit values for the accounts in `df`. See [deb_debit()] to return
#' the debit totals for the accounts in `df`.
#'
#' `deb_credit()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with four columns and one row for each account
#'   present in the transactions data frame (`df`). The values represent the
#'   total value sent by each account to other accounts within `df` in the
#'   form of pounds, shillings, and pence. The names for the pounds,
#'   shillings, and pence columns correspond to the input for `l`, `s`,
#'   and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Total credit of accounts present in trans
#' deb_credit(trans, credit, l, s, d)
#'
#' @export

deb_credit <- function(df,
                       credit = credit,
                       l = l,
                       s = s,
                       d = d,
                       round = 3) {
  credit <- rlang::enquo(credit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  edge_columns <- rlang::quo_name(credit)
  credit_check(df, credit, debit = NULL, edge_columns)
  lsd_column_check(df, l, s, d)

  dplyr::group_by(df, !! credit) %>%
    dplyr::summarise(
      l = deb_librae_sum(!!l, !!s, !!d),
      s = deb_solidi_sum(!!l, !!s, !!d),
      d = deb_denarii_sum(!!l, !!s, !!d, round)) %>%
    dplyr::rename(account_id = credit)
}

#' Calculate the total debit of accounts
#'
#' Calculate the total debit of accounts in a transactions data frame (`df`)
#' in the form of pounds, shillings, and pence.
#'
#' `deb_debit()` is similar to [deb_account_summary()], but it only returns
#' the debit totals for the accounts in `df`. See [deb_credit()] to return
#' the credit totals for the accounts in `df`.
#'
#' `deb_debit()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with four columns and one row for each account
#'   present in the transactions data frame (`df`). The values represent the
#'   total value sent by each account to other accounts within `df` in the
#'   form of pounds, shillings, and pence. The names for the pounds,
#'   shillings, and pence columns correspond to the input for `l`, `s`,
#'   and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Total debit of accounts present in trans
#' deb_debit(trans, debit, l, s, d)
#'
#' @export

deb_debit <- function(df,
                      debit = debit,
                      l = l,
                      s = s,
                      d = d,
                      round = 3) {
  debit <- rlang::enquo(debit)
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  edge_columns <- rlang::quo_name(debit)
  credit_check(df, credit = NULL, debit, edge_columns)
  lsd_column_check(df, l, s, d)

  dplyr::group_by(df, !! debit) %>%
    dplyr::summarise(
      l = deb_librae_sum(!!l, !!s, !!d),
      s = deb_solidi_sum(!!l, !!s, !!d),
      d = deb_denarii_sum(!!l, !!s, !!d, round)) %>%
    dplyr::rename(account_id = debit)
}

#' Calculate the current values of accounts
#'
#' Calculate the current values in the form of pounds, shillings, and pence
#' for all accounts in `df`. If an account has more credits than debits,
#' the current value will be returned as positive numbers. If the debits are
#' greater, the current value will be returned as negative numbers.
#'
#' `deb_current()` is similar to [deb_account_summary()],
#' but it only returns the current values for the accounts in `df`.
#' To see only the open accounts—only those accounts that have a current
#' value greater than or less than zero—see [deb_open()].
#'
#' `deb_current()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with five columns and one row for each account
#'   present in the credit and/or debit variables of `df`. This
#'   represents the current value of the accounts in the form of pounds,
#'   shillings, and pence. The names for the pounds, shillings, and pence
#'   columns correspond to the input for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Current values of accounts present in trans
#' deb_current(trans, credit, debit, l, s, d)
#'
#' @export

deb_current <- function(df,
                        credit = credit,
                        debit = debit,
                        l = l,
                        s = s,
                        d = d,
                        round = 3) {
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
                      round = round) %>%
    dplyr::filter(relation == "current") %>%
    dplyr::select(-relation)
}

#' Calculate the current values of open accounts
#'
#' Calculate the current values in the form of pounds, shillings, and pence
#' for all accounts in `df` and show only those accounts that have a
#' positive or negative balance. If an account has more credits than
#' debits, the current value will be returned as positive numbers. If the
#' debits are greater, the current value will be returned as negative numbers.
#'
#' `deb_open()` is similar to [deb_current()], but it only returns current
#' values for accounts that have a value that is not zero. If accounts that
#' should be closed appear in the results with only a small fraction of a
#' pence remaining, set then `round`` argument to a lower decimal place.
#'
#' `deb_open()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with five columns and one row for each account
#'   present in the credit and/or debit variables in `df` in which the
#'   current value of pounds, shillings, and pence does not equal zero. The
#'   names for the pounds, shillings, and pence columns correspond to the
#'   input for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Current values of open accounts present in trans
#' deb_open(trans, credit, debit, l, s, d)
#'
#' @export

deb_open <- function(df,
                     credit = credit,
                     debit = debit,
                     l = l,
                     s = s,
                     d = d,
                     round = 3) {
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
              round = round) %>%
    dplyr::filter(!!l + !!s/20 + !!d/240 != 0)
}

#' Calculate the balance of a transactions data frame
#'
#' Calculate the balance remaining on `df` in the form of pounds,
#' shillings, and pence. This shows the total credit and debit remaining
#' on the transactions data frame or account book.
#'
#' `deb_balance()` is based on [deb_open()]. The function adds the credits
#' and debits of the accounts that remain open to calculate the capital
#' remaining in the transactions data frame. The values for credit and debit
#' should be the same, as each credit also has a corresponding debit.
#'
#' `deb_balance()` is part of a family of functions meant to be used on
#' data frames that contain transactions between accounts likely contained in
#' an account book. The data frame should possess a similar structure to a
#' [directed edge list](https://www.jessesadler.com/post/network-analysis-with-r/#nodes-edges).
#' In this context, credit and debit variables filled with the ids of the
#' accounts act as the edges of the network. Following the
#' [nomenclature of accounting](https://en.wikipedia.org/wiki/Debits_and_credits),
#' the credit variable represents the account from which a value goes out,
#' while the debit variable represents the account that receives the value.
#' Thus, from the credit account to the debit account.
#'
#' The value for the transactions should be in the non-decimal currency of
#' pounds, shillings, and pence. These functions use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd account functions
#'
#' @inheritParams deb_account
#'
#' @return Returns a tibble with two rows showing the credit and debit
#'   remaining in `df` in the form of pounds, shillings, and pence.
#'   The names for the pounds, shillings, and pence columns correspond to
#'   the input for `l`, `s`, and `d`.
#'
#' @examples
#' # Create a transactions data frame
#' trans <- data.frame(credit = c("a", "b", "a", "c"),
#'                     debit = c("b", "a", "c", "a"),
#'                     l = c(10, 10, 7, 9),
#'                     s = c(15, 15, 11, 2),
#'                     d = c(6, 6, 8, 11))
#'
#' # Credit and debit remaining on trans
#' deb_balance(trans, credit, debit, l, s, d)
#'
#' @export

deb_balance <- function(df,
                        credit = credit,
                        debit = debit,
                        l = l,
                        s = s,
                        d = d,
                        round = 3) {
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
                     round = round)
  credit <- open %>%
    dplyr::filter(!! l + !! s + !! d > 0) %>%
    deb_sum(l = !! l,
            s = !! s,
            d = !! d,
            round = round)

  debit <- open %>%
    dplyr::filter(!! l + !! s + !! d < 0) %>%
    deb_sum(l = !! l,
            s = !! s,
            d = !! d,
            round = round) %>%
    # Make lsd positive
    dplyr::mutate(!! l_column := -(!! l),
                  !! s_column := -(!! s),
                  !! d_column := -(!! d))

  dplyr::bind_rows(credit, debit) %>%
    tibble::add_column(relation = c("credit", "debit"), .before = 1)
}
