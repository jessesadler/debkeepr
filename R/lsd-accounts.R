## lsd-account functions ##

deb_account <- function(df,
                        account_id,
                        credit = from,
                        debit = to,
                        l = l,
                        s = s,
                        d = d,
                        round = 3) {
  credit <- dplyr::enquo(credit)
  debit <- dplyr::enquo(debit)
  l <- dplyr::enquo(l)
  s <- dplyr::enquo(s)
  d <- dplyr::enquo(d)
  # Column names
  l_column <- dplyr::quo_name(l)
  s_column <- dplyr::quo_name(s)
  d_column <- dplyr::quo_name(d)

  credit <- df %>%
    dplyr::filter((!! credit) == account_id) %>%
    dplyr::summarise(
      relation = "credit",
      l = deb_librae(sum(!!l), sum(!!s), sum(!!d)),
      s = deb_solidi(sum(!!l), sum(!!s), sum(!!d)),
      d = deb_denarii(sum(!!l), sum(!!s), sum(!!d), round))

  debit <- df %>%
    dplyr::filter((!! debit) == account_id) %>%
    dplyr::summarise(
      relation = "debit",
      l = deb_librae(sum(!!l), sum(!!s), sum(!!d)),
      s = deb_solidi(sum(!!l), sum(!!s), sum(!!d)),
      d = deb_denarii(sum(!!l), sum(!!s), sum(!!d), round))

  credit_d <- deb_lsd_d(credit$l, credit$s, credit$d)
  debit_d <- deb_lsd_d(debit$l, debit$s, debit$d)

  denarii <- (credit_d - debit_d)

  current <- dplyr::bind_cols(relation = "current", deb_d_lsd(denarii, vector = FALSE, round))

  dplyr::bind_rows(credit, debit, current) %>%
    dplyr::rename(!! l_column := l, !! s_column := s, !! d_column := d)
}

deb_account_summary <- function(df,
                                credit = from,
                                debit = to,
                                l = l,
                                s = s,
                                d = d,
                                round = 3) {
  credit <- dplyr::enquo(credit)
  debit <- dplyr::enquo(debit)
  l <- dplyr::enquo(l)
  s <- dplyr::enquo(s)
  d <- dplyr::enquo(d)

  credits <- df %>%
    dplyr::group_by(!! credit) %>%
    dplyr::summarise(
      relation = "credit",
      denarii = deb_lsd_d(sum(!!l), sum(!!s), sum(!!d), round)) %>%
    dplyr::rename(id = !! credit)

  debits <- df %>%
    dplyr::group_by(!! debit) %>%
    dplyr::summarise(
      relation = "debit",
      denarii = -(deb_lsd_d(sum(!!l), sum(!!s), sum(!!d), round))) %>%
    dplyr::rename(id = !! debit)

  accounts_sum <- dplyr::bind_rows(credits, debits)

  current <- accounts_sum %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      relation = "current",
      denarii = sum(denarii))

  dplyr::bind_rows(accounts_sum, current) %>%
    deb_d_mutate(denarii, l_column = !! l, s_column = !! s, d_column = !! d) %>%
    dplyr::arrange(id) %>%
    dplyr::select(-denarii)
}

deb_current <- function(df,
                        credit = from,
                        debit = to,
                        l = l,
                        s = s,
                        d = d,
                        round = 3) {
  credit <- dplyr::enquo(credit)
  debit <- dplyr::enquo(debit)
  l <- dplyr::enquo(l)
  s <- dplyr::enquo(s)
  d <- dplyr::enquo(d)

  temp <- deb_account_summary(df,
                              credit = !! credit,
                              debit = !! debit,
                              l = !! l,
                              s = !! s,
                              d = !! d,
                              round = round)
  temp %>%
    dplyr::filter(relation == "current") %>%
    dplyr::select(-relation)
}


deb_open <- function(df,
                     credit = from,
                     debit = to,
                     l = l,
                     s = s,
                     d = d,
                     round = 3) {
  credit <- dplyr::enquo(credit)
  debit <- dplyr::enquo(debit)
  l <- dplyr::enquo(l)
  s <- dplyr::enquo(s)
  d <- dplyr::enquo(d)

  temp <- deb_current(df,
                      credit = !! credit,
                      debit = !! debit,
                      l = !! l,
                      s = !! s,
                      d = !! d,
                      round = round)

  temp %>%
    dplyr::filter(!! l + !! s/20 + !! d/240 != 0)
}
