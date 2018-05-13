## lsd interest calculations ##

## Calculate interest
# Works by mutating lsd to denarii,
# calculating interest, then back to lsd
deb_interest <- function(l, s, d,
                         interest = 0.0625,
                         years = 1,
                         with_princple = TRUE,
                         round = 3,
                         vector = FALSE) {
  principle_d <- deb_lsd_d(l, s, d)
  interest_d <-  principle_d * interest * years
  if (with_princple == TRUE) {
    deb_d_lsd(principle_d + interest_d, round, vector)
  } else {
    deb_d_lsd(interest_d, round, vector)
  }
}

deb_interest_mutate <- function(df, l = l, s = s, d = d,
                                interest = 0.0625,
                                years = 1,
                                with_princple = TRUE,
                                suffix = "interest",
                                round = 3) {
  l <- dplyr::enquo(l)
  s <- dplyr::enquo(s)
  d <- dplyr::enquo(d)
  # Column names
  l_column <- paste0(dplyr::quo_name(l), "_", suffix)
  s_column <- paste0(dplyr::quo_name(s), "_", suffix)
  d_column <- paste0(dplyr::quo_name(d), "_", suffix)

  lsd_column_check(df, l, s, d)

  if (with_princple == TRUE) {
    temp <- df %>%
      dplyr::mutate(temp_principle_d = deb_lsd_d(!!l, !!s, !!d),
                    temp_interest_d = temp_principle_d * interest * years,
                    !! l_column := deb_denarii_l(temp_principle_d + temp_interest_d),
                    !! s_column := deb_denarii_s(temp_principle_d + temp_interest_d),
                    !! d_column := deb_denarii_d(temp_principle_d + temp_interest_d))
  } else {
    temp <- df %>%
      dplyr::mutate(temp_principle_d = deb_lsd_d(!!l, !!s, !!d),
                    temp_interest_d = temp_principle_d * interest * years,
                    !! l_column := deb_denarii_l(temp_interest_d),
                    !! s_column := deb_denarii_s(temp_interest_d),
                    !! d_column := deb_denarii_d(temp_interest_d))
  }
  # Remove denarii columns
  temp %>%
    dplyr::select(-temp_principle_d, -temp_interest_d)
}
