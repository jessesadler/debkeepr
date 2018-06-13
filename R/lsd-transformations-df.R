## lsd transformations mutate ##

## Helper functions ##
decimalize_l <- function(l, s, d) {
  l + s/20 + d/240
}

decimalize_s <- function(l, s, d) {
  l * 20 + s + d/12
}

decimalize_d <- function(l, s, d) {
  l * 240 + s * 12 + d
}


deb_lsd_l_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = pounds) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_l(l, s, d))
}

deb_lsd_s_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = shillings) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_s(l, s, d))
}

deb_lsd_d_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = pence) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_d(l, s, d))
}
