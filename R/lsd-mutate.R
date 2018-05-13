## lsd mutate functions ##

### Helper functions to decimal to separate l, s, d ###

## Transform separate l, s, d to separate l, s, and d ##

## librae to l, s, d ##
deb_librae_l <- function(l) {
  dplyr::if_else(l < 0, ceiling(l), floor(l))
}

deb_librae_s <- function(l) {
  # vectorize
  if (length(l) > 1) {
    return(purrr::map_dbl(l, deb_librae_s))
  }

  if (l == round(l)) {
    0
  } else {
    if (l > 0) {
      solidi <- (l - floor(l)) * 20
    } else {
      solidi <- (l - ceiling(l)) * 20
    }
    if (solidi == round(solidi)) {
      solidi
    } else {
      if (solidi > 0) {
        floor(solidi)
      } else {
        ceiling(solidi)
      }
    }
  }
}

deb_librae_d <- function(l, round = 3) {
  # vectorize
  if (length(l) > 1) {
    return(purrr::map_dbl(l, deb_librae_d))
  }

  if (l == round(l)) {
    return(0)
  } else {
    if (l > 0) {
      solidi <- (l - floor(l)) * 20
      denarii <- (solidi - floor(solidi)) * 12
    } else {
      solidi <- (l - ceiling(l)) * 20
      denarii <- (solidi - ceiling(solidi)) * 12
    }
    round(denarii, round)
  }
}

## solidi to l, s, d ##
deb_solidi_l <- function(s) {
  dplyr::if_else(s < 0, -(-s %/% 20), s %/% 20)
}

deb_solidi_s <- function(s) {
  # vectorize
  if (length(s) > 1) {
    return(purrr::map_dbl(s, deb_solidi_s))
  }

  solidi <- dplyr::if_else(s < 0, -(-s %% 20), s %% 20)
  if (solidi == round(solidi)) {
    solidi
  } else {
    if (solidi > 0) {
      floor(solidi)
    } else {
      ceiling(solidi)
    }
  }
}

deb_solidi_d <- function(s, round = 3) {
  # vectorize
  if (length(s) > 1) {
    return(purrr::map_dbl(s, deb_solidi_d))
  }

  if (s == round(s)) {
    return(0)
  } else {
    if (s > 0) {
      denarii <- (s - floor(s)) * 12
    } else {
      denarii <- (s - ceiling(s)) * 12
    }
    round(denarii, round)
  }
}

## denarii to l, s, d ##
deb_denarii_l <- function(d) {
  dplyr::if_else(d < 0, -((-d %/% 12) %/% 20), (d %/% 12) %/% 20)
}

deb_denarii_s <- function(d) {
  dplyr::if_else(d < 0, -((-d %/% 12) %% 20), (d %/% 12) %% 20)
}
deb_denarii_d <- function(d) {
  dplyr::if_else(d < 0, -(-d %% 12), d %% 12)
}

## Mutate l, s, and d, to separate l, s, and d ##

### mutate l to l, s, d
deb_l_mutate <- function(df, l,
                         l_column = "l",
                         s_column = "s",
                         d_column = "d",
                         round = 3) {
  l <- dplyr::enquo(l)

  # Column names
  l_column <- paste(dplyr::quo_name(l_column))
  s_column <- paste(dplyr::quo_name(s_column))
  d_column <- paste(dplyr::quo_name(d_column))

  df %>%
    dplyr::mutate(!! l_column := deb_librae_l(!!l),
                  !! s_column := deb_librae_s(!!l),
                  !! d_column := deb_librae_d(!!l, round))
}

### mutate s to l, s, d
deb_s_mutate <- function(df, s,
                         l_column = "l",
                         s_column = "s",
                         d_column = "d",
                         round = 3) {
  s <- dplyr::enquo(s)

  # Column names
  l_column <- paste(dplyr::quo_name(l_column))
  s_column <- paste(dplyr::quo_name(s_column))
  d_column <- paste(dplyr::quo_name(d_column))

  df %>%
    dplyr::mutate(!! l_column := deb_solidi_l(!!s),
                  !! s_column := deb_solidi_s(!!s),
                  !! d_column := deb_solidi_d(!!s, round))
}

### mutate d to l, s, d
deb_d_mutate <- function(df, d,
                         l_column = "l",
                         s_column = "s",
                         d_column = "d") {
  d <- dplyr::enquo(d)

  # Column names
  l_column <- paste(dplyr::quo_name(l_column))
  s_column <- paste(dplyr::quo_name(s_column))
  d_column <- paste(dplyr::quo_name(d_column))

  df %>%
    dplyr::mutate(!! l_column := deb_denarii_l(!!d),
                  !! s_column := deb_denarii_s(!!d),
                  !! d_column := deb_denarii_d(!!d))
}
