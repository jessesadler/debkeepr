## lsd mutate functions ##

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
    return(purrr::map_dbl(l, deb_librae_d, round))
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
    return(purrr::map_dbl(s, deb_solidi_d, round))
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

# Avoid overwriting l, s, and d columns
# and check that column names and suffix are character vectors of length 1
lsd_column_names <- function(df, l, s, d, suffix) {

  lsd_names <- c(dplyr::quo_name(l), dplyr::quo_name(s), dplyr::quo_name(d))

  if (!is.character(lsd_names)) {
    stop(call. = FALSE, "Column names must be character vectors")
  }

  if (length(lsd_names) != 3) {
    stop(call. = FALSE, "Column names must be character vectors of length 1")
  }

  if (!is.character(suffix)) {
    stop(call. = FALSE, "suffix must be a character vector")
  }

  if (length(suffix) != 1) {
    stop(call. = FALSE, "suffix must be a character vector of length 1")
  }

  if (any(lsd_names %in% names(df)) == TRUE) {
    lsd_names[1] <- paste0(lsd_names[1], suffix)
    lsd_names[2] <- paste0(lsd_names[2], suffix)
    lsd_names[3] <- paste0(lsd_names[3], suffix)
  }
  lsd_names
}


deb_l_mutate <- function(df, l,
                         l_column = l,
                         s_column = s,
                         d_column = d,
                         suffix = ".1",
                         round = 3) {
  l <- dplyr::enquo(l)

  # Column names: avoid overwriting l, s, and d columns
  lsd_names <- lsd_column_names(df,
                                dplyr::enquo(l_column),
                                dplyr::enquo(s_column),
                                dplyr::enquo(d_column),
                                suffix)

  df %>%
    dplyr::mutate(!! lsd_names[1] := deb_librae_l(!!l),
                  !! lsd_names[2] := deb_librae_s(!!l),
                  !! lsd_names[3] := deb_librae_d(!!l, round))
}

### mutate s to l, s, d ###
deb_s_mutate <- function(df, s,
                         l_column = l,
                         s_column = s,
                         d_column = d,
                         suffix = ".1",
                         round = 3) {
  s <- dplyr::enquo(s)

  # Column names: avoid overwriting l, s, and d columns
  lsd_names <- lsd_column_names(df,
                                dplyr::enquo(l_column),
                                dplyr::enquo(s_column),
                                dplyr::enquo(d_column),
                                suffix)

  df %>%
    dplyr::mutate(!! lsd_names[1] := deb_solidi_l(!!s),
                  !! lsd_names[2] := deb_solidi_s(!!s),
                  !! lsd_names[3] := deb_solidi_d(!!s, round))
}

### mutate d to l, s, d ###
deb_d_mutate <- function(df, d,
                         l_column = l,
                         s_column = s,
                         d_column = d,
                         suffix = ".1") {
  d <- dplyr::enquo(d)

  # Column names: avoid overwriting l, s, and d columns
  lsd_names <- lsd_column_names(df,
                                dplyr::enquo(l_column),
                                dplyr::enquo(s_column),
                                dplyr::enquo(d_column),
                                suffix)

  df %>%
    dplyr::mutate(!! lsd_names[1] := deb_denarii_l(!!d),
                  !! lsd_names[2] := deb_denarii_s(!!d),
                  !! lsd_names[3] := deb_denarii_d(!!d))
}
