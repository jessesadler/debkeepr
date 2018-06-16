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
deb_denarii_d <- function(d, round = 3) {
  denarii <- dplyr::if_else(d < 0, -(-d %% 12), d %% 12)
  round(denarii, round)
}

## Mutate l, s, and d, to separate l, s, and d ##

#' Mutate decimal pounds into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a decimalized pounds variable.
#' Thus, the function converts decimalized pounds into the lsd system of
#' pounds, shillings, and pence.
#'
#' @param df A data frame that contains a column of decimalized pounds.
#' @param librae Decimalized pounds column: Unquoted name of a numeric
#'   variable corresponding to pounds. This is the variable that will be
#'   mutated into pounds, shillings, and pence variables.
#' @param l_column An unquoted name for the pounds column created by the
#'   function. Default is l.
#' @param s_column An unquoted name for the shillings column created by
#'   the function. Default is s.
#' @param d_column An unquoted name for the pence column created by the
#'   function. Default is d.
#' @param suffix If the data frame already contains variables with the same
#'   names as `l_column`, `s_column`, or `d_column`, this suffix will be
#'   added to the new variables to distinguish them. Default is ".1".
#' @param round round pence to specified number of decimal places.
#'   Default is 3. Set to 0 to return pence as whole numbers.
#'
#' @return Returns a data frame with three new variables of pounds,
#'   shillings, and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a decimalized pounds variable
#' example <- data.frame(pounds = c(8, 8.325, -8.325, 5.425, 4.5678))
#'
#' deb_l_mutate(example, pounds)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_l_mutate(pounds,
#'                l_column = librae,
#'                s_column = solidi,
#'                d_column = denarii)
#'
#' @export

deb_l_mutate <- function(df, librae,
                         l_column = l,
                         s_column = s,
                         d_column = d,
                         round = 3,
                         suffix = ".1") {

  librae <- rlang::enquo(librae)

  # Check that librae exists in df
  if (rlang::quo_name(librae) %in% names(df) == FALSE) {
    stop(call. = FALSE, "librae column must exist the in df")
  }

  # Check that librae is numeric
  if (!is.numeric(rlang::eval_tidy(librae, df))) {
    stop(call. = FALSE, "librae must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  df %>%
    dplyr::mutate(!! lsd_names[1] := deb_librae_l(!! librae),
                  !! lsd_names[2] := deb_librae_s(!! librae),
                  !! lsd_names[3] := deb_librae_d(!! librae, round))
}

#' Mutate decimal shillings into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a decimalized shillings variable.
#' Thus, the function converts decimalized shillings into the lsd system of
#' pounds, shillings, and pence.
#'
#' @param df A data frame that contains a column of decimalized shillings.
#' @param solidi Decimalized shillings column: Unquoted name of a numeric
#'   variable corresponding to shillings. This is the variable that will be
#'   mutated into pounds, shillings, and pence variables.
#' @inheritParams deb_l_mutate
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a decimalized shillings variable
#' example <- data.frame(shillings = c(166, -166, 166.5, 236.35, -354.845))
#'
#' deb_s_mutate(example, shillings)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_s_mutate(shillings,
#'                l_column = librae,
#'                s_column = solidi,
#'                d_column = denarii)
#'
#' @export

deb_s_mutate <- function(df, solidi,
                         l_column = l,
                         s_column = s,
                         d_column = d,
                         round = 3,
                         suffix = ".1") {
  solidi <- rlang::enquo(solidi)

  # Check that solidi exists in df
  if (rlang::quo_name(solidi) %in% names(df) == FALSE) {
    stop(call. = FALSE, "solidi column must exist the in df")
  }

  # Check that solidi is numeric
  if (!is.numeric(rlang::eval_tidy(solidi, df))) {
    stop(call. = FALSE, "solidi must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  df %>%
    dplyr::mutate(!! lsd_names[1] := deb_solidi_l(!! solidi),
                  !! lsd_names[2] := deb_solidi_s(!! solidi),
                  !! lsd_names[3] := deb_solidi_d(!! solidi, round))
}

#' Mutate decimal pence into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a pence variable. Thus, the
#' function converts pence into the lsd system of pounds, shillings,
#' and pence.
#'
#' @param df A data frame that contains a column of pence.
#' @param denarii Pence column: Unquoted name of a numeric variable
#'   corresponding to pence. This is the variable that will be
#'   mutated into pounds, shillings, and pence variables.
#' @inheritParams deb_l_mutate
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a pence variable
#' example <- data.frame(pence = c(1998, -1998, 387, -5378))
#'
#' deb_d_mutate(example, pence)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_d_mutate(pence,
#'                l_column = librae,
#'                s_column = solidi,
#'                d_column = denarii)
#'
#' @export

deb_d_mutate <- function(df, denarii,
                         l_column = l,
                         s_column = s,
                         d_column = d,
                         round = 3,
                         suffix = ".1") {
  denarii <- rlang::enquo(denarii)

  # Check that denarii exists in df
  if (rlang::quo_name(denarii) %in% names(df) == FALSE) {
    stop(call. = FALSE, "denarii column must exist the in df")
  }

  # Check that denarii is numeric
  if (!is.numeric(rlang::eval_tidy(denarii, df))) {
    stop(call. = FALSE, "denarii must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  df %>%
    dplyr::mutate(!! lsd_names[1] := deb_denarii_l(!! denarii),
                  !! lsd_names[2] := deb_denarii_s(!! denarii),
                  !! lsd_names[3] := deb_denarii_d(!! denarii, round))
}
