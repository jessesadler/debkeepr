## lsd transformations ##

### Normalize through librae ###

#' Convert from pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence to decimalized pounds.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized pounds of length
#'   equivalent to that of `l`, `s`, and `d`.
#'
#' @examples
#' # Create decimalized pounds
#' deb_lsd_l(10, 5, 8)
#'
#' # The pounds, shillings, and pence do not need to be properly grouped
#' deb_lsd_l(l = 6, s = 25, d = 17)
#'
#' # l, s, and d can be vectors of length > 1
#' deb_lsd_l(l = c(8, 4, 6), s = c(87, 35, 64), d = c(46, 58, 96))
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the equivalent decimalized pounds.
#' df <- data.frame(l = c(3, 5, 6, 2),
#'                  s = c(10, 18, 11, 16),
#'                  d = c(9, 11, 10, 5))
#'  dplyr::mutate(df, pounds = deb_lsd_l(l, s, d))
#'
#' @export

deb_lsd_l <- function(lsd) {
  lsd_check(lsd)
  if (is.list(lsd) == TRUE) {
    return(purrr::map_dbl(lsd, deb_lsd_l))
  }

  lsd[1] + lsd[2]/20 + lsd[3]/240
}

#' Convert from decimalized pounds to pounds, shillings, and pence
#'
#' Convert decimalized pounds to the lsd system of pounds, shillings,
#' and pence. The function returns the value from a decimal currency to the
#' non-decimal currency of pounds, shillings, and pence.
#' This is a wrapper around [deb_normalize()].
#'
#' @inheritParams deb_normalize
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. If the input value is negative, the
#'   pounds, shillings, and pence values will all be negative. The number of
#'   rows in the resulting tibble will be equal to the length of `l`. If the
#'   length of `l` is greater than 1 and `vector = TRUE`, the result will
#'   be a list of named vectors of length equal to `l`.
#'
#' @examples
#' # Conversion from pounds to pounds, shillings, and pence
#' # only make sense with decimalized pounds.
#' deb_l_lsd(l = 8.625)
#'
#' # Pounds, shillings, and pence as named vector
#' deb_l_lsd(l = 8.625, vector = TRUE)
#'
#' # The value can be negative
#' deb_l_lsd(-8.625)
#'
#' # l can be a vector of length > 1
#' # Return a tibble with three rows
#' deb_l_lsd(c(8.625, -8.625, 8))
#'
#' # Return a list with three vectors
#' deb_l_lsd(c(8.625, -8.625, 8), vector = TRUE)
#'
#' @export

deb_l_lsd <- function(l, round = 3) {
  if (length(l) > 1) {
    return(purrr::map(l, deb_l_lsd))
  }

  # repeat 0 of length l for vectorization
  deb_normalize(c(l, rep(0, length(l)), rep(0, length(l))), round)
}

### Normalize through solidi ###

#' Convert from pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence to decimalized shillings.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized shillings of length
#'   equivalent to that of `l`, `s`, and `d`.
#'
#' @examples
#' # Create decimalized shillings
#' deb_lsd_s(10, 5, 8)
#'
#' # The pounds, shillings, and pence do not need to be properly grouped
#' deb_lsd_s(l = 6, s = 25, d = 17)
#'
#' # l, s, and d can be vectors of length > 1
#' deb_lsd_s(l = c(8, 4, 6), s = c(87, 35, 64), d = c(46, 58, 96))
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the equivalent decimalized shillings.
#' df <- data.frame(l = c(3, 5, 6, 2),
#'                  s = c(10, 18, 11, 16),
#'                  d = c(9, 11, 10, 5))
#' dplyr::mutate(df, shillings = deb_lsd_s(l, s, d))
#'
#' @export

deb_lsd_s <- function(lsd) {
  lsd_check(lsd)
  if (is.list(lsd) == TRUE) {
    return(purrr::map_dbl(lsd, deb_lsd_s))
  }

  lsd[1] * 20 + lsd[2] + lsd[3]/12
}

#' Convert from decimalized shillings to pounds, shillings, and pence
#'
#' Convert decimalized shillings to the lsd system of pounds, shillings,
#' and pence. The function returns the value from a decimal currency to the
#' non-decimal currency of pounds, shillings, and pence.
#' This is a wrapper around [deb_normalize()].
#'
#' @inheritParams deb_normalize
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. If the input value is negative, the
#'   pounds, shillings, and pence values will all be negative. The number of
#'   rows in the resulting tibble will be equal to the length of `s`. If
#'   the length of `s` is greater than 1 and `vector = TRUE``, the
#'   result will be a list of named vectors of length equal to `s`.
#'
#' @examples
#' # Pounds, shillings, and pence as a tibble
#' deb_s_lsd(s = 2635)
#'
#' # Pounds, shillings, and pence as named vector
#' deb_s_lsd(s = 2635, vector = TRUE)
#'
#' # The value for shillings can be negative
#' deb_d_lsd(-2635)
#'
#' # Or it can have a decimal
#' deb_d_lsd(2635.835)
#'
#' # s can be a vector of length > 1
#' # Return a tibble with three rows
#' deb_s_lsd(c(2635, -2635, 2635.835))
#'
#' # Return a list with three vectors
#' deb_s_lsd(c(2635, -2635, 2635.835), vector = TRUE)
#'
#' @export

deb_s_lsd <- function(s, round = 3) {
  if (length(s) > 1) {
    return(purrr::map(s, deb_s_lsd))
  }

  # repeat 0 of length s for vectorization
  deb_normalize(c(rep(0, length(s)), s, rep(0, length(s))), round)
}

### Normalize through denarii ###

#' Convert from pounds, shillings, and pence to pence
#'
#' Convert pounds, shillings, and pence to pence or denarii.
#' Converting to the lowest denomination turns the non-decimal
#' pounds, shillings, and pence currency into a decimal currency.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized pence of length
#'   equivalent to that of `l`, `s`, and `d`.
#'
#' @examples
#' # Create decimalized pence
#' deb_lsd_d(10, 5, 8)
#'
#' # The pounds, shillings, and pence do not need to be properly grouped
#' deb_lsd_d(l = 6, s = 25, d = 17)
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the equivalent decimalized pence.
#' df <- data.frame(l = c(3, 5, 6, 2),
#'                  s = c(10, 18, 11, 16),
#'                  d = c(9, 11, 10, 5))
#'  dplyr::mutate(df, pence = deb_lsd_d(l, s, d))
#'
#' @export

deb_lsd_d <- function(lsd) {
  lsd_check(lsd)
  if (is.list(lsd) == TRUE) {
    return(purrr::map_dbl(lsd, deb_lsd_d))
  }

  lsd[1] * 240 + lsd[2] * 12 + lsd[3]
}

#' Convert from pence to pounds, shillings, and pence
#'
#' Convert pence to the lsd system of pounds, shillings, and pence.
#' The function returns the value from a decimal currency to the
#' non-decimal currency of pounds, shillings, and pence.
#' This is a wrapper around [deb_normalize()].
#'
#' @inheritParams deb_normalize
#'
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. If the input value is negative, the
#'   pounds, shillings, and pence values will all be negative. The number of
#'   rows in the resulting tibble will be equal to the length of `d`. If
#'   the length of `d` is greater than 1 and `vector = TRUE`, the result will
#'   be a list of named vectors of length equal to `d`.
#'
#' @examples
#' # Pounds, shillings, and pence as a tibble
#' deb_d_lsd(d = 2500)
#'
#' # Pounds, shillings, and pence as named vector
#' deb_d_lsd(d = 2500, vector = TRUE)
#'
#' # The value for pence can be negative
#' deb_d_lsd(-2500)
#'
#' # d can be a vector of length > 1
#' # Return a tibble with three rows
#' deb_d_lsd(c(2500, -2500, 4575))
#'
#' # Return a list with three vectors
#' deb_d_lsd(c(2500, -2500, 4575), vector = TRUE)
#'
#' @export

deb_d_lsd <- function(d, round = 3) {
  if (length(d) > 1) {
    return(purrr::map(d, deb_d_lsd))
  }

  # repeat 0 of length d for vectorization
  deb_normalize(c(rep(0, length(d)), rep(0, length(d)), d), round)
}
