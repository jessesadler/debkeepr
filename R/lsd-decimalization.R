## lsd decimalization ##

### To decimal and from decimal for l, s, and d ###

### librae ###

#' Convert from pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence to decimalized pounds.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized pounds. If `lsd` is a
#'   vector, the returned vector will be length 1. If `lsd` is a list of
#'   vectors, the returned vector will be the same length as `lsd`.
#'
#' @examples
#' # Create decimalized pounds
#' deb_lsd_l(lsd = c(10, 5, 8))
#'
#' # The lsd vector can be negative
#' deb_lsd_l(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_l(lsd = c(6, 25, 17))
#'
#' # To decimalize multiple lsd values use a list of lsd vectors
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_l(lsd = lsd_list)
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
#' @param l Decimalized pounds: numeric vector representing pounds to be
#'   converted into an lsd value.
#' @inheritParams deb_normalize
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the input lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Conversion from pounds to pounds, shillings, and pence
#' # only make sense with decimalized pounds.
#' deb_l_lsd(l = 8.625)
#'
#' # The value can be negative
#' deb_l_lsd(l = -8.625)
#'
#' # l can be a vector of length > 1
#' # Return a list of lsd vectors
#' deb_l_lsd(l = c(8.625, -8.625, 8))
#'
#' @export

deb_l_lsd <- function(l, round = 3) {
  if (length(l) > 1) {
    return(purrr::map(l, ~ deb_l_lsd(., round)))
  }

  if (!is.numeric(l)) {
    stop(call. = FALSE, "l must be numeric")
  }

  # repeat 0 of length l for vectorization
  deb_normalize(c(l, rep(0, length(l)), rep(0, length(l))), round)
}

### solidi ###

#' Convert from pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence to decimalized shillings.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized shillings If `lsd` is a
#'   vector, the returned vector will be length 1. If `lsd` is a list of
#'   vectors, the returned vector will be the same length as `lsd`.
#'
#' @examples
#' # Create decimalized shillings
#' deb_lsd_s(lsd = c(10, 5, 8))
#'
#' # The lsd vector can be negative
#' deb_lsd_s(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_s(lsd = c(6, 25, 17))
#'
#' # To decimalize multiple lsd values use a list of lsd vectors
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_s(lsd = lsd_list)
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
#' @param s Decimalized shillings: numeric vector representing shillings to
#'   be converted into an lsd value.
#' @inheritParams deb_normalize
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the input lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Convert shillings to pounds, shillings, and pence
#' deb_s_lsd(s = 2635)
#'
#' # The value for shillings can be negative
#' deb_d_lsd(-2635)
#'
#' # Or it can have a decimal
#' deb_d_lsd(2635.835)
#'
#' # s can be a vector of length > 1
#' # Return a list of lsd vectors
#' deb_s_lsd(c(2635, -2635, 2635.835))
#'
#' @export

deb_s_lsd <- function(s, round = 3) {
  if (length(s) > 1) {
    return(purrr::map(s, ~ deb_s_lsd(., round)))
  }

  if (!is.numeric(s)) {
    stop(call. = FALSE, "s must be numeric")
  }

  # repeat 0 of length s for vectorization
  deb_normalize(c(rep(0, length(s)), s, rep(0, length(s))), round)
}

### denarii ###

#' Convert from pounds, shillings, and pence to pence
#'
#' Convert pounds, shillings, and pence to pence or denarii.
#' Converting to the lowest denomination turns the non-decimal
#' pounds, shillings, and pence currency into a decimal currency.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized shillings If `lsd` is a
#'   vector, the returned vector will be length 1. If `lsd` is a list of
#'   vectors, the returned vector will be the same length as `lsd`.
#'
#' @examples
#' # Create decimalized pence
#' deb_lsd_d(lsd = c(10, 5, 8))
#'
#' # The lsd vector can be negative
#' deb_lsd_d(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_d(lsd = c(6, 25, 17))
#'
#' # To decimalize multiple lsd values use a list of lsd vectors
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_d(lsd = lsd_list)
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
#' @param d Decimalized pence: numeric vector representing pence to be
#'   converted into an lsd value.
#' @inheritParams deb_normalize
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the input lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Convert pence to pounds, shillings, and pence
#' deb_d_lsd(d = 2500)
#'
#' # The value for pence can be negative
#' deb_d_lsd(-2500)
#'
#' # d can be a vector of length > 1
#' # Return a list of lsd vectors
#' deb_d_lsd(c(2500, -2500, 4575))
#'
#' @export

deb_d_lsd <- function(d, round = 3) {
  if (length(d) > 1) {
    return(purrr::map(d, ~ deb_d_lsd(., round)))
  }

  if (!is.numeric(d)) {
    stop(call. = FALSE, "d must be numeric")
  }

  # repeat 0 of length d for vectorization
  deb_normalize(c(rep(0, length(d)), rep(0, length(d)), d), round)
}
