## lsd decimalization ##

### librae ###

#' Conversion of pounds, shillings and pence to pounds
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
#' # With alternate bases for shillings and pence
#' deb_lsd_l(lsd = c(10, 5, 8), bases = c(20, 16))
#'
#' # The lsd vector can be negative
#' deb_lsd_l(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_l(lsd = c(6, 25, 17))
#'
#' # Decimalize multiple lsd values with a list of lsd vectors
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_l(lsd = lsd_list)
#'
#' @export

deb_lsd_l <- function(lsd, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map_dbl(lsd, ~ deb_lsd_l(., bases)))
  }
  # checks
  lsd_check(lsd)
  bases_check(bases)

  lsd[1] + lsd[2] / bases[1] + lsd[3] / prod(bases)
}

#' Conversion of decimalized pounds to pounds, shillings, and pence
#'
#' Convert decimalized pounds to the lsd system of pounds, shillings, and
#' pence.
#'
#' @param l Decimalized pounds: Numeric vector representing pounds to be
#'   converted into a pounds, shillings, and pence currency.
#' @inheritParams deb_normalize
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Conversion from pounds to pounds, shillings, and pence
#' # only make sense with pounds that has a decimal.
#' deb_l_lsd(l = 8.625)
#'
#' # With alternate bases for shillings and pence
#' deb_l_lsd(l = 8.625, bases = c(20, 16))
#'
#' # The value can be negative
#' deb_l_lsd(l = -8.625)
#'
#' # l can be a vector of length > 1
#' # Return a list of lsd vectors
#' deb_l_lsd(l = c(8.625, -8.625, 8))
#'
#' @export

deb_l_lsd <- function(l, bases = c(20, 12)) {
  if (length(l) > 1) {
    return(purrr::map(l, ~ deb_l_lsd(., bases = bases)))
  }

  if (!is.numeric(l)) {
    stop(call. = FALSE, "l must be numeric")
  }

  # repeat 0 of length l for vectorization
  deb_normalize(c(l, rep(0, length(l)), rep(0, length(l))),
                bases = bases)
}

### solidi ###

#' Conversion of pounds, shillings and pence to shillings
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
#' # With alternate bases for shillings and pence
#' deb_lsd_s(lsd = c(10, 5, 8), bases = c(20, 16))
#'
#' # The lsd vector can be negative
#' deb_lsd_s(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_s(lsd = c(6, 25, 17))
#'
#' # Decimalize multiple lsd values with a list of lsd vectors
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_s(lsd = lsd_list)
#'
#' @export

deb_lsd_s <- function(lsd, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map_dbl(lsd, ~ deb_lsd_s(., bases)))
  }
  # checks
  lsd_check(lsd)
  bases_check(bases)


  lsd[1] * bases[1] + lsd[2] + lsd[3] / bases[2]
}

#' Conversion of decimalized shillings to pounds, shillings, and pence
#'
#' Convert decimalized shillings to the lsd system of pounds, shillings,
#' and pence.
#'
#' @param s Decimalized shillings: Numeric vector representing shillings to be
#'   converted into a pounds, shillings, and pence currency.
#' @inheritParams deb_normalize
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Convert shillings to pounds, shillings, and pence
#' deb_s_lsd(s = 263)
#'
#' # The value for shillings can be negative
#' deb_s_lsd(s = -263)
#'
#' # Or it can have a decimal
#' deb_s_lsd(s = 263.835)
#'
#' # With alternate bases for shillings and pence
#' deb_s_lsd(s = 263.835, bases = c(20, 16))
#'
#' # s can be a vector of length > 1
#' # Return a list of lsd vectors
#' deb_s_lsd(s = c(263, -263, 263.835))
#'
#' @export

deb_s_lsd <- function(s, bases = c(20, 12)) {
  if (length(s) > 1) {
    return(purrr::map(s, ~ deb_s_lsd(., bases = bases)))
  }

  if (!is.numeric(s)) {
    stop(call. = FALSE, "s must be numeric")
  }

  # repeat 0 of length s for vectorization
  deb_normalize(c(rep(0, length(s)), s, rep(0, length(s))),
                bases = bases)
}

### denarii ###

#' Conversion from pounds, shillings, and pence to pence
#'
#' Convert pounds, shillings, and pence to decimalized pence.
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
#' # With alternate bases for shillings and pence
#' deb_lsd_d(lsd = c(10, 5, 8), bases = c(20, 16))
#'
#' # The lsd vector can be negative
#' deb_lsd_d(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_d(lsd = c(6, 25, 17))
#'
#' # Decimalize multiple lsd values with a list of lsd vectors
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_d(lsd = lsd_list)
#'
#' @export

deb_lsd_d <- function(lsd, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map_dbl(lsd, ~ deb_lsd_d(., bases)))
  }
  # checks
  lsd_check(lsd)
  bases_check(bases)

  round(lsd[1] * prod(bases) + lsd[2] * bases[2] + lsd[3], 5)
}

#' Conversion of pence to pounds, shillings, and pence
#'
#' Convert decimalized pence to the lsd system of pounds, shillings, and pence.
#'
#' @param d Decimalized pence: Numeric vector representing pence to be
#'   converted into a pounds, shillings, and pence currency.
#' @inheritParams deb_normalize
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Convert pence to pounds, shillings, and pence
#' deb_d_lsd(d = 2500)
#'
#' # With alternate bases for shillings and pence
#' deb_d_lsd(d = 2500, bases = c(20, 16))
#'
#' # The value for pence can be negative
#' deb_d_lsd(d = -2500)
#'
#' # d can be a vector of length > 1
#' # Return a list of lsd vectors
#' deb_d_lsd(d = c(2500, -2500, 4575))
#'
#' @export

deb_d_lsd <- function(d, bases = c(20, 12)) {
  if (length(d) > 1) {
    return(purrr::map(d, ~ deb_d_lsd(., bases = bases)))
  }

  if (!is.numeric(d)) {
    stop(call. = FALSE, "d must be numeric")
  }

  # repeat 0 of length d for vectorization
  deb_normalize(c(rep(0, length(d)), rep(0, length(d)), d),
                bases = bases)
}
