## lsd decimalization ##

### librae ###

#' Conversion of pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence to decimalized pounds.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized pounds.
#'
#' @examples
#' # Create decimalized pounds
#' deb_lsd_l(lsd = c(10, 5, 8))
#'
#' # With alternative bases for shillings and pence
#' deb_lsd_l(lsd = c(10, 5, 8), bases = c(20, 16))
#'
#' # Decimalization an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(10, 5, 8), bases = c(20, 16))
#' deb_lsd_l(lsd = lsd)
#'
#' # The lsd values can be negative
#' deb_lsd_l(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_l(lsd = c(6, 25, 17))
#'
#' # Decimalize multiple lsd values with a list of lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_l(lsd = lsd_list)
#'
#' # Or an lsd object with alternative bases
#' lsd_list2 <- deb_as_lsd(lsd = lsd_list, bases = c(20, 16))
#' deb_lsd_l(lsd = lsd_list2)
#'
#' @export

deb_lsd_l <- function(lsd, bases = c(20, 12)) {
  # checks
  lsd <- null_check(lsd)
  lsd_check(lsd)
  bases <- validate_bases(lsd, bases)
  bases_check(bases)

  if (is.list(lsd) == TRUE) {
    purrr::map_dbl(lsd, ~ .[1] + .[2] / bases[1] + .[3] / prod(bases))
  } else {
    lsd[1] + lsd[2] / bases[1] + lsd[3] / prod(bases)
  }
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
#' @return Returns an object of class lsd with a bases attribute.
#'
#' @examples
#' # Conversion from pounds to pounds, shillings, and pence
#' # only make sense with pounds that has a decimal.
#' deb_l_lsd(l = 8.625)
#'
#' # With alternative bases for shillings and pence
#' deb_l_lsd(l = 8.625, bases = c(20, 16))
#'
#' # The value can be negative
#' deb_l_lsd(l = -8.625)
#'
#' # l can be a vector of length > 1
#' deb_l_lsd(l = c(8.625, -8.625, 8))
#'
#' @export

deb_l_lsd <- function(l, bases = c(20, 12), round = 5) {
  if (!is.numeric(l)) {
    stop(call. = FALSE, "l must be numeric")
  }

  if (length(l) > 1) {
    lsd <- list(l, rep(0, length(l)), rep(0, length(l))) %>%
      purrr::transpose() %>%
      purrr::simplify_all()
  } else {
    lsd <- c(l, 0, 0)
  }
  deb_normalize(lsd = lsd, bases = bases, round = round)
}

### solidi ###

#' Conversion of pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence to decimalized shillings.
#'
#' @inheritParams deb_normalize
#'
#' @return Returns a numeric vector of decimalized shillings.
#'
#' @examples
#' # Create decimalized shillings
#' deb_lsd_s(lsd = c(10, 5, 8))
#'
#' # With alternative bases for shillings and pence
#' deb_lsd_s(lsd = c(10, 5, 8), bases = c(20, 16))
#'
#' # Decimalization an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(10, 5, 8), bases = c(20, 16))
#' deb_lsd_s(lsd = lsd)
#'
#' # The lsd values can be negative
#' deb_lsd_s(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_s(lsd = c(6, 25, 17))
#'
#' # Decimalize multiple lsd values with a list of lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_s(lsd = lsd_list)
#'
#' # Or an lsd object with alternative bases
#' lsd_list2 <- deb_as_lsd(lsd = lsd_list, bases = c(20, 16))
#' deb_lsd_s(lsd = lsd_list2)
#'
#' @export

deb_lsd_s <- function(lsd, bases = c(20, 12)) {
  # checks
  lsd <- null_check(lsd)
  lsd_check(lsd)
  bases <- validate_bases(lsd, bases)
  bases_check(bases)

  if (is.list(lsd) == TRUE) {
    purrr::map_dbl(lsd, ~ .[1] * bases[1] + .[2] + .[3] / bases[2])
  } else {
    lsd[1] * bases[1] + lsd[2] + lsd[3] / bases[2]
  }
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
#' @return Returns an object of class lsd with a bases attribute.
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
#' # With alternative bases for shillings and pence
#' deb_s_lsd(s = 263.835, bases = c(20, 16))
#'
#' # s can be a vector of length > 1
#' deb_s_lsd(s = c(263, -263, 263.835))
#'
#' @export

deb_s_lsd <- function(s, bases = c(20, 12), round = 5) {
  if (!is.numeric(s)) {
    stop(call. = FALSE, "s must be numeric")
  }

  if (length(s) > 1) {
    lsd <- list(rep(0, length(s)), s, rep(0, length(s))) %>%
      purrr::transpose() %>%
      purrr::simplify_all()
  } else {
    lsd <- c(0, s, 0)
  }
  deb_normalize(lsd = lsd, bases = bases, round = round)
}

### denarii ###

#' Conversion of pounds, shillings, and pence to pence
#'
#' Convert pounds, shillings, and pence to decimalized pence.
#'
#' @inheritParams deb_normalize
#'
#' @return  Returns a numeric vector of decimalized pence.
#'
#' @examples
#' # Create decimalized pence
#' deb_lsd_d(lsd = c(10, 5, 8))
#'
#' # With alternative bases for shillings and pence
#' deb_lsd_d(lsd = c(10, 5, 8), bases = c(20, 16))
#'
#' # Decimalization an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(10, 5, 8), bases = c(20, 16))
#' deb_lsd_d(lsd = lsd)
#'
#' # The lsd values can be negative
#' deb_lsd_d(lsd = c(-10, -5, -8))
#'
#' # The pounds, shillings, and pence do not need to be normalized
#' deb_lsd_d(lsd = c(6, 25, 17))
#'
#' # Decimalize multiple lsd values with a list of lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_lsd_d(lsd = lsd_list)
#'
#' # Or an lsd object with alternative bases
#' lsd_list2 <- deb_as_lsd(lsd = lsd_list, bases = c(20, 16))
#' deb_lsd_d(lsd = lsd_list2)
#'
#' @export

deb_lsd_d <- function(lsd, bases = c(20, 12)) {
  # checks
  lsd <- null_check(lsd)
  lsd_check(lsd)
  bases <- validate_bases(lsd, bases)
  bases_check(bases)

  if (is.list(lsd) == TRUE) {
    purrr::map_dbl(lsd, ~ .[1] * prod(bases) + .[2] * bases[2] + .[3])
  } else {
    lsd[1] * prod(bases) + lsd[2] * bases[2] + lsd[3]
  }
}

#' Conversion of decimalized pence to pounds, shillings, and pence
#'
#' Convert decimalized pence to the lsd system of pounds, shillings, and pence.
#'
#' @param d Decimalized pence: Numeric vector representing pence to be
#'   converted into a pounds, shillings, and pence currency.
#' @inheritParams deb_normalize
#'
#' @return Returns an object of class lsd with a bases attribute.
#'
#' @examples
#' # Convert pence to pounds, shillings, and pence
#' deb_d_lsd(d = 2500)
#'
#' # With alternative bases for shillings and pence
#' deb_d_lsd(d = 2500, bases = c(20, 16))
#'
#' # The value for pence can be negative
#' deb_d_lsd(d = -2500)
#'
#' # d can be a vector of length > 1
#' deb_d_lsd(d = c(2500, -2500, 4575))
#'
#' @export

deb_d_lsd <- function(d, bases = c(20, 12), round = 5) {
  if (!is.numeric(d)) {
    stop(call. = FALSE, "d must be numeric")
  }

  if (length(d) > 1) {
    lsd <- list(rep(0, length(d)), rep(0, length(d)), d) %>%
      purrr::transpose() %>%
      purrr::simplify_all()
  } else {
    lsd <- c(0, 0, d)
  }
  deb_normalize(lsd = lsd, bases = bases, round = round)
}
