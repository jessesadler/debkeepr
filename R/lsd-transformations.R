## lsd transformations ##

### lsd to decimal pounds ###

#' Convert from pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence to decimalized pounds.
#'
#' @inheritParams lsd_check
#'
#' @return Returns a single value or numeric vector of length one.
#'
#' @examples
#' deb_lsd_decimal(10, 5, 8)
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the total decimalized pounds.
#' mutate(transactions, pounds = deb_lsd_decimal(l, s, d))

deb_lsd_decimal <- function(l, s, d) {
  lsd_check(l = l,
            s = s,
            d = d)
  l + s/20 + d/240
}

### Refactor through denarii ###

#' Convert from pounds, shillings and pence to pence
#'
#' Convert pounds, shillings, and pence to pence or denarii.
#' Converting to the lowest denomination turns the non-decimal
#' pounds, shillings, and pence currency into a decimal currency.
#'
#' @inheritParams lsd_check
#'
#' @return Returns a single value or numeric vector of length one.
#'
#' @examples
#' deb_lsd_d(4, 6, 8)
#'
#' # The pounds, shillings, and pence do not need to be properly factored
#' deb_lsd_d(l = 6, s = 25, d = 17)
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the total pence.
#' mutate(transactions, pence = deb_lsd_d(l, s, d))
#'
#' @export

deb_lsd_d <- function(l, s, d) {
  l * 240 + s * 12 + d
}

#' Convert from pence to pounds, shillings and pence
#'
#' Convert pence or denarii to pounds, shillings, and pence. This returns
#' the value from a decimal curreny to the non-decimal currency of pounds,
#' shillings, and pence. This is a wrapper around \code{deb_refactor()}.
#'
#' @param d pence: numeric value. Can be either positive or negative.
#' @param vector Logical (default FALSE), when FALSE the output will
#'   be a tibble, when TRUE the output will be a numeric vector.
#'
#' @return Returns either a tibble with one row of values and columns for the
#'   pounds, shillings, and pence values labeled as l, s, and d or a named
#'   numeric vector with values for pounds, shillings, and pence. If d
#'   is negative, the pounds, shillings, and pence values will all be negative.
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
#' @export

deb_d_lsd <- function(d, vector = FALSE) {
  deb_refactor(0, 0, d, vector = vector)
}

### Refactor through solidi ###

#' Convert from pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence to decimalized shillings.
#'
#' @inheritParams lsd_check
#'
#' @return Returns a single value or numeric vector of length one.
#'
#' @examples
#' deb_lsd_s(10, 5, 8)
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the total decimalized shillings.
#' mutate(transactions, pounds = deb_lsd_s(l, s, d))

deb_lsd_s <- function(l, s, d) {
  l * 20 + s + d/12
}

deb_s_lsd <- function(s, vector = FALSE) {
  deb_refactor(0, s, 0, vector = vector)
}
