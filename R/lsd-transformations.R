## lsd transformations ##

### Refactor through librae ###

#' Convert from pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence to decimalized pounds.
#'
#' @inheritParams deb_refactor
#'
#' @return Returns a single value or numeric vector of length one.
#'
#' @examples
#' # Create decimalized pounds
#' deb_lsd_l(10, 5, 8)
#'
# The pounds, shillings, and pence do not need to be properly grouped
#' deb_lsd_l(l = 6, s = 25, d = 17)
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the equivalent decimalized pounds.
#' df <- tibble::tibble(l = c(3, 5, 6, 2),
#'                      s = c(10, 18, 11, 16),
#'                      d = c(9, 11, 10, 5))
#'  mutate(df, pounds = deb_lsd_l(l, s, d))
#'
#' @export

deb_lsd_l <- function(l, s, d) {
  lsd_check(l = l,
            s = s,
            d = d)
  l + s/20 + d/240
}

#' Convert from decimalized pounds to pounds, shillings, and pence
#'
#' Convert decimalized pounds to the lsd system of pounds, shillings,
#' and pence. The function returns the value from a decimal currency to the
#' non-decimal currency of pounds, shillings, and pence.
#' This is a wrapper around \code{deb_refactor()}.
#'
#' @inheritParams deb_refactor
#'
#' @return Returns either a tibble with one row of values and columns for the
#'   pounds, shillings, and pence values labeled as l, s, and d or a named
#'   numeric vector with values for pounds, shillings, and pence. If l
#'   is negative, the pounds, shillings, and pence values will all be negative.
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
#' @export

deb_l_lsd <- function(l, round = 3, vector = FALSE) {
  deb_refactor(l, 0, 0, round, vector)
}


### Refactor through solidi ###

#' Convert from pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence to decimalized shillings.
#'
#' @inheritParams deb_refactor
#'
#' @return Returns a single value or numeric vector of length one.
#'
#' @examples
#' # Create decimalized shillings
#' deb_lsd_s(10, 5, 8)
#'
#' # The pounds, shillings, and pence do not need to be properly grouped
#' deb_lsd_s(l = 6, s = 25, d = 17)
#'
#' # Can be used with mutate() on a data frame with
#' # pounds, shillings, and pence columns to create
#' # a column for the equivalent decimalized shillings.
#' df <- tibble::tibble(l = c(3, 5, 6, 2),
#'                      s = c(10, 18, 11, 16),
#'                      d = c(9, 11, 10, 5))
#'  mutate(df, shillings = deb_lsd_s(l, s, d))

deb_lsd_s <- function(l, s, d) {
  lsd_check(l = l,
            s = s,
            d = d)
  l * 20 + s + d/12
}

#' Convert from decimalized shillings to pounds, shillings, and pence
#'
#' Convert decimalized shillings to the lsd system of pounds, shillings,
#' and pence. The function returns the value from a decimal currency to the
#' non-decimal currency of pounds, shillings, and pence.
#' This is a wrapper around \code{deb_refactor()}.
#'
#' @inheritParams deb_refactor
#'
#' @return Returns either a tibble with one row of values and columns for the
#'   pounds, shillings, and pence values labeled as l, s, and d or a named
#'   numeric vector with values for pounds, shillings, and pence. If s
#'   is negative, the pounds, shillings, and pence values will all be negative.
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
#' @export

deb_s_lsd <- function(s, round = 3, vector = FALSE) {
  deb_refactor(0, s, 0, round, vector)
}

### Refactor through denarii ###

#' Convert from pounds, shillings, and pence to pence
#'
#' Convert pounds, shillings, and pence to pence or denarii.
#' Converting to the lowest denomination turns the non-decimal
#' pounds, shillings, and pence currency into a decimal currency.
#'
#' @inheritParams deb_refactor
#'
#' @return Returns a single value or numeric vector of length one.
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
#' df <- tibble::tibble(l = c(3, 5, 6, 2),
#'                      s = c(10, 18, 11, 16),
#'                      d = c(9, 11, 10, 5))
#'  mutate(df, pence = deb_lsd_d(l, s, d))
#'
#' @export

deb_lsd_d <- function(l, s, d) {
  lsd_check(l = l,
            s = s,
            d = d)
  l * 240 + s * 12 + d
}

#' Convert from pence to pounds, shillings, and pence
#'
#' Convert pence to the lsd system of pounds, shillings,and pence.
#' The function returns the value from a decimal currency to the
#' non-decimal currency of pounds, shillings, and pence.
#' This is a wrapper around \code{deb_refactor()}.
#'
#' @inheritParams deb_refactor
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

deb_d_lsd <- function(d, round = 3, vector = FALSE) {
  deb_refactor(0, 0, d, round, vector)
}
