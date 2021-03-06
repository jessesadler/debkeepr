## Normalize lsd values ##

#' Check for non-whole numbers in libra and solidus units
#'
#' Move any decimals in libra and solidus units to the denarius unit.
#' The function uses the utility `should_be_int()` to deal with floating
#' point problems.
#'
#' @keywords internal

decimal_check <- function(lsd) {
  l <- field(lsd, "l")
  s <- field(lsd, "s")
  d <- field(lsd, "d")

  field(lsd, "l") <- trunc(l)
  temp_s <- s + (l - trunc(l)) * deb_bases(lsd)[[1]]
  field(lsd, "s") <- trunc(temp_s)
  field(lsd, "d") <- d + (temp_s - trunc(temp_s)) * deb_bases(lsd)[[2]]

  # Deal with floating point problems potentially introduced by the above
  field(lsd, "d") <- dplyr::if_else(should_be_int(field(lsd, "d")),
                                    round(field(lsd, "d")),
                                    field(lsd, "d"))

  lsd
}

#' Check whether lsd value is positive or negative
#' @keywords internal

is_negative <- function(x) {
  field(x, "l") +
    field(x, "s") / deb_bases(x)[[1]] +
    field(x, "d") / prod(deb_bases(x)) < 0
}

#' Normalization function
#'
#' Function that actually performs the normalization of lsd value
#' @keywords internal

normalize <- function(l, s, d, bases) {
  new_lsd(l = l + ((s + d %/% bases[[2]]) %/% bases[[1]]),
          s = (s + d %/% bases[[2]]) %% bases[[1]],
          d = d %% bases[[2]],
          bases = bases)
}

#' Normalization path for positive values
#'
#' Separate normalization functions for positive and negative values to be
#' used in `dplyr::if_else()`. Making them functions simplifies the process.
#' @keywords internal

lsd_normalize <- function(lsd) {

  normalize(l = field(lsd, "l"),
            s = field(lsd, "s"),
            d = field(lsd, "d"),
            bases = deb_bases(lsd))
}

#' Normalization path for negative values
#'
#' Turn values positive and then return to negative value.
#' @keywords internal
lsd_normalize_neg <- function(lsd) {

  ret <- normalize(l = -field(lsd, "l"),
                   s = -field(lsd, "s"),
                   d = -field(lsd, "d"),
                   bases = deb_bases(lsd))

  -ret
}


# deb_normalize methods ---------------------------------------------------

#' Normalize pounds, shillings, and pence values
#'
#' Normalize pounds, shillings, and pence values to given bases of solidus
#' and denarius units.
#'
#' @param x Either an vector of class `deb_lsd` or a numeric vector of
#'   length 3 representing the values to be normalized.
#' @param bases Used only if `x` is a numeric vector. A Numeric vector of
#'   length 2 used to specify the bases for the shillings or s and pence or
#'   d units. Default is `c(20, 12)`, which conforms to the most widely used
#'   system of 1 pound = 20 shillings and 1 shilling = 12 pence.
#' @param ... Arguments passed on to further methods.
#'
#' @return Returns a vector of class `deb_lsd` with normalized solidus and
#'   denarius units.
#' @examples
#'
#' # Normalize a deb_lsd vector
#' x <- deb_lsd(12, 93, 78)
#' y <- deb_lsd(12, 93, 78, bases = c(60, 16))
#' deb_normalize(x)
#' deb_normalize(y)
#'
#' # Normalize a numeric vector of length 3
#' deb_normalize(c(12, 93, 78), bases = c(20, 12))
#' deb_normalize(c(12, 93, 78), bases = c(60, 16))
#'
#' @name normalize
NULL

#' @rdname normalize
#' @export
deb_normalize <- function(x, ...) {
  UseMethod("deb_normalize")
}

#' @rdname normalize
#' @export
deb_normalize.default <- function(x, ...) {
  rlang::abort(
    "`x` must be a <deb_lsd> vector or a numeric vector of length 3.")
}

#' @rdname normalize
#' @export
deb_normalize.deb_lsd <- function(x, ...) {
  checked <- decimal_check(x)
  dplyr::if_else(is_negative(x),
                 lsd_normalize_neg(checked),
                 lsd_normalize(checked))
}

#' @rdname normalize
#' @export
deb_normalize.numeric <- function(x, bases = c(20, 12), ...) {
  if (vec_size(x) != 3L) {
    rlang::abort("`x` must be a numeric vector of length 3.")
  }

  lsd <- deb_lsd(x[[1]], x[[2]], x[[3]], bases)
  deb_normalize(lsd)
}
