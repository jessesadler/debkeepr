## lsd transformations ##

# Helper functions: Check whether entry is positive or negative
# If negative l, s, and d are all made negative

deb_librae <- function(l, s, d) {
  if (l + s/20 + d/240 > 0) {
    l + ((s + d %/% 12) %/% 20)
  } else {
    -(-l + ((-s + -d %/% 12) %/% 20))
  }
}

deb_solidi <- function(l, s, d) {
  if (l + s/20 + d/240 > 0) {
    (s + d %/% 12) %% 20
  } else {
    -((-s + -d %/% 12) %% 20)
  }
}

deb_denarii <- function(l, s, d) {
  if (l + s/20 + d/240 > 0) {
    d %% 12
  } else {
    -(-d %% 12)
  }
}

#' Refactor pounds, shillings, and pence
#'
#' Refactor pounds, shillings, and pence to correct values based
#' on 12 pence in a shilling and 20 shillings in a pound.
#'
#' This function uses the nomenclature of l, s, and d to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#' For more information on the lsd noemclature see
#' \url{https://en.wikipedia.org/wiki/£sd}
#'
#' @inheritParams lsd_check
#' @param vector Logical (default FALSE), when FALSE the output will
#'   be a tibble, when TRUE the output will be a numeric vector.
#'
#' @return Returns either a tibble with one row of values and columns for the
#'   pounds, shillings, and pence values labeled as l, s, and d or a named
#'   numeric vector with values for pounds, shillings, and pence. If the amount
#'   is negative, the pounds, shillings, and pence values will all be negative.
#'
#' @examples
#' # Use to calculate the correct number of pounds, shillings, and pence
#' deb_refactor(l = 5, s = 25, d = 22)
#' deb_refactor(5, 25, 22, vector = TRUE)
#'
#' # It is possible to add within the function
#' deb_refactor(5 + 6, 20 + 18, 8 + 11)
#' # Or even
#' deb_refactor(sum(4, 9, 0), sum(12, 16, 5), sum(11, 0, 6))
#'
#' # deb_refactor can deal with negative values
#' deb_refactor(-5, -25, -22)
#' # Or even a mixture of positive and negative if that occurs for some reason
#' deb_refactor(5, -25, 22)
#'
#' @export

deb_refactor <- function(l, s, d, vector = FALSE) {
  lsd_check(l = l,
            s = s,
            d = d)
  if (vector == FALSE) {
    tibble::tibble(
      l = deb_librae(l, s, d),
      s = deb_solidi(l, s, d),
      d = deb_denarii(l, s, d))
  } else {
    c(
      l = deb_librae(l, s, d),
      s = deb_solidi(l, s, d),
      d = deb_denarii(l, s, d))
  }
}
