## lsd transformations ##

# Helper functions to refactor lsd
deb_librae <- function(l, s, d) {l + ((s + d %/% 12) %/% 20)}
deb_solidi <- function(s, d) {(s + d %/% 12) %% 20}
deb_denarii <- function(d) {d %% 12}

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
#'
#' @inheritParams lsd_check
#' @param vector Logical (default FALSE), when FALSE the output will
#'   be a tibble, when TRUE the output will be a numeric vector.
#'
#' @return Returns either a tibble with one row of values and columns for the
#'   pounds, shillings, and pence values labeled as l, s, and d or a named
#'   numeric vector with values for pounds, shillings, and pence.
#'
#' @examples
#' deb_refactor(5, 25, 22)
#' deb_refactor(5, 25, 22, vector = TRUE)
#'
#' @export

deb_refactor <- function(l, s, d, vector = FALSE) {
  lsd_check(l = l,
            s = s,
            d = d)
  if (vector == FALSE) {
    tibble::tibble(
      l = deb_librae(l, s, d),
      s = deb_solidi(s, d),
      d = deb_denarii(d))
  } else {
    c(
      l = deb_librae(l, s, d),
      s = deb_solidi(s, d),
      d = deb_denarii(d))
  }
}


