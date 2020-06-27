## Utility functions ##

#' Deal with floating point problems
#'
#' Should the value be a whole number or should it have a decimal value.
#' Used in `decimal_check()`.
#' @keywords internal

should_be_int <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Check that bases are natural numbers
#'
#' Check that bases are natural numbers (whole number greater than 0).
#' From integer docs and SO: https://stackoverflow.com/a/4562291
#' @keywords internal
is_natural <- function(x, tol = .Machine$double.eps^0.5) {
  x > tol & abs(x - round(x)) < tol
}

#' Coercion hierarchy for deb_decimal units
#'
#' Hierarchy: d -> s -> l
#' @keywords internal

unit_hierarchy <- function(x, y) {
  if (identical(deb_unit(x), deb_unit(y))) {
    deb_unit(x)
  } else if (any(c(deb_unit(x), deb_unit(y)) == "l")) {
    "l"
  } else {
    "s"
  }
}

#' Are all accounts present in debit and credit columns
#'
#' Used in `deb_account_summary()`, `deb_credit()`, and `deb_debit()`
#' if statement to see whether all accounts have both a credit and debit
#' transaction or not.
#' @keywords internal

all_present <- function(pos, neg) {
  all(pos[[1]] %in% neg[[1]], neg[[1]] %in% pos[[1]])
}
