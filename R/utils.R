## Utility functions ##

#' Deal with floating point problems
#'
#' Should the value be a whole number or should it have a decimal value.
#' Used in `decimal_check()`.
#' @keywords internal
#' @noRd

should_be_int <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Check that bases are natural numbers
#'
#' Check that bases are natural numbers (whole number greater than 0).
#' From integer docs and SO: https://stackoverflow.com/a/4562291
#' @keywords internal
#' @noRd

is_natural <- function(x, tol = .Machine$double.eps^0.5) {
  x > tol & abs(x - round(x)) < tol
}

#' Coercion hierarchy for deb_decimal units
#'
#' Hierarchy: f -> d -> s -> l
#' @keywords internal
#' @noRd

unit_hierarchy <- function(x, y) {
  if (identical(deb_unit(x), deb_unit(y))) {
    deb_unit(x)
  } else if (any(c(deb_unit(x), deb_unit(y)) == "l")) {
    "l"
  } else if (any(c(deb_unit(x), deb_unit(y)) == "s")) {
    "s"
  } else {
    "d"
  }
}

#' Coercion hierarchy for deb_decimal bases
#'
#' Hierarchy: tetra -> lsd
#' Check for equality of bases or mixed bases equality
#' @returns bases attribute. If equal length, bases is returned.
#'   If unequal length, lsd bases is returned, removing f unit.
#' @keywords internal
#' @noRd

bases_hierarchy <- function(x, y, x_arg = "", y_arg = "") {
  # Both lsd or both tetra
  if (vec_size(deb_bases(x)) == vec_size(deb_bases(y))) {
    bases_equal(x, y, x_arg = x_arg, y_arg = y_arg)
    deb_bases(x)

    # x is lsd; y is tetra
  } else if (vec_size(deb_bases(x)) == 2L) {
    mixed_bases_equal(x, y, x_arg = x_arg, y_arg = y_arg)
    deb_bases(x)

    # y is lsd, x is tetra
  } else {
    mixed_bases_equal(y, x, x_arg = x_arg, y_arg = y_arg)
    deb_bases(y)
  }
}

#' Are all accounts present in debit and credit columns
#'
#' Used in `deb_account_summary()`, `deb_credit()`, and `deb_debit()`
#' if statement to see whether all accounts have both a credit and debit
#' transaction or not.
#' @keywords internal
#' @noRd

all_present <- function(pos, neg) {
  all(pos[[1]] %in% neg[[1]], neg[[1]] %in% pos[[1]])
}
