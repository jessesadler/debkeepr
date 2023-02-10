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

all_present <- function(x, y) {
  x <- vec_unique(x[[1]])
  y <- vec_unique(y[[1]])

  all(vec_in(x, y), vec_in(y, x))
}

#' Create deb-type vector given a prototype, default value, and size
#'
#' Used in `deb_account()` and `deb_balance()` to create deb-type vectors
#' with values of 0 or NA of a given length. This places logic of if
#' statements here instead of in those functions, making them much simpler.
#' Default is deb-type vector of length one with value of NA.
#' @keywords internal
#' @param x Template of vector to create. Must be deb-type vector.
#' @param val Value of vector to create and replicate. Default is NA.
#' @param n Desired size of result
#' @noRd

create_debtype <- function(x, val = NA, n = 1L) {
  bases <- deb_bases(x)

  if (deb_is_lsd(x)) {
    vec_rep(deb_as_lsd(val, bases = bases), times = n)
  } else if (deb_is_decimal(x)) {
    unit <- deb_unit(x)
    vec_rep(deb_as_decimal(val, unit = unit, bases = bases), times = n)
  } else if (deb_is_tetra(x)) {
    vec_rep(deb_as_tetra(val, bases = bases), times = n)
  }
}

#' Group and summarise credit or debit columns
#'
#' Used in `deb_account_summary()`, `deb_credit()`, and `deb_debit()` to
#' group either credit or debit columns, sum data column of deb-type
#' vector, and rename the accounts column account_id. data column maintains
#' its name.
#' @keywords internal
#' @param df Transactions data frame.
#' @param relation Credit relation; either credit or debit.
#' @param data deb-type column
#' @param na.rm Inherited from parent function and used in summarise().
#' @return A tibble with two columns named account_id and same as data.
#' @noRd
deb_summarise <- function(df, relation, data, na.rm) {

  df %>%
    dplyr::group_by({{ relation }}) %>%
    dplyr::summarise("{{data}}" := sum({{ data }}, na.rm = na.rm),
                     .groups = "drop") %>%
    dplyr::rename(account_id = {{ relation }})
}

#' Fill in accounts from `y` that are not present in `x` with value of 0
#'
#' Used in `deb_account_summary()`, `deb_credit()`, and `deb_debit()` to
#' deal with situation of having accounts that only appear in credit or debit
#' and there are NAs, so need to distinguish between NAs and 0s. Used in
#' concert with `deb_summarise()`.
#' @keywords internal
#' @param x,y Data frames produced by `deb_summarise()` with account_id
#'   and deb-type value column.
#' @return `x` with accounts that are missing filled in by `y` and given
#'   a value of `0`.
#' @noRd

deb_fill_missing <- function(x, y) {
  data_name <- colnames(x)[[2]]

  x_acc <- x[[1]]
  y_acc <- y[[1]]
  # What is in y that is not in x
  missing <- y_acc[!vec_in(x_acc, y_acc)]

  # glue-style syntax for data-column name
  tibble::tibble(
    account_id = vec_c(x_acc, missing),
    "{data_name}" := vec_c(x[[2]], vec_rep(0, vec_size(missing)))
  )
}
