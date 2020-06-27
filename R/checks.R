## Checks ##

# lsd and bases checks ----------------------------------------------------

#' Checks for deb_lsd functions
#'
#' @description
#' Checks made:
#' - That `l`, `s`, and `d` are numeric
#' - That they are the same length, length 1, or all length 0
#' @keywords internal

lsd_check <- function(l, s, d) {
  # Check that l, s, and d are numeric
  if (!all(rlang::are_na(l))) {
    if (!is.numeric(l)) {
      rlang::abort("`l` must be a numeric vector.")
    }
  }

  if (!all(rlang::are_na(s))) {
    if (!is.numeric(s)) {
      rlang::abort("`s` must be a numeric vector.")
    }
  }

  if (!all(rlang::are_na(d))) {
    if (!is.numeric(d)) {
      rlang::abort("`d` must be a numeric vector.")
    }
  }

  # Check that l, s, and d are same length, length 1, or all length 0
  lengths <- c(vec_size(l), vec_size(s), vec_size(d))

  # Must be either all zero length or no zero length
  if (sum(lengths) == 1L || sum(lengths) == 2L) {
    rlang::abort(
      paste0("`l`, `s`, and `d` must all have values. ",
             "You may have forgotten a value or need to use 0."))
  }

  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if (length(unique(non_scalar)) > 1L) {
    rlang::abort(
      "`l`, `s`, and `d` must be vectors of equal length or length 1.")
  }
}

#' Checks for bases attribute
#'
#' @description
#' Check that:
#'
#' - Bases are numeric vector of length 2
#' - Cannot have NA values
#' - Must be natural (whole) numbers greater that 0
#' @keywords internal

bases_check <- function(bases) {
  if (!is.numeric(bases) || vec_size(bases) != 2L || is.null(bases)) {
    rlang::abort("`bases` must be a numeric vector of length 2.")
  }
  if (any(rlang::are_na(bases))) {
    rlang::abort("`bases` cannot be `NA`.")
  }
  if (!all(is_natural(bases))) {
    rlang::abort("`bases` must be natural numbers greater than zero.")
  }
}

#' Check that object is of type deb_lsd or deb_decimal
#' @keywords internal
deb_ptype_check <- function(x) {
  if (!deb_is_lsd(x) && !deb_is_decimal(x)) {
    rlang::abort("`lsd` must be either of type <deb_lsd> or <deb_decimal>.")
  }
}

# Bases assert ------------------------------------------------------------

#' Bases assert
#'
#' Remove any names of bases and then add unit names
#' @keywords internal
bases_assert <- function(bases) {
  bases <- rlang::set_names(bases, NULL) # vec_assert has error if named
  vec_assert(bases, ptype = integer(), size = 2)
  rlang::set_names(bases, c("s", "d"))
}

# Bases equivalent --------------------------------------------------------

#' Check that bases are equal for two deb-style vectors
#'
#' Used to ensure that deb_lsd and deb_decimal vectors with different bases
#' cannot be combined except explicitly with `deb_convert_bases()`.
#' @keywords internal
bases_equal <- function(x, y) {
  if (!identical(deb_bases(x), deb_bases(y))) {
    rlang::abort(
      paste0("`bases` attributes must be equal to combine <deb_lsd> ",
             "or <deb_decimal> vectors."))
  }
}


# list check --------------------------------------------------------------

#' List check
#'
#' Ensure that lists only include numeric vectors of length 3
#' @keywords internal
list_check <- function(x) {
  if (any(vapply(x, rlang::is_null, logical(1)))) {
    x <- Filter(length, x)
  }
  if (!all(vapply(x, is.numeric, logical(1)))) {
    rlang::abort("`x` must be a list of numeric vectors.")
  }

  lsd_lengths <- unique(list_sizes(x))

  if (!identical(lsd_lengths, 3L)) {
    rlang::abort("`x` must be a list of numeric vectors of length 3.")
  }
}

# Transaction checks ------------------------------------------------------

#' Transaction functions checks
#'
#' @description
#' Check that:
#'
#' - `df` is a dataframe
#' - `lsd`-column is provided
#' - `credit` and `debit` columns are provided
#' - `credit` and `debit` columns must be of the same type
#' - `account_id` in `deb_account()` must be in credit or debit columns
#'
#' @keywords internal
transaction_check <- function(df,
                              cn,
                              credit,
                              debit,
                              edge_columns,
                              account_id = NULL) {

  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data frame.")
  }

  if (rlang::is_false(cn %in% names(df))) {
    rlang::abort(
      "`lsd` must be provided if the default is not present in `df`.")
  }

  if (all(edge_columns %in% names(df)) == FALSE) {
    rlang::abort(
      paste0("Column names for `credit` and `debit` must be provided if",
             " the default names are not present in `df`."))
  }

  credit <- rlang::eval_tidy(credit, df)
  debit <- rlang::eval_tidy(debit, df)
  if (!identical(vec_ptype(credit), vec_ptype(debit))) {
    rlang::abort("`credit` and `debit` must be of the same type.")
  }

  if (!is.null(account_id)) {
    if (rlang::is_false(account_id %in% c(credit, debit))) {
      rlang::abort(
        "`account_id` must be a value present in `credit` and/or `debit`.")
    }
  }
}
