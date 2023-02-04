## Checks ##


# Messages ----------------------------------------------------------------

bases_na <- c("Values in {.arg {arg}} cannot be missing (`NA`).",
              "i" = "{.arg {arg}} must be natural numbers greater than zero.")
bases_natural <- c("{.arg {arg}} must be natural numbers greater than zero.",
                   "x" = "{prob} {?is/are} not {?a/} natural number{?s}.")

# Helpers for iteration placement -----------------------------------------

#' Arg as string helper
#' @details
#' This function is taken from an internal function in the vctrs package
#' found in conditions.R. It helps provide functionality found in
#' [`stop_incompatible_type()`] used in [`vec_ptype2()`]. It is used in
#' concert with code from `cnd_type_message()` to find iteration number
#' for errors.
#'
#' Used in [`bases_equal()`] and [`mixed_bases_equal()`] to provide more
#' in depth error message.
#' @keywords internal
#' @noRd

arg_as_string <- function(arg) {
  if (rlang::is_null(arg)) {
    ""
  } else if (rlang::is_string(arg)) {
    arg
  } else {
    rlang::as_label(arg)
  }
}

# lsd and tetra checks ----------------------------------------------------

#' Helper function for numeric check of units
#' @description
#' Check that all units are numeric. Use cli_abort() with caller_arg()
#' to be able to supply various arguments.
#' @keywords internal
#' @noRd

numeric_check <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!all(rlang::are_na(x))) {
    if (!is.numeric(x)) {
      cli::cli_abort(c("{.arg {arg}} must be a numeric vector.",
                       "x" = "You've supplied a {.cls {class(x)}} vector."),
                     call = call)
    }
    if (any(is.infinite(x))) {
      cli::cli_abort(c("{.arg {arg}} cannot contain infinite (Inf) values."),
                     call = call)
    }
  }
}

#' Checks for deb_lsd functions
#'
#' @description
#' Checks made:
#' - That `l`, `s`, and `d` are numeric
#' - That they are the same length, length 1, or all length 0
#' @keywords internal
#' @noRd

lsd_check <- function(l, s, d, call = rlang::caller_env()) {
  # Check that l, s, and d are numeric
  numeric_check(l, call = call)
  numeric_check(s, call = call)
  numeric_check(d, call = call)

  # Check that l, s, and d are same length, length 1, or all length 0
  lengths <- c(vec_size(l), vec_size(s), vec_size(d))

  # Must be either all zero length or no zero length
  if (!all(lengths == 0) && any(lengths == 0)) {
    missing <- c("`l`", "`s`", "`d`")[which(lengths == 0)]
    cli::cli_abort(c("{missing} {?is/are} absent but must be supplied.",
                   "x" = "`l`, `s`, and `d` must all have values.",
                   "i" = "You may have forgotten a value or need to use 0."),
                   call = call)
  }

  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if (vec_unique_count(non_scalar) > 1L) {
    cli::cli_abort(c(
      "`l`, `s`, and `d` must have compatible lengths",
      "i" = "Only values of length one are recycled.",
      "*" = "Unit `l` has length {lengths[[1]]}.",
      "*" = "Unit `s` has length {lengths[[2]]}.",
      "*" = "Unit `d` has length {lengths[[3]]}."),
      call = call)
  }
}

#' Checks for deb_tetra functions
#'
#' @description
#' Checks made:
#' - That `l`, `s`, `d`, and `f` are numeric
#' - That they are the same length, length 1, or all length 0
#' @keywords internal
#' @noRd

tetra_check <- function(l, s, d, f, call = rlang::caller_env()) {
  # Check that l, s, d, and f are numeric
  numeric_check(l, call = call)
  numeric_check(s, call = call)
  numeric_check(d, call = call)
  numeric_check(f, call = call)

  # Check that l, s, d, and f are same length, length 1, or all length 0
  lengths <- c(vec_size(l), vec_size(s), vec_size(d), vec_size(f))

  # Must be either all zero length or no zero length
  if (!all(lengths == 0) && any(lengths == 0)) {
    missing <- c("`l`", "`s`", "`d`", "`f`")[which(lengths == 0)]
    cli::cli_abort(c("{missing} {?is/are} absent but must be supplied.",
                     "x" = "`l`, `s`, `d`, and `f` must all have values.",
                     "i" = "You may have forgotten a value or need to use 0."),
                   call = call)
  }

  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if (vec_unique_count(non_scalar) > 1L) {
    cli::cli_abort(c(
      "`l`, `s`, and `d` must have compatible lengths.",
      "i" = "Only values of length one are recycled.",
      "*" = "Unit `l` has length {lengths[[1]]}.",
      "*" = "Unit `s` has length {lengths[[2]]}.",
      "*" = "Unit `d` has length {lengths[[3]]}.",
      "*" = "Unit `f` has length {lengths[[4]]}."),
      call = call)
  }
}

# Bases checks ------------------------------------------------------------

#' Checks for lsd bases attribute
#'
#' @description
#' Check that:
#' - Bases are numeric vector of length 2
#' - Cannot have NA values
#' - Must be natural (whole) numbers greater that 0
#' @keywords internal
#' @noRd

lsd_bases_check <- function(bases,
                            arg = rlang::caller_arg(bases),
                            call = rlang::caller_env()) {
  if (!is.numeric(bases) || vec_size(bases) != 2L || is.null(bases)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a numeric vector of length {.val {2}}.",
      "x" = "You've supplied a {.cls {class(bases)}} vector
            of length {.val {vec_size(bases)}}."),
      call = call)
  }
  if (any(rlang::are_na(bases))) {
    cli::cli_abort(message = bases_na, call = call)
  }
  if (!all(is_natural(bases))) {
    prob <- as.character(bases[which(!is_natural(bases))])
    cli::cli_abort(message = bases_natural, call = call)
  }
}

#' Checks for tetra bases attribute
#'
#' @description
#' Check that:
#' - Bases are numeric vector of length 3
#' - Cannot have NA values
#' - Must be natural (whole) numbers greater that 0
#' @keywords internal
#' @noRd

tetra_bases_check <- function(bases,
                              arg = rlang::caller_arg(bases),
                              call = rlang::caller_env()) {
  if (!is.numeric(bases) || vec_size(bases) != 3L || is.null(bases)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a numeric vector of length {.val {3}}.",
      "x" = "You've supplied a {.cls {class(bases)}} vector
            of length {.val {vec_size(bases)}}."),
      call = call)
  }
  if (any(rlang::are_na(bases))) {
    cli::cli_abort(message = bases_na, call = call)
  }
  if (!all(is_natural(bases))) {
    prob <- as.character(bases[which(!is_natural(bases))])
    cli::cli_abort(message = bases_natural, call = call)
  }
}

#' Checks for decimal bases attribute
#'
#' @description
#' Check that:
#' - Bases are numeric vector of length 2 or 3
#' - Cannot have NA values
#' - Must be natural (whole) numbers greater that 0
#' @keywords internal
#' @noRd

dec_bases_check <- function(bases,
                            arg = rlang::caller_arg(bases),
                            call = rlang::caller_env()) {
  if (!is.numeric(bases) || vec_size(bases) != 2L && vec_size(bases) != 3L ||
      is.null(bases)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a numeric vector of length {.val {2}} for
          tripartite values or {.val {3}} for tetrapartite values.",
      "x" = "You've supplied a {.cls {class(bases)}} vector
          of length {.val {vec_size(bases)}}."),
      call = call)
  }
  if (any(rlang::are_na(bases))) {
    cli::cli_abort(message = bases_na, call = call)
  }
  if (!all(is_natural(bases))) {
    prob <- as.character(bases[which(!is_natural(bases))])
    cli::cli_abort(message = bases_natural, call = call)
  }
}

#' Check f unit
#' @description
#' Check that:
#' - f is numeric vector of length 1
#' - Is not NA
#' - Must be natural (whole) number greater that 0
#' @keywords internal
#' @noRd

f_bases_check <- function(f, call = rlang::caller_env()) {
  rlang::check_required(f, call = call)

  if (!is.numeric(f) || vec_size(f) != 1L) {
    cli::cli_abort(c(
      "{.arg f} must be a numeric vector of length {.val {1}}.",
      "x" = "You've supplied a {.cls {class(f)}} vector
            of length {vec_size(f)}."),
      call = call)
  }
  if (rlang::is_na(f)) {
    cli::cli_abort(c(
      "{.arg f} cannot be `NA`.",
      "i" = "{.arg f} must be natural numbers greater than zero."),
      call = call)
  }
  if (!is_natural(f)) {
    cli::cli_abort(c(
      "{.arg f} must be a natural number greater than zero.",
      "x" = "{.val {f}} is not a natural number."),
      call = call)
  }
}

# Bases compatible --------------------------------------------------------

#' Check that bases are equal for two deb-style vectors
#'
#' Used to ensure that deb_lsd, deb_decimal, and deb_tetra vectors with
#' different bases cannot be combined except explicitly with
#' `deb_convert_bases()`.
#' @param x,y deb-style vectors
#' @param x_arg,y_arg Argument names for x and y. These are used in error
#'   messages to inform the user about the locations of incompatible types
#'   (see [`stop_incompatible_type()`]).
#' @param call Supplied to [`abort()`]. Default is `NULL` since revealing
#'   call to `vec_ptype2()` and `vec_cast()` is not helpful.
#' @keywords internal
#' @noRd

bases_equal <- function(x, y, x_arg = "", y_arg = "", call = NULL) {
  x_bases <- deb_bases(x)
  y_bases <- deb_bases(y)

  if (!identical(x_bases, y_bases)) {

    # From cnd_type_message() in vctrs 0.4.1 conditions.R
    # Used to get iteration values
    x_arg <- arg_as_string(x_arg)
    y_arg <- arg_as_string(y_arg)

    if (nzchar(x_arg)) {
      x_name <- paste0(" `", x_arg, "` ")
    } else {
      x_name <- " "
    }

    if (nzchar(y_arg)) {
      y_name <- paste0(" `", y_arg, "` ")
    } else {
      y_name <- " "
    }

    if (vec_size(deb_bases(x)) == 2L) {
      x_bases_label <- paste0("s = ", x_bases[[1]], " and d = ", x_bases[[2]])
      y_bases_label <- paste0("s = ", y_bases[[1]], " and d = ", y_bases[[2]])
    } else {
      x_bases_label <- paste0("s = ", x_bases[[1]], ", d = ", x_bases[[2]],
                              ", and f = ", x_bases[[3]])
      y_bases_label <- paste0("s = ", y_bases[[1]], ", d = ", y_bases[[2]],
                              ", and f = ", y_bases[[3]])
    }

    cli::cli_abort(c(
      "Incompatible {.var bases}.",
      "i" = "{.var bases} must be compatible to combine {.cls deb_lsd},
          {.cls deb_tetra}, or {.cls deb_decimal} vectors.",
      "x" = "Cannot combine: {x_name} {.cls {class(x)[[1]]}}
          vector with {.var bases} {.strong {x_bases_label}}.",
      "x" = "Cannot combine: {y_name} {.cls {class(y)[[1]]}}
          vector with {.var bases} {.strong {y_bases_label}}.",
      "i" = "Use {.code deb_convert_bases()} to convert one or more
          of the vectors to compatible {.var bases}."),
    call = call)
  }
}


# Mixed bases equivalent --------------------------------------------------

#' Check that s and d bases are equal for two deb-style vectors
#'
#' Used to ensure that deb_lsd, deb_decimal, and deb_tetra vectors with
#' tripartite and tetrapartite bases can only be combined if they share the
#' same s and d bases. Create a different function from [`bases_equal()`] for
#' better error message and to keep simplicity.
#'
#' @param x Object with tripartite bases: `s` and `d`.
#' @param y Object with tetrapartite bases: `s`, `d`, and `f`.
#' @inheritParams bases_equal
#' @keywords internal
#' @noRd

mixed_bases_equal <- function(x, y, x_arg = "", y_arg = "", call = NULL) {
  x_bases <- deb_bases(x)
  y_bases <- deb_bases(y)[1:2]

  if (!identical(x_bases, y_bases)) {

    # From cnd_type_message() in vctrs 0.4.1 conditions.R
    # Used to get iteration values
    x_arg <- arg_as_string(x_arg)
    y_arg <- arg_as_string(y_arg)

    if (nzchar(x_arg)) {
      x_name <- paste0(" `", x_arg, "` ")
    } else {
      x_name <- " "
    }

    if (nzchar(y_arg)) {
      y_name <- paste0(" `", y_arg, "` ")
    } else {
      y_name <- " "
    }

    x_bases_label <- paste0("s = ", x_bases[[1]], " and d = ", x_bases[[2]])
    y_bases_label <- paste0("s = ", y_bases[[1]], " and d = ", y_bases[[2]])

    cli::cli_abort(c(
      "Incompatible {.var bases}.",
      "i" = "{.var bases} of the 's' and 'd' units must be equal to combine
          {.cls deb_lsd}, {.cls deb_tetra}, or {.cls deb_decimal} vectors.",
      "x" = "Cannot combine: {x_name} {.cls {class(x)[[1]]}}
          vector with {.var bases} {.strong {x_bases_label}}.",
      "x" = "Cannot combine: {y_name} {.cls {class(y)[[1]]}}
          vector with {.var bases} {.strong {y_bases_label}}.",
      "i" = "Use {.code deb_convert_bases()} to convert one or more
          of the vectors to compatible {.var bases}."),
      call = call)
  }
}

# list checks -------------------------------------------------------------

#' lsd list check
#'
#' Ensure that lists only include numeric vectors of length 3
#' @keywords internal
#' @noRd

lsd_list_check <- function(x, call = rlang::caller_env()) {
  if (any(vapply(x, rlang::is_null, logical(1)))) {
    x <- Filter(length, x)
  }
  if (!all(vapply(x, is.numeric, logical(1)))) {
    rlang::abort("`x` must be a list of numeric vectors.",
      call = call)
  }

  lsd_lengths <- unique(list_sizes(x))

  if (!identical(lsd_lengths, 3L)) {
    sizes <- list_sizes(x)
    cli::cli_abort(c(
      "{.var x} must be a list of numeric vectors of length 3.",
      "x" = "You've supplied a list of {.cls numeric} vectors of
           length(s) {sizes}."),
      call = call)
  }
}

#' tetra list check
#'
#' Ensure that lists only include numeric vectors of length 4
#' @keywords internal
#' @noRd

tetra_list_check <- function(x, call = rlang::caller_env()) {
  if (any(vapply(x, rlang::is_null, logical(1)))) {
    x <- Filter(length, x)
  }
  if (!all(vapply(x, is.numeric, logical(1)))) {
    rlang::abort("`x` must be a list of numeric vectors.",
                 call = call)
  }

  lsd_lengths <- unique(list_sizes(x))

  if (!identical(lsd_lengths, 4L)) {
    sizes <- list_sizes(x)
    cli::cli_abort(c(
      "{.var x} must be a list of numeric vectors of length 4.",
      "x" = "You've supplied a list of {.cls numeric} vectors of
           length(s) {sizes}."),
      call = call)
  }
}

# Transaction checks ------------------------------------------------------

#' Check that object is of type deb_lsd or deb_decimal
#' @keywords internal
#' @noRd

deb_ptype_check <- function(x, call = rlang::caller_env()) {
  if (!deb_is_lsd(x) && !deb_is_decimal(x) && !deb_is_tetra(x)) {
    cli::cli_abort(c(
      "`lsd` must be of a column of type <deb_lsd>, <deb_tetra>, or
          <deb_decimal>.",
      "x" = "You've supplied a column of type {.cls {class(x)}}."),
      call = call)
  }
}

#' Transaction functions checks
#'
#' @description
#' Check that:
#'
#' - `df` is a dataframe
#' - `lsd`column is provided
#' - `credit` and `debit` columns are provided
#' - `credit` and `debit` columns must be of the same type
#'
#' @keywords internal
#' @noRd

transaction_check <- function(df,
                              cn,
                              credit,
                              debit,
                              edge_columns,
                              call = rlang::caller_env()) {

  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "{.var df} must be a data frame.",
      "x" = "You've supplied a {.cls {class(df)}}."),
      call = call)
  }

  if (rlang::is_false(cn %in% names(df))) {
    rlang::abort(
      "`lsd` must be provided if the default is not present in `df`.",
      call = call)
  }

  if (all(edge_columns %in% names(df)) == FALSE) {
    cli::cli_abort("Column names for `credit` and `debit` must be provided if
                   the default names are not present in `df`.",
                   call = call)
  }

  credit <- rlang::eval_tidy(credit, df)
  debit <- rlang::eval_tidy(debit, df)
  if (!identical(vec_ptype(credit), vec_ptype(debit))) {
    cli::cli_abort(c(
      "The {.var credit} and {.var debit} columns must be of the same type.",
      "*" = "{.var credit} is type {.cls {class(credit)[[1]]}}.",
      "*" = "{.var debit} is type {.cls {class(debit)[[1]]}}."),
                   call = call)
  }
}
