## Checks ##

# lsd and bases checks ----------------------------------------------------

lsd_check <- function(l, s, d) {
  # Check that l, s, and d are numeric
  if (!all(rlang::are_na(l))) {
    if (!is.numeric(l)) {
      stop(call. = FALSE, "`l` must be a numeric vector.")
    }
  }

  if (!all(rlang::are_na(s))) {
    if (!is.numeric(s)) {
      stop(call. = FALSE, "`s` must be a numeric vector.")
    }
  }

  if (!all(rlang::are_na(d))) {
    if (!is.numeric(d)) {
      stop(call. = FALSE, "`d` must be a numeric vector.")
    }
  }

  # Check that l, s, and d are same length, length 1, or all length 0
  lengths <- c(vec_size(l), vec_size(s), vec_size(d))

  # Must be either all zero length or no zero length
  if (sum(lengths) == 1L || sum(lengths) == 2L) {
    stop(call. = FALSE,
         paste0("`l`, `s`, and `d` must all have values. ",
                "You may have forgotten a value or need to use 0."))
  }

  # Must be only one length other than scalar
  non_scalar <- lengths[lengths != 1L]
  if (length(unique(non_scalar)) > 1L) {
    stop(call. = FALSE,
         "`l`, `s`, and `d` must be vectors of equal length or length 1.")
  }
}

# Check that bases are natural number: whole number greater than 0
# From integer docs and SO: https://stackoverflow.com/a/4562291
is_natural <- function(x, tol = .Machine$double.eps^0.5) {
  x > tol & abs(x - round(x)) < tol
}

bases_check <- function(bases) {
  if (!is.numeric(bases) || vctrs::vec_size(bases) != 2L || is.null(bases)) {
    stop(call. = FALSE, "`bases` must be a numeric vector of length 2.")
  }
  if (any(rlang::are_na(bases))) {
    stop(call. = FALSE, "`bases` cannot be `NA`.")
  }
  if (!all(is_natural(bases))) {
    stop(call. = FALSE, "`bases` must be natural numbers greater than zero.")
  }
}


# Bases assert ------------------------------------------------------------

bases_assert <- function(bases) {
  bases <- rlang::set_names(bases, NULL) # vec_assert has error if named
  vctrs::vec_assert(bases, ptype = integer(), size = 2)
  rlang::set_names(bases, c("s", "d"))
}

# Bases equivalent --------------------------------------------------------

# Check that bases are equal for two deb-style objects
bases_equal <- function(x, y) {
  if (!identical(deb_bases(x), deb_bases(y))) {
    stop(call. = FALSE,
         paste0("`bases` attributes must be equal to combine <deb_lsd> ",
                "or <deb_decimal> objects."))
  }
}


# Units equivalent --------------------------------------------------------

unit_equal <- function(x, y) {
  if (!identical(attr(x, "unit"), attr(y, "unit"))) {
    stop(call. = FALSE,
         "`unit` attributes must be equal to combine <deb_decimal> objects.")
  }
}


# Transaction checks ------------------------------------------------------

transaction_check <- function(df,
                              cn,
                              credit,
                              debit,
                              edge_columns,
                              account_id = NULL) {

  if (!is.data.frame(df)) {
    stop(call. = FALSE, "`df` must be a data frame.")
  }

  if (rlang::is_false(cn %in% names(df))) {
    stop(call. = FALSE,
         "`lsd` must be provided if the default is not present in `df`.")
  }

  if (all(edge_columns %in% names(df)) == FALSE) {
    stop(call. = FALSE,
         paste("Column names for `credit` and `debit` must be provided if",
               "the default names are not present in `df`.", sep = " "))
  }

  credit <- rlang::eval_tidy(credit, df)
  debit <- rlang::eval_tidy(debit, df)
  if (!identical(vctrs::vec_ptype(credit), vctrs::vec_ptype(debit))) {
    stop(call. = FALSE, "`credit` and `debit` must be of the same prototype.")
  }

  if (!is.null(account_id)) {
    if (rlang::is_false(account_id %in% c(credit, debit))) {
      stop(call. = FALSE,
           "`account_id` must be a value present in `credit` and/or `debit`.")
    }
  }
}

deb_ptype_check <- function(x) {
  if (!deb_is_lsd(x) && !deb_is_decimal(x)) {
    stop(call. = FALSE,
         "`lsd` must be either a <deb_lsd> or a <deb_decimal> object.")
  }
}
