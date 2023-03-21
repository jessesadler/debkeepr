## Mathematical functions with deb_lsd and deb_tetra##

#' Math group with `deb_lsd` and `deb_tetra` vectors
#'
#' @description
#' Math and Summary group of functions with `deb_lsd` and `deb_tetra` vectors.
#' Implemented functions:
#' - [Summary] group: `sum()`, `any()`, and `all()`.
#' - [Math] group: `abs()`, `round()`, `signif()`, `ceiling()`,
#'   `floor()`, `trunc()`, `cummax()`, `cummin()`, and `cumsum()`.
#' - Additional generics: `mean()`, `is.nan()`, `is.finite()`, and
#'   `is.infinite()`.
#'
#' All other functions from the groups not currently implemented,
#'   including `median()`, `quantile()`, and `summary()`.
#'
#' @details
#' `sum()` and `cumsum()` return a normalized `deb_lsd` or `deb_tetra` values.
#'
#' Round family of functions only affect the denarius (`d`) unit of a
#' `deb_lsd` value and the farthing (`f`) unit of `deb_tetra` value.
#' All values are normalized.
#'
#' If you need a wider implementation of Math and Summary group functions,
#' use a `deb_decimal` vector. However, `median()`, `quantile()`, and
#' `summary()` are also not currently implemented for `deb_decimal` vectors.
#' To use these functions cast `deb_lsd`, `deb_tetra`, and `deb_decimal`
#' vectors to numeric.
#'
#' @param x An vector of class `deb_lsd` or `deb_tetra`.
#' @param ... `deb_lsd` or `deb_tetra` vectors in `sum()` and arguments
#'   passed on to further methods in `mean()`.
#' @param na.rm Logical. Should missing values (including `NaN``) be removed?
#' @param digits Integer. Indicating the number of decimal places
#'   (`round()`) or significant digits (`signif()`) to be used.
#'
#' @returns A `deb_lsd` or `deb_tetra` vector with normalized values.
#'
#' @examples
#' x <- deb_lsd(l = c(5, 8, 12),
#'              s = c(16, 6, 13),
#'              d = c(6, 11, 0))
#' y <- deb_tetra(l = c(5, 8, 12),
#'                s = c(16, 6, 13),
#'                d = c(6, 11, 0),
#'                f = c(3, 2, 3))
#'
#' # All values are normalized with sum and cumsum
#' sum(x)
#' sum(y)
#' cumsum(x)
#' cumsum(y)
#' mean(x)
#' mean(y)
#'
#' # Round family on deb_lsd affects the denarius unit
#' x2 <- deb_lsd(5, 12, 5.8365)
#' y2 <- deb_tetra(5, 12, 8, 4.125)
#' round(x2)
#' round(y2)
#' round(x2, digits = 2)
#' signif(x2, digits = 2)
#' ceiling(x2)
#' ceiling(y2)
#' floor(x2)
#' floor(y2)
#' trunc(x2)
#' trunc(y2)
#'
#' # The returned values are normalized whether
#' # they are positive or negative
#' x3 <- deb_lsd(9, 19, 11.825)
#' x4 <- deb_lsd(-9, -19, -11.825)
#' round(x3)
#' round(x3, digits = 1)
#'
#' ceiling(x3)
#' floor(x4)
#'
#' trunc(x3)
#' trunc(x4)
#'
#' @name mathematics
NULL

# deb_lsd mathematic functions --------------------------------------------

#' @export
sum.deb_lsd <- function(..., na.rm = FALSE) {
  x <- vec_c(...)
  # Remove NA so fields that are not NA are not added
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  ret <- new_lsd(sum(field(x, "l"), na.rm = na.rm),
                 sum(field(x, "s"), na.rm = na.rm),
                 sum(field(x, "d"), na.rm = na.rm),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @export
mean.deb_lsd <- function(x, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  sum(x) / vec_size(x)
}

#' @export
abs.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(abs(dec))
}


# Cumulative functions

#' @export
cumsum.deb_lsd <- function(x) {
  ret <- new_lsd(cumsum(field(x, "l")),
                 cumsum(field(x, "s")),
                 cumsum(field(x, "d")),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @export
cummin.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(cummin(dec))
}

#' @export
cummax.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(cummax(dec))
}

# Finite and infinite

#' @export
is.finite.deb_lsd <- function(x) {
  !vec_detect_missing(x)
}

#' @export
is.infinite.deb_lsd <- function(x) {
  vec_rep(FALSE, vec_size(x))
}

#' @export
is.nan.deb_lsd <- function(x) {
  vec_rep(FALSE, vec_size(x))
}

# lsd rounding ------------------------------------------------------------

#' @export
round.deb_lsd <- function(x, digits = 0) {
  x <- decimal_check(x)
  field(x, "d") <- round(field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @export
signif.deb_lsd <- function(x, digits = 6) {
  field(x, "d") <- signif(field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @export
ceiling.deb_lsd <- function(x) {
  x <- decimal_check(x)
  field(x, "d") <- ceiling(field(x, "d"))
  deb_normalize(x)
}

#' @export
floor.deb_lsd <- function(x) {
  x <- decimal_check(x)
  field(x, "d") <- floor(field(x, "d"))
  deb_normalize(x)
}

#' @export
trunc.deb_lsd <- function(x, ...) {
  x <- decimal_check(x)
  field(x, "d") <- trunc(field(x, "d"))
  deb_normalize(x)
}

#' Error message for unimplemented mathematics functions
#' @param .fn A mathematical function from the base package.
#' @param .x A vector.
#' @param ... Additional arguments passed to `.fn`.
#' @returns A `deb_lsd` vector.
#' @export
vec_math.deb_lsd <- function(.fn, .x, ...) {
  # Implement sum to allow various types of deb-style vectors in ...
  switch(.fn,
         sum = sum(.x),
         rlang::abort(paste0("`", .fn, ".", class(.x)[[1]],
                             "()` not implemented."))
         )
}


# deb_tetra mathematic functions ------------------------------------------

#' @export
sum.deb_tetra <- function(..., na.rm = FALSE) {
  x <- vec_c(...)
  # Remove NA so fields that are not NA are not added
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  # Sum with deb_tetra and deb_lsd vectors is turned into
  # deb_lsd, so x will be deb_lsd after vec_c() above.
  # Enables: sum(tetra, lsd) == sum(lsd, tetra)
  if (deb_is_tetra(x)) {
    ret <- new_tetra(sum(field(x, "l"), na.rm = na.rm),
                     sum(field(x, "s"), na.rm = na.rm),
                     sum(field(x, "d"), na.rm = na.rm),
                     sum(field(x, "f"), na.rm = na.rm),
                     bases = deb_bases(x))
  } else if (deb_is_lsd(x)) {
    ret <- new_lsd(sum(field(x, "l"), na.rm = na.rm),
                   sum(field(x, "s"), na.rm = na.rm),
                   sum(field(x, "d"), na.rm = na.rm),
                   bases = deb_bases(x))
  }
  deb_normalize(ret)
}

#' @export
mean.deb_tetra <- function(x, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  sum(x) / vec_size(x)
}

#' @export
abs.deb_tetra <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_tetra(abs(dec))
}


# Cumulative functions

#' @export
cumsum.deb_tetra <- function(x) {
  ret <- new_tetra(cumsum(field(x, "l")),
                   cumsum(field(x, "s")),
                   cumsum(field(x, "d")),
                   cumsum(field(x, "f")),
                   bases = deb_bases(x))

  deb_normalize(ret)
}

#' @export
cummin.deb_tetra <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_tetra(cummin(dec))
}

#' @export
cummax.deb_tetra <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_tetra(cummax(dec))
}

# Finite and infinite

#' @export
is.finite.deb_tetra <- function(x) {
  !vec_detect_missing(x)
}

#' @export
is.infinite.deb_tetra <- function(x) {
  vec_rep(FALSE, vec_size(x))
}

#' @export
is.nan.deb_tetra <- function(x) {
  vec_rep(FALSE, vec_size(x))
}

# tetra rounding ----------------------------------------------------------

#' @export
round.deb_tetra <- function(x, digits = 0) {
  x <- decimal_check(x)
  field(x, "f") <- round(field(x, "f"), digits = digits)
  deb_normalize(x)
}

#' @export
signif.deb_tetra <- function(x, digits = 6) {
  field(x, "f") <- signif(field(x, "f"), digits = digits)
  deb_normalize(x)
}

#' @export
ceiling.deb_tetra <- function(x) {
  x <- decimal_check(x)
  field(x, "f") <- ceiling(field(x, "f"))
  deb_normalize(x)
}

#' @export
floor.deb_tetra <- function(x) {
  x <- decimal_check(x)
  field(x, "f") <- floor(field(x, "f"))
  deb_normalize(x)
}

#' @export
trunc.deb_tetra <- function(x, ...) {
  x <- decimal_check(x)
  field(x, "f") <- trunc(field(x, "f"))
  deb_normalize(x)
}

#' Error message for unimplemented mathematics functions
#' @param .fn A mathematical function from the base package.
#' @param .x A vector.
#' @param ... Additional arguments passed to `.fn`.
#' @returns A `deb_tetra` vector.
#' @export
vec_math.deb_tetra <- function(.fn, .x, ...) {
  # Implement sum to allow various types of deb-style vectors in ...
  switch(.fn,
         sum = sum(.x),
         rlang::abort(paste0("`", .fn, ".", class(.x)[[1]],
                             "()` not implemented."))
  )
}
