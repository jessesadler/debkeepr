## Arithmetic for deb_lsd and deb_decimal ##

#' Math group with `deb_lsd` objects
#'
#' @description
#' Math and Summary group of functions with `deb_lsd` objects.
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
#' `sum()` and `cumsum()` return a normalized `deb_lsd` values.
#'
#' Round family of functions only affect the denarius (`d`) unit of a
#' `deb_lsd` value. All values are normalized.
#'
#' If you need a wider implementation of Math and Summary group functions,
#' use a `deb_decimal` vector. However, `median()`, `quantile()`, and
#' `summary()` are also not currently implemented for `deb_decimal` vectors.
#' To use these functions cast `deb_lsd` and `deb_decimal` vectors to numeric.
#'
#' @param x An object of class `deb_lsd`.
#' @param ... `deb_lsd` vectors in `sum()` and arguments passed on to
#'   further methods in `mean()`.
#' @param na.rm Logical. Should missing values (including `NaN``) be removed?
#' @param digits Integer. Indicating the number of decimal places
#'   (`round()`) or significant digits (`signif()`) to be used.
#'
#' @return A `deb_lsd` vector with normalized values.
#'
#' @examples
#' x <- deb_lsd(l = c(5, 8, 12),
#'              s = c(16, 6, 13),
#'              d = c(6, 11, 0))
#'
#' # All values are normalized with sum and cumsum
#' sum(x)
#' cumsum(x)
#' mean(x)
#'
#' # Round family on deb_lsd affects the denarius unit
#' x2 <- deb_lsd(5, 12, 5.8365)
#' round(x2)
#' round(x2, digits = 2)
#' signif(x2, digits = 2)
#' ceiling(x2)
#' floor(x2)
#' trunc(x2)
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

#' @rdname mathematics
#' @export
sum.deb_lsd <- function(..., na.rm = FALSE) {
  x <- vctrs::vec_c(...)
  # Remove NA so fields that are not NA are not added
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  ret <- new_lsd(sum(vctrs::field(x, "l"), na.rm = na.rm),
                 sum(vctrs::field(x, "s"), na.rm = na.rm),
                 sum(vctrs::field(x, "d"), na.rm = na.rm),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @rdname mathematics
#' @export
mean.deb_lsd <- function(x, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }
  sum(x) / vctrs::vec_size(x)
}

#' @export
abs.deb_lsd <- function(x) {
  dec <- deb_as_decimal(x)
  deb_as_lsd(abs(dec))
}


# Cumulative functions

#' @rdname mathematics
#' @export
cumsum.deb_lsd <- function(x) {
  ret <- new_lsd(cumsum(vctrs::field(x, "l")),
                 cumsum(vctrs::field(x, "s")),
                 cumsum(vctrs::field(x, "d")),
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
  vec_math("is.finite", deb_as_decimal(x))
}

#' @export
is.infinite.deb_lsd <- function(x) {
  vec_math("is.infinite", deb_as_decimal(x))
}

#' @export
is.nan.deb_lsd <- function(x) {
  vec_math("is.nan", deb_as_decimal(x))
}

# Rounding ----------------------------------------------------------------

#' @rdname mathematics
#' @export
round.deb_lsd <- function(x, digits = 0) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- round(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
signif.deb_lsd <- function(x, digits = 6) {
  vctrs::field(x, "d") <- signif(vctrs::field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
ceiling.deb_lsd <- function(x) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- ceiling(vctrs::field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
floor.deb_lsd <- function(x) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- floor(vctrs::field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
trunc.deb_lsd <- function(x, ...) {
  x <- decimal_check(x)
  vctrs::field(x, "d") <- trunc(vctrs::field(x, "d"))
  deb_normalize(x)
}

# Methods

#' @rdname vctrs-compat
#' @method vec_math deb_lsd
#' @export
#' @export vec_math.deb_lsd
vec_math.deb_lsd <- function(.fn, .x, ...) {
  stop(call. = FALSE,
       paste0("`", .fn, ".", class(.x)[[1]], "()` not implemented."))
}


# deb_lsd arithmetic operators --------------------------------------------

## Arithmetic boilerplate ##

#' @rdname vctrs-compat
#' @method vec_arith deb_lsd
#' @export
#' @export vec_arith.deb_lsd
vec_arith.deb_lsd <- function(op, x, y) {
  UseMethod("vec_arith.deb_lsd", y)
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd default
#' @export
vec_arith.deb_lsd.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}


# Operators with lsd and lsd ----------------------------------------------

lsd_plus <- function(x, y) {
  c(x, y) %<-% vctrs::vec_recycle_common(x, y)

  ret <- new_lsd(vctrs::field(x, "l") + vctrs::field(y, "l"),
                 vctrs::field(x, "s") + vctrs::field(y, "s"),
                 vctrs::field(x, "d") + vctrs::field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

lsd_minus <- function(x, y) {
  c(x, y) %<-% vctrs::vec_recycle_common(x, y)

  ret <- new_lsd(vctrs::field(x, "l") - vctrs::field(y, "l"),
                 vctrs::field(x, "s") - vctrs::field(y, "s"),
                 vctrs::field(x, "d") - vctrs::field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd deb_lsd
#' @export
vec_arith.deb_lsd.deb_lsd <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(x, y),
    "-" = lsd_minus(x, y),
    "/" = as.double(x) / as.double(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Operators with deb_lsd and numeric --------------------------------------

lsd_multiply <- function(x, multiplier) {
  c(x, multiplier) %<-% vctrs::vec_recycle_common(x, multiplier)

  ret <- new_lsd(vctrs::field(x, "l") * multiplier,
                 vctrs::field(x, "s") * multiplier,
                 vctrs::field(x, "d") * multiplier,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

lsd_divide <- function(x, divisor) {
  c(x, divisor) %<-% vctrs::vec_recycle_common(x, divisor)

  ret <- new_lsd(vctrs::field(x, "l") / divisor,
                 vctrs::field(x, "s") / divisor,
                 vctrs::field(x, "d") / divisor,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# deb_lsd and numeric

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd numeric
#' @export
vec_arith.deb_lsd.numeric <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(x, multiplier = y),
    "/" = lsd_divide(x, divisor = y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_lsd

#' @rdname vctrs-compat
#' @method vec_arith.numeric deb_lsd
#' @export
vec_arith.numeric.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(y, multiplier = x),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Unary operators with deb_lsd --------------------------------------------

lsd_negate <- function(x) {
  vctrs::field(x, "l") <- vctrs::field(x, "l") * -1
  vctrs::field(x, "s") <- vctrs::field(x, "s") * -1
  vctrs::field(x, "d") <- vctrs::field(x, "d") * -1

  x
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd MISSING
#' @export
vec_arith.deb_lsd.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = lsd_negate(x),
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# deb_decimal arithmetic operators ----------------------------------------

## Arithmetic boilerplate ##

#' @rdname vctrs-compat
#' @method vec_arith deb_decimal
#' @export
#' @export vec_arith.deb_decimal
vec_arith.deb_decimal <- function(op, x, y) {
  UseMethod("vec_arith.deb_decimal", y)
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal default
#' @export
vec_arith.deb_decimal.default <- function(op, x, y) {
  vctrs::stop_incompatible_op(op, x, y)
}


# Operators with deb_decimal and deb_decimal ------------------------------

dec_arithmetic <- function(op, x, y) {
  xy <- vctrs::vec_cast_common(x, y)
  vctrs::vec_arith_base(op, xy[[1]], xy[[2]])
}

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal deb_decimal
#' @export
vec_arith.deb_decimal.deb_decimal <- function(op, x, y) {
  # Ensure bases are equal
  bases_equal(x, y)

  switch(
    op,
    "+" = ,
    "-" = new_decimal(dec_arithmetic(op, x, y),
                      unit = unit_hierarchy(x, y),
                      bases = deb_bases(x)),
    "/" = dec_arithmetic(op, x, y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Operators with deb_decimal and numeric ----------------------------------

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal numeric
#' @export
vec_arith.deb_decimal.numeric <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "/" = ,
    "*" = ,
    "^" = ,
    "%%" = ,
    "%/%" = new_decimal(vctrs::vec_arith_base(op, x, y),
                        unit = deb_unit(x),
                        bases = deb_bases(x)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_decimal

#' @rdname vctrs-compat
#' @method vec_arith.numeric deb_decimal
#' @export
vec_arith.numeric.deb_decimal <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "*" = new_decimal(vctrs::vec_arith_base(op, x, y),
                        unit = deb_unit(y),
                        bases = deb_bases(y)),
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Unary operators with deb_decimal ----------------------------------------

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal MISSING
#' @export
vec_arith.deb_decimal.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = x * -1,
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}


# Operators with deb_lsd and deb_decimal ----------------------------------

# deb_lsd and deb_decimal

#' @rdname vctrs-compat
#' @method vec_arith.deb_lsd deb_decimal
#' @export
vec_arith.deb_lsd.deb_decimal <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(x, deb_as_lsd(y)),
    "-" = lsd_minus(x, deb_as_lsd(y)),
    "/" = as.double(x) / vctrs::vec_data(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

# deb_decimal and deb_lsd

#' @rdname vctrs-compat
#' @method vec_arith.deb_decimal deb_lsd
#' @export
vec_arith.deb_decimal.deb_lsd <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(deb_as_lsd(x), y),
    "-" = lsd_minus(deb_as_lsd(x), y),
    "/" = vctrs::vec_data(x) / as.double(y),
    vctrs::stop_incompatible_op(op, x, y)
  )
}
