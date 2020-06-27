## Arithmetic for deb_lsd and deb_decimal ##

#' Math group with `deb_lsd` vectors
#'
#' @description
#' Math and Summary group of functions with `deb_lsd` vectors.
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
#' @param x An vector of class `deb_lsd`.
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

#' @rdname mathematics
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

#' @rdname mathematics
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
  field(x, "d") <- round(field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
signif.deb_lsd <- function(x, digits = 6) {
  field(x, "d") <- signif(field(x, "d"), digits = digits)
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
ceiling.deb_lsd <- function(x) {
  x <- decimal_check(x)
  field(x, "d") <- ceiling(field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
#' @export
floor.deb_lsd <- function(x) {
  x <- decimal_check(x)
  field(x, "d") <- floor(field(x, "d"))
  deb_normalize(x)
}

#' @rdname mathematics
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
#' @export
vec_math.deb_lsd <- function(.fn, .x, ...) {
  rlang::abort(paste0("`", .fn, ".", class(.x)[[1]], "()` not implemented."))
}



# Arithmetic operators ----------------------------------------------------

#' Arithmetic operations for debvctrs
#' @param x,y Vectors.
#' @param op Arithmetic operation.
#' @name arithmetic
NULL

# deb_lsd arithmetic operators --------------------------------------------

## Arithmetic boilerplate ##

#' @rdname arithmetic
#' @method vec_arith deb_lsd
#' @export
vec_arith.deb_lsd <- function(op, x, y) {
  UseMethod("vec_arith.deb_lsd", y)
}

#' @rdname arithmetic
#' @method vec_arith.deb_lsd default
#' @export
vec_arith.deb_lsd.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}


# Operators with lsd and lsd ----------------------------------------------

lsd_plus <- function(x, y) {
  c(x, y) %<-% vec_recycle_common(x, y)

  ret <- new_lsd(field(x, "l") + field(y, "l"),
                 field(x, "s") + field(y, "s"),
                 field(x, "d") + field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

lsd_minus <- function(x, y) {
  c(x, y) %<-% vec_recycle_common(x, y)

  ret <- new_lsd(field(x, "l") - field(y, "l"),
                 field(x, "s") - field(y, "s"),
                 field(x, "d") - field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' @rdname arithmetic
#' @method vec_arith.deb_lsd deb_lsd
#' @export
vec_arith.deb_lsd.deb_lsd <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(x, y),
    "-" = lsd_minus(x, y),
    "/" = as.double(x) / as.double(y),
    stop_incompatible_op(op, x, y)
  )
}


# Operators with deb_lsd and numeric --------------------------------------

lsd_multiply <- function(x, multiplier) {
  c(x, multiplier) %<-% vec_recycle_common(x, multiplier)

  ret <- new_lsd(field(x, "l") * multiplier,
                 field(x, "s") * multiplier,
                 field(x, "d") * multiplier,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# Divide lsd by numeric
lsd_dividend <- function(x, divisor) {
  c(x, divisor) %<-% vec_recycle_common(x, divisor)

  ret <- new_lsd(field(x, "l") / divisor,
                 field(x, "s") / divisor,
                 field(x, "d") / divisor,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

# Divide numeric by lsd
lsd_divisor <- function(dividend, x) {
  c(dividend, x) %<-% vec_recycle_common(dividend, x)

  ret <- dividend / deb_as_decimal(x)

  deb_as_lsd(ret)
}

# deb_lsd and numeric

#' @rdname arithmetic
#' @method vec_arith.deb_lsd numeric
#' @export
vec_arith.deb_lsd.numeric <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(x, multiplier = y),
    "/" = lsd_dividend(x, divisor = y),
    stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_lsd

#' @rdname arithmetic
#' @method vec_arith.numeric deb_lsd
#' @export
vec_arith.numeric.deb_lsd <- function(op, x, y) {
  switch(
    op,
    "*" = lsd_multiply(y, multiplier = x),
    "/" = lsd_divisor(dividend = x, y),
    stop_incompatible_op(op, x, y)
  )
}


# Unary operators with deb_lsd --------------------------------------------

lsd_negate <- function(x) {
  field(x, "l") <- field(x, "l") * -1
  field(x, "s") <- field(x, "s") * -1
  field(x, "d") <- field(x, "d") * -1

  x
}

#' @rdname arithmetic
#' @method vec_arith.deb_lsd MISSING
#' @export
vec_arith.deb_lsd.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = lsd_negate(x),
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

# deb_decimal arithmetic operators ----------------------------------------

## Arithmetic boilerplate ##

#' @rdname arithmetic
#' @method vec_arith deb_decimal
#' @export
vec_arith.deb_decimal <- function(op, x, y) {
  UseMethod("vec_arith.deb_decimal", y)
}

#' @rdname arithmetic
#' @method vec_arith.deb_decimal default
#' @export
vec_arith.deb_decimal.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}


# Operators with deb_decimal and deb_decimal ------------------------------

dec_arithmetic <- function(op, x, y) {
  xy <- vec_cast_common(x, y)
  vec_arith_base(op, xy[[1]], xy[[2]])
}

#' @rdname arithmetic
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
    stop_incompatible_op(op, x, y)
  )
}


# Operators with deb_decimal and numeric ----------------------------------

#' @rdname arithmetic
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
    "%/%" = new_decimal(vec_arith_base(op, x, y),
                        unit = deb_unit(x),
                        bases = deb_bases(x)),
    stop_incompatible_op(op, x, y)
  )
}

# numeric and deb_decimal

#' @rdname arithmetic
#' @method vec_arith.numeric deb_decimal
#' @export
vec_arith.numeric.deb_decimal <- function(op, x, y) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "*" = ,
    "/" = new_decimal(vec_arith_base(op, x, y),
                      unit = deb_unit(y),
                      bases = deb_bases(y)),
    stop_incompatible_op(op, x, y)
  )
}


# Unary operators with deb_decimal ----------------------------------------

#' @rdname arithmetic
#' @method vec_arith.deb_decimal MISSING
#' @export
vec_arith.deb_decimal.MISSING <- function(op, x, y) {
  switch(
    op,
    `-` = x * -1,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}


# Operators with deb_lsd and deb_decimal ----------------------------------

# deb_lsd and deb_decimal

#' @rdname arithmetic
#' @method vec_arith.deb_lsd deb_decimal
#' @export
vec_arith.deb_lsd.deb_decimal <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(x, deb_as_lsd(y)),
    "-" = lsd_minus(x, deb_as_lsd(y)),
    "/" = as.double(x) / vec_data(y),
    stop_incompatible_op(op, x, y)
  )
}

# deb_decimal and deb_lsd

#' @rdname arithmetic
#' @method vec_arith.deb_decimal deb_lsd
#' @export
vec_arith.deb_decimal.deb_lsd <- function(op, x, y) {
  bases_equal(x, y)

  switch(
    op,
    "+" = lsd_plus(deb_as_lsd(x), y),
    "-" = lsd_minus(deb_as_lsd(x), y),
    "/" = vec_data(x) / as.double(y),
    stop_incompatible_op(op, x, y)
  )
}
