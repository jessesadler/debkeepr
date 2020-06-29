## Arithmetic operations for deb_lsd and deb_decimal ##

#' Arithmetic operations for debkeepr
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

#' Addition for deb_lsd
#'
#' @keywords internal

lsd_plus <- function(x, y) {
  c(x, y) %<-% vec_recycle_common(x, y)

  ret <- new_lsd(field(x, "l") + field(y, "l"),
                 field(x, "s") + field(y, "s"),
                 field(x, "d") + field(y, "d"),
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' Subtraction for deb_lsd
#'
#' @keywords internal

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

#' Multiplication for deb_lsd and numeric
#'
#' @keywords internal

lsd_multiply <- function(x, multiplier) {
  c(x, multiplier) %<-% vec_recycle_common(x, multiplier)

  ret <- new_lsd(field(x, "l") * multiplier,
                 field(x, "s") * multiplier,
                 field(x, "d") * multiplier,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' Division for deb_lsd and numeric
#'
#' @keywords internal

lsd_dividend <- function(x, divisor) {
  c(x, divisor) %<-% vec_recycle_common(x, divisor)

  ret <- new_lsd(field(x, "l") / divisor,
                 field(x, "s") / divisor,
                 field(x, "d") / divisor,
                 bases = deb_bases(x))

  deb_normalize(ret)
}

#' Division for numeric over deb_lsd
#'
#' @keywords internal

lsd_divisor <- function(dividend, x) {
  c(dividend, x) %<-% vec_recycle_common(dividend, x)

  ret <- dividend / deb_as_decimal(x)

  deb_as_lsd(ret)
}

# Implement deb_lsd() and numeric()

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

# Implement numeric() and deb_lsd()

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

#' Make deb_lsd negative
#'
#' @keywords internal

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

#' Arithmetic operations for deb_decimal
#'
#' Stand in for vec_arith_base() that converts unit attributes
#' if they are different.
#'
#' @keywords internal

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
