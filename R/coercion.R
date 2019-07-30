## Coercion for deb_lsd and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# Boilerplate

#' @rdname vctrs-compat
#' @method vec_ptype2 deb_lsd
#' @export
#' @export vec_ptype2.deb_lsd
vec_ptype2.deb_lsd <- function(x, y, ...) UseMethod("vec_ptype2.deb_lsd", y)

#' @method vec_ptype2.deb_lsd default
#' @export
vec_ptype2.deb_lsd.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# deb_lsd and deb_lsd

#' @method vec_ptype2.deb_lsd deb_lsd
#' @export
vec_ptype2.deb_lsd.deb_lsd <- function(x, y, ...) {
  bases_equal(x, y)
  new_lsd(bases = deb_bases(x))
}

# deb_lsd and double

#' @method vec_ptype2.deb_lsd double
#' @export
vec_ptype2.deb_lsd.double <- function(x, y, ...) x

#' @method vec_ptype2.double deb_lsd
#' @export
vec_ptype2.double.deb_lsd <- function(x, y, ...) y

# deb_lsd and integer

#' @method vec_ptype2.deb_lsd integer
#' @export
vec_ptype2.deb_lsd.integer <- function(x, y, ...) x

#' @method vec_ptype2.integer deb_lsd
#' @export
vec_ptype2.integer.deb_lsd <- function(x, y, ...) y

# deb_decimal -------------------------------------------------------------

# Boilerplate
#' @rdname vctrs-compat
#' @method vec_ptype2 deb_decimal
#' @export
#' @export vec_ptype2.deb_decimal
vec_ptype2.deb_decimal <- function(x, y, ...) {
  UseMethod("vec_ptype2.deb_decimal", y)
}

#' @method vec_ptype2.deb_decimal default
#' @export
vec_ptype2.deb_decimal.default <- function(x, y, ...,
                                           x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# deb_decimal and deb_decimal

#' @method vec_ptype2.deb_decimal deb_decimal
#' @export
vec_ptype2.deb_decimal.deb_decimal <- function(x, y, ...) {
  bases_equal(x, y)
  unit <- unit_hierarchy(x, y)

  new_decimal(bases = deb_bases(x), unit = unit)
}

# deb_decimal and double

#' @method vec_ptype2.deb_decimal double
#' @export
vec_ptype2.deb_decimal.double <- function(x, y, ...) x

#' @method vec_ptype2.double deb_decimal
#' @export
vec_ptype2.double.deb_decimal <- function(x, y, ...) y

# deb_decimal and integer

#' @method vec_ptype2.deb_decimal integer
#' @export
vec_ptype2.deb_decimal.integer <- function(x, y, ...) x

#' @method vec_ptype2.integer deb_decimal
#' @export
vec_ptype2.integer.deb_decimal <- function(x, y, ...) y

# deb_lsd and deb_decimal -------------------------------------------------

# deb_lsd and double

#' @method vec_ptype2.deb_lsd deb_decimal
#' @export
vec_ptype2.deb_lsd.deb_decimal <- function(x, y, ...) x

#' @method vec_ptype2.deb_decimal deb_lsd
#' @export
vec_ptype2.deb_decimal.deb_lsd <- function(x, y, ...) y
