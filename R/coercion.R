## Coercion for deb_lsd and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# deb_lsd and deb_lsd

#' @export
vec_ptype2.deb_lsd.deb_lsd <- function(x, y, ...) {
  bases_equal(x, y)
  new_lsd(bases = deb_bases(x))
}

# deb_lsd and double

#' @export
vec_ptype2.deb_lsd.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.deb_lsd <- function(x, y, ...) y

# deb_lsd and integer

#' @export
vec_ptype2.deb_lsd.integer <- function(x, y, ...) x

#' @export
vec_ptype2.integer.deb_lsd <- function(x, y, ...) y

# deb_decimal -------------------------------------------------------------

# deb_decimal and deb_decimal

#' @export
vec_ptype2.deb_decimal.deb_decimal <- function(x, y, ...) {
  bases_equal(x, y)
  unit <- unit_hierarchy(x, y)

  new_decimal(bases = deb_bases(x), unit = unit)
}

# deb_decimal and double

#' @export
vec_ptype2.deb_decimal.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.deb_decimal <- function(x, y, ...) y

# deb_decimal and integer

#' @export
vec_ptype2.deb_decimal.integer <- function(x, y, ...) x

#' @export
vec_ptype2.integer.deb_decimal <- function(x, y, ...) y

# deb_lsd and deb_decimal -------------------------------------------------

#' @export
vec_ptype2.deb_lsd.deb_decimal <- function(x, y, ...) x

#' @export
vec_ptype2.deb_decimal.deb_lsd <- function(x, y, ...) y
