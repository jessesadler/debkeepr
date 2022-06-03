## Coercion for deb_lsd, deb_tetra, and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# deb_lsd and deb_lsd

#' @export
vec_ptype2.deb_lsd.deb_lsd <- function(x, y, ..., x_arg = "", y_arg = "") {
  bases_equal(x, y, x_arg = x_arg, y_arg = y_arg)
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
vec_ptype2.deb_decimal.deb_decimal <- function(x, y, ...,
                                               x_arg = "", y_arg = "") {
  bases <- bases_hierarchy(x, y, x_arg = x_arg, y_arg = y_arg)
  unit <- unit_hierarchy(x, y)

  new_decimal(bases = bases, unit = unit)
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


# deb_tetra ---------------------------------------------------------------

# deb_tetra and deb_tetra

#' @export
vec_ptype2.deb_tetra.deb_tetra <- function(x, y, ..., x_arg = "", y_arg = "") {
  bases_equal(x, y, x_arg = x_arg, y_arg = y_arg)
  new_tetra(bases = deb_bases(x))
}

# deb_tetra and double

#' @export
vec_ptype2.deb_tetra.double <- function(x, y, ...) x

#' @export
vec_ptype2.double.deb_tetra <- function(x, y, ...) y

# deb_tetra and integer

#' @export
vec_ptype2.deb_tetra.integer <- function(x, y, ...) x

#' @export
vec_ptype2.integer.deb_tetra <- function(x, y, ...) y


# deb_lsd and deb_decimal -------------------------------------------------

#' @export
vec_ptype2.deb_lsd.deb_decimal <- function(x, y, ...) x

#' @export
vec_ptype2.deb_decimal.deb_lsd <- function(x, y, ...) y


# deb_lsd and deb_tetra ---------------------------------------------------

#' @export
vec_ptype2.deb_lsd.deb_tetra <- function(x, y, ...) x

#' @export
vec_ptype2.deb_tetra.deb_lsd <- function(x, y, ...) y


# deb_tetra and deb_decimal -------------------------------------------------

#' @export
vec_ptype2.deb_tetra.deb_decimal <- function(x, y, ...) x

#' @export
vec_ptype2.deb_decimal.deb_tetra <- function(x, y, ...) y
