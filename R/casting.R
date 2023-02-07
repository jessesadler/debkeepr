## Casting for deb_lsd, deb_tetra, and deb_decimal ##

# deb_lsd -----------------------------------------------------------------

# deb_lsd to deb_lsd

#' @export
vec_cast.deb_lsd.deb_lsd <- function(x, to, ...) {
  bases_equal(x, to)
  x
}

# deb_lsd to double

#' @export
vec_cast.double.deb_lsd <- function(x, to, ...) {
  l <- field(x, "l")
  s <- field(x, "s")
  d <- field(x, "d")
  bases <- deb_bases(x)

  l + s / bases[[1]] + d / prod(bases)
}

# double to deb_lsd

#' @export
vec_cast.deb_lsd.double <- function(x, to, ...) {
# If statements enables casting from a numeric prototype:
  if (vec_size(x) == 0) {
    deb_lsd(bases = deb_bases(to))
  } else {
    lsd <- deb_lsd(x, 0, 0, bases = deb_bases(to))
    deb_normalize(lsd)
  }
}

# integer to deb_lsd

#' @export
vec_cast.deb_lsd.integer <- function(x, to, ...) {
  # If statements enables casting from a numeric prototype:
  if (vec_size(x) == 0) {
    deb_lsd(bases = deb_bases(to))
  } else {
    lsd <- deb_lsd(x, 0, 0, bases = deb_bases(to))
  }
}

# deb_lsd to character
# Enables View(as.data.frame(deb_lsd))

#' @export
vec_cast.character.deb_lsd <- function(x, to, ...) {
  format(x, ...)
}

# deb_decimal -------------------------------------------------------------

# deb_decimal to deb_decimal
# Logic to convert between units

#' @export
vec_cast.deb_decimal.deb_decimal <- function(x, to, ...) {
  # Deal with possibility of mixed bases: tetra becomes lsd
  # Both lsd or both tetra
  if (vec_size(deb_bases(x)) == vec_size(deb_bases(to))) {
    bases_equal(x, to)
    bases <- deb_bases(x)
  # x is lsd; y is tetra
  } else if (vec_size(deb_bases(x)) == 2L) {
    mixed_bases_equal(x, to)
    bases <- deb_bases(to)
  # x is tetra; to is lsd
  } else {
    mixed_bases_equal(to, x)
    bases <- deb_bases(to)
    # If unit of x is f, convert to d
    if (deb_unit(x) == "f") {
      x <- deb_convert_unit(x, "d")
    }
  }

  from_unit <- deb_unit(x)
  to_unit <- deb_unit(to)

  # If unit is the same return x, but make sure bases are changed
  # if there are mixed bases.
  if (from_unit == to_unit) {
    attr(x, "bases") <- bases
    return(x)
  }
  # Deal with conversion of units
  # Need separate case_when() functions for lsd and tetra bases
  # to avoid bases[[3]] from being out of bounds.

  if (vec_size(bases) == 2L) {
    converted <- dplyr::case_when(
      from_unit == "l" & to_unit == "s" ~ x * bases[[1]],
      from_unit == "l" & to_unit == "d" ~ x * prod(bases[1:2]),
      from_unit == "s" & to_unit == "d" ~ x * bases[[2]],
      from_unit == "s" & to_unit == "l" ~ x / bases[[1]],
      from_unit == "d" & to_unit == "l" ~ x / prod(bases[1:2]),
      from_unit == "d" & to_unit == "s" ~ x / bases[[2]]
    )
  } else {
    converted <- dplyr::case_when(
      from_unit == "l" & to_unit == "s" ~ x * bases[[1]],
      from_unit == "l" & to_unit == "d" ~ x * prod(bases[1:2]),
      from_unit == "s" & to_unit == "d" ~ x * bases[[2]],
      from_unit == "s" & to_unit == "l" ~ x / bases[[1]],
      from_unit == "d" & to_unit == "l" ~ x / prod(bases[1:2]),
      from_unit == "d" & to_unit == "s" ~ x / bases[[2]],
      # With tetra farthing unit
      from_unit == "l" & to_unit == "f" ~ x * prod(bases),
      from_unit == "s" & to_unit == "f" ~ x * prod(bases[2:3]),
      from_unit == "d" & to_unit == "f" ~ x * bases[[3]],
      from_unit == "f" & to_unit == "l" ~ x / prod(bases),
      from_unit == "f" & to_unit == "s" ~ x / prod(bases[2:3]),
      from_unit == "f" & to_unit == "d" ~ x / bases[[3]]
    )
  }

  attr(converted, "unit") <- to_unit
  attr(converted, "bases") <- bases

  converted
}

# double to deb_decimal and back

#' @export
vec_cast.deb_decimal.double  <- function(x, to, ...) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

#' @export
vec_cast.double.deb_decimal  <- function(x, to, ...) vec_data(x)

# integer to deb_decimal

#' @export
vec_cast.deb_decimal.integer  <- function(x, to, ...) {
  deb_decimal(x, unit = deb_unit(to), bases = deb_bases(to))
}

# deb_decimal to character

#' @export
vec_cast.character.deb_decimal <- function(x, to, ...) {
  as.character(vec_data(x))
}

# deb_tetra ---------------------------------------------------------------

# deb_tetra to deb_tetra

#' @export
vec_cast.deb_tetra.deb_tetra <- function(x, to, ...) {
  bases_equal(x, to)
  x
}

# deb_tetra to double

#' @export
vec_cast.double.deb_tetra <- function(x, to, ...) {
  l <- field(x, "l")
  s <- field(x, "s")
  d <- field(x, "d")
  f <- field(x, "f")
  bases <- deb_bases(x)

  l + s / bases[[1]] + d / prod(bases[1:2]) + f / prod(bases)
}

# double to deb_tetra

#' @export
vec_cast.deb_tetra.double <- function(x, to, ...) {
  # If statements enables casting from a numeric prototype:
  if (vec_size(x) == 0) {
    deb_tetra(bases = deb_bases(to))
  } else {
    tetra <- deb_tetra(x, 0, 0, 0, bases = deb_bases(to))
    deb_normalize(tetra)
  }
}

# integer to deb_tetra

#' @export
vec_cast.deb_tetra.integer <- function(x, to, ...) {
  # If statements enables casting from a numeric prototype:
  if (vec_size(x) == 0) {
    deb_tetra(bases = deb_bases(to))
  } else {
    tetra <- deb_tetra(x, 0, 0, 0, bases = deb_bases(to))
  }
}

# deb_tetra to character
# Enables View(as.data.frame(deb_tetra))

#' @export
vec_cast.character.deb_tetra <- function(x, to, ...) {
  format(x, ...)
}

# lsd and list ------------------------------------------------------------

# list to deb_lsd
# Use base methods to not depend on purrr
# Based on compat-purrr.R in rlang

#' @export
vec_cast.deb_lsd.list <- function(x, to, ...) {
  lsd_list_check(x)

  deb_lsd(l = vapply(x, `[[`, i = 1, double(1)),
          s = vapply(x, `[[`, i = 2, double(1)),
          d = vapply(x, `[[`, i = 3, double(1)),
          bases = deb_bases(to))
}

# list to deb_decimal

#' @export
vec_cast.deb_decimal.list <- function(x, to, ...) {
  lsd_list_check(x)

  lsd <- vec_cast(x, to = deb_lsd(bases = deb_bases(to)))

  lsd_to_decimal(lsd, to)
}

# list to deb_tetra

#' @export
vec_cast.deb_tetra.list <- function(x, to, ...) {
  tetra_list_check(x)

  deb_tetra(l = vapply(x, `[[`, i = 1, double(1)),
            s = vapply(x, `[[`, i = 2, double(1)),
            d = vapply(x, `[[`, i = 3, double(1)),
            f = vapply(x, `[[`, i = 4, double(1)),
            bases = deb_bases(to))
}

# deb_lsd to list of deb_lsd values

#' @export
vec_cast.list.deb_lsd <- function(x, to, ...) {
  # unclass deb_lsd vector and create list of list
  # seq_along x to get i
  fields <- seq_along(x)
  inside_out <- lapply(fields, function(i) {
    lapply(unclass(x), .subset2, i)
  })
  # Flatten list
  lapply(inside_out, unlist, use.names = FALSE)
}

# deb_tetra to list of deb_tetra values

#' @export
vec_cast.list.deb_tetra <- function(x, to, ...) {
  # unclass deb_tetra vector and create list of list
  # seq_along x to get i
  fields <- seq_along(x)
  inside_out <- lapply(fields, function(i) {
    lapply(unclass(x), .subset2, i)
  })
  # Flatten list
  lapply(inside_out, unlist, use.names = FALSE)
}

# Cast deb_lsd or deb_tetra to list

#' Cast `deb_lsd` or `deb_tetra` to a list of values
#'
#' Cast a `deb_lsd` or `deb_tetra` vector to a list of numeric vectors either
#' three or four values per list item corresponding to lsd or tetra values.
#'
#' @details
#' `deb_as_list()` turns a `deb_lsd` or `deb_tetra` vector into a list of
#' numeric vectors of length 3 or 4. It is the inverse of `deb_as_lsd()` and
#' `deb_as_tetra()`. Compare to `as.list()`, which creates a list of `deb_lsd`
#' or `deb_tetra` vectors or `unclass()`, which creates a list of length 3 or
#' 4 with numeric vectors corresponding to the units.
#'
#' @param x A `deb_lsd` or `deb_tetra` vector to cast to a list of values.
#' @param ... Arguments passed on to further methods.
#'
#' @seealso [`deb_as_lsd()`] and [`deb_as_tetra()`] for the inverse of
#'   `deb_as_list()`.
#' @returns A list of numeric vectors of length 3 or 4, corresponding to lsd
#'   or tetra values.
#' @examples
#'
#' # deb_lsd vector
#' x <- deb_lsd(l = 0:3, s = 4:7, d = 8:11)
#'
#' deb_as_list(x)
#'
#' # deb_tetra vector
#'
#' y <- deb_tetra(l = 0:3, s = 4:7, d = 8:11, f = 1:4)
#'
#' deb_as_list(y)
#'
#' # This is the inverse of `deb_as_lsd()` of a list of lsd values
#' z <- deb_as_list(x)
#'
#' identical(x, deb_as_lsd(z))
#'
#' @name list-lsd
NULL

#' @rdname list-lsd
#' @export
deb_as_list  <- function(x, ...) {
  UseMethod("deb_as_list")
}

#' @rdname list-lsd
#' @export
deb_as_list.default <- function(x, ...) {
  cli::cli_abort(c("{.var x } must be a <deb_lsd> or <deb_tetra> vector.",
                   "x" = "You've supplied a {.cls {class(x)[[1]]}}."))
}

#' @rdname list-lsd
#' @export
deb_as_list.deb_lsd <- function(x, ...) {
  vec_cast(x, list())
}

#' @rdname list-lsd
#' @export
deb_as_list.deb_tetra <- function(x, ...) {
  vec_cast(x, list())
}


# To deb_lsd --------------------------------------------------------------

#' deb_decimal to deb_lsd utility
#'
#' Find unit and normalize
#' @keywords internal
#' @noRd

decimal_to_lsd <- function(x) {
  bases <- deb_bases(x)
  unit <- deb_unit(x)

  if (vec_size(bases) == 3L) {
    # If unit of x is f, convert to d
    if (deb_unit(x) == "f") {
      x <- deb_convert_unit(x, "d")
      unit <- "d"
    }
    # Convert bases to lsd bases
    bases <- bases[1:2]
  }

  if (unit == "l") {
    lsd <- deb_lsd(x, 0, 0, bases = bases)
  } else if (unit == "s") {
    lsd <- deb_lsd(0, x, 0, bases = bases)
  } else if (unit == "d") {
    lsd <- deb_lsd(0, 0, x, bases = bases)
  }
  deb_normalize(lsd)
}

#' @export
vec_cast.deb_lsd.deb_decimal <- function(x, to, ...) {
  # Distinguish between tetra and lsd bases
  if (vec_size(deb_bases(x)) == 2L) {
    bases_equal(x, to)
  } else {
    mixed_bases_equal(to, x)
  }

  decimal_to_lsd(x)
}

#' deb_tetra to deb_lsd utility
#'
#' Add f / farthing base to d
#' @keywords internal
#' @noRd

tetra_to_lsd <- function(x) {
  bases <- deb_bases(x)

  lsd <- new_lsd(l = field(x, "l"),
                 s = field(x, "s"),
                 d = field(x, "d") + field(x, "f") / bases[[3]],
                 bases = bases[1:2])

  deb_normalize(lsd)
}

#' @export
vec_cast.deb_lsd.deb_tetra <- function(x, to, ...) {
  mixed_bases_equal(to, x)

  tetra_to_lsd(x)
}


# To deb_decimal ----------------------------------------------------------

#' deb_lsd to deb_decimal utility
#'
#' Arithmetic based on the unit
#' @keywords internal
#' @noRd

lsd_to_decimal <- function(x, to) {
  l <- field(x, "l")
  s <- field(x, "s")
  d <- field(x, "d")
  bases <- deb_bases(x)
  unit <- deb_unit(to)

  if (vec_size(deb_bases(to)) == 3L) {
    bases <- deb_bases(to)
  }

  if (unit == "l") {
    decimalized <- l + s / bases[[1]] + d / prod(bases[1:2])
  } else if (unit == "s") {
    decimalized <- l * bases[[1]] + s + d / bases[[2]]
  } else if (unit == "d") {
    decimalized <- l * prod(bases[1:2]) + s * bases[[2]] + d
  } else if (unit == "f") {
    decimalized <- l * prod(bases) + s * prod(bases[2:3]) + d * bases[[3]]
  }
  new_decimal(x = decimalized,
              unit = unit,
              bases = bases)
}

#' @export
vec_cast.deb_decimal.deb_lsd <- function(x, to, ...) {
  # Distinguish between tetra and lsd bases
  # lsd casts to tetra deb_decimal only with subassignment
  if (vec_size(deb_bases(to)) == 2L) {
    bases_equal(x, to)
  } else {
    mixed_bases_equal(x, to)
  }

  lsd_to_decimal(x, to)
}


#' deb_tetra to deb_decimal utility
#'
#' Arithmetic based on the unit
#' @keywords internal
#' @noRd

tetra_to_decimal <- function(x, to) {
  l <- field(x, "l")
  s <- field(x, "s")
  d <- field(x, "d")
  f <- field(x, "f")
  bases <- deb_bases(x)
  unit <- deb_unit(to)

  if (unit == "l") {
    decimalized <- l + s / bases[[1]] + d / prod(bases[1:2]) + f / prod(bases)
  } else if (unit == "s") {
    decimalized <- l * bases[[1]] + s + d / bases[[2]] + f / prod(bases[2:3])
  } else if (unit == "d") {
    decimalized <- l * prod(bases[1:2]) + s * bases[[2]] + d + f / bases[[3]]
  } else if (unit == "f") {
    decimalized <- l * prod(bases) + s * prod(bases[2:3]) + d * bases[[3]] + f
  }
  # Allow cast to lsd deb_decimal
  if (vec_size(deb_bases(to)) == 2L) {
    bases <- deb_bases(to)
  }

  new_decimal(x = decimalized,
              unit = unit,
              bases = bases)
}

#' @export
vec_cast.deb_decimal.deb_tetra <- function(x, to, ...) {
  if (vec_size(deb_bases(to)) == 3L) {
    bases_equal(x, to)
  } else {
    mixed_bases_equal(to, x)
  }

  tetra_to_decimal(x, to)
}


# To deb_tetra ------------------------------------------------------------

#' deb_lsd to deb_tetra utility
#'
#' Add f unit and normalize
#' @keywords internal
#' @noRd

lsd_to_tetra <- function(x, to) {
  tetra <- new_tetra(l = field(x, "l"),
                     s = field(x, "s"),
                     d = field(x, "d"),
                     f = rep(0, vec_size(x)),
                     bases = deb_bases(to))

  deb_normalize(tetra)
}

#' @export
vec_cast.deb_tetra.deb_lsd <- function(x, to, ...) {
  mixed_bases_equal(x, to)

  lsd_to_tetra(x, to)
}

#' deb_decimal to deb_tetra utility
#'
#' Find unit and normalize
#' @keywords internal
#' @noRd

decimal_to_tetra <- function(x, to) {
  bases <- deb_bases(x)
  unit <- deb_unit(x)

  # To allow deb_decimal with lsd bases
  if (vec_size(deb_bases(x)) == 3L) {
    bases <- deb_bases(x)
  } else {
    bases <- deb_bases(to)
  }

  if (unit == "l") {
    tetra <- deb_tetra(x, 0, 0, 0, bases = bases)
  } else if (unit == "s") {
    tetra <- deb_tetra(0, x, 0, 0, bases = bases)
  } else if (unit == "d") {
    tetra <- deb_tetra(0, 0, x, 0, bases = bases)
  } else if (unit == "f") {
    tetra <- deb_tetra(0, 0, 0, x, bases = bases)
  }
  deb_normalize(tetra)
}

#' @export
vec_cast.deb_tetra.deb_decimal <- function(x, to, ...) {
  # Distinguish between tetra and lsd bases
  if (vec_size(deb_bases(x)) == 3L) {
    bases_equal(x, to)
  } else {
    mixed_bases_equal(x, to)
  }

  decimal_to_tetra(x, to)
}

# deb_lsd casting methods -------------------------------------------------

#' Cast to `deb_lsd`
#'
#' Cast `x` to a `deb_lsd` vector.
#'
#' @details Casting a list of numeric vectors of length 3 to `deb_lsd`
#' provides an alternate way to create a `deb_lsd` vector than [`deb_lsd()`].
#' This method may be helpful because the data is input by the value instead
#' of by the unit.
#'
#' @param x An object to coerce to `deb_lsd`.
#' @param ... Arguments passed on to further methods.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @returns A `deb_lsd` vector.
#' @seealso [`deb_as_decimal()`] and [`deb_as_tetra()`]
#'
#' @examples
#'
#' # Cast a deb_decimal vector to deb_lsd
#' x <- c(5.825, 3.25, 22/3)
#' d1 <- deb_decimal(x)
#' deb_as_lsd(d1)
#'
#' # Bases are automatically applied when
#' # casting from deb_decimal to deb_lsd
#' d2 <- deb_decimal(x, bases = c(60, 16))
#' deb_as_lsd(d2)
#'
#' # Cast a deb_tetra vector to deb_lsd
#' # This removes the 'f' or farthings unit.
#' y <- deb_tetra(l = c(5, 13, 7),
#'                s = c(12, 8, 16),
#'                d = c(3, 11, 0),
#'                f = c(1, 3, 2))
#' deb_as_lsd(y)
#'
#' # Cast a numeric vector to deb_lsd
#' deb_as_lsd(x)
#'
#' # Use the bases argument to apply non-default bases
#' deb_as_lsd(x, bases = c(60, 16))
#'
#' # Casting a list to deb_lsd provides an alternate to deb_lsd()
#' # This can be helpful for legibility. Compare:
#'
#' deb_as_lsd(
#'   list(c(5, 12, 3),
#'        c(13, 8, 11),
#'        c(7, 16, 0))
#'   )
#'
#' deb_lsd(l = c(5, 13, 7),
#'         s = c(12, 8, 16),
#'         d = c(3, 11, 0))
#'
#' @name cast-lsd
NULL

#' @rdname cast-lsd
#' @export
deb_as_lsd  <- function(x, ...) {
  UseMethod("deb_as_lsd")
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.default  <- function(x, ...) {
  vec_cast(x, deb_lsd())
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_lsd <- function(x, ...) x

#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_decimal <- function(x, ...) {
  decimal_to_lsd(x)
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_tetra <- function(x, ...) {
  tetra_to_lsd(x)
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.numeric <- function(x, bases = c(20, 12), ...) {
  vec_cast(x, to = deb_lsd(bases = bases))
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.logical <- function(x, bases = c(20, 12), ...) {
  vec_cast(x, to = deb_lsd(bases = bases))
}

#' @rdname cast-lsd
#' @export
deb_as_lsd.list <- function(x, bases = c(20, 12), ...) {
  vec_cast(x, to = deb_lsd(bases = bases))
}

# deb_decimal casting methods ---------------------------------------------

#' Cast to `deb_decimal`
#'
#' Cast `x` to a `deb_decimal` vector.
#'
#' @details Like [`deb_as_lsd()`], `deb_as_decimal()` provides a method to
#' cast a list of numeric vectors of length 3 to `deb_decimal`. This may be
#' helpful because the data is input by the value instead of by the unit.
#'
#' @param x An object to coerce to `deb_decimal`.
#' @param ... Arguments passed on to further methods.
#' @param unit A character vector of length one indicating the unit for the
#'   decimalized values, either `"l"` (libra, the default), `"s"` (solidus),
#'   or `"d"` (denarius).
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @returns A `deb_decimal` vector.
#' @seealso [`deb_as_lsd()`] and [`deb_as_tetra()`]
#'
#' @examples
#'
#' # Cast a deb_lsd vector to deb_decimal
#' x <- deb_lsd(l = c(5, 3, 7),
#'              s = c(16, 5, 6),
#'              d = c(6, 0, 8))
#' deb_as_decimal(x)
#'
#' # Bases are automatically applied when
#' # casting from deb_lsd to deb_decimal
#' x2 <- deb_lsd(l = c(5, 3, 7),
#'               s = c(16, 5, 6),
#'               d = c(6, 0, 8),
#'               bases = c(60, 16))
#' deb_as_decimal(x2)
#'
#' # Cast a deb_tetra vector to deb_decimal
#' # Bases are automatically applied, creating
#' # a deb_decimal vector with three bases units.
#' y <- deb_tetra(l = c(5, 13, 7),
#'                s = c(12, 8, 16),
#'                d = c(3, 11, 0),
#'                f = c(1, 3, 2))
#' deb_as_decimal(y)
#'
#' # Cast a numeric vector to deb_decimal
#' z <- c(5.825, 3.25, 22/3)
#' deb_as_decimal(z)
#'
#' # Use the unit and bases arguments to specify
#' # the unit and apply non-default bases
#' deb_as_decimal(z, unit = "s", bases = c(60, 16))
#'
#' # Casting a list to deb_decimal provides an
#' # alternative to get lsd values to deb_decimal.
#' lsd_list <- list(c(5, 12, 3),
#'                  c(13, 8, 11),
#'                  c(7, 16, 0))
#' deb_as_decimal(lsd_list)
#'
#' @name cast-decimal
NULL

#' @rdname cast-decimal
#' @export
deb_as_decimal <- function(x, ...) {
  UseMethod("deb_as_decimal")
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.default <- function(x, ...) {
  vec_cast(x, deb_decimal())
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_decimal <- function(x, ...) x

#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_lsd <- function(x, unit = c("l", "s", "d"), ...) {
  unit <- rlang::arg_match(unit)
  lsd_to_decimal(x, to = deb_decimal(unit = unit))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_tetra <- function(x, unit = c("l", "s", "d", "f"), ...) {
  unit <- rlang::arg_match(unit)
  # Need to explicitly add bases to deb_decimal() to avoid error of not
  # having a three unit bases attribute if "f" unit is chosen.
  tetra_to_decimal(x, to = deb_decimal(unit = unit, bases = deb_bases(x)))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.numeric <- function(x,
                                   unit = c("l", "s", "d", "f"),
                                   bases = c(20, 12), ...) {
  vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.logical <- function(x,
                                   unit = c("l", "s", "d", "f"),
                                   bases = c(20, 12), ...) {
  vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.list <- function(x,
                                unit = c("l", "s", "d", "f"),
                                bases = c(20, 12), ...) {
  vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

# deb_tetra casting methods -----------------------------------------------

#' Cast to `deb_tetra`
#'
#' Cast `x` to a `deb_tetra` vector.
#'
#' @details Casting a list of numeric vectors of length 4 to `deb_tetra`
#' provides an alternate way to create a `deb_tetra` vector than
#' [`deb_tetra()`]. This method may be helpful because the data is input
#' by the value instead of by the unit.
#'
#' @param x An object to coerce to `deb_tetra`.
#' @param ... Arguments passed on to further methods.
#' @param f Integer of length 1 to represent the base of the farthing unit.
#'   Must be provided to cast from deb_lsd or deb_decimal vectors with
#'   tripartite bases to deb_tetra.
#' @param bases Numeric vector of length 3 used to specify the bases for the
#'   solidus or s, denarius or d, and farthing or f units. Default is
#'   `c(20, 12, 4)`, which conforms to the English system of 1 pound =
#'   20 shillings, 1 shilling = 12 pence, and 1 pence = 4 farthing.
#'
#' @returns A `deb_tetra` vector.
#' @seealso [`deb_as_lsd()`] and [`deb_as_decimal()`]
#'
#' @examples
#'
#' # To cast from deb_lsd to deb_tetra an "f" unit must be supplied
#'
#' # Compare
#' lsd1 <- deb_lsd(8, 12, 4)
#' lsd2 <- deb_lsd(8, 12, 4, bases = c(60, 16))
#'
#' deb_as_tetra(lsd1, f = 4)
#' deb_as_tetra(lsd2, f = 8)
#'
#' # Cast a deb_decimal vector with four units to deb_tetra.
#' # Bases are automatically applied when casting from
#' # tetrapartite deb_decimal to deb_tetra.
#' x <- c(5.11875, 3.76875, 25/3)
#' d1 <- deb_decimal(x, bases = c(20, 12, 4))
#' deb_as_tetra(d1)
#'
#' # Use "f" argument to cast from tripartite deb_decimal
#' # to deb_tetra
#' d2 <- deb_decimal(x)
#' deb_as_tetra(d2, f = 4)
#'
#' # Cast a numeric vector to deb_tetra
#' deb_as_tetra(x)
#'
#' # Use the bases argument to apply non-default bases
#' deb_as_tetra(x, bases = c(60, 16, 8))
#'
#' # Casting a list to deb_tetra provides an alternate
#' # to deb_tetra(). This can be helpful for legibility.
#' # Compare:
#'
#' deb_as_tetra(
#'   list(c(5, 12, 3, 2),
#'        c(13, 8, 11, 1),
#'        c(7, 16, 0, 3))
#'   )
#'
#' deb_tetra(l = c(5, 13, 7),
#'           s = c(12, 8, 16),
#'           d = c(3, 11, 0),
#'           f = c(2, 1, 3))
#'
#' @name cast-tetra
NULL

#' @rdname cast-tetra
#' @export
deb_as_tetra  <- function(x, ...) {
  UseMethod("deb_as_tetra")
}

#' @rdname cast-tetra
#' @export
deb_as_tetra.default  <- function(x, ...) {
  vec_cast(x, deb_tetra())
}

#' @rdname cast-tetra
#' @export
deb_as_tetra.deb_tetra <- function(x, ...) x

#' @rdname cast-tetra
#' @export
deb_as_tetra.deb_lsd <- function(x, f, ...) {

  f_bases_check(f)

  bases <- vec_c(deb_bases(x), f = f)
  vec_cast(x, to = deb_tetra(bases = bases))
}

#' @rdname cast-tetra
#' @export
deb_as_tetra.deb_decimal <- function(x, f, ...) {
  if (vec_size(deb_bases(x)) == 2L) {
    f_bases_check(f)

    bases <- vec_c(deb_bases(x), f = f)
    vec_cast(x, to = deb_tetra(bases = bases))
  } else {
    decimal_to_tetra(x)
  }
}

#' @rdname cast-tetra
#' @export
deb_as_tetra.numeric <- function(x, bases = c(20, 12, 4), ...) {
  vec_cast(x, to = deb_tetra(bases = bases))
}

#' @rdname cast-tetra
#' @export
deb_as_tetra.logical <- function(x, bases = c(20, 12, 4), ...) {
  vec_cast(x, to = deb_tetra(bases = bases))
}

#' @rdname cast-tetra
#' @export
deb_as_tetra.list <- function(x, bases = c(20, 12, 4), ...) {
  vec_cast(x, to = deb_tetra(bases = bases))
}
