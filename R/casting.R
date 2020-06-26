## Casting for deb_lsd and deb_decimal ##

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

#' @export
vec_cast.deb_decimal.deb_decimal <- function(x, to, ...) {
  bases_equal(x, to)

  from_unit <- deb_unit(x)
  to_unit <- deb_unit(to)

  if (from_unit == to_unit) {
    return(x)
  }

  bases <- deb_bases(x)

  converted <- dplyr::case_when(
    from_unit == "l" & to_unit == "s" ~ x * bases[[1]],
    from_unit == "l" & to_unit == "d" ~ x * prod(bases),
    from_unit == "s" & to_unit == "d" ~ x * bases[[2]],
    from_unit == "s" & to_unit == "l" ~ x / bases[[1]],
    from_unit == "d" & to_unit == "l" ~ x / prod(bases),
    from_unit == "d" & to_unit == "s" ~ x / bases[[2]]
  )

  attr(converted, "unit") <- to_unit

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

# lsd and list ------------------------------------------------------------

# list to deb_lsd

#' @export
vec_cast.deb_lsd.list <- function(x, to, ...) {
  list_check(x)

  deb_lsd(l = vapply(x, `[[`, i = 1, double(1)),
          s = vapply(x, `[[`, i = 2, double(1)),
          d = vapply(x, `[[`, i = 3, double(1)),
          bases = deb_bases(to))
}

# list to deb_decimal

#' @export
vec_cast.deb_decimal.list <- function(x, to, ...) {
  list_check(x)

  lsd <- vec_cast(x, to = deb_lsd(bases = deb_bases(to)))

  lsd_to_decimal(lsd, to)
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

# Cast deb_lsd to list

#' Cast `deb_lsd` to a list of lsd values
#'
#' Cast a `deb_lsd` vector to a list of numeric vectors containing lsd values.
#'
#' @details
#' `deb_as_list()` turns a `deb_lsd` vector into a list of numeric vectors of
#' length 3. It is the inverse of `deb_as_lsd()`. Compare to `as.list()`,
#' which creates a list of `deb_lsd` vectors or `unclass()`, which creates a
#' list of length 3 with numeric vectors for pounds, shillings, and pence.
#'
#' @param x A `deb_lsd` object to cast to a list of lsd values.
#' @param ... Arguments passed on to further methods.
#'
#' @seealso [`deb_as_lsd()`] for the inverse of `deb_as_list()`.
#' @return A list of numeric vectors of length 3, corresponding to lsd values.
#' @examples
#'
#' # deb_lsd vector
#' x <- deb_lsd(l = 0:3, s = 4:7, d = 8:11)
#'
#' deb_as_list(x)
#'
#' # This is the inverse of `deb_as_lsd()` of a list of lsd values
#' y <- deb_as_list(x)
#'
#' identical(x, deb_as_lsd(y))
#'
#' @export

deb_as_list <- function(x, ...) {
  if (!deb_is_lsd(x)) {
    rlang::abort("`x` must be a <deb_lsd> vector.")
  }
  vec_cast(x, list())
}

# deb_decimal to deb_lsd --------------------------------------------------

decimal_to_lsd <- function(x) {
  bases <- deb_bases(x)
  unit <- deb_unit(x)

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
  bases_equal(x, to)

  decimal_to_lsd(x)
}

# deb_lsd to deb_decimal --------------------------------------------------

lsd_to_decimal <- function(x, to) {
  l <- field(x, "l")
  s <- field(x, "s")
  d <- field(x, "d")
  bases <- deb_bases(x)
  unit <- deb_unit(to)

  if (unit == "l") {
    decimalized <- l + s / bases[[1]] + d / prod(bases)
  } else if (unit == "s") {
    decimalized <- l * bases[[1]] + s + d / bases[[2]]
  } else if (unit == "d") {
    decimalized <- l * prod(bases) + s * bases[[2]] + d
  }
  new_decimal(x = decimalized,
              unit = unit,
              bases = bases)
}

#' @export
vec_cast.deb_decimal.deb_lsd <- function(x, to, ...) {
  bases_equal(x, to)

  lsd_to_decimal(x, to)
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
#' @return A `deb_lsd` object.
#' @seealso [`deb_as_decimal()`]
#'
#' @examples
#'
#' # Cast a deb_decimal object to deb_lsd
#' x <- c(5.825, 3.25, 22/3)
#' d1 <- deb_decimal(x)
#' deb_as_lsd(d1)
#'
#' # Bases are automatically applied when
#' # casting from deb_decimal to deb_lsd
#' d2 <- deb_decimal(x, bases = c(60, 16))
#' deb_as_lsd(d2)
#'
#' # Cast a numeric vector to deb_lsd
#' deb_as_lsd(x)
#'
#' # Use the bases argument to apply non-default bases
#' deb_as_lsd(x, bases = c(60, 16))
#'
#' # Cast a list to deb_lsd provides an alternate to deb_lsd()
#' # This can be helpful for legibility. Compare:
#'
#' list(c(5, 12, 3),
#'      c(13, 8, 11),
#'      c(7, 16, 0)) %>%
#'   deb_as_lsd()
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
deb_as_lsd.deb_lsd <- function(x, ...) x

#' @rdname cast-lsd
#' @export
deb_as_lsd.deb_decimal <- function(x, ...) {
  decimal_to_lsd(x)
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
#' @return A `deb_decimal` object.
#' @seealso [`deb_as_lsd()`]
#'
#' @examples
#'
#' # Cast a deb_lsd object to deb_decimal
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
#' # Cast a numeric vector to deb_decimal
#' y <- c(5.825, 3.25, 22/3)
#' deb_as_decimal(y)
#'
#' # Use the unit and bases arguments to specify
#' # the unit and apply non-default bases
#' deb_as_decimal(y, unit = "s", bases = c(60, 16))
#'
#' # Cast a list to deb_lsd provides an alternate
#' # to deb_lsd() %>% deb_decimal()
#'
#' list(c(5, 12, 3),
#'      c(13, 8, 11),
#'      c(7, 16, 0)) %>%
#'   deb_as_decimal()
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
deb_as_decimal.deb_decimal <- function(x, ...) x

#' @rdname cast-decimal
#' @export
deb_as_decimal.deb_lsd <- function(x, unit = c("l", "s", "d"), ...) {
  unit <- rlang::arg_match(unit)
  lsd_to_decimal(x, to = deb_decimal(unit = unit))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.numeric <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12), ...) {
  vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.logical <- function(x,
                                   unit = c("l", "s", "d"),
                                   bases = c(20, 12), ...) {
  vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}

#' @rdname cast-decimal
#' @export
deb_as_decimal.list <- function(x,
                                unit = c("l", "s", "d"),
                                bases = c(20, 12), ...) {
  vec_cast(x, to = deb_decimal(unit = unit, bases = bases))
}
