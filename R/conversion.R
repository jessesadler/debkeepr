## bases and unit conversion ##

# Convert bases -----------------------------------------------------------

#' Convert bases of `deb_lsd`, `deb_tetra`, and `deb_decimal` vectors
#'
#' Convert bases of `deb_lsd`, `deb_tetra`, and `deb_decimal` vectors
#'
#' `deb_convert_bases()` is the only way to change the bases of the solidus,
#' denarius, and farthing units associated with vectors of class `deb_lsd`,
#' `deb_tetra`, and `deb_decimal`.
#'
#' @param x A vector of class `deb_lsd`, `deb_tetra`, or `deb_decimal`.
#' @param to Numeric vector of length 2 or 3, representing the bases for the
#'   solidus, denarius, and optionally farthing units to be converted to.
#'
#' @return A vector of the same class as `x` with converted `bases` attribute.
#' @examples
#'
#' lsd <- deb_lsd(5, 3, 8)
#' dec <- deb_decimal(8.825)
#' dec_tetra <- deb_decimal(1.840625, bases = c(20, 12, 4))
#' tetra <- deb_tetra(1, 16, 9, 3)
#'
#' deb_convert_bases(lsd, to = c(60, 16))
#' deb_convert_bases(dec, to = c(60, 16))
#' deb_convert_bases(dec_tetra, c(60, 16, 8))
#' deb_convert_bases(tetra, to = c(60, 16, 8))
#'
#' @name convert-bases
NULL

#' @rdname convert-bases
#' @export
deb_convert_bases <- function(x, to) {
  UseMethod("deb_convert_bases")
}

deb_convert_bases.default <- function(x, to) {
  rlang::abort(
    "`x` must be a <deb_lsd>, <deb_tetra>, or <deb_decimal> vector.")
}

#' @rdname convert-bases
#' @export
deb_convert_bases.deb_lsd <- function(x, to) {
  lsd_bases_check(to)

  from <- deb_bases(x)
  to <- vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d"))

  if (identical(from, to)) {
    return(x)
  }
  # Cast to numeric and back to lsd
  num <- as.numeric(x)
  deb_as_lsd(num, bases = to)
}

#' @rdname convert-bases
#' @export
deb_convert_bases.deb_decimal <- function(x, to) {
  lsd_bases_check(to)

  from <- deb_bases(x)
  to <- vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d"))

  if (deb_unit(x) == "l") {
    converted <- x
  } else if (deb_unit(x) == "s") {
    converted <- x * to[[1]] / from[[1]]
  } else if (deb_unit(x) == "d") {
    converted <- x * prod(to) / prod(from)
  }

  attr(converted, "bases") <- to

  converted
}

#' @rdname convert-bases
#' @export
deb_convert_bases.deb_tetra <- function(x, to) {
  tetra_bases_check(to)

  from <- deb_bases(x)
  to <- vec_cast(to, to = integer())
  to <- rlang::set_names(to, c("s", "d", "f"))

  if (identical(from, to)) {
    return(x)
  }
  # Cast to numeric and back to tetra
  num <- as.numeric(x)
  deb_as_tetra(num, bases = to)
}

# Convert units -----------------------------------------------------------

#' Convert the unit of `deb_decimal` vectors
#'
#' Convert the `unit` attribute of `deb_decimal` vectors.
#'
#' `deb_convert_unit()` converts the `unit` of a `deb_decimal` vector to
#' either `"l"`, `"s"`, `"d"`, or optionally `"f"` if the vector has
#' tetrapartite bases. This changes the representation of the vector,
#' but the value remains equivalent.
#'
#' @param x A vector of class `deb_decimal`.
#' @param to A character vector of length one indicating the unit to be
#'   converted to. Choice of `"l"` (libra, the default), `"s"` (solidus),
#'   `"d"` (denarius), or `"f"` (farthing).
#'
#' @return A `deb_decimal` vector with a converted `unit` attribute.
#' @export
#' @examples
#'
#' x <- deb_decimal(c(8.825, 15.125, 3.65))
#' y <- deb_decimal(c(56.45, 106.525, 200.4), unit = "s")
#' z <- deb_decimal(c(8472, 14520,  3504),
#'                  unit = "f",
#'                  bases = c(20, 12, 4))
#'
#' deb_convert_unit(x, to = "s")
#' deb_convert_unit(x, to = "d")
#' deb_convert_unit(y, to = "l")
#' deb_convert_unit(y, to = "d")
#' deb_convert_unit(z, to = "l")
#' deb_convert_unit(z, to = "s")

deb_convert_unit <- function(x, to = c("l", "s", "d", "f")) {
  if (!deb_is_decimal(x)) {
    rlang::abort("`x` must be a <deb_decimal> vector.")
  }
  to_unit <- rlang::arg_match(to)

  vec_cast(x, deb_decimal(unit = to_unit, bases = deb_bases(x)))
}
