## bases and unit conversion ##

# Convert bases -----------------------------------------------------------

#' Convert bases of `deb_lsd`, `deb_tetra`, and `deb_decimal` vectors
#'
#' Convert bases of `deb_lsd`, `deb_tetra`, and `deb_decimal` vectors
#'
#' @details
#' `deb_convert_bases()` is the only way to change the bases of the solidus,
#' denarius, and farthing units associated with vectors of class `deb_lsd`,
#' `deb_tetra`, and `deb_decimal`. It also provides a means to convert
#' between tripartite and tetrapartite bases with `deb_decimal` vectors.
#'
#' If `x` is a `deb_decimal` vector with tetrapartite bases and unit `"f"`
#' and `to` is a numeric vector of length 2, the unit will be converted
#' to `"d"`.
#'
#' @param x A vector of class `deb_lsd`, `deb_tetra`, or `deb_decimal`.
#' @param to Numeric vector of length 2 or 3, representing the bases for the
#'   solidus, denarius, and optionally farthing units to be converted to.
#'
#' @returns A vector of the same class as `x` with converted `bases` attribute.
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
#' # Convert between tripartite and tetrapartite bases
#' deb_convert_bases(dec, to = c(60, 16, 8))
#' deb_convert_bases(dec_tetra, to = c(20, 12))
#'
#' @name convert-bases
NULL

#' @rdname convert-bases
#' @export
deb_convert_bases <- function(x, to) {
  UseMethod("deb_convert_bases")
}

#' @rdname convert-bases
#' @export
deb_convert_bases.default <- function(x, to) {
  cli::cli_abort(c("{.arg x} must be a <deb_lsd>, <deb_tetra>, or
                     <deb_decimal> vector.",
                   "x" = "You've supplied a {.cls {class(x)}} vector."))
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

  dec_bases_check(to)
  from <- deb_bases(x)
  # Cast "to" to correct bases format
  to <- vec_cast(to, to = integer())
  if (vec_size(to) == 2) {
    to <- rlang::set_names(to, c("s", "d"))
  } else if (vec_size(to) == 3) {
    to <- rlang::set_names(to, c("s", "d", "f"))
  }

  # If from has lsd bases, math is the same whether to is lsd or tetra bases
  if (vec_size(from) == 2) {

    if (deb_unit(x) == "l") {
      converted <- x
    } else if (deb_unit(x) == "s") {
      converted <- x * to[[1]] / from[[1]]
    } else if (deb_unit(x) == "d") {
      converted <- x * prod(to[1:2]) / prod(from)
    }
    attr(converted, "bases") <- to

  # from with tetra bases
  } else if (vec_size(from) == 3) {
    # Convert unit from "f" to "d" if to has lsd bases
    if (vec_size(to) == 2 && deb_unit(x) == "f") {
      x <- deb_convert_unit(x, "d")
    }

    if (deb_unit(x) == "l") {
      converted <- x
    } else if (deb_unit(x) == "s") {
      converted <- x * to[[1]] / from[[1]]
    } else if (deb_unit(x) == "d") {
      converted <- x * prod(to[1:2]) / prod(from[1:2])
    } else if (deb_unit(x) == "f") {
      converted <- x * prod(to) / prod(from)
    }
    attr(converted, "bases") <- to
  }

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
#' @returns A `deb_decimal` vector with a converted `unit` attribute.
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
    cli::cli_abort(c("{.arg x} must be a <deb_decimal> vector.",
                     "x" = "You've supplied a {.cls {class(x)[[1]]}} vector."))
  }
  to_unit <- rlang::arg_match(to)
  if (to_unit == "f" && vec_size(deb_bases(x)) != 3L) {
    cli::cli_abort(c(
    "`x` must be a <deb_decimal> vector with tetrapartite bases to convert
        to the 'f' unit.",
    "i" = "Choose to = 'l', 's', or 'd' instead."))
  }
  vec_cast(x, deb_decimal(unit = to_unit, bases = deb_bases(x)))
}
