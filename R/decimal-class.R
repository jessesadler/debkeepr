## Define deb_decimal class ##

# Constructor -------------------------------------------------------------

#' Internal constructor to create `deb_decimal` type
#'
#' Asserts that `x` is a `double()`, that `unit` is "l", "s", or "d", and
#' that `bases` is an `integer()` of length 2. Creates the object through
#' `new_vctr()`.
#'
#' @return A vector of class `deb_decimal`.
#' @keywords internal

new_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20L, 12L)) {
  unit <- rlang::arg_match(unit)

  vec_assert(x, ptype = double())
  bases <- bases_assert(bases)

  new_vctr(x,
           unit = unit,
           bases = bases,
           class = "deb_decimal",
           inherit_base_type = TRUE)
}


# Helper ------------------------------------------------------------------

#' A decimalized class for pounds, shillings, and pence values
#'
#' Create a vector of class `deb_decimal` to integrate non-decimal currencies
#' into standardized forms of analysis provided by R.
#'
#' @details
#' The `deb_decimal` class and the `debkeepr` package use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence units. The abbreviations derive from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii coins, and, for a time at least, 240
#' denarii were made from one libra or pound of silver. The custom of counting
#' coins in dozens (solidi) and scores of dozens (librae) spread throughout the
#' Carolingian Empire and became engrained in much of Europe. However, a
#' variety of accounting systems arose at different times that used
#' [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
#' solidus and denarius units. The `bases` attribute of `deb_decimal` vectors
#' makes it possible to specify alternative bases for the solidus and denarius
#' units, while the `unit` attribute identifies the decimalized unit: either
#' libra, solidus, or denarius.
#'
#' @seealso
#' The `deb_decimal` class works in concert with the `deb_lsd` class, which
#' maintains the tripartite unit structure of most non-decimal currencies.
#' See [`deb_lsd()`].
#'
#' @param x A numeric vector representing the decimalized values of a
#'   non-decimal currency.
#' @param unit A character vector of length one indicating the unit for the
#'   decimalized values, either `"l"` (libra, the default), `"s"` (solidus),
#'   or `"d"` (denarius).
#' @inheritParams deb_lsd
#'
#' @return Returns a vector of class `deb_decimal`.
#' @export
#' @examples
#'
#' deb_decimal(c(5.25, 3.825, 8.5))
#'
#' # Set the unit of the deb_decimal vector
#' deb_decimal(c(105, 76.5, 170), unit = "s")
#' deb_decimal(c(1260, 918, 240), unit = "d")
#'
#' # Set the bases of the deb_decimal vector
#' deb_decimal(c(5.25, 3.825, 8.5), bases = c(60, 16))
#'
#' # Create a prototype or vector of length 0
#' deb_decimal()

deb_decimal <- function(x = double(),
                        unit = c("l", "s", "d"),
                        bases = c(20, 12)) {
  unit <- rlang::arg_match(unit)
  bases_check(bases)

  x <- vec_cast(x, to = double())
  bases <- vec_cast(bases, to = integer())

  new_decimal(x = x, unit = unit, bases = bases)
}


# Compatibility with S4 ---------------------------------------------------

methods::setOldClass(c("deb_decimal", "vctrs_vctr"))


# Attribute access --------------------------------------------------------

#' Access the unit attribute of a `deb_decimal` vector
#'
#' @keywords internal

deb_unit <- function(x) attr(x, "unit")


# Class check -------------------------------------------------------------

#' Test if an object is of class `deb_decimal`
#'
#' Test if an object is of class `deb_decimal`.
#'
#' @param x An object.
#'
#' @return `TRUE` if object is of class `deb_decimal` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- deb_decimal(c(5.25, 3.825, 8.5))
#' y <- c(5.25, 3.825, 8.5)
#'
#' deb_is_lsd(x)
#' deb_is_lsd(y)

deb_is_decimal <- function(x) inherits(x, "deb_decimal")


# Format method -----------------------------------------------------------
# No format.deb_decimal to keep default vector printing

#' Full name of unit for footer
#'
#' @keywords internal

unit_word <- function(x) {
  if (attr(x, "unit") == "l") {
    unit <- "libra"
  } else if (attr(x, "unit") == "s") {
    unit <- "solidus"
  } else {
    unit <- "denarius"
  }
  unit
}

#' Print footer with unit and bases
#'
#' @keywords internal
#' @export

obj_print_footer.deb_decimal <- function(x, ...) {
  # Use full name of unit
  unit <- unit_word(x)

  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Unit: ", unit, "\n",
      "# Bases: ", s, "s ", d, "d", "\n", sep = "")
}

# Abbreviated name type ---------------------------------------------------

#' Abbreviated name for tibble columns
#'
#' @keywords internal
#' @export

vec_ptype_abbr.deb_decimal <- function(x) {
  paste0(attr(x, "unit"), "[",
         attr(x, "bases")[[1]], "s:",
         attr(x, "bases")[[2]], "d]")
}
