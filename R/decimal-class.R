## Define deb_decimal class ##

# Constructor -------------------------------------------------------------

#' Internal constructor to create `deb_decimal` type
#'
#' Asserts that `x` is a `double()`, that `unit` is "l", "s", "d", or "f"
#' and that `bases` is an `integer()` of length 2 or 3. Creates the object
#' through `new_vctr()`.
#'
#' @returns A vector of class `deb_decimal`.
#' @keywords internal
#' @noRd

new_decimal <- function(x = double(),
                        unit = c("l", "s", "d", "f"),
                        bases = c(20L, 12L)) {
  unit <- rlang::arg_match(unit)

  if (!rlang::is_double(x)) {
    rlang::abort("`x` must be a double vector.")
  }
  if (!rlang::is_integer(bases)) {
    rlang::abort("`bases` must be an integer vector.")
  }

  # Check size of bases and add names to bases vector
  if (vec_size(bases) == 2L) {
    bases <- rlang::set_names(bases, c("s", "d"))
  } else if (vec_size(bases) == 3L) {
    bases <- rlang::set_names(bases, c("s", "d", "f"))
  } else {
    cli::cli_abort(c(
      "{.var bases} must be a numeric vector of length {.val {2}} for
            tripartite values or {.val {3}} for tetrapartite values.",
      "x" = "You've supplied a {.cls {class(bases)}} vector
            of length {vec_size(bases)}."))
  }

  new_vctr(x,
           unit = unit,
           bases = bases,
           class = "deb_decimal",
           inherit_base_type = TRUE)
}


# Helper ------------------------------------------------------------------

#' A decimalized class for tripartite and tetrapartite values
#'
#' Create a vector of class `deb_decimal` to integrate non-decimal currencies
#' and other measurements that use tripartite or tetrapartite units into
#' standardized forms of analysis provided by R.
#'
#' @details
#' The `deb_decimal` class and the `debkeepr` package use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent the tripartite
#' system of pounds, shillings, and pence units. The abbreviations derive from
#' the Latin terms [*libra*](https://en.wikipedia.org/wiki/French_livre),
#' [*solidus*](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [*denarius*](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' *solidus* came to represent 12 *denarii* coins, and, for a time at least,
#' 240 *denarii* were made from one *libra* or pound of silver. The custom of
#' counting coins in dozens (*solidi*) and scores of dozens (*librae*) spread
#' throughout the Carolingian Empire and became ingrained in much of Europe.
#' However, a variety of accounting systems arose at different times that used
#' [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
#' *solidus* and *denarius* units and even additional units. The `deb_decimal`
#' class decimalizes either tripartite or tetrapartite values. The `bases`
#' attribute makes it possible to specify the bases for the *solidus*,
#' *denarius*, and optionally farthing units. The `unit` attribute identifies
#' the decimalized unit: either *libra*, *solidus*, *denarius*, or farthing.
#'
#' `deb_decimal` vectors can either be tripartite, like `deb_lsd`, or
#' tetrapartite, like `deb_tetra`. These two kinds of `deb_decimal` vectors
#' are distinguished by the length of `bases` attribute (2 for tripartite and
#' 3 for tetrapartite) and the addition of the farthing unit for tetrapartite.
#' If the *solidus* and *denarius* bases are equal, tripartite and tetrapartite
#' `deb_decimal` vectors can be combined. The result is a `deb_decimal` vector
#' with tripartite bases.
#'
#' @seealso
#' The `deb_decimal` class works in concert with the `deb_lsd` and `deb_tetra`
#' classes. These classes maintain the tripartite (`deb_lsd`) and tetrapartite
#' (`deb_tetra`) unit structure of non-decimal currencies and values.
#' See [`deb_lsd()`] and [`deb_tetra()`].
#'
#' @param x A numeric vector representing the decimalized values of either
#'   tripartite or tetrapartite values.
#' @param unit A character vector of length one indicating the unit for the
#'   decimalized values, either `"l"` (*libra*, the default), `"s"`
#'   (*solidus*), `"d"` (*denarius*), or `"f"` (farthing). `"f"` is only
#'   valid if the `bases` argument is a numeric vector of length 3 (a
#'   tetrapartite value).
#' @param bases Numeric vector of length 2 or 3 used to specify the bases for
#'   the solidus or s, denarius or d, and optionally the farthing or f units.
#'   Default is `c(20, 12)`, which conforms to the most widely used tripartite
#'   system of 1 pound = 20 shillings and 1 shilling = 12 pence.
#'
#' @returns Returns a vector of class `deb_decimal`.
#' @export
#' @examples
#'
#' # deb_decimal with tripartite units
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
#'
#' # To create a tetrapartite value, provide numeric vector
#' # of length 3 to bases argument
#' deb_decimal(c(5.11875, 3.234375, 8.2875),
#'             bases = c(20, 12, 4))
#' deb_decimal(c(4914, 3105, 7956),
#'             unit = "f",
#'             bases = c(20, 12, 4))

deb_decimal <- function(x = double(),
                        unit = c("l", "s", "d", "f"),
                        bases = c(20, 12)) {
  unit <- rlang::arg_match(unit)
  if (unit == "f" && vec_size(bases) != 3L) {
    cli::cli_abort(
      "{.var bases} must be a numeric vector of length {.val {3}}
        to use 'f' unit.")
  }
  dec_bases_check(bases)

  x <- vec_cast(x, to = double())
  if (any(is.infinite(x))) {
    cli::cli_abort("{.arg x} cannot contain infinite ({.val {Inf}}) values.")
  }
  # Convert any NaN to NA
  x[vec_detect_missing(x)] <- NA
  bases <- vec_cast(bases, to = integer())

  new_decimal(x = x, unit = unit, bases = bases)
}


# Compatibility with S4 ---------------------------------------------------

methods::setOldClass(c("deb_decimal", "vctrs_vctr"))


# Attribute access --------------------------------------------------------

#' Access the unit attribute of a `deb_decimal` vector
#'
#' @keywords internal
#' @noRd

deb_unit <- function(x) attr(x, "unit")


# Class check -------------------------------------------------------------

#' Test if an object is of class `deb_decimal`
#'
#' Test if an object is of class `deb_decimal`.
#'
#' @param x An object.
#'
#' @returns `TRUE` if object is of class `deb_decimal` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- deb_decimal(c(5.25, 3.825, 8.5))
#' y <- c(5.25, 3.825, 8.5)
#'
#' deb_is_decimal(x)
#' deb_is_decimal(y)

deb_is_decimal <- function(x) inherits(x, "deb_decimal")


# Format method -----------------------------------------------------------
# No format.deb_decimal to keep default vector printing

#' Full name of unit for footer
#'
#' @keywords internal
#' @noRd

unit_word <- function(x) {
  if (attr(x, "unit") == "l") {
    unit <- "libra"
  } else if (attr(x, "unit") == "s") {
    unit <- "solidus"
  } else if (attr(x, "unit") == "d") {
    unit <- "denarius"
  } else {
    unit <- "farthing"
  }
  unit
}

# Print footer with unit and bases

#' @export
obj_print_footer.deb_decimal <- function(x, ...) {
  # Use full name of unit
  unit <- unit_word(x)

  if (vec_size(deb_bases(x)) == 2L) {
    s <- format(attr(x, "bases")[[1]])
    d <- format(attr(x, "bases")[[2]])
    cat("# Unit: ", unit, "\n",
        "# Bases: ", s, "s ", d, "d", "\n", sep = "")
  } else {
    s <- format(attr(x, "bases")[[1]])
    d <- format(attr(x, "bases")[[2]])
    f <- format(attr(x, "bases")[[3]])
    cat("# Unit: ", unit, "\n",
        "# Bases: ", s, "s ", d, "d ", f, "f", "\n", sep = "")
  }
}

# Abbreviated name for tibble columns -------------------------------------

#' @export
vec_ptype_abbr.deb_decimal <- function(x, ...) {

  if (vec_size(deb_bases(x)) == 2L) {
    paste0(attr(x, "unit"), "[",
           attr(x, "bases")[[1]], "s:",
           attr(x, "bases")[[2]], "d]")
  } else {
    paste0(attr(x, "unit"), "[",
           attr(x, "bases")[[1]], "s:",
           attr(x, "bases")[[2]], "d:",
           attr(x, "bases")[[3]], "f]")
  }
}
