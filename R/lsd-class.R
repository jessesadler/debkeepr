## Define deb_lsd class ##

# Constructor -------------------------------------------------------------

#' Internal constructor to create deb_lsd type
#'
#' Asserts that `l`, `s` and `d` are of type `double()` and that `bases` is an
#' `integer()` of length 2. Creates the vector through `new_rcrd()`.
#'
#' @returns An object of class `deb_lsd`.
#' @keywords internal
#' @noRd

new_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20L, 12L)) {
  # double assert
  if (!rlang::is_double(l)) {
    rlang::abort("`l` must be a double vector.")
  }
  if (!rlang::is_double(s)) {
    rlang::abort("`s` must be a double vector.")
  }
  if (!rlang::is_double(d)) {
    rlang::abort("`d` must be a double vector.")
  }
  # Bases assert
  if (!rlang::is_integer(bases)) {
    rlang::abort("`bases` must be an integer vector.")
  }
  if (vec_size(bases) != 2L) {
    rlang::abort("`bases` must be an integer vector of length 2.")
  }

  bases <- rlang::set_names(bases, c("s", "d"))

  new_rcrd(list(l = l, s = s, d = d),
           bases = bases,
           class = "deb_lsd")
}


# Helper ------------------------------------------------------------------

#' A class for pounds, shillings and pence values
#'
#' Create a vector of class `deb_lsd` to integrate non-decimal currencies
#' into standardized forms of analysis provided by R.
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
#' *solidus* and *denarius* units. The `bases` attribute of `deb_decimal`
#' vectors makes it possible to specify alternative bases for the *solidus* and
#' *denarius* units.
#'
#' The length of `l`, `s`, and `d` must either be all equal, or a vector of
#' length 1 can be recycled to the length of the other argument(s). See
#' the [vctrs package](https://vctrs.r-lib.org/articles/type-size.html)
#' for further details on recycling vectors. In addition, `l`, `s`, and `d`
#' must either all have no values, resulting in a vector of length 0, or all
#' possess numeric vectors.
#'
#' @seealso
#' The `deb_lsd` class works in concert with the `deb_decimal` class, which
#' represents non-decimal currencies as decimalized values. See
#' [`deb_decimal()`]. To represent values with tetrapartite units see
#' [`deb_tetra()`].
#'
#' @param l Numeric vector representing the pounds unit.
#' @param s Numeric vector representing the shillings unit.
#' @param d Numeric vector representing the pence unit.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   solidus or s and denarius or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#'
#' @returns Returns a vector of class `deb_lsd`.
#' @export
#' @examples
#'
#' deb_lsd(5, 3, 8)
#' deb_lsd(l = c(10, 8, 5),
#'         s = c(6, 13, 8),
#'         d = c(8, 4, 10))
#'
#' # Recycle length 1 vector
#' deb_lsd(l = c(10, 8, 5),
#'         s = c(6, 13, 8),
#'         d = 0)
#'
#' # Set the bases of the deb_lsd vector
#' deb_lsd(5, 3, 8, bases = c(60, 16))
#' deb_lsd(l = c(10, 28, 5),
#'         s = c(6, 33, 13),
#'         d = c(8, 42, 10),
#'         bases = c(60, 16))
#'
#' # Create a prototype or vector of length 0
#' deb_lsd()

deb_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = c(20, 12)) {
  # checks
  lsd_check(l, s, d)
  lsd_bases_check(bases)

  c(l, s, d) %<-% vec_cast_common(l, s, d, .to = double())
  c(l, s, d) %<-% vec_recycle_common(l, s, d)

  bases <- vec_cast(bases, to = integer())

  new_lsd(l = l, s = s, d = d, bases = bases)
}



# Compatibility with S4 ---------------------------------------------------

methods::setOldClass(c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))


# Attribute access --------------------------------------------------------

#' Access the bases attribute of a `deb_lsd` vector
#'
#' @keywords internal
#' @noRd

deb_bases <- function(x) attr(x, "bases")


# Class check -------------------------------------------------------------

#' Test if an object is of class `deb_lsd`
#'
#' Test if an object is of class `deb_lsd`.
#'
#' @param x An object.
#'
#' @returns `TRUE` if object is of class `deb_lsd` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- deb_lsd(5, 3, 8)
#' y <- c(5, 3, 8)
#'
#' deb_is_lsd(x)
#' deb_is_lsd(y)

deb_is_lsd <- function(x) inherits(x, "deb_lsd")


# Format method -----------------------------------------------------------

# deb_lsd format method for object printing

#' @export
format.deb_lsd <- function(x, ...) {
  l <- round(field(x, "l"), 3) # only print 3 decimals
  s <- round(field(x, "s"), 3)
  d <- round(field(x, "d"), 3)

  out <- paste0(l, ":", s, "s:", d, "d")
  out[vec_detect_missing(l) | vec_detect_missing(s) | vec_detect_missing(d)] <- NA
  out
}

# Print footer with bases

#' @export
obj_print_footer.deb_lsd <- function(x, ...) {
  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  cat("# Bases: ", s, "s ", d, "d", "\n", sep = "")
}


# Abbreviated name for tibble columns -------------------------------------

#' @export
vec_ptype_abbr.deb_lsd <- function(x, ...) {
  paste0("lsd[", attr(x, "bases")[[1]], "s:", attr(x, "bases")[[2]], "d]")
}
