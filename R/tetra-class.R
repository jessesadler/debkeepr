## Define deb_tetra class ##

# Constructor -------------------------------------------------------------

#' Internal constructor to create deb_tetra type
#'
#' Asserts that `l`, `s`, `d`, and `f` are of type `double()` and that `bases`
#' is an `integer()` of length 3. Creates the vector through `new_rcrd()`.
#'
#' @returns An object of class `deb_tetra`.
#' @keywords internal
#' @noRd

new_tetra <- function(l = double(),
                      s = double(),
                      d = double(),
                      f = double(),
                      bases = c(20L, 12L, 4L)) {
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
  if (!rlang::is_double(f)) {
    rlang::abort("`f` must be a double vector.")
  }
  # Bases assert
  if (!rlang::is_integer(bases)) {
    rlang::abort("`bases` must be an integer vector.")
  }
  if (vec_size(bases) != 3L) {
    rlang::abort("`bases` must be an integer vector of length 3.")
  }

  bases <- rlang::set_names(bases, c("s", "d", "f"))

  new_rcrd(list(l = l, s = s, d = d, f = f),
           bases = bases,
           class = "deb_tetra")
}


# Helper ------------------------------------------------------------------

#' A class for tetrapartite values
#'
#' Create a vector of class `deb_tetra` to integrate values with four units
#' into standardized forms of analysis provided by R.
#'
#' @details
#' The `deb_tetra` class extends the concept of the `deb_lsd` class to
#' incorporate currencies and other types of values that consist of four units.
#' A variety of currencies and measurements of weights expanded beyond the
#' conventional tripartite system of pounds, shillings, and pence to include a
#' fourth unit. `deb_tetra` adds a fourth unit, named `f` for farthing, to the
#' [l, s, and d units](https://en.wikipedia.org/wiki/£sd) used by `deb_lsd`.
#' The `bases` attribute of `deb_tetra` vectors makes it possible to specify
#' alternative bases for the *solidus*, *denarius*, and farthing units.
#'
#' The length of `l`, `s`, `d`, and `f` must either be all equal, or a vector of
#' length 1 can be recycled to the length of the other argument(s). See
#' the [vctrs package](https://vctrs.r-lib.org/articles/type-size.html)
#' for further details on recycling vectors. In addition, `l`, `s`, `d`, and `f`
#' must either all have no values, resulting in a vector of length 0, or all
#' possess numeric vectors.
#'
#' @seealso
#' The `deb_tetra` class works in concert with the `deb_decimal` class,
#' which can represent tetrapartite values as decimalized values. See
#' [`deb_decimal()`]. To represent values with tripartite units see
#' [`deb_lsd()`].
#'
#' @param l Numeric vector representing the pounds unit.
#' @param s Numeric vector representing the shillings unit.
#' @param d Numeric vector representing the pence unit.
#' @param f Numeric vector representing the farthing or fourth unit.
#' @param bases Numeric vector of length 3 used to specify the bases for the
#'   solidus or s, denarius or d, and farthing or f units. Default is
#'   `c(20, 12, 4)`, which conforms to the English system of 1 pound =
#'   20 shillings, 1 shilling = 12 pence, and 1 pence = 4 farthing.
#'
#' @returns Returns a vector of class `deb_tetra`.
#' @export
#' @examples
#'
#' deb_tetra(5, 3, 8, 2)
#' deb_tetra(l = c(10, 8, 5),
#'           s = c(6, 13, 8),
#'           d = c(8, 4, 10),
#'           f = c(2, 3, 1))
#'
#' # Recycle length 1 vector
#' deb_tetra(l = c(10, 8, 5),
#'           s = c(6, 13, 8),
#'           d = c(8, 4, 10),
#'           f = 2)
#'
#' # Set the bases of the deb_tetra vector
#' deb_tetra(5, 3, 8, 2, bases = c(60, 16, 8))
#' deb_tetra(l = c(10, 28, 5),
#'           s = c(6, 33, 13),
#'           d = c(8, 12, 10),
#'           f = c(5, 3, 6),
#'           bases = c(60, 16, 8))
#'
#' # Create a prototype or vector of length 0
#' deb_tetra()

deb_tetra <- function(l = double(),
                      s = double(),
                      d = double(),
                      f = double(),
                      bases = c(20, 12, 4)) {
  # Need to figure out checks
  tetra_check(l, s, d, f)
  tetra_bases_check(bases)

  c(l, s, d, f) %<-% vec_cast_common(l, s, d, f, .to = double())
  c(l, s, d, f) %<-% vec_recycle_common(l, s, d, f)

  bases <- vec_cast(bases, to = integer())

  new_tetra(l = l, s = s, d = d, f = f, bases = bases)
}



# Compatibility with S4 ---------------------------------------------------

methods::setOldClass(c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))


# Class check -------------------------------------------------------------

#' Test if an object is of class `deb_tetra`
#'
#' Test if an object is of class `deb_tetra`.
#'
#' @param x An object.
#'
#' @returns `TRUE` if object is of class `deb_tetra` and `FALSE` if it is not.
#' @export
#' @examples
#' x <- deb_tetra(5, 3, 8, 2)
#' y <- c(5, 3, 8, 2)
#'
#' deb_is_tetra(x)
#' deb_is_tetra(y)

deb_is_tetra <- function(x) inherits(x, "deb_tetra")


# Format method -----------------------------------------------------------

# deb_tetra format method for object printing

#' @export
format.deb_tetra <- function(x, ...) {
  l <- round(field(x, "l"), 3) # only print 3 decimals
  s <- round(field(x, "s"), 3)
  d <- round(field(x, "d"), 3)
  f <- round(field(x, "f"), 3)

  out <- paste0(l, ":", s, "s:", d, "d:", f, "f")
  out[vec_detect_missing(l) | vec_detect_missing(s) |
        vec_detect_missing(d) | vec_detect_missing(f)] <- NA
  out
}

# Print footer with bases

#' @export
obj_print_footer.deb_tetra <- function(x, ...) {
  s <- format(attr(x, "bases")[[1]])
  d <- format(attr(x, "bases")[[2]])
  f <- format(attr(x, "bases")[[3]])
  cat("# Bases: ", s, "s ", d, "d ", f, "f", "\n", sep = "")
}


# Abbreviated name for tibble columns -------------------------------------

#' @export
vec_ptype_abbr.deb_tetra <- function(x, ...) {
  paste0("tetra[", attr(x, "bases")[[1]], "s:",
         attr(x, "bases")[[2]], "d:",
         attr(x, "bases")[[3]], "f]")
}
