## Define lsd class ##

## Construction ---------------------------------------------------------------
new_lsd <- function(l = double(),
                    s = double(),
                    d = double(),
                    bases = integer()) {

  vctrs::vec_assert(l, ptype = double())
  vctrs::vec_assert(s, ptype = double())
  vctrs::vec_assert(d, ptype = double())
  vctrs::vec_assert(bases, ptype = integer(), size = 2)

  vctrs::new_rcrd(list(l = l, s = s, d = d),
                  bases = bases, class = "deb_lsd")
}

## Coercion -------------------------------------------------------------------

#' A class for pounds, shillings and pence values
#'
#' Pounds, shillings, and pence values are stored as a list of numeric vectors
#' of length 3 that possesses a bases attribute to record the non-decimal bases
#' for the shillings and pence units of the values. The first position of each
#' vector represents the pounds value or l. The second position represents the
#' shillings value or s. And the third position represents the pence value or
#' d. The bases attribute is stored as a numeric vector of length 2 with the
#' first value recording the shillings base and the second value the base of
#' the pence units. lsd objects can be used as list columns in a data frame.
#'
#' The lsd class and the `debkeepr` package use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence units. The abbreviations derive from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii coins, and 240 denarii were made from
#' one libra or pound of silver. The custom of counting coins in dozens
#' (solidi) and scores of dozens (librae) spread throughout the Carolingian
#' Empire and became engrained in much of Europe. However,
#' [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
#' solidus and denarius units were also in use. The `bases` attribute makes
#' it possible to specify alternative bases for the solidus and denarius units.
#'
#' @param l Numeric vector representing the pounds unit.
#' @param s Numeric vector representing the shillings unit.
#' @param d Numeric vector representing the pence unit.
#' @param lsd Numeric vector of length 3 or list of numeric vectors of length
#'   3. The first position of the vector represents the pounds value or l. The
#'   second position represents the shillings value or s. And the third
#'   position represents the pence value or d.
#' @param x An object.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#' @param ... Arguments passed on to further methods.
#'
#' @return Returns an object of class lsd with a bases attribute.
#'
#' @examples
#' ## Create lsd objects from separate l, s, and d vectors ##
#'
#' # lsd object for £10 6s. 8d.
#' deb_lsd(l = 10, s = 6, d = 8, bases = c(20, 12))
#'
#' # lsd object for the Dutch system of guilders, stuivers, and penningen
#' deb_lsd(l = 10, s = 6, d = 8, bases = c(20, 16))
#'
#' # lsd object from vectors of length > 1: all must be the same length
#' deb_lsd(l = c(10, 8, 5),
#'         s = c(6, 13, 8),
#'         d = c(8, 4, 10),
#'         bases = c(20, 12))
#'
#' ## Create lsd objects from numeric vectors of length 3 ##
#'
#' # lsd object for £10 6s. 8d.
#' deb_as_lsd(lsd = c(10, 6, 8), bases = c(20, 12))
#'
#' # lsd object for the Dutch system of guilders, stuivers, and penningen
#' deb_as_lsd(lsd = c(10, 8, 14), bases = c(20, 16))
#'
#' # lsd object from a list of vectors
#' deb_as_lsd(lsd = list(c(10, 6, 8),
#'                       c(8, 13, 4),
#'                       c(5, 8, 10)))
#'
#' @name lsd
NULL

#' @rdname lsd
#' @export
deb_lsd <- function(l, s, d, bases = c(20, 12)) {
  c(l, s, d) %<-% vctrs::vec_cast_common(l, s, d, .to = double())
  c(l, s, d) %<-% vctrs::vec_recycle_common(l, s, d)

  bases_check(bases)
  bases <- vctrs::vec_cast(bases, integer())

  new_lsd(l = l, s = s, d = d, bases = bases)
}

## About lsd ------------------------------------------------------------------

#' Test if an object is of class lsd
#'
#' Test if an object is of class lsd.
#'
#' @param lsd An object.
#'
#' @return `TRUE` if object is of class lsd and `FALSE` if it is not.
#'
#' @examples
#' x <- c(5, 3, 8)
#' y <- deb_as_lsd(x)
#'
#' deb_is_lsd(x)
#' deb_is_lsd(y)
#'
#' @export

deb_is_lsd <- function(x) {
  inherits(x, "deb_lsd")
}

#' Find the bases of lsd objects
#'
#' Find the bases for the shillings (s) and pence (d) units of lsd objects.
#'
#' @param ... Objects of class lsd.
#'
#' @return Returns list with a named numeric vector of length 2 corresponding
#'   to the shillings (s) and pence (d) units of each lsd object.
#'
#' @examples
#' x <- deb_as_lsd(lsd = c(5, 3, 8), bases = c(20, 12))
#' y <- deb_as_lsd(lsd = c(5, 3, 8), bases = c(20, 16))
#'
#' deb_bases(x)
#' deb_bases(x, y)
#'
#' @export

deb_bases <- function(lsd) {
  bases <- attr(lsd, "bases")
  names(bases) <- c("s", "d")
  bases
}


## Format ------------------------------------------------------------------

#' @rdname lsd
#' @export
format.deb_lsd <- function(x, ...) {
  l <- vctrs::field(x, "l")
  s <- vctrs::field(x, "s")
  d <- vctrs::field(x, "d")

  out <- paste0(l, ":", s, "s:", d, "d")
  out[is.na(l) | is.na(s) | is.na(d)] <- NA

  out
}

obj_print_footer.deb_lsd <- function(x, ...) {
  s <- format(attr(x, "bases")[1])
  d <- format(attr(x, "bases")[2])
  cat("# Bases: ", s, "s ", d, "d", "\n", sep = "")
}


# Abbreviated name type
vec_ptype_abbr.deb_lsd <- function(x) {
  paste0("lsd[", deb_bases(x)[1], "s:", deb_bases(x)[2], "d]")
}
