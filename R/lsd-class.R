## Define lsd class ##

## Construction ---------------------------------------------------------------
new_lsd <- function(x, bases) {
  stopifnot(is.list(x))
  stopifnot(is.numeric(bases), length(bases) == 2)

  structure(x,
            class = "lsd",
            bases = bases)
}

to_lsd <- function(lsd, bases) {
  # vectors
  if (rlang::is_list(lsd) == FALSE & rlang::is_vector(lsd) == TRUE) {
    lsd <- list(lsd)
  }
  new_lsd(lsd, bases)
}

## Validate bases -------------------------------------------------------------

# Extract bases attribute if it exists, otherwise uses bases
validate_bases <- function(lsd, bases) {
  if (inherits(lsd, "lsd")) {
    attributes(lsd)$bases
  } else {
      bases
  }
}

validate_bases2 <- function(lsd1, lsd2, bases) {
  # Two lsd objects with different bases
  if (deb_is_lsd(lsd1) == TRUE & deb_is_lsd(lsd2) == TRUE) {
    if (identical(attributes(lsd1)$bases, attributes(lsd2)$bases) == FALSE) {
      stop(call. = FALSE, "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")
    }
  }
  # Options for lsd vs not lsd for determining bases
  if (deb_is_lsd(lsd1) == TRUE & deb_is_lsd(lsd2) == TRUE) {
    attributes(lsd1)$bases
  } else if (deb_is_lsd(lsd1) == TRUE & deb_is_lsd(lsd2) == FALSE) {
    attributes(lsd1)$bases
  } else if (deb_is_lsd(lsd1) == FALSE & deb_is_lsd(lsd2) == TRUE) {
    attributes(lsd2)$bases
  } else {
    bases
  }
}

validate_bases_p <- function(lsd, bases) {
  # If no lsd objects or lists with lsd objects
  if (any(purrr::map_lgl(lsd, deb_is_lsd)) == FALSE) {
    bases
  } else {
    lsd_class <- purrr::map_lgl(lsd, deb_is_lsd)
    lsd_class <- lsd[which(lsd_class == TRUE)]

    lsd_bases <- purrr::map(lsd_class, ~ attributes(.)$bases)

    if (length(unique(lsd_bases)) != 1) {
      stop(call. = FALSE, "All objects of class lsd must have the same bases")
    } else {
      purrr::flatten_dbl(unique(lsd_bases))
    }
  }
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

  lsd <- list(l, s, d)
  # checks
  bases_check(bases)
  separate_lsd_check(lsd)

  purrr::transpose(lsd) %>%
    purrr::simplify_all() %>%
    to_lsd(bases = bases)
}

#' @rdname lsd
#' @export
deb_as_lsd <- function(lsd, bases = c(20, 12), ...) {
  UseMethod("deb_as_lsd")
}

#' @rdname lsd
#' @export
deb_as_lsd.default <- function(lsd, bases = c(20, 12), ...) {
  stop(call. = FALSE, "Cannot coerce an object of class ",
       paste(class(lsd), collapse = "/"), " into an lsd object.")
}

#' @rdname lsd
#' @export
deb_as_lsd.numeric <- function(lsd, bases = c(20, 12), ...) {
  lsd_check(lsd)
  bases_check(bases)

  to_lsd(lsd, bases)
}

#' @rdname lsd
#' @export
deb_as_lsd.list <- function(lsd, bases = c(20, 12), ...) {
  lsd_check(lsd)
  bases_check(bases)

  to_lsd(lsd, bases)
}

#' @rdname lsd
#' @export
deb_as_lsd.lsd <- function(lsd, ...) lsd

## about lsd ------------------------------------------------------------------

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

deb_is_lsd <- function(lsd) {
  inherits(lsd, "lsd")
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

deb_bases <- function(...) {
  lsd_list <- list(...)
  if (all(purrr::map_lgl(lsd_list, deb_is_lsd)) == FALSE) {
    stop(call. = FALSE, "Objects must be of class lsd")
  }
  if (length(lsd_list) == 1) {
    purrr::map(lsd_list,
               ~ rlang::set_names(attributes(.)$bases, c("s", "d"))) %>%
      purrr::as_vector()
  } else {
    purrr::map(lsd_list,
               ~ rlang::set_names(attributes(.)$bases, c("s", "d")))
  }
}

## internal generics ----------------------------------------------------------

## Subset lsd ##
#' @export
`[.lsd` <- function(x, ...) {
  to_lsd(NextMethod(), bases = attr(x, "bases"))
}

## Combine ##
#' @export
`c.lsd` <- function(...) {
  lsd_list <- list(...)
  purrr::map(lsd_list, lsd_check)
  bases <- validate_bases_p(lsd_list, bases)
  bases_check(bases)

  lsd <- purrr::modify_if(lsd_list, is.numeric, list) %>%
    purrr::flatten()
  to_lsd(lsd, bases)
}

## print ##

#' @rdname lsd
#' @export
print.lsd <- function(x, ...) {
  # Turn NA and NULL to NA vector to enable change to df
  if (any(purrr::map_lgl(x, ~ length(.) != 3))) {
    missing <- purrr::map_lgl(x, ~ length(.) != 3)
    x[which(missing == TRUE)] <- list(c(as.numeric(NA), as.numeric(NA), as.numeric(NA)))
  }

  lsd <-  x %>%
    purrr::map(~ rlang::set_names(., c("l", "s", "d"))) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    as.data.frame()

  row.names(lsd) <- paste0("[", 1:length(x), "]")

  print.data.frame(lsd, row.names = TRUE, ...)
}
