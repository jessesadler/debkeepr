name_lsd_vector <- function(x) {
  stopifnot(is.numeric(x), length(x) == 3)

  rlang::set_names(x, c("l", "s", "d"))
}

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
    lsd <- list(name_lsd_vector(lsd))
  } else {
    # lists
    lsd <- purrr::map(lsd, name_lsd_vector)
  }
  new_lsd(lsd, bases)
}

# Extract bases attribute if it exists, otherwise uses bases
validate_bases <- function(lsd, bases) {
  # Vectors
  if (rlang::is_list(lsd) == FALSE & rlang::is_vector(lsd) == TRUE) {
    bases
  } else {
    if (inherits(lsd, "lsd")) {
      attributes(lsd)$bases
    } else {
      bases
    }
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

## Coercion ##

#' A class for pounds, shillings and pence values
#'
#' Pounds, shillings, and pence values are stored as either numeric vectors
#' of length 3 or lists of numeric vectors of length 3 that possess a bases
#' attribute to record the bases for the shillings or s and pence or d units
#' of the values. An lsd object is a named numeric vector with a bases
#' attribute. An lsd_list object builds upon this. It is a list of lsd objects
#' that share the same bases, and it possesses a matching bases attribute
#' itself.
#'
#' The lsd class and the `debkeepr` package use the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence units. The abbreviations derive from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii, and 240 denarii were made from one
#' libra or pound of silver. The custom of counting coins in dozens (solidi)
#' and scores of dozens (librae) spread throughout the Carolingian Empire and
#' became engrained in much of Europe. However,
#' [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
#' solidus and denarius units were also in use. The `bases` attribute makes
#' it possible to specify alternative bases for the solidus and denarius units.
#'
#' @param lsd Numeric vector of length 3 or list of numeric vectors of length
#'   3. The first position of the vector represents the pounds value or l. The
#'   second position represents the shillings value or s. And the third
#'   position represents the pence value or d.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence.
#' @param ... Arguments passed on to further methods.
#'
#' @return Returns an lsd or lsd_list object with a bases attribute.
#'
#' @examples
#' # Create lsd object for £10 6s. 8d. with a numeric vector
#' deb_as_lsd(lsd = c(10, 6, 8), bases = c(20, 12))
#'
#' # Create lsd object for the Dutch system
#' # of guilders, stuivers, and penningen
#' deb_as_lsd(lsd = c(10, 8, 14), bases = c(20, 16))
#'
#' # Create lsd_list object from a list of vectors
#' # with the default bases of 20s. 12d.
#' deb_as_lsd(lsd = list(c(10, 6, 8),
#'                        c(8, 13, 4),
#'                        c(5, 8, 10)))
#'
#' # A list of lsd objects must all have the same bases,
#' # and will use the bases attribute of the lsd objects
#' lsd1 <- deb_as_lsd(lsd = c(10, 8, 14), bases = c(20, 16))
#' lsd2 <- deb_as_lsd(lsd = c(16, 13, 2), bases = c(20, 16))
#' list_lsd1 <- list(lsd1, lsd2)
#' deb_as_lsd(lsd = list_lsd1)
#'
#' # A list of lsd objects with different bases will throw and error
#' lsd3 <- deb_as_lsd(lsd = c(6, 11, 8), bases = c(20, 12))
#' list_lsd2 <- list(lsd1, lsd2, lsd3)
#'
#' \dontrun{
#' deb_as_lsd(list_lsd2)
#' }
#'
#' @name lsd
NULL

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
  bases <- validate_bases(lsd, bases)

  lsd_check(lsd)
  bases_check(bases)

  to_lsd(lsd, bases)
}

#' @rdname lsd
#' @export
deb_as_lsd.lsd <- function(lsd, bases, ...) lsd


#' Test if an object is of class lsd
#'
#' Test if an object is of class lsd.
#'
#' @param lsd An object.
#'
#' @return `TRUE` if object is of class lsd and `FALSE` if it
#'   is not.
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

#' Find the bases of an lsd object
#'
#' Find the bases for the shillings (s) and pence (d) units of an lsd object.
#'
#' @param lsd An object of class lsd.
#'
#' @return Returns a named numeric vector of length 2 corresponding to the
#'   shillings (s) and pence (d) units of `lsd`.
#'
#' @examples
#' x <- deb_as_lsd(lsd = c(5, 3, 8), bases = c(20, 12))
#' y <- deb_as_lsd(lsd = c(5, 3, 8), bases = c(20, 16))
#'
#' deb_bases(x)
#' deb_bases(y)
#'
#' @export

deb_bases <- function(lsd) {
  if (deb_is_lsd(lsd) == FALSE) {
    stop(call. = FALSE, "lsd must be of class lsd")
  } else {
    rlang::set_names(attributes(lsd)$bases, c("s", "d"))
  }
}

## Subset lsd ##
#' @export
`[.lsd` <- function(x, ...) {
  new_lsd(NextMethod(), bases = attr(x, "bases"))
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

#' @export
print.lsd <- function(x, ...) {
  x <- purrr::map(x, format)
  NextMethod(x, quote = FALSE, ...)
}
