## Equality and comparison ##

# deb_decimal gets this for free because it is based on double()

#' Equality and comparison
#' @param x A deb_lsd vector.
#' @param ... Arguments passed on to further methods.
#' @returns A data frame or numeric vector to be used for comparison.
#' @name comparison
NULL

# deb_lsd equality --------------------------------------------------------

#' @rdname comparison
#' @method vec_proxy_equal deb_lsd
#' @export
#' @export vec_proxy_equal.deb_lsd
vec_proxy_equal.deb_lsd <- function(x, ...) {
  x <- deb_normalize(x)
  data.frame(l = field(x, "l"),
             s = field(x, "s"),
             d = field(x, "d"))
}


# deb_lsd comparison ------------------------------------------------------

#' @rdname comparison
#' @method vec_proxy_compare deb_lsd
#' @export
#' @export vec_proxy_compare.deb_lsd
vec_proxy_compare.deb_lsd <- function(x, ...) {
  field(x, "l") +
    field(x, "s") / deb_bases(x)[[1]] +
    field(x, "d") / prod(deb_bases(x))
}

# deb_tetra equality ------------------------------------------------------

#' @rdname comparison
#' @method vec_proxy_equal deb_tetra
#' @export
#' @export vec_proxy_equal.deb_tetra
vec_proxy_equal.deb_tetra <- function(x, ...) {
  x <- deb_normalize(x)
  data.frame(l = field(x, "l"),
             s = field(x, "s"),
             d = field(x, "d"),
             f = field(x, "f"))
}


# deb_tetra comparison ----------------------------------------------------

#' @rdname comparison
#' @method vec_proxy_compare deb_tetra
#' @export
#' @export vec_proxy_compare.deb_tetra
vec_proxy_compare.deb_tetra <- function(x, ...) {
  field(x, "l") +
    field(x, "s") / deb_bases(x)[[1]] +
    field(x, "d") / prod(deb_bases(x)[1:2]) +
    field(x, "f") / prod(deb_bases(x))
}
