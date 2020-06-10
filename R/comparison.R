## Equality and comparison for deb_lsd ##

# deb_decimal gets this for free because it is based on double()

#' Equality and comparison
#' @param x A deb_lsd object.
#' @param ... Arguments passed on to further methods.
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
  field(x, "l") + field(x, "s") /
    deb_bases(x)[[1]] + field(x, "d") / prod(deb_bases(x))
}
