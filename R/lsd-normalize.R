## Normalize lsd values ##


# Error messages ----------------------------------------------------------
lsd_msg <- c("{.var bases} must be a numeric vector of length {.val {2}}
                 to normalize a tripartite value.",
             "x" = "You've supplied a {.cls {class(bases)}} vector of
                 length {.val {vec_size(bases)}}.")
tetra_msg <- c("{.var bases} must be a numeric vector of length {.val {3}}
                   to normalize a tetrapartite value.",
               "x" = "You've supplied a {.cls {class(bases)}} vector of
                   length {.val {vec_size(bases)}}.")

# Normalization pieces ----------------------------------------------------

#' Check for non-whole numbers in libra and solidus units
#'
#' Move any decimals in libra and solidus units to the denarius unit
#' for lsd and any decimals is libra, solidus, or denarius to the farthing
#' unit in tetra. The function uses the utility `should_be_int()` to deal
#' with floating point problems.
#'
#' @keywords internal
#' @noRd

decimal_check <- function(x) {
  l <- field(x, "l")
  s <- field(x, "s")
  d <- field(x, "d")

  field(x, "l") <- trunc(l)
  temp_s <- s + (l - trunc(l)) * deb_bases(x)[[1]]
  field(x, "s") <- trunc(temp_s)
  temp_d <- d + (temp_s - trunc(temp_s)) * deb_bases(x)[[2]]

  if (deb_is_lsd(x)) {
    # Deal with floating point problems potentially introduced by the above
    field(x, "d") <- dplyr::if_else(should_be_int(temp_d),
                                    round(temp_d),
                                    temp_d)
  } else if (deb_is_tetra(x)) {

    field(x, "d") <- trunc(temp_d)
    temp_f <- field(x, "f") + (temp_d - trunc(temp_d)) * deb_bases(x)[[3]]
    field(x, "f") <- dplyr::if_else(should_be_int(temp_f),
                                    round(temp_f),
                                    temp_f)
  }
  x
}

#' Check whether lsd value is positive or negative
#' @keywords internal
#' @noRd

is_negative <- function(x) {
  if (deb_is_lsd(x)) {
    field(x, "l") +
      field(x, "s") / deb_bases(x)[[1]] +
      field(x, "d") / prod(deb_bases(x)) < 0
  } else if (deb_is_tetra(x)) {
    field(x, "l") +
      field(x, "s") / deb_bases(x)[[1]] +
      field(x, "d") / prod(deb_bases(x)[1:2]) +
      field(x, "f") / prod(deb_bases(x)) < 0
  }
}

#' Normalization function
#'
#' Function that actually performs the normalization of lsd value
#' @keywords internal
#' @noRd

normalize <- function(l, s, d, bases) {

  solidus <- s + (d %/% bases[[2]])
  libra <- l + (solidus %/% bases[[1]])

  new_lsd(l = libra,
          s = solidus %% bases[[1]],
          d = d %% bases[[2]],
          bases = bases)
}

#' Normalization path for positive values
#'
#' Separate normalization functions for positive and negative values to be
#' used in `dplyr::if_else()`. Making them functions simplifies the process.
#' @keywords internal
#' @noRd

lsd_normalize <- function(lsd) {

  normalize(l = field(lsd, "l"),
            s = field(lsd, "s"),
            d = field(lsd, "d"),
            bases = deb_bases(lsd))
}

#' Normalization path for negative values
#'
#' Turn values positive and then return to negative value.
#' @keywords internal
#' @noRd

lsd_normalize_neg <- function(lsd) {

  ret <- normalize(l = -field(lsd, "l"),
                   s = -field(lsd, "s"),
                   d = -field(lsd, "d"),
                   bases = deb_bases(lsd))

  -ret
}


#' Tetra  normalization function
#'
#' Function that actually performs the normalization of tetra values
#' @keywords internal
#' @noRd

tetra_normalize <- function(l, s, d, f, bases) {

  denarius <- d + (f %/% bases[[3]])
  solidus <- s + (denarius %/% bases[[2]])
  libra <- l + (solidus %/% bases[[1]])

  new_tetra(l = libra,
            s = solidus %% bases[[1]],
            d = denarius %% bases[[2]],
            f = f %% bases[[3]],
            bases = bases)
}

#' Normalization path for positive tetra values
#'
#' Separate normalization functions for positive and negative values to be
#' used in `dplyr::if_else()`. Making them functions simplifies the process.
#' @keywords internal
#' @noRd

tetra_normalize_pos <- function(tetra) {

  tetra_normalize(l = field(tetra, "l"),
                  s = field(tetra, "s"),
                  d = field(tetra, "d"),
                  f = field(tetra, "f"),
                  bases = deb_bases(tetra))
}

#' Normalization path for negative tetra values
#'
#' Turn values positive and then return to negative value.
#' @keywords internal
#' @noRd

tetra_normalize_neg <- function(tetra) {

  ret <- tetra_normalize(l = -field(tetra, "l"),
                         s = -field(tetra, "s"),
                         d = -field(tetra, "d"),
                         f = -field(tetra, "f"),
                         bases = deb_bases(tetra))

  -ret
}

# deb_normalize methods ---------------------------------------------------

#' Normalize tripartite and tetrapartite values
#'
#' Normalize tripartite and tetrapartite values values to given bases.
#'
#' @param x Either an vector of class `deb_lsd`, `deb_tetra`, or a numeric
#'   vector of length 3 or 4 representing the values to be normalized.
#' @param bases Used only if `x` is a numeric vector. A Numeric vector of
#'   length 2 or 3 used to specify the bases for the solidus or s, denarius or
#'   d, and optionally the farthing or f units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and 1
#'   shilling = 12 pence.
#' @param ... Arguments passed on to further methods.
#'
#' @returns Returns a vector of class `deb_lsd` with normalized solidus and
#'   denarius units or a vector of class `deb_tetra` with normalized solidus,
#'   denarius, and farthing units.
#' @examples
#'
#' # Normalize a deb_lsd vector
#' x <- deb_lsd(12, 93, 78)
#' x_alt <- deb_lsd(12, 93, 78, bases = c(60, 16))
#' deb_normalize(x)
#' deb_normalize(x_alt)
#'
#' # Normalize a deb_tetra vector
#' t <- deb_tetra(12, 83, 78, 42)
#' t_alt <- deb_tetra(12, 83, 78, 42, bases = c(60, 16, 8))
#' deb_normalize(t)
#' deb_normalize(t_alt)
#'
#' # Normalize a numeric vector of length 3
#' deb_normalize(c(12, 93, 78))
#' deb_normalize(c(12, 93, 78), bases = c(60, 16))
#'
#' # Normalize a numeric vector of length 4
#' # Must provide bases of length 3
#' deb_normalize(c(12, 93, 78, 42), bases = c(20, 12, 4))
#' deb_normalize(c(12, 93, 78, 42), bases = c(60, 16, 8))
#'
#' @name normalization
NULL

#' @rdname normalization
#' @export
deb_normalize <- function(x, ...) {
  UseMethod("deb_normalize")
}

#' @rdname normalization
#' @export
deb_normalize.default <- function(x, ...) {
  cli::cli_abort(c("{.var x } must be a <deb_lsd> or <deb_tetra> vector or a
                     numeric vector of length {.val {3}} or {.val {4}}.",
                   "x" = "You've supplied a {.cls {class(x)}} vector."))
}

#' @rdname normalization
#' @export
deb_normalize.deb_lsd <- function(x, ...) {
  checked <- decimal_check(x)
  dplyr::if_else(is_negative(x),
                 lsd_normalize_neg(checked),
                 lsd_normalize(checked))
}

#' @rdname normalization
#' @export
deb_normalize.numeric <- function(x, bases = c(20, 12), ...) {
  if (vec_size(x) == 3L) {
    # Check bases
    if (!is.numeric(bases) || vec_size(bases) != 2L) {
      cli::cli_abort(message = lsd_msg)
    }
    lsd <- deb_lsd(x[[1]], x[[2]], x[[3]], bases)
    ret <- deb_normalize(lsd)

  } else if (vec_size(x) == 4L) {
    # Check bases
    if (!is.numeric(bases) || vec_size(bases) != 3L) {
      cli::cli_abort(tetra_msg)
    }
    tetra <- deb_tetra(x[[1]], x[[2]], x[[3]], x[[4]], bases)
    ret <- deb_normalize(tetra)

  } else {
    cli::cli_abort(c(
      "{.var x } must be a <deb_lsd> or <deb_tetra> vector or a numeric
          vector of length {.val {3}} or {.val {4}}.",
      "x" = "You've supplied a {.cls {class(x)}} vector of length
          {.val {vec_size(x)}}."))
  }
  ret
}

#' @rdname normalization
#' @export
deb_normalize.deb_tetra <- function(x, ...) {
  checked <- decimal_check(x)
  dplyr::if_else(is_negative(x),
                 tetra_normalize_neg(checked),
                 tetra_normalize_pos(checked))
}
