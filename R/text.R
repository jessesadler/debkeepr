## Transform deb_lsd, deb_decimal, and deb_tetra to text for labels ##

#' Format `deb_lsd`, `deb_decimal`, and `deb_tetra` vectors as text
#'
#' Flexible way to format `deb_lsd`, `deb_decimal`, and `deb_tetra` vectors
#' for use as labels or text.
#'
#' @details
#' `deb_text` is similar to `as.character()` in that both return a character
#' vector of the values of `deb_lsd`, `deb_decimal`, and `deb_tetra` vectors.
#' However, `as.character()` uses the normal printing method for these vectors.
#' `deb_text()` provides a convenient way to nicely format `deb_lsd`,
#' `deb_decimal`, and `deb_tetra` vectors for use as text or labels with
#' options for customization.
#'
#' `deb_text()` uses `formatC()` to format the numeric values of `x`. Numbers
#' are printed in non-scientific format and trailing zeros are dropped.
#'
#' All character vector arguments should be length 1.
#'
#' @seealso [`formatC()`] for further options passed to `...`.
#'
#' @param x A vector of class `deb_lsd`, `deb_decimal`, or `deb_tetra`.
#' @param digits Desired number of digits after the decimal mark to which to
#'   round the numeric values. Default is `0`.
#' @param currency Character used for the currency mark. Default is pound sign.
#' @param l.mark Character used following the pounds (l) unit.
#'   Default is `""`.
#' @param s.mark Character used following the shillings (s) unit.
#'   Default is `"s."`.
#' @param d.mark Character used following the pence (d) unit.
#'   Default is `"d."`.
#' @param f.mark Character used following the farthing (f) unit with
#'   tetrapartite values. Default is `"f."`.
#' @param sep Character to separate pounds, shillings, pence, and optionally
#'   farthing units. Default is `" "`.
#' @param big.mark Character used to mark intervals to the left of the decimal
#'   mark. Default is `","` with default `big.interval` of `3`.
#' @param decimal.mark Character used for decimal mark. Default is `"."`.
#' @param suffix Character placed after the values. Default is `""`.
#' @param ... Arguments passed on to further methods.
#'
#' @returns A Character vector of formatted values.
#'
#' @examples
#' lsd <- deb_lsd(l = c(10000, 0, -10000),
#'                s = c(8, 0, -8),
#'                d = c(5.8252, 0, -5.8252))
#' dec <- deb_decimal(c(10000.8252, 0, -10000.8252))
#' tetra <- deb_tetra(l = c(10000, 0, -10000),
#'                    s = c(8, 0, -8),
#'                    d = c(5, 0, -5),
#'                    f = c(2.8252, 0, -2.8252))
#'
#' deb_text(lsd)
#' deb_text(dec)
#' deb_text(tetra)
#'
#' # Compact format for deb_lsd with suffix to distinguish currency
#' deb_text(lsd, s.mark = "", d.mark = "",
#'          sep = ".", suffix = " Flemish")
#'
#' # Control the number of digits
#' deb_text(lsd, digits = 3)
#' deb_text(dec, digits = 3)
#' deb_text(tetra, digits = 3)
#'
#' # Change big mark and decimal mark
#' deb_text(lsd, digits = 4, big.mark = ".", decimal.mark = ",")
#' deb_text(dec, digits = 4, big.mark = ".", decimal.mark = ",")
#' deb_text(tetra, digits = 4, big.mark = ".", decimal.mark = ",")
#'
#' @name text
NULL

#' @rdname text
#' @export
deb_text <- function(x, ...) {
  UseMethod("deb_text")
}

#' @rdname text
#' @export
deb_text.default <- function(x, ...) {
  cli::cli_abort(c("{.arg x} must be a <deb_lsd>, <deb_tetra>, or
                     <deb_decimal> vector.",
                   "x" = "You've supplied a {.cls {class(x)}} vector."))
}

#' @rdname text
#' @export
deb_text.deb_lsd <- function(x,
                             digits = 0,
                             currency = "\u00A3",
                             l.mark = "",
                             s.mark = "s.",
                             d.mark = "d.",
                             sep = " ",
                             big.mark = ",",
                             decimal.mark = ".",
                             suffix = "",
                             ...) {
  # Deal with positive vs negative
  currency <- dplyr::if_else(x >= 0, currency, paste0("-", currency))
  x <- dplyr::if_else(x >= 0, x, -x)

  # Format numeric value of l and d
  l <- formatC(field(x, "l"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE,  ...)
  s <- formatC(field(x, "s"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)
  d <- formatC(field(x, "d"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)

  out <- paste0(currency, l, l.mark, sep, s, s.mark, sep, d, d.mark, suffix)
  out[is.na(x)] <- NA
  out
}

#' @rdname text
#' @export
deb_text.deb_decimal <- function(x,
                                 digits = 0,
                                 currency = "\u00A3",
                                 big.mark = ",",
                                 decimal.mark = ".",
                                 suffix = "",
                                 ...) {
  # Deal with positive vs negative
  currency <- dplyr::if_else(x >= 0, currency, paste0("-", currency))
  x <- dplyr::if_else(x >= 0, x, -x)

  # Format numeric value
  dec_chr <- formatC(x, format = "f", digits = digits, big.mark = big.mark,
                     decimal.mark = decimal.mark, drop0trailing = TRUE, ...)
  out <- paste0(currency, dec_chr, suffix)
  out[is.na(x)] <- NA
  out
}

#' @rdname text
#' @export
deb_text.deb_tetra <- function(x,
                               digits = 0,
                               currency = "\u00A3",
                               l.mark = "",
                               s.mark = "s.",
                               d.mark = "d.",
                               f.mark = "f.",
                               sep = " ",
                               big.mark = ",",
                               decimal.mark = ".",
                               suffix = "",
                               ...) {
  # Deal with positive vs negative
  currency <- dplyr::if_else(x >= 0, currency, paste0("-", currency))
  x <- dplyr::if_else(x >= 0, x, -x)

  # Format numeric value of l and d
  l <- formatC(field(x, "l"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE,  ...)
  s <- formatC(field(x, "s"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)
  d <- formatC(field(x, "d"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)
  f <- formatC(field(x, "f"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)

  out <- paste0(currency, l, l.mark, sep, s, s.mark, sep,
                d, d.mark, sep, f, f.mark, suffix)
  out[is.na(x)] <- NA
  out
}
