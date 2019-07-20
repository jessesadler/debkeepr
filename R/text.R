## Transform deb_lsd and deb_decimal to text for labels ##

#' Format `deb_lsd` and `deb_decimal` objects as text
#'
#' Flexible way to nicely format `deb_lsd` and `deb_decimal` objects for
#' use as labels or texts.
#'
#' @details
#' `deb_text` is similar to `as.character()` in that both return a character
#' vector of the values of `deb_lsd` and `deb_decimal` vectors. However,
#' `as.character()` uses the normal printing method for these objects.
#' `deb_text()` provides a convenient way to nicely format `deb_lsd` and
#' `deb_decimal` objects for use as text or labels with options for
#' customization.
#'
#' `deb_text()` uses `formatC()` to format the numeric values of `x`. Numbers
#' are printed in non-scientific format and trailing zeros are dropped.
#'
#' All character vector arguments should be length 1.
#'
#' @seealso [`formatC()`] for further options passed to `...`.
#'
#' @param x An object of class `deb_lsd` or `deb_decimal`.
#' @param digits Desired number of digits after the decimal mark to which to
#'   round the numeric values. Default is `0`.
#' @param currency Character used for the currency mark. Default is pound sign.
#' @param s.mark Character used following the shillings (s) unit.
#'   Default is `"s."`.
#' @param d.mark Character used following the pence (d) unit.
#'   Default is `"d."`.
#' @param sep Character to separate pounds, shillings, and pence units.
#'   Default is `" "`.
#' @param big.mark Character used to mark intervals to the left of the decimal
#'   mark. Default is `","` with default `big.interval` of `3`.
#' @param decimal.mark Character used for decimal mark. Default is `"."`.
#' @param suffix Character placed after the values. Default is `""`.
#' @param ... Arguments passed on to further methods.
#'
#' @return A Character vector of formatted values.
#'
#' @examples
#' lsd <- deb_lsd(l = c(10000, 0, -10000),
#'                s = c(8, 0, -8),
#'                d = c(5.8252, 0, -5.8252))
#' dec <- deb_decimal(c(10000.8252, 0, -10000.8252))
#'
#' deb_text(lsd)
#' deb_text(dec)
#'
#' # Compact format for deb_lsd with suffix to distinguish currency
#' deb_text(lsd, s.mark = "", d.mark = "",
#'          sep = ".", suffix = " Flemish")
#'
#' # Control the number of digits
#' deb_text(lsd, digits = 3)
#' deb_text(dec, digits = 3)
#'
#' # Change big mark and decimal mark
#' deb_text(lsd, digits = 4, big.mark = ".", decimal.mark = ",")
#' deb_text(dec, digits = 4, big.mark = ".", decimal.mark = ",")
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
  stop(call. = FALSE,
       "`x` must be a <deb_lsd> or <deb_decimal> vector.")
}

#' @rdname text
#' @export
deb_text.deb_lsd <- function(x,
                             digits = 0,
                             currency = "\u00A3",
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
  l <- formatC(vctrs::field(x, "l"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE,  ...)
  s <- formatC(vctrs::field(x, "s"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)
  d <- formatC(vctrs::field(x, "d"), format = "f", digits = digits,
               big.mark = big.mark, decimal.mark = decimal.mark,
               drop0trailing = TRUE, ...)

  out <- paste0(currency, l, sep, s, s.mark, sep, d, d.mark, suffix)
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
