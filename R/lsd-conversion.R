## lsd base conversion ##

#' Convert between different bases for shillings and pence units
#'
#' Convert between pounds, shillings, and pence currencies that use different
#' bases for the shillings and pence units.
#'
#' `deb_convert_bases()` is similar to [deb_exchange()], but the latter performs
#' exchange on currencies that share the same shillings and pence bases.
#'
#' `deb_convert_bases()` is the only way to change the bases attribute of an
#' lsd object.
#'
#' @inheritParams deb_normalize
#' @param bases1 Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d values of `lsd`. If `lsd` is of class lsd,
#'   the bases attribute will be used in the place of this argument.
#' @param bases2 Numeric vector of length 2 used to specify the bases for
#'   the shillings or s and pence or d values to which `lsd` will be converted.
#' @param ratio The ratio between the two currencies that possess different
#'   bases. This is the value by which `lsd` is multiplied. Numeric vector of
#'   length 1 with the default of `1`.
#'
#' @return Returns an object of class lsd with a bases attribute conforming
#'   to `bases2`.
#'
#' @examples
#' # Conversion between pounds Flemish of 20 shillings and 12 pence
#' # to guilders of 20 stuivers and 16 penningen at the rate of
#' # 6 guilders equals £1 Flemish
#' deb_convert_bases(lsd = c(204, 3, 3),
#'                   bases1 = c(20, 12),
#'                   bases2 = c(20, 16),
#'                   ratio = 6)
#'
#' # Convert from guilders to pounds Flemish
#' # Flip the bases argument and change the ratio
#' deb_convert_bases(lsd = c(1224, 19, 8),
#'                   bases1 = c(20, 16),
#'                   bases2 = c(20, 12),
#'                   ratio = 1/6)
#'
#' # If input is an lsd object, bases1 will use the bases attribute
#' guilders <- deb_as_lsd(lsd = c(1224, 19, 8), bases = c(20, 16))
#' deb_convert_bases(lsd = guilders,
#'                   bases2 = c(20, 12),
#'                   ratio = 1/6)
#'
#' # Conversion from French crowns of 60 sous and 12 deniers to
#' # pound sterling of 20 shillings and 12 pence at the rate of
#' # 72d. French crowns equals £1 sterling or 240d. sterling
#' deb_convert_bases(lsd = c(214, 50, 10),
#'                   bases1 = c(60, 12),
#'                   bases2 = c(20, 12),
#'                   ratio = 72/240)
#'
#' # Base conversion can also be done in concert with deb_exchange()
#' # Convert from guilders to pounds sterling at the rate of 12s. Flemish
#' deb_convert_bases(lsd = guilders,
#'                   bases2 = c(20, 12),
#'                   ratio = 1/6) %>%
#'   deb_exchange(shillings_rate = 12)
#'
#' # Convert an lsd object of guilders to pounds Flemish
#' guilders_list <- deb_as_lsd(lsd = list(c(1224, 19, 8),
#'                                        c(101, 5, 13),
#'                                        c(225, 13, 15)),
#'                             bases = c(20, 16))
#' deb_convert_bases(lsd = guilders_list,
#'                   bases2 = c(20, 12),
#'                   ratio = 1/6)
#'
#' @export

deb_convert_bases <- function(lsd,
                              bases1 = NULL, bases2,
                              ratio = 1, round = 5) {
  ratio_check(ratio)
  lsd <- null_check(lsd)
  bases1 <- validate_bases(lsd, bases1)

  if (is.null(bases1)) {
    stop(call. = FALSE, "lsd must have a bases attribute or a value must be provided for bases1")
  }

  librae <- deb_lsd_l(lsd = lsd, bases = bases1) * ratio
  deb_l_lsd(l = librae, bases = bases2, round = round)
}
