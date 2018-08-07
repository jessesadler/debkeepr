## lsd base conversion ##

#' Convert between different bases for shillings and pence units
#'
#' Convert between pounds, shillings, and pence currencies that use different
#' bases for the shillings and pence units.
#'
#' `deb_convert_bases()` is similar to [deb_exchange()], but the latter performs
#' exchange on currencies that share the same shillings and pence bases.
#'
#' @inheritParams deb_normalize
#' @param bases1 Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d values of `lsd`. `bases1` represents the
#'   bases for the current `lsd` vector(s).
#' @param bases2 Numeric vector of length 2 used to specify the bases for
#'   the shillings or s and pence or d values to which `lsd` will be converted.
#' @param ratio The ratio between the two currencies that possess different
#'   bases. This is the value by which `lsd` is multiplied. Numeric vector of
#'   length 1 with the default of `1`.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings, and
#'   pence with the bases for shillings and pence conforming to `bases2`.
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
#' deb_convert_bases(lsd = c(1224, 19, 8),
#'                   bases1 = c(20, 16),
#'                   bases2 = c(20, 12),
#'                   ratio = 1/6) %>%
#'   deb_exchange(shillings_rate = 12)
#'
#' # Convert a list of lsd vectors of guilders to pounds Flemish
#' guilders_list <- list(c(1224, 19, 8), c(101, 5, 13), c(225, 13, 15))
#' deb_convert_bases(lsd = guilders_list,
#'                   bases1 = c(20, 16),
#'                   bases2 = c(20, 12),
#'                   ratio = 1/6)
#'
#' @export

deb_convert_bases <- function(lsd, bases1, bases2, ratio = 1) {
  ratio_check(ratio)

  librae <- deb_lsd_l(lsd = lsd, bases = bases1) * ratio
  deb_l_lsd(l = librae, bases = bases2)
}


#' Convert between different bases for shillings and pence units in a data
#' frame
#'
#' Uses [dplyr::mutate()] to convert pounds, shillings, and pence currencies
#' that use different bases for the shillings and pence variables in a data
#' frame. The converted values are returned in the form of three new variables
#' representing the calculated pounds, shillings and pence for the new bases.
#'
#' `deb_convert_bases_mutate()` is similar to [deb_exchange_mutate()], but the
#' latter performs exchanges on currencies that share the same shillings and
#' pence bases.
#'
#' @inheritParams deb_normalize_df
#' @param bases1 Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d values of `l`, `s`, and `d` variables.
#' @param bases2 Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d values to which `l`, `s`, and `d` will be
#'   converted.
#' @param ratio The ratio between the two currencies that possess different
#'   bases. This is the value by which `l`, `s`, and `d` variables are
#'   multiplied. Numeric vector of length 1 with the default of `1`.
#' @param suffix Suffix added to the column names for the pounds, shillings,
#'   and pence columns representing the converted values to distinguish them
#'   from the original pounds, shillings, and pence columns. Default is ".1".
#'   Must be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence corresponding to the shillings and pence bases in `bases2`.
#'
#' @examples
#' # Conversion between pounds Flemish of 20 shillings and 12 pence
#' # to guilders of 20 stuivers and 16 penningen at the rate of
#' # 6 guilders equals £1 Flemish
#' flemish <- data.frame(l = c(204, 82, 36),
#'                       s = c(3, 15, 12),
#'                       d = c(3, 9, 6))
#' deb_convert_bases_mutate(df = flemish,
#'                          l = l, s = s, d = d,
#'                          bases1 = c(20, 12),
#'                          bases2 = c(20, 16),
#'                          ratio = 6)
#'
#' # Convert from guilders to pounds Flemish
#' # Flip the bases argument and change the ratio
#' guilders <- data.frame(l = c(1224, 101, 225),
#'                        s = c(19, 5, 13),
#'                        d = c(8, 13, 15))
#' deb_convert_bases_mutate(df = guilders,
#'                          l = l, s = s, d = d,
#'                          bases1 = c(20, 16),
#'                          bases2 = c(20, 12),
#'                          ratio = 1/6)
#'
#' @export

deb_convert_bases_mutate <- function(df,
                                     l = l, s = s, d = d,
                                     bases1,
                                     bases2,
                                     ratio = 1,
                                     suffix = ".1") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  ratio_check(ratio)

  ret <- deb_lsd_l_mutate(df,
                          l = !! l,
                          s = !! s,
                          d = !! d,
                          column_name = temp_librae_col,
                          bases = bases1) %>%
    dplyr::mutate(temp_librae_col = temp_librae_col * ratio) %>%
    deb_l_lsd_mutate(pounds = temp_librae_col,
                     l_column = !! l,
                     s_column = !! s,
                     d_column = !! d,
                     bases = bases2,
                     suffix = suffix) %>%
    dplyr::select(-temp_librae_col)

  # Get rid of no visible binding for global variable from CMD check
  temp_librae_col <- NULL

  ret
}
