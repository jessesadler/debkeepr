## lsd list column ##

#' Create an lsd list column
#'
#' Create an lsd list column in a data frame from separate pounds, shillings,
#' and pence variables in a data frame.
#'
#' The newly created lsd list column possesses a bases attribute to record the
#' bases for the shillings or s and pence or d units of the values. The lsd
#' column can be manipulated with `debkeepr` functions in concert with
#' [dplyr::mutate()].
#'
#' @param df A data frame that contains pounds, shillings, and pence variables.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding
#'   to the pounds or librae unit. Default is l.
#' @param s Shillings column: Unquoted name of numeric variable corresponding
#'   to the shillings or solidi unit. Default is s.
#' @param d Pence column: Unquoted name of numeric variable corresponding to
#'   the pence or denarii unit. Default is d.
#' @inheritParams deb_as_lsd
#' @param lsd_column Unquoted name of the lsd list column to be created.
#'   Default is lsd.
#' @param replace Logical (default `FALSE`): when `TRUE` the original pounds,
#'   shillings, and pence columns — `l`, `s`, and `d` — will be removed.
#'
#' @return Returns a data frame with a list column of class lsd.
#'
#' @examples
#' # Data frame with separate l, s, and d variables
#' example <- data.frame(accounts = c(1, 2, 1, 2),
#'                       l = c(3, 5, 6, 2),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#'
#' # Create an lsd list column
#' deb_as_lsd_mutate(df = example,
#'                   l = l, s = s, d = d,
#'                   bases = c(20, 12))
#'
#' # Create an lsd list column with alternative bases
#' deb_as_lsd_mutate(df = example,
#'                   l = l, s = s, d = d,
#'                   bases = c(20, 16))
#'
#' # Can replace original pounds, shillings, and pence variables
#' # and can choose the name for the lsd list column
#' deb_as_lsd_mutate(df = example,
#'                   l = l, s = s, d = d,
#'                   bases = c(20, 16),
#'                   lsd_column = guilders,
#'                   replace = TRUE)
#'
#' @export

deb_as_lsd_mutate <- function(df,
                              l = l, s = s, d = d,
                              bases = c(20, 12),
                              lsd_column = lsd,
                              replace = FALSE) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)
  lsd_column <- rlang::quo_name(rlang::enquo(lsd_column))

  lsd_column_check(df, l, s, d)
  bases_check(bases)

  ret <- dplyr::mutate(df, !! lsd_column := deb_lsd(!! l, !! s, !! d, bases))

  if (replace == TRUE) {
    ret <- dplyr::select(ret, -(!!l), -(!!s), -(!!d))
  }
  ret
}

#' Create separate pounds, shillings, and pence columns
#'
#' Create separate pounds, shillings, and pence columns from an lsd list column
#' in a data frame.
#'
#' The newly created pounds, shillings, and pence variables will lose their
#' connection to the lsd bases attribute in the lsd list column.
#'
#' @param df A data frame that contains an lsd list column.
#' @param lsd lsd list column: Unquoted name of an lsd list column of pounds,
#'   shillings, and pence values with a bases attribute.
#' @param l_column An unquoted name for the pounds column created by the
#'   function. Default is l.
#' @param s_column An unquoted name for the shillings column created by
#'   the function. Default is s.
#' @param d_column An unquoted name for the pence column created by the
#'   function. Default is d.
#' @param suffix Suffix added to `l_column`, `s_column`, or `d_column` if `df`
#'   already contains variables with the same names. Default is ".1". Must be a
#'   character vector of length 1.
#' @param replace Logical (default `FALSE`): when `TRUE` the original lsd list
#'   column — `lsd` — will be removed.
#'
#' @return Returns a data frame with a three new variables representing pounds,
#'   shillings, and pence with equivalent values to `lsd`.
#'
#' @examples
#' library(tibble)
#'
#' # Tibble with an lsd list column
#' example <- tibble(accounts = c(1, 2, 1, 2),
#'                   lsd = deb_lsd(l = c(3, 5, 6, 2),
#'                                 s = c(10, 18, 11, 16),
#'                                 d = c(9, 11, 10, 5)))
#'
#' # Create separate l, s, and d variables
#' deb_from_lsd_mutate(df = example,
#'                   lsd = lsd)
#'
#'
#' # Can replace original lsd list column and choose the
#' # names for the pounds, shillings, and pence variables
#' deb_from_lsd_mutate(df = example,
#'                    lsd = lsd,
#'                    l_column = pounds,
#'                    s_column = shillings,
#'                    d_column = pence,
#'                    replace = TRUE)
#'
#' @export

deb_from_lsd_mutate <- function(df,
                                lsd = lsd,
                                l_column = l,
                                s_column = s,
                                d_column = d,
                                suffix = ".1",
                                replace = FALSE) {
  lsd <- rlang::enquo(lsd)
  lsd_list_column_check(df, lsd)

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)
  ret <- dplyr::mutate(df,
                       !! lsd_names[1] := purrr::map_dbl(!! lsd, 1),
                       !! lsd_names[2] := purrr::map_dbl(!! lsd, 2),
                       !! lsd_names[3] := purrr::map_dbl(!! lsd, 3))
  if (replace == TRUE) {
    ret <- dplyr::select(ret, -(!!lsd))
  }
  ret

}
