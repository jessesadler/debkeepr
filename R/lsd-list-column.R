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
#' @param lsd_column Unquoted name of the lsd list column. Default is lsd.
#' @param replace Logical (default `FALSE`): when `TRUE` the original pounds,
#'   shillings, and pence columns — `l`, `s`, and `d` — will be removed.

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

  ret <- dplyr::mutate(df, temp_lsd_column = purrr::pmap(
    dplyr::select(df, !! l, !! s, !! d), c)) %>%
    dplyr::mutate(temp_lsd_column = to_lsd(temp_lsd_column, bases)) %>%
    dplyr::rename(!! lsd_column := temp_lsd_column)

  if (replace == TRUE) {
    ret <- dplyr::select(ret, -(!!l), -(!!s), -(!!d))
  }
  temp_lsd_column <- NULL

  ret
}
