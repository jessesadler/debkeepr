## Sum of lsd columns ##

#' Sum of pounds, shillings, and pence columns in a data frame
#'
#' Uses \code{summarise()} from \code{dplyr} to add pounds, shillings, and pence
#' columns in a data frame and refactor the values so that the result has
#' properly formatted values.
#'
#' When used on a data frame without any grouping, the result will be a data
#' frame with a single row consisting of columns for pounds, shillings, and
#' pence. When used in conjunction with \code{group_by()}, \code{deb_sum()}
#' will summarize the pounds, shillings, and pence columns for each group.
#'
#' @param df A data frame that contains columns with pounds, shillings,
#'   and pence variables.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding to pounds.
#' @param s Shillings column: Unquoted name of numeric variable corresponding to shillings.
#' @param d Pence column: Unquoted name of numeric variable corresponding to pence.
#' @param round round pence to specified number of decimal places.
#'   Default is 3. Set to 0 if you want pence to always be a whole number.
#'
#' @return Returns a data frame with one level of grouping dropped.
#'   Any variables other than l, s, and d that are not grouped will
#'   be dropped. If the sum of any group is a negative value, the l,
#'   s, and d values for that group will all be returned as negative.
#'
#' @examples
#' # Use on an ungrouped data frame adds all values of pounds, shillings,
#' # and pence, resulting in a data frame with one row.
#' example1 <- tibble::tibble(group = c(1, 2, 1, 2),
#'                            l = c(3, 5, 6, 2),
#'                            s = c(10, 18, 11, 16),
#'                            d = c(9, 11, 10, 5))
#' deb_sum(example1, l, s, d)
#'
#' # Use with group_by() summarizes the values of pounds, shillings,
#' # and pence for each group.
#' example1 %>%
#'   group_by(group) %>%
#'   deb_sum(l, s, d)
#'
#' # The default for the function is to have pounds, shillings, and pence
#' # columns labeled as l, s, d. If this is true, you do not need to
#' # include these arguments.
#' example1 %>%
#'   group_by(group) %>%
#'   deb_sum()
#'
#' # The function can take into account negative values
#' example2 <- tibble::tibble(group = c(1, 2, 1, 2),
#'                            l = c(-3, 5, -6, 2),
#'                            s = c(-10, 18, -11, 16),
#'                            d = c(-9, 11, -10, 5))
#' example2 %>%
#'   group_by(group) %>%
#'   deb_sum(l, s, d)
#'
#' @export

deb_sum <- function(df, l = l, s = s, d = d, round = 3) {
  l <- dplyr::enquo(l)
  s <- dplyr::enquo(s)
  d <- dplyr::enquo(d)
  # Column names
  l_column <- dplyr::quo_name(l)
  s_column <- dplyr::quo_name(s)
  d_column <- dplyr::quo_name(d)

  lsd_column_check(df, l, s, d)

  # Use temp columns and rename so that l, s, and d do not get overwritten
  df %>%
    dplyr::summarise(temp_l_col = deb_librae(sum(!!l), sum(!!s), sum(!!d)),
                     temp_s_col = deb_solidi(sum(!!l), sum(!!s), sum(!!d)),
                     temp_d_col = deb_denarii(sum(!!l), sum(!!s), sum(!!d), round)) %>%
    dplyr::rename(!! l_column := temp_l_col,
                  !! s_column := temp_s_col,
                  !! d_column := temp_d_col)
}
