## Sum of lsd columns ##

## Helper functions to sum l, s, and d ##

## Librae ##
deb_librae_sum <- function(l, s, d, lsd_bases = c(20, 12)) {
  deb_librae(sum(l), sum(s), sum(d), lsd_bases = lsd_bases)
}

## Solidi ##
deb_solidi_sum <- function(l, s, d, lsd_bases = c(20, 12)) {
  deb_solidi(sum(l), sum(s), sum(d), lsd_bases = lsd_bases)
}

## Denarii ##
deb_denarii_sum <- function(l, s, d, lsd_bases = c(20, 12), round = 3) {
  deb_denarii(sum(l), sum(s), sum(d), lsd_bases = lsd_bases, round = round)
}

#' Sum of pounds, shillings, and pence columns in a data frame
#'
#' Uses [dplyr::summarise()] to add pounds, shillings, and pence
#' columns in a data frame and normalizes the values.
#'
#' When used on a data frame without any grouping, the result will be a data
#' frame with a single row consisting of columns for pounds, shillings, and
#' pence. When used in conjunction with [dplyr::group_by()], [deb_sum()] will
#' summarize the pounds, shillings, and pence columns for each group.
#'
#' @inheritParams deb_normalize_df
#'
#' @return Returns a data frame with one level of grouping dropped. Any
#'   variables other than `l`, `s`, and `d` that are not grouped will be
#'   dropped. If the sum of any group is a negative value, the `l`, `s`,
#'   and `d` values for that group will all be returned as negative.
#'
#' @examples
#' # Use on an ungrouped data frame adds all values of pounds, shillings,
#' # and pence, resulting in a data frame with one row.
#' example1 <- data.frame(group = c(1, 2, 1, 2),
#'                        l = c(3, 5, 6, 2),
#'                        s = c(10, 18, 11, 16),
#'                        d = c(9, 11, 10, 5))
#' deb_sum(example1, l, s, d)
#'
#' # Use with group_by() summarizes the values of pounds, shillings,
#' # and pence for each group.
#' example1 %>%
#'   dplyr::group_by(group) %>%
#'   deb_sum(l, s, d)
#'
#' # The default for the function is to have pounds, shillings, and pence
#' # columns labeled as l, s, d. If this is true, you do not need to
#' # include these arguments.
#' example1 %>%
#'   dplyr::group_by(group) %>%
#'   deb_sum()
#'
#' # The function can take into account negative values
#' example2 <- data.frame(group = c(1, 2, 1, 2),
#'                        l = c(-3, 5, -6, 2),
#'                        s = c(-10, 18, -11, 16),
#'                        d = c(-9, 11, -10, 5))
#' example2 %>%
#'   dplyr::group_by(group) %>%
#'   deb_sum(l, s, d)
#'
#' @export

deb_sum <- function(df, l = l, s = s, d = d, lsd_bases = c(20, 12), round = 3) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  paramenter_check(lsd_bases, round)

  # Column names
  l_column <- rlang::quo_name(l)
  s_column <- rlang::quo_name(s)
  d_column <- rlang::quo_name(d)

  # Use temp columns and rename so that l, s, and d do not get overwritten
  ret <- df %>%
    dplyr::summarise(temp_librae_col = deb_librae_sum(!!l, !!s, !!d, lsd_bases),
                     temp_solidi_col = deb_solidi_sum(!!l, !!s, !!d, lsd_bases),
                     temp_denarii_col = deb_denarii_sum(!!l, !!s, !!d, lsd_bases, round)) %>%
    dplyr::rename(!! l_column := temp_librae_col,
                  !! s_column := temp_solidi_col,
                  !! d_column := temp_denarii_col)

  # Get rid of no visible binding for global variable from CMD check
  temp_librae_col <- NULL
  temp_solidi_col <- NULL
  temp_denarii_col <- NULL

  ret
}
