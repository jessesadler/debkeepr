## lsd decimalization mutate ##

### From lsd to decimalized l, s, and d in a data frame ###

## Helper functions ##
decimalize_l <- function(l, s, d) {
  l + s/20 + d/240
}

decimalize_s <- function(l, s, d) {
  l * 20 + s + d/12
}

decimalize_d <- function(l, s, d) {
  l * 240 + s * 12 + d
}

#' Convert from pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence variables in a data frame to a
#' decimalized pounds variable.
#'
#' @inheritParams deb_normalize_df
#' @param column_name Unquoted name of the column for the decimalized pounds.
#'   Default is `pounds`.
#'
#' @return Returns a data frame with a new decimalized pounds variable.
#'
#' @examples
#' # Create a new decimalized pounds variable
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_lsd_l_mutate(example, l, s, d)
#'
#' # Change the name of the decimlaized pounds variable
#' deb_lsd_l_mutate(example, l, s, d, column_name = pounds)
#'
#' @export

deb_lsd_l_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = librae) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_l(l, s, d))
}

#' Convert from pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence variables in a data frame to a
#' decimalized shillings variable.
#'
#' @inheritParams deb_normalize_df
#' @param column_name Unquoted name of the column for the decimalized
#'   shillings. Default is `shillings`.
#'
#' @return Returns a data frame with a new decimalized shillings variable.
#'
#' @examples
#' # Create a new decimalized shillings variable
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_lsd_s_mutate(example, l, s, d)
#'
#' # Change the name of the decimlaized shillings variable
#' deb_lsd_s_mutate(example, l, s, d, column_name = shillings)
#'
#' @export

deb_lsd_s_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = solidi) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_s(l, s, d))
}

#' Convert from pounds, shillings and pence to pence
#'
#' Convert pounds, shillings, and pence variables in a data frame to a
#' decimalized pence variable. Converting to the lowest denomination turns
#' the non-decimal pounds, shillings, and pence currency into a decimal
#' currency.
#'
#' @inheritParams deb_normalize_df
#' @param column_name Unquoted name of the column for the decimalized
#'   pence. Default is `pence`.
#'
#' @return Returns a data frame with a new decimalized pence variable.
#'
#' @examples
#' # Create a new decimalized pence variable
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_lsd_d_mutate(example, l, s, d)
#'
#' # Change the name of the decimlaized pence variable
#' deb_lsd_d_mutate(example, l, s, d, column_name = pence)
#'
#' @export

deb_lsd_d_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = denarii) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_d(l, s, d))
}
