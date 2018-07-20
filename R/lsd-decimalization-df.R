## lsd decimalization mutate ##

### From lsd to decimalized l, s, and d in a data frame ###

## Helper functions ##
decimalize_l <- function(l, s, d, lsd_bases = c(20, 12)) {
  l + s / lsd_bases[1] + d / prod(lsd_bases)
}

decimalize_s <- function(l, s, d, lsd_bases = c(20, 12)) {
  l * lsd_bases[1] + s + d / lsd_bases[2]
}

decimalize_d <- function(l, s, d, lsd_bases = c(20, 12)) {
  round(l * prod(lsd_bases) + s * lsd_bases[2] + d, 5)
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
                             column_name = librae,
                             lsd_bases = c(20, 12)) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  bases_check(lsd_bases)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_l(!! l, !! s, !! d, lsd_bases))
}

#' Mutate decimal pounds into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a decimalized pounds variable.
#' Thus, the function converts decimalized pounds into the lsd system of
#' pounds, shillings, and pence.
#'
#' @param df A data frame that contains a column of decimalized pounds.
#' @param librae Decimalized pounds column: Unquoted name of a numeric
#'   variable corresponding to pounds. This is the variable that will be
#'   mutated into pounds, shillings, and pence variables.
#' @param l_column An unquoted name for the pounds column created by the
#'   function. Default is l.
#' @param s_column An unquoted name for the shillings column created by
#'   the function. Default is s.
#' @param d_column An unquoted name for the pence column created by the
#'   function. Default is d.
#' @inheritParams deb_normalize_df
#' @param suffix If the data frame already contains variables with the same
#'   names as `l_column`, `s_column`, or `d_column`, this suffix will be
#'   added to the new variables to distinguish them. Default is ".1".
#'
#' @return Returns a data frame with three new variables of pounds,
#'   shillings, and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a decimalized pounds variable
#' example <- data.frame(pounds = c(8, 8.325, -8.325, 5.425, 4.5678))
#'
#' deb_l_lsd_mutate(df = example, librae = pounds)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_l_lsd_mutate(librae = pounds,
#'                    l_column = librae,
#'                    s_column = solidi,
#'                    d_column = denarii)
#'
#' @export

deb_l_lsd_mutate <- function(df, librae,
                             l_column = l,
                             s_column = s,
                             d_column = d,
                             lsd_bases = c(20, 12),
                             suffix = ".1") {

  librae <- rlang::enquo(librae)

  # Check that librae exists in df
  if (rlang::quo_name(librae) %in% names(df) == FALSE) {
    stop(call. = FALSE, "librae column must exist in df")
  }

  # Check that librae is numeric
  if (!is.numeric(rlang::eval_tidy(librae, df))) {
    stop(call. = FALSE, "librae must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  lsd_mutate_columns(df,
                     l = !! librae,
                     s = rep(0, length(!! librae)),
                     d = rep(0, length(!! librae)),
                     lsd_names = lsd_names,
                     replace = FALSE,
                     lsd_bases = lsd_bases)
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
                             column_name = solidi,
                             lsd_bases = c(20, 12)) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  bases_check(lsd_bases)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_s(!! l, !! s, !! d, lsd_bases))
}

#' Mutate decimal shillings into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a decimalized shillings variable.
#' Thus, the function converts decimalized shillings into the lsd system of
#' pounds, shillings, and pence.
#'
#' @param df A data frame that contains a column of decimalized shillings.
#' @param solidi Decimalized shillings column: Unquoted name of a numeric
#'   variable corresponding to shillings. This is the variable that will be
#'   mutated into pounds, shillings, and pence variables.
#' @inheritParams deb_l_lsd_mutate
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a decimalized shillings variable
#' example <- data.frame(shillings = c(166, -166, 166.5, 236.35, -354.845))
#'
#' deb_s_lsd_mutate(df = example, solidi = shillings)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_s_lsd_mutate(solidi = shillings,
#'                    l_column = librae,
#'                    s_column = solidi,
#'                    d_column = denarii)
#'
#' @export

deb_s_lsd_mutate <- function(df, solidi,
                             l_column = l,
                             s_column = s,
                             d_column = d,
                             lsd_bases = c(20, 12),
                             suffix = ".1") {
  solidi <- rlang::enquo(solidi)

  # Check that solidi exists in df
  if (rlang::quo_name(solidi) %in% names(df) == FALSE) {
    stop(call. = FALSE, "solidi column must exist in df")
  }

  # Check that solidi is numeric
  if (!is.numeric(rlang::eval_tidy(solidi, df))) {
    stop(call. = FALSE, "solidi must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  lsd_mutate_columns(df,
                     l = rep(0, length(!! solidi)),
                     s = !! solidi,
                     d = rep(0, length(!! solidi)),
                     lsd_names = lsd_names,
                     replace = FALSE,
                     lsd_bases = lsd_bases)
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
                             column_name = denarii,
                             lsd_bases = c(20, 12)) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  bases_check(lsd_bases)

  column_name <- rlang::enquo(column_name)
  column_name <- rlang::quo_name(column_name)

  dplyr::mutate(df, !! column_name := decimalize_d(!! l, !! s, !! d, lsd_bases))
}

#' Mutate decimal pence into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a pence variable. Thus, the
#' function converts pence into the lsd system of pounds, shillings,
#' and pence.
#'
#' @param df A data frame that contains a column of pence.
#' @param denarii Pence column: Unquoted name of a numeric variable
#'   corresponding to pence. This is the variable that will be
#'   mutated into pounds, shillings, and pence variables.
#' @inheritParams deb_l_lsd_mutate
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a pence variable
#' example <- data.frame(pence = c(1998, -1998, 387, -5378))
#'
#' deb_d_lsd_mutate(df = example, denarii = pence)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_d_lsd_mutate(denarii = pence,
#'                    l_column = librae,
#'                    s_column = solidi,
#'                    d_column = denarii)
#'
#' @export

deb_d_lsd_mutate <- function(df, denarii,
                             l_column = l,
                             s_column = s,
                             d_column = d,
                             lsd_bases = c(20, 12),
                             suffix = ".1") {
  denarii <- rlang::enquo(denarii)

  # Check that denarii exists in df
  if (rlang::quo_name(denarii) %in% names(df) == FALSE) {
    stop(call. = FALSE, "denarii column must exist in df")
  }

  # Check that denarii is numeric
  if (!is.numeric(rlang::eval_tidy(denarii, df))) {
    stop(call. = FALSE, "denarii must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  lsd_mutate_columns(df,
                     l = rep(0, length(!! denarii)),
                     s = rep(0, length(!! denarii)),
                     d = !! denarii,
                     lsd_names = lsd_names,
                     replace = FALSE,
                     lsd_bases = lsd_bases)
}
