## lsd decimalization mutate ##

### From lsd to decimalized l, s, and d in a data frame ###

## Helper functions ##
decimalize_l <- function(l, s, d, bases = c(20, 12)) {
  l + s / bases[1] + d / prod(bases)
}

decimalize_s <- function(l, s, d, bases = c(20, 12)) {
  l * bases[1] + s + d / bases[2]
}

decimalize_d <- function(l, s, d, bases = c(20, 12), round = 5) {
  round(l * prod(bases) + s * bases[2] + d, round)
}

#' Conversion of pounds, shillings and pence to pounds
#'
#' Convert pounds, shillings, and pence variables in a data frame to a
#' decimalized pounds variable.
#'
#' @inheritParams deb_normalize_df
#' @param column_name Unquoted name of the column for the decimalized pounds.
#'   Default is pounds.
#' @param suffix Suffix added to `column_name` if `df` already contains a
#'   variable with the same name. Default is ".decimal". Must be a character
#'   vector of length 1.
#'
#' @return Returns a data frame with a new decimalized pounds variable.
#'
#' @examples
#' # Create a new decimalized pounds variable
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_lsd_l_mutate(df = example, l = l, s = s, d = d)
#'
#' # Change the name of the decimalized pounds variable
#' deb_lsd_l_mutate(df = example,
#'                  l = l, s = s, d = d,
#'                  column_name = librae)
#'
#' @export

deb_lsd_l_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = pounds,
                             bases = c(20, 12),
                             suffix = ".decimal") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  bases_check(bases)
  
  # Ensure that new column does not overwrite one of lsd columns
  suffix <- suffix_check(suffix)
  column_name <- decimal_columns(df, rlang::enquo(column_name), suffix)

  dplyr::mutate(df, !! column_name := decimalize_l(!! l, !! s, !! d, bases))
}

#' Conversion of decimalized pounds into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to convert a decimalized pounds variable into the lsd
#' system of pounds, shillings, and pence. The values are returned in the form
#' of three new variables representing the equivalent pounds, shillings and
#' pence.
#'
#' @param df A data frame that contains a column with decimalized pounds.
#' @param pounds Decimalized pounds column: Unquoted name of a numeric variable
#'   corresponding to pounds. The variable that will be mutated into pounds,
#'   shillings, and pence variables.
#' @param l_column An unquoted name for the pounds column created by the
#'   function. Default is l.
#' @param s_column An unquoted name for the shillings column created by
#'   the function. Default is s.
#' @param d_column An unquoted name for the pence column created by the
#'   function. Default is d.
#' @inheritParams deb_normalize_df
#' @param suffix Suffix added to `l_column`, `s_column`, or `d_column` if `df`
#'   already contains variables with the same names. Default is ".1". Must be a
#'   character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds,
#'   shillings, and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a decimalized pounds variable
#' example <- data.frame(pounds = c(8, 8.325, -8.325, 5.425, 4.5678))
#' deb_l_lsd_mutate(df = example, pounds = pounds)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_l_lsd_mutate(pounds = pounds,
#'                    l_column = librae,
#'                    s_column = solidi,
#'                    d_column = denarii)
#'
#' @export

deb_l_lsd_mutate <- function(df, pounds,
                             l_column = l,
                             s_column = s,
                             d_column = d,
                             bases = c(20, 12),
                             round = 5,
                             suffix = ".1") {

  pounds <- rlang::enquo(pounds)

  # Check that pounds exists in df
  if (rlang::quo_name(pounds) %in% names(df) == FALSE) {
    stop(call. = FALSE, "pounds column must exist in df")
  }

  # Check that pounds is numeric
  if (!is.numeric(rlang::eval_tidy(pounds, df))) {
    stop(call. = FALSE, "pounds must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  lsd_mutate_columns(df,
                     l = !! pounds,
                     s = rep(0, length(!! pounds)),
                     d = rep(0, length(!! pounds)),
                     lsd_names = lsd_names,
                     replace = FALSE,
                     bases = bases,
                     round = round)
}

#' Conversion of pounds, shillings and pence to shillings
#'
#' Convert pounds, shillings, and pence variables in a data frame to a
#' decimalized shillings variable.
#'
#' @inheritParams deb_normalize_df
#' @inheritParams deb_lsd_l_mutate
#' @param column_name Unquoted name of the column for the decimalized
#'   shillings. Default is shillings.
#'
#' @return Returns a data frame with a new decimalized shillings variable.
#'
#' @examples
#' # Create a new decimalized shillings variable
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_lsd_s_mutate(df = example, l = l, s = s, d = d)
#'
#' # Change the name of the decimalized shillings variable
#' deb_lsd_s_mutate(df = example,
#'                  l = l, s = s, d = d,
#'                  column_name = solidi)
#'
#' @export

deb_lsd_s_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = shillings,
                             bases = c(20, 12),
                             suffix = ".decimal") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  bases_check(bases)
  
  # Ensure that new column does not overwrite one of lsd columns
  suffix <- suffix_check(suffix)
  column_name <- decimal_columns(df, rlang::enquo(column_name), suffix)

  dplyr::mutate(df, !! column_name := decimalize_s(!! l, !! s, !! d, bases))
}

#' Conversion of decimalized shillings into pounds, shillings, and pence
#' variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a decimalized shillings variable.
#' Thus, the function converts decimalized shillings into the lsd system of
#' pounds, shillings, and pence.
#'
#' @param df A data frame that contains a column of decimalized shillings.
#' @param shillings Decimalized shillings column: Unquoted name of a numeric
#'   variable corresponding to shillings. The variable that will be mutated
#'   into pounds, shillings, and pence variables.
#' @inheritParams deb_l_lsd_mutate
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a decimalized shillings variable
#' example <- data.frame(shillings = c(166, -166, 166.5, 236.35, -354.845))
#' deb_s_lsd_mutate(df = example, shillings = shillings)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_s_lsd_mutate(shillings = shillings,
#'                    l_column = librae,
#'                    s_column = solidi,
#'                    d_column = denarii)
#'
#' @export

deb_s_lsd_mutate <- function(df, shillings,
                             l_column = l,
                             s_column = s,
                             d_column = d,
                             bases = c(20, 12),
                             round = 5,
                             suffix = ".1") {
  shillings <- rlang::enquo(shillings)

  # Check that shillings exists in df
  if (rlang::quo_name(shillings) %in% names(df) == FALSE) {
    stop(call. = FALSE, "shillings column must exist in df")
  }

  # Check that shillings is numeric
  if (!is.numeric(rlang::eval_tidy(shillings, df))) {
    stop(call. = FALSE, "shillings must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  lsd_mutate_columns(df,
                     l = rep(0, length(!! shillings)),
                     s = !! shillings,
                     d = rep(0, length(!! shillings)),
                     lsd_names = lsd_names,
                     replace = FALSE,
                     bases = bases,
                     round = round)
}

#' Conversion of pounds, shillings and pence to pence
#'
#' Convert pounds, shillings, and pence variables in a data frame to a
#' decimalized pence variable.
#'
#' @inheritParams deb_normalize_df
#' @inheritParams deb_lsd_l_mutate
#' @param column_name Unquoted name of the column for the decimalized
#'   pence. Default is pence.
#'
#' @return Returns a data frame with a new decimalized pence variable.
#'
#' @examples
#' # Create a new decimalized pence variable
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_lsd_d_mutate(df = example, l = l, s = s, d = d)
#'
#' # Change the name of the decimalized pence variable
#' deb_lsd_d_mutate(df = example,
#'                  l = l, s = s, d = d,
#'                  column_name = denarii)
#'
#' @export

deb_lsd_d_mutate <- function(df,
                             l = l, s = s, d = d,
                             column_name = pence,
                             bases = c(20, 12),
                             round = 5,
                             suffix = ".decimal") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  lsd_column_check(df, l, s, d)
  bases_check(bases)
  round_check(round)
  
  # Ensure that new column does not overwrite one of lsd columns
  suffix <- suffix_check(suffix)
  column_name <- decimal_columns(df, rlang::enquo(column_name), suffix)

  dplyr::mutate(df, !! column_name := decimalize_d(!! l, !! s, !! d, bases, round))
}

#' Conversion of decimalized pence into pounds, shillings, and pence variables
#'
#' Uses [dplyr::mutate()] to add equivalent pounds, shillings, and pence
#' variables to a data frame that contains a pence variable. Thus, the
#' function converts pence into the lsd system of pounds, shillings,
#' and pence.
#'
#' @param df A data frame that contains a column of pence.
#' @param pence Decimalized pence column: Unquoted name of a numeric variable
#'   corresponding to pence The variable that will be mutated into pounds,
#'   shillings, and pence variables.
#' @inheritParams deb_l_lsd_mutate
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Create equivalent pounds, shillings, and pence
#' # variables from a pence variable
#' example <- data.frame(pence = c(1998, -1998, 387, -5378))
#' deb_d_lsd_mutate(df = example, pence = pence)
#'
#' # You can choose the names for the created columns
#' example %>%
#'   deb_d_lsd_mutate(pence = pence,
#'                    l_column = librae,
#'                    s_column = solidi,
#'                    d_column = pence)
#'
#' @export

deb_d_lsd_mutate <- function(df, pence,
                             l_column = l,
                             s_column = s,
                             d_column = d,
                             bases = c(20, 12),
                             round = 5,
                             suffix = ".1") {
  pence <- rlang::enquo(pence)

  # Check that pence exists in df
  if (rlang::quo_name(pence) %in% names(df) == FALSE) {
    stop(call. = FALSE, "pence column must exist in df")
  }

  # Check that pence is numeric
  if (!is.numeric(rlang::eval_tidy(pence, df))) {
    stop(call. = FALSE, "pence must be numeric")
  }

  # Column names: avoid overwriting l, s, and d columns
  suffix <- suffix_check(suffix)
  lsd_names <- lsd_column_names(df,
                                rlang::enquo(l_column),
                                rlang::enquo(s_column),
                                rlang::enquo(d_column),
                                suffix)

  lsd_mutate_columns(df,
                     l = rep(0, length(!! pence)),
                     s = rep(0, length(!! pence)),
                     d = !! pence,
                     lsd_names = lsd_names,
                     replace = FALSE,
                     bases = bases,
                     round = round)
}
