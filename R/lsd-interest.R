## lsd interest calculations ##

#' Calculation of the interest of pounds, shillings, and pence
#'
#' Calculate the interest of pounds, shillings, and pence
#' given an interest rate and a duration. The function does not
#' calculate compound interest.
#'
#' @inheritParams deb_normalize
#' @param interest Interest rate in decimal form. A numeric vector of length 1.
#'   Default is 0.0625, or 6.25 percent, which was default interest rate in the
#'   Low Countries at the end of the sixteenth century.
#' @param duration Duration over which the interest is calculated, or the
#'   number of times that the interest should be added. A numeric vector of
#'   length 1. Default is 1.
#' @param with_principal Logical (default `TRUE`): when `TRUE` the resulting
#'   pounds, shillings and pence value(s) will include the principal as well as
#'   the interest. When `FALSE` only the interest will be returned.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Calculate the interest with the principal over a certain duration
#' # If £10.14.5 were lent over a period of 5 years at 6.25%
#' deb_interest(lsd = c(10, 14, 5), duration = 5)
#'
#' # If £10.14.5 were lent over a period of 5 years at 8%
#' deb_interest(lsd = c(10, 14, 5), interest = 0.08, duration = 5)
#'
#' # Or you can calculate only the interest
#' deb_interest(lsd = c(10, 14, 5), duration = 5, with_principal = FALSE)
#'
#' # Interest of a list of lsd vectors at a single rate
#' # This returns a list of named lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#'
#' deb_interest(lsd = lsd_list, interest = 0.08, duration = 5)
#'
#' @export

deb_interest <- function(lsd,
                         interest = 0.0625,
                         duration = 1,
                         with_principal = TRUE,
                         bases = c(20, 12),
                         round = 5) {
  # vectorize
  if (is.list(lsd) == TRUE) {
    return(purrr::map(lsd, ~ deb_interest(.,
                                          interest = interest,
                                          duration = duration,
                                          with_principal = with_principal,
                                          bases = bases,
                                          round = round)))
  }

  interest_check(interest, duration, with_principal)

  if (with_principal == TRUE) {
    deb_normalize(lsd + lsd * interest * duration, bases = bases, round = round)
  } else {
    deb_normalize(lsd * interest * duration, bases = bases, round = round)
  }
}

#' Calculation of the interest of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to calculate the interest of pounds, shillings, and
#' pence variables in a data frame given an interest rate and a duration. The
#' interest — with or without the principal included — is returned in the form
#' of three new variables representing the calculated pounds, shillings and
#' pence for the interest. The function does not calculate compound interest.
#'
#' @inheritParams deb_normalize_df
#' @inheritParams deb_interest
#' @param suffix Suffix added to the column names for the pounds, shillings,
#'   and pence columns representing the interest to distinguish them from the
#'   original pounds, shillings, and pence columns. Default is ".interest".
#'   Must be a character vector of length 1.
#' @param replace Logical (default `FALSE`): when `TRUE` the new pounds,
#'   shillings, and pence variables will replace the original ones.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the calculated interest.
#'
#' @examples
#' # Calculate the interest of a set of values in a data frame
#' example <- data.frame(l = c(3, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_interest_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     duration = 5)
#'
#' # Calculate the interest without the principal
#' example %>%
#'   deb_interest_mutate(l = l, s = s, d = d,
#'                       duration = 5,
#'                       with_principal = FALSE)
#'
#' # Change the suffix for the new columns from the default
#' deb_interest_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     duration = 5,
#'                     suffix = "_5years")
#'
#' # Replace the existing pounds, shillings, and pence
#' # with the interest values through replace argument
#' deb_interest_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     duration = 5,
#'                     replace = TRUE)
#'
#' @export

deb_interest_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                interest = 0.0625,
                                duration = 1,
                                with_principal = TRUE,
                                bases = c(20, 12),
                                round = 5,
                                replace = FALSE,
                                suffix = ".interest") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  interest_check(interest, duration, with_principal)
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  # Multiply interest by duration to get value by which
  # to multiply l, s, and d
  x <- interest * duration

  if (with_principal == TRUE) {
    lsd_mutate_columns(df,
                       l = (!! l * x) + !! l,
                       s = (!! s * x) + !! s,
                       d = (!! d * x) + !! d,
                       lsd_names = lsd_names,
                       replace = replace,
                       bases = bases,
                       round = round)
  } else {
    lsd_mutate_columns(df,
                       l = !! l * x,
                       s = !! s * x,
                       d = !! d * x,
                       lsd_names = lsd_names,
                       replace = replace,
                       bases = bases,
                       round = round)
  }
}
