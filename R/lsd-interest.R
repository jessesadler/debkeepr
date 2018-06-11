## lsd interest calculations ##

#' Calculate the interest of pounds, shillings, and pence
#'
#' Calculate the interest of pounds, shillings, and pence
#' given an interest rate and a duration. The function does not
#' calculate compound interest.
#'
#' @inheritParams deb_normalize
#' @param interest Interest rate in decimal form. A numeric vector
#'   of length 1. Default is 0.0625, or 6.25 percent, which was default
#'   interest rate in the Low Countries at the end of the sixteenth century.
#' @param duration Duration over which the interest is calculated, or
#'   number of times that the interest should be added. A numeric vector
#'   of length 1. Default is 1.
#' @param with_principal Logical (default `TRUE`: when `TRUE` the resulting
#'   pounds, shillings and pence will include the interest charged as well
#'   as the principal. When `FALSE` only the interest will be shown.
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. The number of rows in the resulting
#'   tibble will be equal to the length of the input vectors. If the length of
#'   `l`, `s`, and `d` is greater than 1 and `vector = TRUE`, the result will
#'   be a list of named vectors of length equal to the input vectors.
#'
#' @examples
#' # Calculate the interest with the principal over a certain duration
#' # If £10.14.5 were lent over a period of 5 years
#' deb_interest(l = 10, s = 14, d = 5, duration = 5)
#' deb_interest(l = 10, s = 14, d = 5, duration = 5, vector = TRUE)
#'
#' # Or you can calculate only the interest
#' deb_interest(l = 10, s = 14, d = 5, duration = 5, with_principal = FALSE)
#'
#' # l, s, and d can be vectors of length > 1
#' # Return a tibble with two rows
#' deb_interest(l = c(8, 10), s = c(15, 6), d = c(4, 9))
#'
#' # Return a list with two vectors
#' deb_interest(l = c(8, 10), s = c(15, 6), d = c(4, 9), vector = TRUE)
#'
#' # Use the round argument to return pence with the desired accuracy
#' deb_interest(l = 10, s = 14, d = 5, duration = 5, round = 0)
#'
#' @export

deb_interest <- function(l, s, d,
                         interest = 0.0625,
                         duration = 1,
                         with_principal = TRUE,
                         round = 3,
                         vector = FALSE) {
  # Checks
  lsd_check(l, s, d, round, vector)
  interest_check(interest, duration, with_principal)

  principle_d <- deb_lsd_d(l, s, d)

  interest_d <-  principle_d * interest * duration

  if (with_principal == TRUE) {
    deb_d_lsd(principle_d + interest_d, round, vector)
  } else {
    deb_d_lsd(interest_d, round, vector)
  }
}

#' Calculate the interest of pounds, shillings, and pence
#'
#' Uses [dplyr::mutate()] to calculate the interest of pounds,
#' shillings, and pence variables given an interest rate and a duration.
#' The interest—with or without the principal included—is returned in the
#' form of three new variables representing the calculated pounds, shillings
#' and pence for the interest. The function does not calculate
#' compound interest.
#'
#' @inheritParams deb_sum
#' @inheritParams deb_interest
#' @param suffix Suffix added to the column names for the pounds,
#'   shillings, and pence columns representing the interest so that
#'   they are distinguished from the pounds, shillings, and pence
#'   columns of the principal used to make the calculation. Default is
#'   ".interest". Should be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the calculated interest.
#'
#' @examples
#' # Calculate the interest of a set of values in a data frame
#' example <- tibble::tibble(l = c(3, 10, 26, 12),
#'                           s = c(10, 18, 11, 16),
#'                           d = c(9, 11, 10, 5))
#' deb_interest_mutate(example, l, s, d,
#'                     duration = 5)
#'
#' # Calculate the interest without the principal
#' example %>%
#'   deb_interest_mutate(l, s, d,
#'                       duration = 5,
#'                       with_principal = FALSE)
#'
#' # Change the suffix for the new columns from the default
#' # and round the pence to be whole numbers.
#' deb_interest_mutate(example,
#'                     l, s, d,
#'                     duration = 5,
#'                     suffix = "_5years",
#'                     round = 0)
#'
#' @export

deb_interest_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                interest = 0.0625,
                                duration = 1,
                                with_principal = TRUE,
                                suffix = ".interest",
                                round = 3) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  lsd_column_check(df, l, s, d)
  # Column names: avoid overwriting l, s, and d columns
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  interest_check(interest, duration, with_principal)

  # Multiply interest by duration to get value by which
  # to multiply l, s, and d
  x <- interest * duration

  if (with_principal == TRUE) {
    lsd_mutate_columns(df,
                       (!! l * x) + !! l,
                       (!! s * x) + !! s,
                       (!! d * x) + !! d,
                       lsd_names,
                       round = round)
  } else {
    lsd_mutate_columns(df,
                       !! l * x,
                       !! s * x,
                       !! d * x,
                       lsd_names,
                       round = round)
  }
}
