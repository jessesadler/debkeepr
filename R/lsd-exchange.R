## lsd conversions ##

#' Convert between pounds, shillings and pence currencies
#'
#' Convert between different currencies that are in the form of pounds,
#' shillings, and pence given an exchange rate calculated on the basis of
#' shillings.
#'
#' `deb_exchange()` is a wrapper around [deb_multiply()].
#'
#' @inheritParams deb_normalize
#' @param rate_per_solidi The exchange rate. This follows the common practice
#'   of calculating the exchange rate between different currencies in terms
#'   of shillings. Thus, in terms of a given number over 20. A numeric vector
#'   of length 1.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the input lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Exchange at the rate of 31 shillings
#' deb_exchange(lsd = c(850, 16, 5), rate_per_solidi = 31)
#'
#' # If the exchange rate is in shillings and pence, you can either
#' # decimalize the shillings or add the pence and divide by 12 in
#' # the rate_per_solidi argument. If the decimalized shillings has
#' # a repeating decimal, the latter approach is preferable.
#'
#' # Exchange at the rate of 31 shillings 4 pence
#' deb_exchange(lsd = c(850, 16, 5), rate_per_solidi = 31.3333)
#' deb_exchange(lsd = c(850, 16, 5), rate_per_solidi = 31 + 4/12)
#'
#' # Exchange of a list of lsd vectors at a single rate
#' # This returns a list of named lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#'
#' deb_exchange(lsd = lsd_list, rate_per_solidi = 31)
#'
#' @export

deb_exchange <- function(lsd,
                         rate_per_solidi,
                         round = 3) {
  # Check exchange rate
  exchange_rate_check(rate_per_solidi)

  deb_multiply(lsd,
               x = rate_per_solidi/20,
               round = round)
}

# Helper function to go from lsd to lsd when l = 0
normalized_to_sd <- function(lsd) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map(lsd, normalized_to_sd))
  }

  lsd[2] <- lsd[1] * 20 + lsd[2]
  lsd[1] <- 0
  lsd
}

#' Conversion rate per shillings
#'
#' Calculate the exchange rate between two sets of values in the form of
#' pounds, shillings, and pence. The rate is returned in the form of shillings
#' and pence, which follows the contemporary practice and the form used in the
#' `rate_per_solidi` argument in [deb_exchange()] and [deb_exchange_mutate()].
#'
#' If `lsd1` and `lsd2` are lists of different lengths or one is a vector,
#' the shorter list will be recycled.
#'
#' @inheritParams deb_normalize
#' @param lsd1,lsd2 Numeric vectors of length 3 or lists of numeric vectors of
#'   length 3. The first position of the vectors represents the pounds value or
#'   l. The second position represents the shillings value or s. And the third
#'   position represents the pence value or d. `lsd2` is the value (or values)
#'   that is reduced to £1 and against which `lsd1` is compared. Thus, `lsd2`
#'   = £1 and `lsd1` = the returned value.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. The rate is returned in the form of shillings and pence, which
#'   follows the contemporary practice and the form used in the
#'   `rate_per_solidi` argument in [deb_exchange()] and
#'   [deb_exchange_mutate()]. If `lsd1` > `lsd2` the returned vector will have
#'   a shillings value greater than 20.
#'
#' @examples
#' # Find the exchange rate if £166 13s 4d in one currency is
#' # equivalent to £100 0s 0d in another currency
#' deb_rate_per_shilling(lsd1 = c(166, 13, 4), lsd2 = c(100, 0, 0))
#'
#' # The rate returned is in the non-normalized form of shillings and pence
#' # Can normalize the returned value by piping to `deb_normalize()`.
#' deb_rate_per_shilling(lsd1 = c(166, 13, 4), lsd2 = c(100, 0, 0)) %>%
#'   deb_normalize()
#'
#' # To find the exchange rate for multiple currencies
#' # use a list of lsd vectors for `lsd1` or `lsd2`
#' list_a <- list(c(150, 0, 0), c(125, 0, 0), c(175, 13, 4))
#' list_b <- list(c(125, 0, 0), c(100, 8, 4), c(100, 0, 0))
#'
#' deb_rate_per_shilling(lsd1 = list_a, lsd2 = list_b)
#'
#' # Or find the exchange rate of multiple currencies to a
#' # single currency by using a vector for `lsd2`
#'
#' deb_rate_per_shilling(lsd1 = list_a, lsd2 = c(100, 0, 0))
#'
#' @export

deb_rate_per_shilling <- function(lsd1, lsd2, round = 3) {
  rate <- deb_lsd_l(lsd1) / deb_lsd_l(lsd2)
  normalized <- deb_l_lsd(rate, round)

  normalized_to_sd(normalized)
}

#' Convert between pounds, shillings and pence currencies in a data frame
#'
#' Uses [dplyr::mutate()] to convert between different currencies that are in
#' the form of pounds, shillings, and pence variables in a data frame given
#' an exchange rate calculated on the basis of shillings. The converted values
#' are returned in the form of three new variables representing the calculated
#' pounds, shillings and pence for the interest.
#'
#' @inheritParams deb_multiply_mutate
#' @inheritParams deb_exchange
#' @param suffix Suffix added to the column names for the pounds, shillings, and pence
#'   columns representing the converted currency so that they are distinguished
#'   from the pounds, shillings, and pence columns of the original currency.
#'   Default is ".exchange". Should be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the converted currency.
#'
#' @examples
#' # Exchange rate of 31 shillings
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_exchange_mutate(example, l, s, d,
#'                     rate_per_solidi = 31)
#'
#' # If the exchange rate is in shillings and pence, you can either
#' # decimalize the shillings or add the pence and divide by 12 in
#' # the rate_per_solidi argument. If the decimalized shillings has
#' # a repeating decimal, the latter approach is preferable.
#'
#' # Exchange at the rate of 31 shillings 4 pence
#' deb_exchange_mutate(example, l, s, d,
#'                     rate_per_solidi = 31.3333)
#' deb_exchange_mutate(example, l, s, d,
#'                     rate_per_solidi = 31 + 4/12)
#'
#' # Replace the existing pounds, shillings, and pence
#' # with the converted values through replace argument
#' deb_exchange_mutate(example, l, s, d,
#'                     rate_per_solidi = 31,
#'                     replace = TRUE)
#'
#' @export

deb_exchange_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                rate_per_solidi,
                                round = 3,
                                replace = FALSE,
                                suffix = ".exchange") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  exchange_rate_check(rate_per_solidi)
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  x <- rate_per_solidi/20

  lsd_mutate_columns(df,
                     !! l * x,
                     !! s * x,
                     !! d * x,
                     lsd_names,
                     replace,
                     round)
}
