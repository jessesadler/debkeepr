## lsd exchanges by solidi ##

#' Exchange between pounds, shillings and pence currencies
#'
#' Exchange between pounds, shillings, and pence currencies that share the
#' same bases for shillings and pence given an exchange rate calculated on
#' the basis of shillings.
#'
#' `deb_exchange()` is a wrapper around [deb_multiply()]. It uses shillings
#' as the basis for the conversion between the currencies. If the conversion
#' between currencies is presented in pounds or pence, it may be easier to
#' use [deb_multiply()]. If the bases for the shillings and pence units differ
#' between the two currencies use [deb_convert_bases()].
#'
#' @inheritParams deb_normalize
#' @param shillings_rate The exchange rate. This follows the common practice
#'   of calculating the exchange rate between different currencies in terms
#'   of shillings. Thus, in terms of a given number over the base of shillings
#'   provided in the `bases` argument. A numeric vector of length 1.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Exchange at the rate of 31 shillings
#' deb_exchange(lsd = c(850, 16, 5), shillings_rate = 31)
#'
#' # If the exchange rate is in shillings and pence, you can either
#' # decimalize the shillings or add the pence and divide by 12 in
#' # the shillings_rate argument. If the decimalized shillings has
#' # a repeating decimal, the latter approach is preferable.
#'
#' # Exchange at the rate of 31 shillings 4 pence
#' deb_exchange(lsd = c(850, 16, 5), shillings_rate = 31.33333)
#' deb_exchange(lsd = c(850, 16, 5), shillings_rate = 31 + 4/12)
#'
#' # Exchange of a list of lsd vectors at a single rate
#' # This returns a list of named lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#'
#' deb_exchange(lsd = lsd_list, shillings_rate = 31)
#'
#' @export

deb_exchange <- function(lsd,
                         shillings_rate,
                         bases = c(20, 12),
                         round = 5) {
  # Check exchange rate
  shillings_check(shillings_rate)

  shillings_rate <- shillings_rate / bases[1]

  deb_multiply(lsd,
               x = shillings_rate,
               bases = bases,
               round = round)
}

# Helper function to go from lsd to lsd when l = 0
normalized_to_sd <- function(lsd, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map(lsd, ~ normalized_to_sd(., bases)))
  }

  lsd[2] <- lsd[1] * bases[1] + lsd[2]
  lsd[1] <- 0
  lsd
}

normalized_to_d <- function(lsd, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map(lsd, ~ normalized_to_sd(., bases)))
  }

  lsd[3] <- round(lsd[1] * prod(bases) + lsd[2] * bases[2] + lsd[3], 5)
  lsd[1] <- 0
  lsd[2] <- 0
  lsd
}

#' Exchange rate between pounds, shillings, and pence currencies
#'
#' Calculate the exchange rate between two sets of values in the form of
#' pounds, shillings, and pence. The rate is returned in the form of either
#' pounds, shillings, and pence; shillings and pence; or just pence. `lsd1`
#' and `lsd2` must have the same bases for the shillings and pence units.
#'
#' If `lsd1` and `lsd2` are lists of different lengths or one is a vector,
#' the shorter list will be recycled.
#'
#' @inheritParams deb_normalize
#' @param lsd1 Pounds, shillings, and pence value that is reduced to £1 and
#'   against which `lsd2` is compared. Thus, `lsd1` is the "fixed currency" and
#'   `lsd2` is the "variable currency". `lsd1` is a numeric vector of length 3
#'   or list of numeric vectors of length 3. The first position of the vectors
#'   represents the pounds value or l. The second position represents the
#'   shillings value or s. And the third position represents the pence value
#'   or d.
#' @param lsd2 Pounds, shillings, and pence value that is compared to `lsd1`.
#'   When `lsd1` equals £1, `lsd2` equals the returned value. Thus, `lsd1` is
#'   the "fixed currency" and `lsd2` is the "variable currency". `lsd2` is a
#'   numeric vector of length 3 or list of numeric vectors of length 3. The
#'   first position of the vectors represents the pounds value or l. The second
#'   position represents the shillings value or s. And the third position
#'   represents the pence value or d.
#' @param output Choice of either `"shillings"`, `"pence"`, or `"pounds"` for
#'   the format in which the exchange rate will be returned. `"shillings"`,
#'   the default, returns the exchange rate in terms of shillings and pence.
#'   `"pence"` returns the exchange rate in terms of pence. `"pounds"` returns
#'   the exchange rate in terms of pounds, shillings, and pence.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. The format of the returned value is determined by the `output`
#'   argument. If `lsd1` > `lsd2`, the returned value will be greater that £1.
#'
#' @examples
#' # Find the exchange rate if £166 13s 4d in one currency is
#' # equivalent to £100 0s 0d in another currency in terms of shillings
#' deb_exchange_rate(lsd1 = c(166, 13, 4), lsd2 = c(100, 0, 0))
#'
#' # Exchange rate for the opposite direction
#' deb_exchange_rate(lsd1 = c(100, 0, 0), lsd2 = c(166, 13, 4))
#'
#' # Exchange rate in terms of pence
#' deb_exchange_rate(lsd1 = c(125, 0, 0),
#'                   lsd2 = c(46, 17, 6),
#'                   output = "pence")
#'
#' # Exchange rate in terms of pounds, shillings, and pence
#' deb_exchange_rate(lsd1 = c(100, 0, 0),
#'                   lsd2 = c(166, 13, 4),
#'                   output = "pounds")
#'
#' # To find decimalized ratio between currencies use
#' # deb_exchange_rate() with deb_lsd_l()
#' deb_exchange_rate(lsd1 = c(100, 0, 0), lsd2 = c(166, 13, 4)) %>%
#'   deb_lsd_l()
#'
#' # To find the exchange rate for multiple currencies
#' # use a list of lsd vectors for `lsd1` or `lsd2`
#' list_a <- list(c(150, 0, 0), c(125, 0, 0), c(175, 13, 4))
#' list_b <- list(c(125, 0, 0), c(75, 8, 4), c(100, 10, 0))
#'
#' deb_exchange_rate(lsd1 = list_a, lsd2 = list_b)
#'
#' # Or find the exchange rate of multiple currencies to a
#' # single currency by using a vector for `lsd2`
#' deb_exchange_rate(lsd1 = list_a, lsd2 = c(100, 0, 0))
#'
#' @export

deb_exchange_rate <- function(lsd1, lsd2,
                              output = c("shillings", "pence", "pounds"),
                              bases = c(20, 12),
                              round = 5) {
  output <- rlang::arg_match(output)

  rate <- deb_lsd_l(lsd2, bases = bases) / deb_lsd_l(lsd1, bases = bases)
  normalized <- deb_l_lsd(rate, bases = bases, round = round)

  res <- dplyr::case_when(output == "shillings" ~ normalized_to_sd(normalized, bases),
                          output == "pence" ~ normalized_to_d(normalized, bases),
                          output == "pounds" ~ normalized)

  if (is.list(res) == TRUE) {
    purrr::map(res, ~ stats::setNames(., c("l", "s", "d")))
  } else {
    stats::setNames(res, c("l", "s", "d"))
  }
}

#' Calculation of the inverse of an exchange rate
#'
#' Given an exchange rate between two currencies, calculate the inverse rate,
#' or the rate in the opposite direction.
#'
#' @inheritParams deb_exchange_rate
#' @param exchange_rate Numeric vector of length 3 or list of numeric vectors
#'   of length 3 representing the exchange rate to be inverted. The first
#'   position of the vector represents the pounds value or l. The second
#'   position represents the shillings value or s. And the third position
#'   represents the pence value or d. The values do not need to be normalized.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. The format of the returned value is determined by the `output`
#'   argument, either pounds, shillings, and pence; shillings and pence; or
#'   just pence.
#'
#' @examples
#' # Find the inverse exchange rate of 33s. 4d.
#' # Flemish pounds per pound sterling
#' deb_invert_rate(exchange_rate = c(0, 33, 4))
#'
#' # Inverse of an exchange rate of £2 13s. 4d. in pence
#' deb_invert_rate(exchange_rate = c(2, 13, 4),
#'                 output = "pence")
#'
#' # Find the inverse of multiple exchange rates
#' rates <- list(c(0, 33, 4), c(0, 31, 0), c(0, 30, 0))
#' deb_invert_rate(exchange_rate = rates)
#'
#' @export

deb_invert_rate <- function(exchange_rate,
                            output = c("shillings", "pence", "pounds"),
                            bases = c(20, 12),
                            round = 5) {
  exchange_rate_check(exchange_rate)
  output <- rlang::arg_match(output)

  converted <- 1 / deb_lsd_l(exchange_rate, bases = bases)
  normalized <- deb_l_lsd(converted, bases = bases, round = round)

  res <- dplyr::case_when(output == "shillings" ~ normalized_to_sd(normalized, bases),
                          output == "pence" ~ normalized_to_d(normalized, bases),
                          output == "pounds" ~ normalized)

  if (is.list(res) == TRUE) {
    purrr::map(res, ~ stats::setNames(., c("l", "s", "d")))
  } else {
    stats::setNames(res, c("l", "s", "d"))
  }
}

#' Exchange between pounds, shillings and pence currencies in a data frame
#'
#' Uses [dplyr::mutate()] to convert between different currencies that are in
#' the form of pounds, shillings, and pence variables in a data frame and share
#' the same bases for shillings and pence given an exchange rate calculated on
#' the basis of shillings. The converted values are returned in the form of
#' three new variables representing the calculated pounds, shillings and pence
#' for the new currency.
#'
#' `deb_exchange_mutate()` uses shillings as the basis for the conversion
#' between the currencies. If the conversion between currencies is presented
#' in pounds or pence, it may be easier to use [deb_multiply_mutate()]. If the
#' bases for the shillings and pence units differ between the two currencies
#' use [deb_convert_bases_mutate()].
#'
#' @inheritParams deb_multiply_mutate
#' @inheritParams deb_exchange
#' @param suffix Suffix added to the column names for the pounds, shillings,
#'   and pence columns representing the converted currency to distinguish them
#'   from the original currency. Default is ".exchange". Must be a character
#'   vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the converted currency.
#'
#' @examples
#' # Exchange rate of 31 shillings to the pound
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_exchange_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     shillings_rate = 31)
#'
#' # If the exchange rate is in shillings and pence, you can either
#' # decimalize the shillings or add the pence and divide by 12 in
#' # the shillings_rate argument. If the decimalized shillings has
#' # a repeating decimal, the latter approach is preferable.
#'
#' # Exchange at the rate of 31 shillings 4 pence
#' deb_exchange_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     shillings_rate = 31.3333)
#' deb_exchange_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     shillings_rate = 31 + 4/12)
#'
#' # Replace the existing pounds, shillings, and pence
#' # with the converted values through replace argument
#' deb_exchange_mutate(df = example,
#'                     l = l, s = s, d = d,
#'                     shillings_rate = 31,
#'                     replace = TRUE)
#'
#' @export

deb_exchange_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                shillings_rate,
                                bases = c(20, 12),
                                round = 5,
                                replace = FALSE,
                                suffix = ".exchange") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  shillings_check(shillings_rate)
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  x <- shillings_rate / bases[1]

  lsd_mutate_columns(df,
                     !! l * x,
                     !! s * x,
                     !! d * x,
                     lsd_names = lsd_names,
                     replace = replace,
                     bases = bases,
                     round = round)
}
