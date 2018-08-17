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
#' @return Returns an object of class lsd with a bases attribute.
#'
#' @examples
#' # Exchange at the rate of 31 shillings
#' deb_exchange(lsd = c(850, 16, 10), shillings_rate = 31)
#'
#' # Exchange between currencies that use the same alternative bases
#' deb_exchange(lsd = c(850, 16, 10),
#'              shillings_rate = 31,
#'              bases = c(20, 16))
#'
#' # Exchange for an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(850, 16, 10), bases = c(20, 16))
#' deb_exchange(lsd = lsd, shillings_rate = 31)
#'
#' # If the exchange rate is in shillings and pence, you can either
#' # decimalize the shillings or add the pence and divide by 12 in
#' # the shillings_rate argument. If the decimalized shillings has
#' # a repeating decimal, the latter approach is preferable.
#'
#' # Exchange at the rate of 31 shillings 4 pence
#' deb_exchange(lsd = c(850, 16, 10), shillings_rate = 31.33333)
#' deb_exchange(lsd = c(850, 16, 10), shillings_rate = 31 + 4/12)
#'
#' # Exchange of a list of lsd values at a single rate
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#' deb_exchange(lsd = lsd_list, shillings_rate = 31)
#'
#' # Or an lsd object with alternative bases
#' lsd_list2 <- deb_as_lsd(lsd = lsd_list, bases = c(20, 16))
#' deb_exchange(lsd = lsd_list2, shillings_rate = 31)
#'
#' @export

deb_exchange <- function(lsd,
                         shillings_rate,
                         bases = c(20, 12),
                         round = 5) {
  # Check exchange rate
  shillings_check(shillings_rate)
  bases <- validate_bases(lsd, bases)
  shillings_rate <- shillings_rate / bases[1]

  deb_multiply(lsd,
               x = shillings_rate,
               bases = bases,
               round = round)
}

# Helper function to go from lsd to lsd when l = 0
normalized_to_sd <- function(lsd, bases) {
  if (is.list(lsd) == TRUE) {
    ret <- purrr::map(lsd, ~ normalized_to_sd(., bases))
    return(to_lsd(ret, bases))
  }

  lsd[2] <- lsd[1] * bases[1] + lsd[2]
  lsd[1] <- 0
  lsd
}

normalized_to_d <- function(lsd, bases) {
  if (is.list(lsd) == TRUE) {
    ret <- purrr::map(lsd, ~ normalized_to_d(., bases))
    return(to_lsd(ret, bases))
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
#' @inheritParams deb_add
#' @param lsd1,lsd2 Pounds, shillings, and pence value that is reduced to £1
#'   and against which `lsd2` is compared. Thus, `lsd1` is the "fixed currency"
#'   and `lsd2` is the "variable currency". Objects of class lsd or objects
#'   that can be coerced to class lsd: numeric vectors of length 3 or lists of
#'   such vectors.
#' @param output Choice of either `"shillings"`, `"pence"`, or `"pounds"` for
#'   the format in which the exchange rate will be returned. `"shillings"`,
#'   the default, returns the exchange rate in terms of shillings and pence.
#'   `"pence"` returns the exchange rate in terms of pence. `"pounds"` returns
#'   the exchange rate in terms of pounds, shillings, and pence.
#'
#' @return Returns an object of class lsd with a bases attribute. The format
#'   of the returned value is determined by the `output` argument. If `lsd1` >
#'   `lsd2`, the returned value will be less than £1.
#'
#' @examples
#' # Find the exchange rate if £166 13s. 4d. in one currency is
#' # equivalent to £100 0s. 0d, in another currency in terms of shillings
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
#' # Exchange rate between currencies that use the same alternative bases
#' deb_exchange_rate(lsd1 = c(100, 0, 0),
#'                   lsd2 = c(166, 13, 4),
#'                   bases = c(20, 16))
#'
#' # Exchange for an object of class lsd will use the bases attribute
#' lsd_100 <- deb_as_lsd(lsd = c(100, 0, 0), bases = c(20, 16))
#' deb_exchange_rate(lsd1 = lsd_100, lsd2 = c(166, 13, 4))
#'
#' # If lsd1 and lsd2 have different bases, the function will throw an error
#' lsd_166 <- deb_as_lsd(lsd = c(166, 13, 4), bases = c(20, 12))
#' \dontrun{
#' deb_exchange_rate(lsd1 = lsd_100, lsd2 = lsd_166)
#' }
#'
#' # To find the exchange rate for multiple currencies use a
#' # list of lsd values or lsd objects for `lsd1` or `lsd2`
#' list_a <- list(c(150, 0, 0), c(125, 0, 0), c(175, 13, 4))
#' list_b <- list(c(125, 0, 0), c(75, 8, 4), c(100, 10, 0))
#'
#' deb_exchange_rate(lsd1 = list_a, lsd2 = list_b)
#'
#' # Or find the exchange rate of multiple currencies to a
#' # single currency by using a vector for `lsd2`
#' deb_exchange_rate(lsd1 = list_a, lsd2 = lsd_166)
#'
#' @export

deb_exchange_rate <- function(lsd1, lsd2,
                              output = c("shillings", "pence", "pounds"),
                              bases = c(20, 12),
                              round = 5) {
  output <- rlang::arg_match(output)
  bases <- validate_bases2(lsd1, lsd2, bases)

  rate <- deb_lsd_l(lsd2, bases = bases) / deb_lsd_l(lsd1, bases = bases)
  normalized <- deb_l_lsd(rate, bases = bases, round = round)

  if (output == "shillings") {
    normalized_to_sd(normalized, bases)
  } else if (output == "pence") {
    normalized_to_d(normalized, bases)
  } else {
    normalized
  }
}

#' Calculation of the inverse of an exchange rate
#'
#' Given an exchange rate between two currencies, calculate the inverse rate,
#' or the rate in the opposite direction.
#'
#' @inheritParams deb_normalize
#' @inheritParams deb_exchange_rate
#' @param exchange_rate An lsd value. An object of class lsd or an object that
#'   can be coerced to class lsd: a numeric vector of length 3 or a list of
#'   such vectors. The values do not need to be normalized.
#'
#' @return Returns an object of class lsd with a bases attribute. The format
#'   of the returned value is determined by the `output` argument, either
#'   pounds, shillings, and pence; shillings and pence; or just pence.
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
#' # Inverse of an exchange rate with alternative bases
#' deb_invert_rate(exchange_rate = c(0, 33, 4), bases = c(20, 16))
#'
#' # Inverse rate of an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(0, 33, 4), bases = c(20, 16))
#' deb_invert_rate(exchange_rate = lsd)
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
  bases <- validate_bases(exchange_rate, bases)

  converted <- 1 / deb_lsd_l(exchange_rate, bases = bases)
  normalized <- deb_l_lsd(converted, bases = bases, round = round)

  if (output == "shillings") {
    normalized_to_sd(normalized, bases)
  } else if (output == "pence") {
    normalized_to_d(normalized, bases)
  } else {
    normalized
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
