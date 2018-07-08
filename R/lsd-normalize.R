## Normalize lsd ##

# Check and deal with decimals in l or s
# If value is negative, turn l, s, and d positive
# Returns vector in form c(l, s, d)
lsd_decimal_check <- function(lsd, lsd_bases) {
  if (is.list(lsd) == TRUE) {
    l <- purrr::map_dbl(lsd, 1)
    s <- purrr::map_dbl(lsd, 2)
    d <- purrr::map_dbl(lsd, 3)
  } else {
    l <- lsd[1]
    s <- lsd[2]
    d <- lsd[3]
  }

  # vectorize
  if (length(l) > 1) {
    return(purrr::map(lsd, ~ lsd_decimal_check(., lsd_bases)))
  }

  # Check if the value is positive
  # Return positive values so only need to use floor
  if (l + s / lsd_bases[1] + d / prod(lsd_bases) < 0) {
    l <- -l
    s <- -s
    d <- -d
  }
  # Check for decimals in l
  if (l != round(l)) {
    temp_s <- s + (l - floor(l)) * lsd_bases[1]
    l <- floor(l)
    if (temp_s != round(temp_s)) {
      s <- floor(temp_s)
      d <- d + (temp_s - floor(temp_s)) * lsd_bases[2]
    } else {
      s <- temp_s
    }
  }
  # Check for decimals in s
  if (s != round(s)) {
    d <- d + (s - floor(s)) * lsd_bases[2]
    s <- floor(s)
  }
  c(l, s, d)
}

# Actual normalization
lsd_normalize <- function(lsd, round, lsd_bases) {
  # vector
  lsd[1] <- lsd[1] + ((lsd[2] + lsd[3] %/% lsd_bases[2]) %/% lsd_bases[1])
  lsd[2] <- (lsd[2] + lsd[3] %/% lsd_bases[2]) %% lsd_bases[1]
  lsd[3] <- round(lsd[3] %% lsd_bases[2], round)

  stats::setNames(lsd, c("l", "s", "d"))
}

#' Normalize pounds, shillings, and pence
#'
#' Normalize pounds, shillings, and pence to standard unit bases.
#'
#' `deb_normalize()` uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence, which derives from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii, and 240 denarii were made from one
#' libra or pound of silver. The custom of counting coins in dozens (solidi)
#' and scores of dozens (libra) spread throughout the Carolingian Empire and
#' became engrained in much of Europe. However,
#' [other ratios](https://en.wikipedia.org/wiki/Non-decimal_currency) between
#' libra, solidus, and denarius were also in use. The `lsd_bases` argument
#' makes it possible to specify alternative bases for the solidus and denarius
#' values.
#'
#' @param lsd Numeric vector of length 3 or list of numeric vectors of length
#'   3. The first position of the vector represents the pounds value or l. The
#'   second position represents the shillings value or s. And the third
#'   position represents the pence value or d.
#' @param lsd_bases Numeric vector of length 2 used to specify the bases for
#'   the s or solidus and d or denarius values in `lsd` vectors. Default is
#'   `c(20, 12)`, which conforms to the most widely used system of 1 libra =
#'   20 solidi and 1 solidus = 12 denarii. This argument makes it possible to
#'   use alternative bases for the solidus and denarius values that were also
#'   in use.
#' @param round Round pence to specified number of decimal places.
#'   Default is 3. Set to 0 to return pence as whole numbers.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the input lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Use to normalize the values of pounds, shillings, and pence
#' deb_normalize(lsd = c(5, 55, 22))
#'
#' # Normalize values with alternative bases for solidus and denarius units
#' # For instance, following Dutch system of gulden, stuivers, and penningen
#' deb_normalize(lsd = c(5, 55, 22), lsd_bases = c(20, 16))
#'
#' # It is possible to perform math within the function
#' deb_normalize(lsd = c(5 + 6, 20 + 18, 8 + 11))
#'
#' # deb_normalize can deal with negative values
#' deb_normalize(lsd = c(-5, -25, -22))
#'
#' # Or a mixture of positive and negative values
#' # if that occurs for some reason
#' deb_normalize(lsd = c(5, -25, 22))
#'
#' # Can also properly normalize decimalized pounds and shillings
#' deb_normalize(lsd = c(8.7, 33.65, 15))
#'
#' # Use the round argument to return whole pence
#' deb_normalize(lsd = c(8.7, 33.65, 15), round = 0)
#'
#' # To normalize multiple lsd values use a list of lsd vectors
#' lsd_list <- list(c(4, 34, 89), c(-9, -75, -19), c(15.85, 36.15, 56))
#' deb_normalize(lsd = lsd_list)
#'
#' # It is possible to do arithmetic within the lsd argument
#' # if inputs are all vectors.
#' deb_normalize(lsd = c(56, 8, 5) + c(19, 5, 7))
#' deb_normalize(lsd = c(56, 8, 5) - c(19, 5, 7))
#' deb_normalize(lsd = c(56, 8, 5) * 3)
#' deb_normalize(lsd = c(56, 8, 5) / 3)
#'
#' # This will not work if one of the objects is a list
#' # Use arithmetic functions for this
#' \dontrun{
#' deb_normalize(list(c(56, 8, 5), c(27, 12, 4)) + list(c(19, 5, 7), c(6, 3, 2)))
#' deb_normalize(list(c(56, 8, 5), c(27, 12, 4)) - c(19, 5, 7))
#' deb_normalize(list(c(56, 8, 5), c(27, 12, 4)) * 3)
#' deb_normalize(list(c(56, 8, 5), c(27, 12, 4)) / 3)
#' }
#'
#' @export

deb_normalize <- function(lsd, lsd_bases = c(20, 12), round = 3) {

  lsd_check(lsd)
  paramenter_check(lsd_bases, round)
  checked <- lsd_decimal_check(lsd, lsd_bases)

  if (is.list(lsd) == FALSE) {
    # vector
    normalized <- lsd_normalize(checked, round, lsd_bases)

    # Positive and negative
    if (sum(lsd / c(1, lsd_bases[1], prod(lsd_bases))) > 0) {
      normalized
    } else {
      -normalized
    }
  } else {
    # list
    normalized <- purrr::map(checked, ~ lsd_normalize(., lsd_bases = lsd_bases, round = round))

    # Positive and negative
    dplyr::if_else(purrr::map(lsd, ~ sum(. / c(1, lsd_bases[1], prod(lsd_bases)))) > 0,
                   purrr::map(normalized, `+`),
                   purrr::map(normalized, `-`))
  }
}

## Normalize data frame ##

#' Normalize pounds, shillings, and pence variables in a data frame
#'
#' Normalize pounds, shillings, and pence variables in a data frame to
#' to standard unit bases.
#'
#' `deb_normalize_df()` uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence, which derives from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii, and 240 denarii were made from one
#' libra or pound of silver. The custom of counting coins in dozens (solidi)
#' and scores of dozens (libra) spread throughout the Carolingian Empire and
#' became engrained in much of Europe. However,
#' [other ratios](https://en.wikipedia.org/wiki/Non-decimal_currency) between
#' libra, solidus, and denarius were also in use. The `lsd_bases` argument
#' makes it possible to specify alternative bases for the solidus and denarius
#' values.
#'
#' @param df A data frame that contains pounds, shillings, and pence variables.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding
#'   to pounds. Default is l.
#' @param s Shillings column: Unquoted name of numeric variable corresponding
#'   to shillings. Default is s.
#' @param d Pence column: Unquoted name of numeric variable corresponding to
#'   pence. Default is d.
#' @param lsd_bases Numeric vector of length 2 used to specify the bases for
#'   the s or solidus and d or denarius values in `lsd` vectors. Default is
#'   `c(20, 12)`, which conforms to the most widely used system of 1 libra =
#'   20 solidi and 1 solidus = 12 denarii. This argument makes it possible to
#'   use alternative bases for the solidus and denarius values that were also
#'   in use.
#' @param round Round pence to specified number of decimal places.
#'   Default is 3. Set to 0 if you want pence to always be a whole number.
#' @param replace Logical (default `TRUE`): when `TRUE` the new pounds,
#'   shillings, and pence variables will replace the original ones.
#' @param suffix Suffix added to the column names for the pounds, shillings,
#'   and pence columns to distinguish new variables from the original pounds,
#'   shillings, and pence columns if `replace = FALSE`. Default is ".1".
#'   Should be a character vector of length 1.
#'
#' @return Returns a data frame with normalized pounds, shillings, and pence,
#'   variables.
#'
#' @examples
#' # Data frame with pounds, shillings, and pence variables
#' example <- data.frame(l = c(35, -10, 26.725, 12),
#'                       s = c(50, -48, 311.85, 76),
#'                       d = c(89, -181, 70, 205))
#' # Normalize the values of pounds, shillings, and pence
#' deb_normalize_df(example, l = l, s = s, d = d)
#'
#' # Normalize values with alternative bases for solidus and denarius
#' # For instance, following Dutch system of gulden, stuivers, and penningen
#' deb_normalize_df(example, l = l, s = s, d = d, lsd_bases = c(20, 16))
#'
#' @export

deb_normalize_df <- function(df,
                             l = l, s = s, d = d,
                             lsd_bases = c(20, 12),
                             round = 3,
                             replace = TRUE,
                             suffix = ".1") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  lsd_mutate_columns(df,
                     !! l, !! s, !! d,
                     lsd_names = lsd_names,
                     replace = replace,
                     lsd_bases = lsd_bases,
                     round = round)
}
