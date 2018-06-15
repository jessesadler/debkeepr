## Normalize lsd ##

# Check and deal with decimals in l or s
# If value is negative, turn l, s, and d positive
# Returns vector in form c(l, s, d)
lsd_decimal_check <- function(lsd) {
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
    return(purrr::map(lsd, lsd_decimal_check))
  }

  # Check if the value is positive
  # Return positive values so only need to use floor
  if (l + s/20 + d/240 < 0) {
    l <- -l
    s <- -s
    d <- -d
  }
  # Check for decimals in l
  if (l != round(l)) {
    temp_s <- s + (l - floor(l)) * 20
    l <- floor(l)
    if (temp_s != round(temp_s)) {
      s <- floor(temp_s)
      d <- d + (temp_s - floor(temp_s)) * 12
    } else {
      s <- temp_s
    }
  }
  # Check for decimals in s
  if (s != round(s)) {
    d <- d + (s - floor(s)) * 12
    s <- floor(s)
  }
  c(l, s, d)
}

# Actual normalization
lsd_normalize <- function(lsd, round) {
  # vector
  lsd[1] <- lsd[1] + ((lsd[2] + lsd[3] %/% 12) %/% 20)
  lsd[2] <- (lsd[2] + lsd[3] %/% 12) %% 20
  lsd[3] <- round(lsd[3] %% 12, round)

  setNames(lsd, c("l", "s", "d"))
}

#' Normalize pounds, shillings, and pence
#'
#' Normalize pounds, shillings, and pence to the correct values based
#' on 12 pence in a shilling and 20 shillings in a pound.
#'
#' This function uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra were adopted
#' by at least the 8th century and spread throughout Europe through the
#' Carolingian Empire.
#'
#' @param lsd Numeric vector of length 3 or list of numeric vectors of length
#'   3. The first position of the vector represents the pounds value or l. The
#'   second position represents the shillings value or s. And the third
#'   position represents the pence value or d.
#' @param round Round pence to specified number of decimal places.
#'   Default is 3. Set to 0 to return pence as whole numbers.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the input lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Use to calculate the correct number of pounds, shillings, and pence
#' deb_normalize(lsd = c(5, 55, 22))
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
#' @export

deb_normalize <- function(lsd, round = 3) {

  lsd_check(lsd, round)
  checked <- lsd_decimal_check(lsd)

  if (is.list(lsd) == FALSE) {
    # vector
    normalized <- lsd_normalize(checked, round)

    # Positive and negative
    if (sum(lsd / c(1, 20, 240)) > 0) {
      normalized
    } else {
      -normalized
    }
  } else {
    # list
    normalized <- purrr::map(checked, ~ lsd_normalize(., round))

    # Positive and negative
    dplyr::if_else(purrr::map(lsd, ~ sum(. / c(1, 20, 240))) > 0,
                   purrr::map(normalized, `+`),
                   purrr::map(normalized, `-`))
  }
}

## Normalize data frame ##

#' Normalize pounds, shillings, and pence variables in a data frame
#'
#' Normalize pounds, shillings, and pence variables in a data frame to the
#' correct values based on 12 pence in a shilling and 20 shillings in a pound.
#'
#' This function uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra were adopted
#' by at least the 8th century and spread throughout Europe through the
#' Carolingian Empire.
#'
#' @param df A data frame that contains pounds, shillings, and pence variables.
#' @param l Pounds column: Unquoted name of a numeric variable corresponding
#'   to pounds. Default is l.
#' @param s Shillings column: Unquoted name of numeric variable corresponding
#'   to shillings. Default is s.
#' @param d Pence column: Unquoted name of numeric variable corresponding to
#'   pence. Default is d.
#' @param round Round pence to specified number of decimal places.
#'   Default is 3. Set to 0 if you want pence to always be a whole number.
#' @param replace Logical (default `TRUE`): when `TRUE` the new pounds,
#'   shillings, and pence variables will replace the original ones.
#' @param suffix Suffix added to the column names for the pounds, shillings,
#'   and pence columns to distinguish them from the original pounds, shillings,
#'   and pence columns if `replace = FALSE`. Default is ".1". Should be a
#'   charactern vector of length 1.
#'
#' @return Returns a data frame with normalized pounds, shillings, and pence,
#'   variables.
#'
#' @examples
#' # Data frame with pounds, shillings, and pence variables
#' example <- data.frame(l = c(35, -10, 26.725, 12),
#'                       s = c(50, -48, 311.85, 76),
#'                       d = c(89, -181, 70, 205))
#' # Normalize lsd values
#' deb_normalize_df(example, l, s, d)
#'
#' @export

deb_normalize_df <- function(df,
                             l = l, s = s, d = d,
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
                     lsd_names,
                     replace,
                     round)
}
