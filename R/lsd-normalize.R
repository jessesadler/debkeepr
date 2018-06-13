## Normalize lsd ##

# Check and deal with decimals in l or s
# If value is negative, turn l, s, and d positive
# Returns vector in form c(l, s, d)
deb_decimal_check <- function(lsd) {
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
    return(purrr::map(lsd, deb_decimal_check))
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
#' Normalize pounds, shillings, and pence to correct values based
#' on 12 pence in a shilling and 20 shillings in a pound.
#'
#' This function uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @param l Pounds: numeric vector of the same length as `s` and `d`.
#' @param s Shillings: numeric vector of the same length as `l` and `d`.
#' @param d Pence: numeric vector of the same length as `l` and `s`.
#' @param round Round pence to specified number of decimal places.
#'   Default is 3. Set to 0 to return pence as whole numbers.
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. If the input lsd value is negative, the
#'   pounds, shillings, and pence values will all be negative. The number of
#'   rows in the resulting tibble will be equal to the length of the input
#'   vectors. If the length of `l`, `s`, and `d` is greater than 1 and
#'   `vector = TRUE`, the result will be a list of named vectors of length
#'   equal to the input vectors.
#'
#' @examples
#' # Use to calculate the correct number of pounds, shillings, and pence
#' deb_normalize(l = 5, s = 25, d = 22)
#' deb_normalize(5, 25, 22, vector = TRUE)
#'
#' # It is possible to perform math within the function
#' deb_normalize(5 + 6, 20 + 18, 8 + 11)
#' # Or even
#' deb_normalize(sum(4, 9, 0), sum(12, 16, 5), sum(11, 0, 6))
#'
#' # deb_normalize can deal with negative values
#' deb_normalize(-5, -25, -22)
#' # Or a mixture of positive and negative if that occurs for some reason
#' deb_normalize(5, -25, 22)
#'
#' # deb_normalize can also properly normalize decimalized pounds and shillings
#' deb_normalize(8.7, 33.65, 15)
#'
#' # l, s, and d can be vectors of length > 1
#' # Return a tibble with two rows
#' deb_normalize(l = c(8, 10), s = c(25, 86), d = c(34, 29))
#'
#' # Return a list with two vectors
#' deb_normalize(l = c(8, 10), s = c(25, 86), d = c(34, 29), vector = TRUE)
#'
#' # This makes it possible to normalize a data frame of lsd values
#' example <- data.frame(l = c(8, 10, 15),
#'                       s = c(25, 86, 102),
#'                       d = c(34, 29, 87))
#' deb_normalize(example$l, example$s, example$d)
#'
#' @export

deb_normalize <- function(lsd, round = 3) {

  lsd_check(lsd, round)
  checked <- deb_decimal_check(lsd)

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

  if (replace == TRUE) {
    suffix <- ""
  }
  # Column names: avoid overwriting l, s, and d columns
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  lsd_mutate_columns(df,
                     !! l, !! s, !! d,
                     lsd_names,
                     round = round)
}
