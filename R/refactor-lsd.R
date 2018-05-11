## Refactor lsd ##

# Check and deal with decimals in l or s
# If value is negative, turn l, s, and d positive
deb_decimal_check <- function(l, s, d) {
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

# Individual helper functions
# Check decimal, deal with negative numbers, and
# refactor to correct value.

deb_librae <- function(l, s, d) {
  lsd <- deb_decimal_check(l, s, d)
  librae <- lsd[1]
  solidi <- lsd[2]
  denarii <- lsd[3]
  librae <- librae + ((solidi + denarii %/% 12) %/% 20)
  if (l + s/20 + d/240 > 0) {
    librae
  } else {
    -librae
  }
}

deb_solidi <- function(l, s, d) {
  lsd <- deb_decimal_check(l, s, d)
  solidi <- lsd[2]
  denarii <- lsd[3]
  solidi <- (solidi + denarii %/% 12) %% 20
  if (l + s/20 + d/240 > 0) {
    solidi
  } else {
    -solidi
  }
}

deb_denarii <- function(l, s, d, round) {
  lsd <- deb_decimal_check(l, s, d)
  denarii <- lsd[3]
  denarii <- round(denarii %% 12, round)
  if (l + s/20 + d/240 > 0) {
    denarii
  } else {
    -denarii
  }
}

#' Refactor pounds, shillings, and pence
#'
#' Refactor pounds, shillings, and pence to correct values based
#' on 12 pence in a shilling and 20 shillings in a pound.
#'
#' This function uses the nomenclature of l, s, and d to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#' For more information on the lsd noemclature see
#' \url{https://en.wikipedia.org/wiki/£sd}
#'
#' @inheritParams lsd_check
#' @param round round pence to specified number of decimal places. Default is 3.
#'   Set to 0 if you want pence to always be a whole number.
#' @param vector Logical (default FALSE), when FALSE the output will
#'   be a tibble, when TRUE the output will be a numeric vector.
#'
#' @return Returns either a tibble with one row of values and columns for the
#'   pounds, shillings, and pence values labeled as l, s, and d or a named
#'   numeric vector with values for pounds, shillings, and pence. If the amount
#'   is negative, the pounds, shillings, and pence values will all be negative.
#'
#' @examples
#' # Use to calculate the correct number of pounds, shillings, and pence
#' deb_refactor(l = 5, s = 25, d = 22)
#' deb_refactor(5, 25, 22, vector = TRUE)
#'
#' # It is possible to add within the function
#' deb_refactor(5 + 6, 20 + 18, 8 + 11)
#' # Or even
#' deb_refactor(sum(4, 9, 0), sum(12, 16, 5), sum(11, 0, 6))
#'
#' # deb_refactor can deal with negative values
#' deb_refactor(-5, -25, -22)
#' # Or even a mixture of positive and negative if that occurs for some reason
#' deb_refactor(5, -25, 22)
#'
#' @export

deb_refactor <- function(l, s, d, round = 3, vector = FALSE) {
  librae <- deb_librae(l, s, d)
  solidi <- deb_solidi(l, s, d)
  denarii <- deb_denarii(l, s, d, round)
  if (vector == FALSE) {
    tibble::tibble(
      l = librae,
      s = solidi,
      d = denarii)
  } else {
    c(
      l = librae,
      s = solidi,
      d = denarii)
  }
}
