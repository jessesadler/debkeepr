## Refactor lsd ##

# Check and deal with decimals in l or s
# If value is negative, turn l, s, and d positive
# Returns vector in form c(l, s, d)
deb_decimal_check <- function(l, s, d) {
  # vectorize
  if (length(l) > 1) {
    return(purrr::pmap(list(l, s, d), deb_decimal_check))
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


## Transform lsd to separate l, s, and d ##
# Decimal check, split whether l, s, and d are > 1 or 1,
# refactor to correct value, and return to negative is necessary.
# Returns either single value or numeric vector

deb_librae <- function(l, s, d) {
  lsd <- deb_decimal_check(l, s, d)

  if (is.list(lsd) == FALSE) {
    librae <- lsd[1] + ((lsd[2] + lsd[3] %/% 12) %/% 20)
    dplyr::if_else(l + s/20 + d/240 > 0,
                   librae,
                   -librae)
  } else {
    librae <- purrr::map_dbl(lsd, ~ .[1] + ((.[2] + .[3] %/% 12) %/% 20))
    dplyr::if_else(l + s/20 + d/240 > 0,
                   librae,
                   purrr::map_dbl(librae, `-`))
  }
}

deb_solidi <- function(l, s, d) {
  lsd <- deb_decimal_check(l, s, d)

  if (is.list(lsd) == FALSE) {
    solidi <- (lsd[2] + lsd[3] %/% 12) %% 20
    dplyr::if_else(l + s/20 + d/240 > 0,
                   solidi,
                   -solidi)
  } else {
    solidi <- purrr::map_dbl(lsd, ~ (.[2] + .[3] %/% 12) %% 20)
    dplyr::if_else(l + s/20 + d/240 > 0,
                   solidi,
                   purrr::map_dbl(solidi, `-`))
  }
}

deb_denarii <- function(l, s, d, round = 3) {
  lsd <- deb_decimal_check(l, s, d)

  if (is.list(lsd) == FALSE) {
    denarii <- round(lsd[3] %% 12, round)
    dplyr::if_else(l + s/20 + d/240 > 0,
                   denarii,
                   -denarii)
  } else {
    denarii <- purrr::map_dbl(lsd, ~ round(.[3] %% 12, round))
    dplyr::if_else(l + s/20 + d/240 > 0,
                   denarii,
                   purrr::map_dbl(denarii, `-`))
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
#' \url{https://en.wikipedia.org/wiki/£sd}.
#'
#' @param l Pounds: numeric vector of the same length as s and d.
#' @param s Shillings: numeric vector of the same length as l and d.
#' @param d Pence: numeric vector of the same length as l and s.
#' @param round round pence to specified number of decimal places.
#' Default is 3. Set to 0 if you want pence to always be a whole number.
#' @param vector Logical (default \code{FALSE}): when \code{FALSE} the output
#'   will be a tibble. When \code{TRUE} the output will be a named numeric
#'   vector or list of named numeric vectors if the length of l, s, and d is
#'   greater than 1.
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. If the input lsd value is negative, the
#'   pounds, shillings, and pence values will all be negative. The number of
#'   rows in the resulting tibble will be equal to the length of the input
#'   vectors. If the length of l, s, and d is greater than 1 and
#'   \code{vector = TRUE}, the result will be a list of named vectors of length
#'   equal to the input vectors.
#'
#' @examples
#' # Use to calculate the correct number of pounds, shillings, and pence
#' deb_refactor(l = 5, s = 25, d = 22)
#' deb_refactor(5, 25, 22, vector = TRUE)
#'
#' # It is possible to perform math within the function
#' deb_refactor(5 + 6, 20 + 18, 8 + 11)
#' # Or even
#' deb_refactor(sum(4, 9, 0), sum(12, 16, 5), sum(11, 0, 6))
#'
#' # deb_refactor can deal with negative values
#' deb_refactor(-5, -25, -22)
#' # Or a mixture of positive and negative if that occurs for some reason
#' deb_refactor(5, -25, 22)
#'
#' # deb_refactor can also properly refactor decimalized pounds and shillings
#' deb_refactor(8.7, 33.65, 15)
#'
#' # l, s, and d can be vectors of length > 1
#' # Return a tibble with two rows
#' deb_refactor(l = c(8, 10), s = c(25, 86), d = c(34, 29))
#'
#' # Return a list with two vectors
#' deb_refactor(l = c(8, 10), s = c(25, 86), d = c(34, 29), vector = TRUE)
#'
#' # This makes it possible to refactor a tibble of lsd values
#' ex_tbl <- tibble::tibble(l = c(8, 10),
#'                          s = c(25, 86),
#'                          d = c(34, 29))
#' deb_refactor(ex_tbl$l, ex_tbl$s, ex_tbl$d)
#'
#' @export

deb_refactor <- function(l, s, d, round = 3, vector = FALSE) {
  lsd_check(l, s, d, round, vector)
  # Create values with different names so that l, s, and d are not overwritten
  librae <- deb_librae(l, s, d)
  solidi <- deb_solidi(l, s, d)
  denarii <- deb_denarii(l, s, d, round)
  if (vector == FALSE) {
    tibble::tibble(
      l = librae,
      s = solidi,
      d = denarii)
  } else {
    if (length(l) > 1) {
      # Create list of lsd vectors
      list(l = librae,
           s = solidi,
           d = denarii) %>%
        purrr::transpose() %>%
        purrr::simplify_all()
    } else {
      # single lsd vector
      c(l = librae,
        s = solidi,
        d = denarii)
    }
  }
}
