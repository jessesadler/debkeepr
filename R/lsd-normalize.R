## Normalize lsd ##

# Check and deal with decimals in l or s
# If value is negative, turn l, s, and d positive
# Returns vector in form c(l, s, d)
lsd_decimal_check <- function(lsd, bases) {
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
    return(purrr::map(lsd, ~ lsd_decimal_check(., bases)))
  }
  # Return all NA if any is NA
  if (any(is.na(c(l, s, d)))) {
    return(c(NA, NA, NA))
  }

  # Check if the value is positive
  # Return positive values so only need to use floor
  if (l + s / bases[1] + d / prod(bases) < 0) {
    l <- -l
    s <- -s
    d <- -d
  }
  # Check for decimals in l
  if (l != round(l)) {
    temp_s <- s + (l - floor(l)) * bases[1]
    l <- floor(l)
    if (temp_s != round(temp_s)) {
      s <- floor(temp_s)
      d <- d + (temp_s - floor(temp_s)) * bases[2]
    } else {
      s <- temp_s
    }
  }
  # Check for decimals in s
  if (s != round(s)) {
    d <- d + (s - floor(s)) * bases[2]
    s <- floor(s)
  }
  c(l, s, d)
}

# Actual normalization
lsd_normalize <- function(lsd, bases, round) {
  if (is.list(lsd)) {
    return(purrr::map(lsd, ~ lsd_normalize(., bases, round)))
  }

  lsd[1] <- lsd[1] + ((lsd[2] + lsd[3] %/% bases[2]) %/% bases[1])
  lsd[2] <- (lsd[2] + lsd[3] %/% bases[2]) %% bases[1]
  lsd[3] <- round(lsd[3] %% bases[2], round)

  if (any(is.na(lsd))) {
    return(lsd)
  }

  # Case when denarii rounds up to its base
  if (dplyr::near(lsd[3], bases[2])) {
    lsd[2] <- lsd[2] + 1
    lsd[3] <- 0
    if (dplyr::near(lsd[2], bases[1])) {
      lsd[1] <- lsd[1] + 1
      lsd[2] <- 0
    }
  }
  lsd
}

# If lsd is negative return normalized as negative
lsd_negative <- function(normalized, lsd, bases) {
  # Vectorize
  if (is.list(lsd) & is.list(normalized)) {
    return(purrr::map2(normalized, lsd, ~ lsd_negative(normalized = .x,
                                                       lsd = .y,
                                                       bases = bases)))
  }

  if (is.list(lsd) & is.vector(normalized)) {
    normalized <- list(normalized)
    return(purrr::map2(normalized, lsd, ~ lsd_negative(normalized = .x,
                                                       lsd = .y,
                                                       bases = bases)))
  }

  # NA
  if (any(is.na(normalized))) {
    return(normalized)
  }

  if (sum(lsd / c(1, bases[1], prod(bases))) > 0) {
    normalized
  } else {
    -normalized
  }
}

#' Normalize pounds, shillings, and pence
#'
#' Normalize pounds, shillings, and pence to standard unit bases.
#'
#' `deb_normalize()` uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to represent pounds,
#' shillings, and pence units. The abbreviations derive from the Latin terms
#' [libra](https://en.wikipedia.org/wiki/French_livre),
#' [solidus](https://en.wikipedia.org/wiki/Solidus_(coin)), and
#' [denarius](https://en.wikipedia.org/wiki/Denarius). In the 8th century a
#' solidus came to represent 12 denarii coins, and 240 denarii were made from
#' one libra or pound of silver. The custom of counting coins in dozens
#' (solidi) and scores of dozens (librae) spread throughout the Carolingian
#' Empire and became engrained in much of Europe. However,
#' [other bases](https://en.wikipedia.org/wiki/Non-decimal_currency) for the
#' solidus and denarius units were also in use. The `bases` attribute makes
#' it possible to specify alternative bases for the solidus and denarius units.
#'
#' @param lsd An lsd value. An object of class lsd or an object that can be
#'   coerced to class lsd: a numeric vector of length 3 or a list of such
#'   vectors.
#' @param bases Numeric vector of length 2 used to specify the bases for the
#'   shillings or s and pence or d units. Default is `c(20, 12)`, which
#'   conforms to the most widely used system of 1 pound = 20 shillings and
#'   1 shilling = 12 pence. If `lsd` is of class lsd, the bases attribute will
#'   be used in the place of this argument.
#' @param round Round pence unit to specified number of decimal places.
#'   Default is 5. Set to 0 to return pence as whole number.
#'
#' @return Returns an object of class lsd with a bases attribute.
#'
#' @examples
#' # Use to normalize the values of pounds, shillings, and pence
#' deb_normalize(lsd = c(5, 55, 42))
#'
#' # Normalize values with alternative bases for solidus and denarius units
#' # For instance, the Dutch system of guilders, stuivers, and penningen
#' deb_normalize(lsd = c(5, 55, 42), bases = c(20, 16))
#'
#' # Normalizing an object of class lsd will use the bases attribute
#' lsd <- deb_as_lsd(lsd = c(5, 55, 42), bases = c(20, 16))
#' deb_normalize(lsd = lsd)
#'
#' # Normalize multiple values with a list of numeric vectors
#' lsd_list <- list(c(4, 34, 89), c(-9, -75, -19), c(15.85, 36.15, 56))
#' deb_normalize(lsd = lsd_list)
#'
#' # Or an lsd object with alternative bases
#' lsd_list2 <- deb_as_lsd(lsd = lsd_list, bases = c(20, 16))
#' deb_normalize(lsd = lsd_list2)
#'
#' # It is possible to perform arithmetic within the function
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
#' # Round argument can be used to round pence unit to
#' # specific decimal place and properly normalize value
#' # Compare with default of round = 5 to round = 0
#' deb_normalize(lsd = c(5.7, 44.742, 15), round = 5)
#' deb_normalize(lsd = c(5.7, 44.742, 15), round = 0)
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

deb_normalize <- function(lsd, bases = c(20, 12), round = 5) {
  lsd_check(lsd)
  bases <- validate_bases(lsd, bases)
  bases_check(bases)
  round_check(round)
  checked <- lsd_decimal_check(lsd, bases)
  normalized <- lsd_normalize(checked, bases, round)
  ret <- lsd_negative(normalized, lsd, bases)
  to_lsd(ret, bases)
}
