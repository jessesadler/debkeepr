## lsd Arithmetic ##

#' Multiplication of pounds, shillings, and pence
#'
#' Multiply pounds, shillings, and pence by a given multiplier (`x`).
#' The result is normalized to correct values based on 12 pence in a
#' shilling and 20 shillings in a pound. This is a wrapper around
#' [deb_normalize()].
#'
#' This function uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param x Multiplier. The value by which the given pounds, shillings, and
#'   pence will be multiplied. Should be a numeric vector of length 1.
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. The number of rows in the resulting
#'   tibble will be equal to the length of the input vectors. If the length of
#'   `l`, `s`, and `d` is greater than 1 and `vector = TRUE`, the result will
#'   be a list of named vectors of length equal to the input vectors.
#'
#' @examples
#' # Multiply pounds, shillings, and pence by 5
#' deb_multiply(x = 5, l = 5, s = 15, d = 8)
#' deb_multiply(x = 5, 5, 15, 8, vector = TRUE)
#'
#' # Calculate commission of 3% of sales of £3095 17s 6d
#' deb_multiply(x = 0.03, 3095, 17, 6)
#'
#' # The l, s, and d values do not have to be normalized
#' deb_multiply(x = 8, 10, 38, 65)
#'
#' # Like deb_normalize(), deb_multiply() can handle negative numbers
#' deb_multiply(x = 5, -5, -15, -8)
#'
#' # Or decimals
#' deb_multiply(x = 5, l = 5.625, s = 15.35, d = 8)
#'
#' # l, s, and d can be vectors of length > 1
#' # Return a tibble with two rows
#' deb_multiply(x = 6,
#'              l = c(8, 10),
#'              s = c(15, 6),
#'              d = c(3, 9))
#'
#' # Return a list with two vectors
#' deb_multiply(x = 6,
#'              l = c(8, 10),
#'              s = c(15, 6),
#'              d = c(3, 9),
#'              vector = TRUE)
#'
#' @export

deb_multiply <- function(lsd, x, round = 3) {
  arithmetic_check(x)

  deb_normalize(lsd * x, round = round)
}


#' Multiplication of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to multiply pounds, shillings, and pence by a given
#' multiplier (`x`). The value is returned in the form of three new variables
#' representing the calculated pounds, shillings and pence.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_interest_mutate
#' @inheritParams deb_multiply
#' @param suffix Suffix added to the column names for the pounds, shillings, and
#'   pence columns representing the multiplied values so that they are
#'   distinguished from the original pounds, shillings, and pence columns.
#'   Default is ".1". Should be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the multiplied values.
#'
#' @examples
#' # Multiply a data frame of values by 5
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_multiply_mutate(example, x = 5, l, s, d)
#'
#' # Change the suffix added to the new variables
#' deb_multiply_mutate(example, x = 5, l, s, d,
#'                     suffix = ".x5")
#'
#' # Replace the existing pounds, shillings, and pence
#' # with multiplied values
#' deb_multiply_mutate(example, x = 5, l, s, d,
#'                     replace = TRUE)
#'
#' @export

deb_multiply_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                x,
                                suffix = ".1",
                                round = 3,
                                replace = FALSE) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  arithmetic_check(x)
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  lsd_mutate_columns(df,
                     !! l * x,
                     !! s * x,
                     !! d * x,
                     lsd_names,
                     replace,
                     round)
}

#' Division of pounds, shillings, and pence
#'
#' Divide pounds, shillings, and pence by a given divisor (`x`).
#' The result is normalized to correct values based on 12 pence in a
#' shilling and 20 shillings in a pound. This is a wrapper around
#' [deb_normalize()].
#'
#' This function uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param x Divisor. The value by which the given pounds, shillings, and
#'   pence will be divided. Should be a numeric vector of length 1.
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. The number of rows in the resulting
#'   tibble will be equal to the length of the input vectors. If the length of
#'   `l`, `s`, and `d` is greater than 1 and `vector = TRUE`, the result will
#'   be a list of named vectors of length equal to the input vectors.
#'
#' @examples
#' # Divide pounds, shillings, and pence by a divisor
#' deb_divide(x = 5, l = 63, s = 15, d = 8)
#' deb_divide(x = 5, 63, 15, 8, vector = TRUE)
#'
#' # The l, s, and d values do not have to be normalized
#' deb_divide(x = 8, 109, 38, 65)
#'
#' # Like deb_normalize(), deb_divide() can handle negative numbers
#' deb_divide(x = 5, -63, -15, -8)
#' deb_divide(x = -5, 63, 15, 8)
#'
#' # Or decimals
#' deb_divide(x = 5, l = 63.625, s = 15.35, d = 8)
#'
#' # l, s, and d can be vectors of length > 1
#' # Return a tibble with two rows
#' deb_divide(x = 6,
#'              l = c(28, 30),
#'              s = c(15, 6),
#'              d = c(3, 9))
#'
#' # Return a list with two vectors
#' deb_divide(x = 6,
#'              l = c(28, 30),
#'              s = c(15, 6),
#'              d = c(3, 9),
#'              vector = TRUE)
#'
#' @export

deb_divide <- function(lsd, x, round = 3) {
  arithmetic_check(x)

  deb_normalize(lsd / x, round = round)
}


#' Division of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to divide pounds, shillings, and pence by a given
#' divisor (`x`). The value is returned in the form of three new variables
#' representing the calculated pounds, shillings and pence.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_interest_mutate
#' @inheritParams deb_divide
#' @param suffix Suffix added to the column names for the pounds, shillings, and
#'   pence columns representing the divided values so that they are
#'   distinguished from the original pounds, shillings, and pence columns.
#'   Default is ".1". Should be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the divided values.
#'
#' @examples
#' # Divide a data frame of values by 5
#' example <- data.frame(l = c(356, 10, 26, 12),
#'                       s = c(100, 18, 11, 16),
#'                       d = c(98, 11, 10, 5))
#' deb_divide_mutate(example, x = 5, l, s, d)
#'
#' # Change the suffix added to the new variables
#' deb_divide_mutate(example, x = 5, l, s, d,
#'                   suffix = ".div5")
#'
#' # Replace the existing pounds, shillings, and pence
#' # with divided values
#' deb_divide_mutate(example, x = 5, l, s, d,
#'                   replace = TRUE)
#'
#' @export

deb_divide_mutate <- function(df,
                              l = l,
                              s = s,
                              d = d,
                              x,
                              suffix = ".1",
                              round = 3,
                              replace = FALSE) {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  arithmetic_check(x)
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  lsd_mutate_columns(df,
                     !! l / x,
                     !! s / x,
                     !! d / x,
                     lsd_names,
                     replace,
                     round)
}

#' Subtract two values of pounds, shillings, and pence
#'
#' Subtract and normalize two values of pounds, shillings, and pence
#' that are given in the form of either two numeric vectors or two lists
#' of numeric vectors (`lsd1` and `lsd2`). This is a wrapper around
#' [deb_normalize()].
#'
#' If `lsd1` and `lsd2` are lists of different lengths, the shorter list
#' will be recycled.
#'
#' `deb_subtract()` uses the nomenclature of
#' [l, s, and d](https://en.wikipedia.org/wiki/£sd) to refer to pounds,
#' shillings, and pence. This derives from the Latin terms for librae,
#' solidi, and denarii. One solidus was equivalent to 12 denarii, and
#' 240 denarii coins were made from on libra of silver. The nomenclature
#' and values of 12 denarii to 1 solidus and 20 solidi to 1 libra was
#' adopted by Charlemagne and spread throughout Europe under different names.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param lsd1,lsd2 Either numeric vectors of length 3 representing the pounds,
#'   shillings, and pence of the two values or lists of numeric vectors
#'   of length 3. lsd2 is the value or values to be stubtracted from lsd1:
#'   `lsd1 - lsd2`.
#'
#' @return Returns either a tibble with columns for the pounds, shillings, and
#'   pence values labeled as l, s, and d or a named numeric vector with values
#'   for pounds, shillings, and pence. If `lsd1` and `lsd2` are lists, the
#'   number of rows in the resulting tibble will be the length of the longest
#'   list. If `lsd1` and `lsd2` are lists and `vector = TRUE`, the result will
#'   be a list of named vectors the length of the longest list.
#'
#' @examples
#' # Subtract pounds, shillings, and pence
#' deb_subtract(lsd1 = c(56, 8, 5), lsd2 = c(19, 5, 7))
#' deb_subtract(lsd1 = c(56, 8, 5), lsd2 = c(19, 5, 7), vector = TRUE)
#'
#' # The l, s, and d values do not have to be normalized
#' deb_subtract(lsd1 = c(76, 65, 35), lsd2 = c(9, 54, 17))
#'
#' # A warning is given if any values in lsd1 or lsd2 are negative
#' # to ensure that this is what the user wants.
#' deb_subtract(lsd1 = c(56, 8, 5), lsd2 = c(-19, -5, -7))
#'
#' # Like deb_normalize(), deb_subtract can handle decimals
#' deb_subtract(lsd1 = c(56.85, 8.4, 5), lsd2 = c(19.25, 5, 7))
#'
#' # lsd1 and lsd2 can be lists of numerical vectors
#' # Each vector is lsd1 will be subtracted from the
#' # corresponding vector in lsd2.
#' # Returns tibble with 2 rows
#' deb_subtract(lsd1 = list(c(56, 8, 5), c(27, 12, 4)),
#'              lsd2 = list(c(19, 5, 7), c(6, 3, 2)))
#'
#' # Or a list with two numeric vectors
#' deb_subtract(lsd1 = list(c(56, 8, 5), c(27, 12, 4)),
#'              lsd2 = list(c(19, 5, 7), c(6, 3, 2)),
#'              vector = TRUE)
#'
#' @export

deb_subtract <- function(lsd1, lsd2, round = 3, vector = FALSE) {
  # vectorize
  if (is.list(lsd1) & is.list(lsd2)) {
    lsd_list <- c(lsd1, lsd2)
    if (identical(purrr::map_dbl(lsd_list, length), rep(3, length(lsd_list))) == FALSE) {
      stop(call. = FALSE, "The vectors in lsd1 and lsd2 must all be length 3")
    }
    if (vector == TRUE) {
      return(purrr::map2(lsd1, lsd2, ~ deb_subtract(.x, .y, round, vector)))
    } else {
      return(dplyr::bind_rows(purrr::map2(lsd1, lsd2, ~ deb_subtract(.x, .y, round, vector))))
    }
  }

  # Checks
  lsds <- c(lsd1, lsd2)
  if (is.list(c(lsd1, lsd2)) & typeof(lsd1) != typeof(lsd2)) {
    stop(call. = FALSE, "lsd1 and lsd2 must either be both vectors or lists")
  }
  if (!is.numeric(lsds)) {
    stop(call. = FALSE, "lsd1 and lsd2 must be numeric vectors or lists of numeric vectors")
  }
  if (length(lsds) != 6) {
    stop(call. = FALSE, "lsd1 and lsd2 must be vectors of length 3")
  }
  if (any(lsds < 0)) {
    warning(call. = FALSE, "Negative values are present in lsd1 or lsd2")
  }

  deb_normalize(lsd1[1] - lsd2[1], lsd1[2] - lsd2[2], lsd1[3] - lsd2[3],
                round = round, vector = vector)
}
