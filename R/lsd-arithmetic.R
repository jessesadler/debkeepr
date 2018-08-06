## lsd Arithmetic ##

#' Multiplication of pounds, shillings, and pence
#'
#' Multiply pounds, shillings, and pence by a given multiplier (`x`).
#' The result is normalized to standard bases for shillings and pence
#' according to the `bases` argument. This is a wrapper around
#' [deb_normalize()].
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param x Multiplier. The value by which the given pounds, shillings, and
#'   pence will be multiplied. Should be a numeric vector of length 1.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Multiply pounds, shillings, and pence by 5
#' deb_multiply(lsd = c(5, 15, 8), x = 5)
#'
#' # Calculate commission of 3% of sales of £3095 17s 6d
#' deb_multiply(lsd = c(3095, 17, 6), x = 0.03)
#'
#' # The l, s, and d values do not have to be normalized
#' deb_multiply(lsd = c(10, 38, 65), x = 5)
#'
#' # Like deb_normalize(), deb_multiply() can handle negative numbers
#' deb_multiply(lsd = c(-5, -15, -8), x = 5)
#' deb_multiply(lsd = c(5, 15, 8), x = -5)
#'
#' # Or decimals
#' deb_multiply(lsd = c(5.625, 15.35, 8), x = 5)
#'
#' # Multiply a list of lsd vectors by a multiplier
#' # This returns a list of named lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#'
#' deb_multiply(lsd = lsd_list, x = 5)
#'
#' @export

deb_multiply <- function(lsd, x, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map(lsd, ~ deb_multiply(., x,
                                          bases = bases)))
  }

  arithmetic_check(x)

  deb_normalize(lsd = lsd * x, bases = bases)
}


#' Multiplication of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to multiply pounds, shillings, and pence by a given
#' multiplier (`x`). The values are returned in the form of three new variables
#' representing the calculated pounds, shillings and pence.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize_df
#' @inheritParams deb_multiply
#' @param replace Logical (default `FALSE`): when `TRUE` the new pounds,
#'   shillings, and pence variables will replace the original ones.
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
#' deb_multiply_mutate(example, l, s, d, x = 5)
#'
#' # Change the suffix added to the new variables
#' deb_multiply_mutate(example, l, s, d,
#'                     x = 5,
#'                     suffix = ".x5")
#'
#' # Replace the existing pounds, shillings, and pence
#' # with multiplied values
#' deb_multiply_mutate(example, l, s, d,
#'                     x = 5,
#'                     replace = TRUE)
#'
#' @export

deb_multiply_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                x,
                                bases = c(20, 12),
                                replace = FALSE,
                                suffix = ".1") {
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
                     lsd_names = lsd_names,
                     replace = replace,
                     bases = bases)
}

#' Division of pounds, shillings, and pence
#'
#' Divide pounds, shillings, and pence by a given divisor (`x`).
#' The result is normalized to standard bases for shillings and pence
#' according to the `bases` argument. This is a wrapper around
#' [deb_normalize()].
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param x Divisor. The value by which the given pounds, shillings, and
#'   pence will be divided. Should be a numeric vector of length 1.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence.
#'
#' @examples
#' # Divide pounds, shillings, and pence by a divisor
#' deb_divide(lsd = c(63, 15, 8), x = 5)
#'
#' # The l, s, and d values do not have to be normalized
#' deb_divide(lsd = c(109, 38, 65), x = 8)
#'
#' # Like deb_normalize(), deb_divide() can handle negative numbers
#' deb_divide(lsd = c(-63, -15, -8), x = 5)
#' deb_divide(lsd = c(63, 15, 8), x = -5)
#'
#' # Or decimals
#' deb_divide(lsd = c(63.625, 15.35, 8), x = 5)
#'
#' # Divide a list of lsd vectors by a diviso
#' # This returns a list of named lsd values
#' lsd_list <- list(c(40, 5, 9), c(29, 7, 1), c(35, 6, 5))
#'
#' deb_divide(lsd = lsd_list, x = 5)
#'
#' @export

deb_divide <- function(lsd, x, bases = c(20, 12)) {
  if (is.list(lsd) == TRUE) {
    return(purrr::map(lsd, ~ deb_divide(., x, bases = bases)))
  }

  arithmetic_check(x)

  deb_normalize(lsd / x, bases = bases)
}


#' Division of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to divide pounds, shillings, and pence by a given
#' divisor (`x`). The value is returned in the form of three new variables
#' representing the calculated pounds, shillings and pence.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize_df
#' @inheritParams deb_divide
#' @param replace Logical (default `FALSE`): when `TRUE` the new pounds,
#'   shillings, and pence variables will replace the original ones.
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
#' deb_divide_mutate(example, l, s, d, x = 5)
#'
#' # Change the suffix added to the new variables
#' deb_divide_mutate(example, l, s, d,
#'                   x = 5,
#'                   suffix = ".div5")
#'
#' # Replace the existing pounds, shillings, and pence
#' # with divided values
#' deb_divide_mutate(example, l, s, d,
#'                   x = 5,
#'                   replace = TRUE)
#'
#' @export

deb_divide_mutate <- function(df,
                              l = l,
                              s = s,
                              d = d,
                              x,
                              bases = c(20, 12),
                              replace = FALSE,
                              suffix = ".1") {
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
                     lsd_names = lsd_names,
                     replace = replace,
                     bases = bases)
}

#' Add two values of pounds, shillings, and pence
#'
#' Add and normalize two values of pounds, shillings, and pence that are
#' given in the form of either numeric vectors or lists of numeric vectors
#' (`lsd1` and `lsd2`). If `lsd1` and `lsd2` are lists of different lengths
#' or one is a vector, the shorter list will be recycled.
#'
#' See [deb_sum()] to get the sum of multiple numeric vectors and/or lists of
#' numeric vectors, reducing the inputs to a single numeric vector.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param lsd1,lsd2 Numeric vectors of length 3 or lists of numeric vectors of
#'   length 3. The first position of the vector represents the pounds value or
#'   l. The second position represents the shillings value or s. And the third
#'   position represents the pence value or d.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the resulting lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Add pounds, shillings, and pence
#' deb_add(lsd1 = c(56, 8, 5), lsd2 = c(19, 5, 7))
#'
#' # This is equivalent to using deb_sum
#' deb_sum(c(56, 8, 5), c(19, 5, 7))
#'
#' # deb_add is useful when one or both of
#' # lsd1 and lsd2 are lists of numerical vectors
#'
#' # With two lists of the same length each vector in lsd1
#' # will be added to the corresponding vector in lsd2.
#' deb_add(lsd1 = list(c(56, 8, 5), c(27, 12, 4)),
#'         lsd2 = list(c(19, 5, 7), c(6, 3, 2)))
#'
#' # As opposed to using deb_normalize, which will not work
#' \dontrun{
#' deb_normalize(list(c(56, 8, 5), c(27, 12, 4)) + list(c(19, 5, 7), c(6, 3, 2)))
#' }
#'
#' # If lsd1 and lsd2 are lists of different lengths
#' # or one is a vector, the shorter argument is recycled.
#' deb_add(lsd1 = list(c(56, 8, 5),
#'                     c(27, 12, 4),
#'                     c(78, 14, 11)),
#'         lsd2 = c(6, 18, 10))
#'
#' @export

deb_add <- function(lsd1, lsd2, bases = c(20, 12)) {

  if (is.list(lsd1) | is.list(lsd2) == TRUE) {
    # ensure that both are lists to recycle correctly
    if (is.list(lsd1) == FALSE) {
      lsd1 <- list(lsd1)
    }
    if (is.list(lsd2) == FALSE) {
      lsd2 <- list(lsd2)
    }
    purrr::map2(lsd1, lsd2, ~ deb_normalize(lsd = .x + .y,
                                            bases = bases))
  } else {
    deb_normalize(lsd1 + lsd2, bases = bases)
  }
}

#' Addition of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to add pounds, shillings, and pence by a given
#' pounds, shillings, and pence value. The values are returned in the form of
#' three new variables representing the calculated pounds, shillings and pence.
#'
#' See [deb_sum_df()] to get a sum of the lsd values in a data frame.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize_df
#' @param lsd Numeric vector of length 3 that represents the lsd value to be
#'   added to the `l`, `s`, and `d` variables. The first position of the vector
#'   represents the pounds value or l. The second position represents the
#'   shillings value or s. And the third position represents the pence value
#'   or d.
#' @param replace Logical (default `FALSE`): when `TRUE` the new pounds,
#'   shillings, and pence variables will replace the original ones.
#' @param suffix Suffix added to the column names for the pounds, shillings, and
#'   pence columns representing the added values so that they are
#'   distinguished from the original pounds, shillings, and pence columns.
#'   Default is ".1". Should be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the added values.
#'
#' @examples
#' # Add a data frame of lsd values by £5 15s. 8d
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_add_mutate(example, l, s, d, lsd = c(5, 15, 8))
#'
#' # Replace the existing pounds, shillings, and pence
#' # with added values
#' deb_add_mutate(example, l, s, d,
#'                lsd = c(5, 15, 8),
#'                replace = TRUE)
#'
#' @export

deb_add_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                lsd,
                                bases = c(20, 12),
                                replace = FALSE,
                                suffix = ".1") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  if (!is.numeric(lsd)) {
    stop(call. = FALSE, "lsd must be a numeric vector")
  }
  if (length(lsd) != 3) {
    stop(call. = FALSE, paste("lsd must be a vector of length of 3.",
                              "There must be a value for pounds, shillings, and pence.",
                              sep = "\n"))
  }
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  lsd_mutate_columns(df,
                     !! l + lsd[1],
                     !! s + lsd[2],
                     !! d + lsd[3],
                     lsd_names = lsd_names,
                     replace = replace,
                     bases = bases)
}

#' Subtract two values of pounds, shillings, and pence
#'
#' Subtract and normalize two values of pounds, shillings, and pence
#' that are given in the form of either numeric vectors or lists
#' of numeric vectors (`lsd1` and `lsd2`). This is a wrapper around
#' [deb_normalize()].
#'
#' If `lsd1` and `lsd2` are lists of different lengths or one is a vector,
#' the shorter list will be recycled.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize
#' @param lsd1,lsd2 Numeric vectors of length 3 or lists of numeric vectors of
#'   length 3. The first position of the vector represents the pounds value or
#'   l. The second position represents the shillings value or s. And the third
#'   position represents the pence value or d. lsd2 is the value or values to be
#'   subtracted from lsd1: `lsd1 - lsd2`.
#'
#' @return Returns either a named numeric vector of length 3 or a list of
#'   named numeric vectors representing the values of pounds, shillings,
#'   and pence. If the resulting lsd value is negative, the l, s, and d values
#'   will all be negative.
#'
#' @examples
#' # Subtract pounds, shillings, and pence
#' deb_subtract(lsd1 = c(56, 8, 5), lsd2 = c(19, 5, 7))
#'
#' # This is equivalent to using deb_normalize
#' deb_normalize(lsd = c(56, 8, 5) - c(19, 5, 7))
#'
#' # deb_subtract is useful when one or both of
#' # lsd1 and lsd2 are lists of numerical vectors
#'
#' # With two lists of the same length each vector in lsd1
#' # will be subtracted from the corresponding vector in lsd2.
#' deb_subtract(lsd1 = list(c(56, 8, 5), c(27, 12, 4)),
#'              lsd2 = list(c(19, 5, 7), c(6, 3, 2)))
#'
#' # As opposed to using deb_normalize, which will not work
#' \dontrun{
#' deb_normalize(list(c(56, 8, 5), c(27, 12, 4)) - list(c(19, 5, 7), c(6, 3, 2)))
#' }
#'
#' # If lsd1 and lsd2 are lists of different lengths
#' # or one is a vector, the shorter argument is recycled.
#' deb_subtract(lsd1 = list(c(56, 8, 5),
#'                          c(27, 12, 4),
#'                          c(78, 14, 11)),
#'              lsd2 = c(6, 18, 10))
#'
#' @export

deb_subtract <- function(lsd1, lsd2, bases = c(20, 12)) {

  if (is.list(lsd1) | is.list(lsd2) == TRUE) {
    # ensure that both are lists to recycle correctly
    if (is.list(lsd1) == FALSE) {
      lsd1 <- list(lsd1)
    }
    if (is.list(lsd2) == FALSE) {
      lsd2 <- list(lsd2)
    }
    purrr::map2(lsd1, lsd2, ~ deb_normalize(lsd = .x - .y,
                                                   bases = bases))
  } else {
    deb_normalize(lsd1 - lsd2, bases = bases)
  }
}

#' Subtraction of pounds, shillings, and pence in a data frame
#'
#' Uses [dplyr::mutate()] to subtract pounds, shillings, and pence by a given
#' pounds, shillings, and pence value. The values are returned in the form of
#' three new variables representing the calculated pounds, shillings and pence.
#'
#' @family lsd arithmetic functions
#'
#' @inheritParams deb_normalize_df
#' @param lsd Numeric vector of length 3 that represents the lsd value to be
#'   subtracted from the `l`, `s`, and `d` variables. The first position of the
#'   vector represents the pounds value or l. The second position represents
#'   the shillings value or s. And the third position represents the pence
#'   value or d.
#' @param suffix Suffix added to the column names for the pounds, shillings, and
#'   pence columns representing the subtracted values so that they are
#'   distinguished from the original pounds, shillings, and pence columns.
#'   Default is ".1". Should be a character vector of length 1.
#'
#' @return Returns a data frame with three new variables of pounds, shillings,
#'   and pence representing the subtracted values.
#'
#' @examples
#' # Subtract a data frame of lsd values by £5 15s. 8d
#' example <- data.frame(l = c(35, 10, 26, 12),
#'                       s = c(10, 18, 11, 16),
#'                       d = c(9, 11, 10, 5))
#' deb_subtract_mutate(example, l, s, d, lsd = c(5, 15, 8))
#'
#' # Replace the existing pounds, shillings, and pence
#' # with multiplied values
#' deb_subtract_mutate(example, l, s, d,
#'                     lsd = c(5, 15, 8),
#'                     replace = TRUE)
#'
#' @export

deb_subtract_mutate <- function(df,
                                l = l,
                                s = s,
                                d = d,
                                lsd,
                                bases = c(20, 12),
                                replace = FALSE,
                                suffix = ".1") {
  l <- rlang::enquo(l)
  s <- rlang::enquo(s)
  d <- rlang::enquo(d)

  # Checks
  if (!is.numeric(lsd)) {
    stop(call. = FALSE, "lsd must be a numeric vector")
  }
  if (length(lsd) != 3) {
    stop(call. = FALSE, paste("lsd must be a vector of length of 3.",
                              "There must be a value for pounds, shillings, and pence.",
                              sep = "\n"))
  }
  lsd_column_check(df, l, s, d)
  suffix <- suffix_check(suffix, replace)
  lsd_names <- lsd_column_names(df, l, s, d, suffix)

  lsd_mutate_columns(df,
                     !! l - lsd[1],
                     !! s - lsd[2],
                     !! d - lsd[3],
                     lsd_names = lsd_names,
                     replace = replace,
                     bases = bases)
}
