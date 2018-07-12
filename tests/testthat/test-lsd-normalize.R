context("test-lsd-normalize.R")

## Normalization functions, as well as basic checks for
## lsd vector and lsd data frame

ex_vector <- c(5, 84, 53)
negative_vector <- c(-5, -84, -53)
decimal_vector <- c(5.875, 84.325, 55)
mix_vector <- c(5, -67, -35)
ex_list <- list(c(35, 50, 89),
                c(-10, -48, -181),
                c(26.875, 84.365, 55),
                c(12, 76, 205))
list_answer <- list(c(l = 37, s = 17, d = 5),
                    c(l = -13, s = -3, d = -1),
                    c(l = 31, s = 6, d = 5.38),
                    c(l = 16, s = 13, d = 1))
list_ratio <- list(c(l = 37, s = 15, d = 9),
                   c(l = -12, s = -19, d = -5),
                   c(l = 31, s = 5, d = 4.84),
                   c(l = 16, s = 8, d = 13))
transactions <- data.frame(credit = sample(letters[1:4]),
                           debit = sample(letters[1:4]))

ex_df <- data.frame(l = c(35, -10, 26.875, 12),
                    s = c(50, -48, 84.365, 76),
                    d = c(89, -181, 55, 205))
df_answer <- data.frame(l = c(37, -13, 31, 16),
                        s = c(17, -3, 6, 13),
                        d = c(5, -1, 5.38, 1))
df_ratio <- data.frame(l = c(37, -12, 31, 16),
                       s = c(15, -19, 5, 8),
                       d = c(9, -5, 4.84, 13))
df_answer2 <- data.frame(l.1 = c(37, -13, 31, 16),
                         s.1 = c(17, -3, 6, 13),
                         d.1 = c(5, -1, 5.38, 1))
character_df <- data.frame(ch = c("hello", "goodbye"),
                           n1 = c(6, 7),
                           n2 = c(3, 4))
column_names <- data.frame(pounds = c(37, -13, 31, 16),
                           shillings = c(17, -3, 6, 13),
                           pence = c(5, -1, 2.6, 1))
na_df <- data.frame(l = c(35, -10, 26.875, 12),
                    s = c(50, -48, 84.365, 76),
                    d = c(89, -181, 55, NA))
na_answer <- data.frame(l = c(37, -13, 31, NA),
                        s = c(17, -3, 6, NA),
                        d = c(5, -1, 5.38, NA))

## Error messages from lsd_check ##
test_that("non-vector is an error", {
  expect_error(deb_normalize(ex_df),
               "lsd must be either a numeric vector or list of numeric vectors")
})

test_that("non-numeric is an error", {
  expect_error(deb_normalize(c("hello", "goodbye")),
               "lsd must be a numeric vector")
  expect_error(deb_normalize(list(c("hello", "goodbye"), c(TRUE, FALSE))),
               "lsd must be a list of numeric vectors")
})

test_that("length of lsd is 3", {
  expect_error(deb_normalize(c(10, 9, 3, 5)),
               paste("lsd must be a vector of length of 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
  expect_error(deb_normalize(list(c(10, 9, 3, 5), c(6, 3), c(4, 6, 8))),
               paste("lsd must be a list of numeric vectors of length 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
})

test_that("lsd_bases checks work", {
  expect_error(deb_normalize(ex_vector, lsd_bases = NULL),
               "lsd_bases must be a numeric vector")
  expect_error(deb_normalize(ex_vector, lsd_bases = c("t", "r")),
               "lsd_bases must be a numeric vector")
  expect_error(deb_normalize(ex_vector, lsd_bases = c(8, 2, 4)),
               "lsd_bases must be a numeric vector of length of 2")
  expect_error(deb_normalize(ex_vector, lsd_bases = 8),
               "lsd_bases must be a numeric vector of length of 2")
  expect_error(deb_normalize(ex_vector, lsd_bases = c(20, 0)),
               "Neither of the values in lsd_bases can be 0")
  expect_error(deb_normalize(ex_vector, lsd_bases = c(0, 0)),
               "Neither of the values in lsd_bases can be 0")
  expect_error(deb_normalize(ex_vector, lsd_bases = c(20, -12)),
               "The values in lsd_bases must both be positive")
  expect_error(deb_normalize(ex_vector, lsd_bases = c(-20, -12)),
               "The values in lsd_bases must both be positive")

  expect_error(deb_normalize_df(ex_df, lsd_bases = NULL),
               "lsd_bases must be a numeric vector")
  expect_error(deb_normalize_df(ex_df, lsd_bases = c(20, 0)),
               "Neither of the values in lsd_bases can be 0")
  expect_error(deb_normalize_df(ex_df, lsd_bases = c(20, -12)),
               "The values in lsd_bases must both be positive")
})

## lsd_decimal ##
test_that("lsd_decimal_check", {
  expect_equal(lsd_decimal_check(decimal_vector, lsd_bases = c(20, 12)), c(5, 101, 64.9))
  expect_equal(lsd_decimal_check(-decimal_vector, lsd_bases = c(20, 12)), c(5, 101, 64.9))
  expect_equal(lsd_decimal_check(c(8.5, 7, 0), lsd_bases = c(20, 12)), c(8, 17, 0))
  expect_equal(lsd_decimal_check(c(8.5, 7, 0), lsd_bases = c(16, 12)), c(8, 15, 0))
  expect_equal(lsd_decimal_check(c(8, 29.875, 30), lsd_bases = c(20, 12)), c(8, 29, 40.5))
  expect_equal(lsd_decimal_check(c(NA, 3, 4)), c(NA, NA, NA))
})

## Normalization of lsd ##
test_that("it goes together in deb_normalize", {
  expect_equal(deb_normalize(ex_vector), c(l = 9, s = 8, d = 5))
  expect_equal(deb_normalize(negative_vector), c(l = -9, s = -8, d = -5))
  expect_equal(deb_normalize(decimal_vector), c(l = 10, s = 6, d = 4.9))
  expect_equal(deb_normalize(mix_vector), c(l = 1, s = 10, d = 1))
  expect_equal(deb_normalize(c(NA, 4, 5)),
               stats::setNames(as.numeric(c(NA, NA, NA)), c("l", "s", "d")))
})

## Vectorization ##
test_that("vectorization works", {
  expect_equal(is.list(deb_normalize(ex_list)), TRUE)
  expect_equal(length(deb_normalize(ex_list)), 4)
  expect_equal(deb_normalize(ex_list), list_answer)
  expect_equal(deb_normalize(ex_list, lsd_bases = c(20, 16)), list_ratio)
})

## lsd_basess ##
test_that("different lsd_basess work", {
  expect_equal(deb_normalize(ex_vector, lsd_bases = c(20, 12)),
               c(l = 9, s = 8, d = 5))
  expect_equal(deb_normalize(ex_vector, lsd_bases = c(20, 16)),
               c(l = 9, s = 7, d = 5))
  expect_equal(deb_normalize(negative_vector, lsd_bases = c(8, 16)),
               c(l = -15, s = -7, d = -5))
})

# Checks for data frames #
test_that("lsd_column_check work", {
  expect_error(deb_normalize_df(df, l, s, d),
               "df must be a data frame or data-frame like object")
  expect_error(deb_normalize_df(ex_df, pounds, shillings, pence),
               paste("Column names for l, s, and d must be provided if the",
                     "default names of l, s, and d are not present in the data frame",
                     sep = "\n"))
  expect_error(deb_normalize_df(character_df, ch, n1, n2),
               "l must be a numeric variable")
  expect_error(deb_normalize_df(character_df, n1, ch, n2),
               "s must be a numeric variable")
  expect_error(deb_normalize_df(character_df, n1, n2, ch),
               "d must be a numeric variable")
})

test_that("suffix check", {
  expect_error(deb_normalize_df(ex_df, replace = FALSE, suffix = 1),
               "suffix must be a character vector")
  expect_error(deb_normalize_df(ex_df, replace = FALSE, suffix = c(".1", ".2")),
               "suffix must be a character vector of length 1")
  expect_error(deb_normalize_df(ex_df, replace = FALSE, suffix = ""),
               paste("suffix cannot be an empty character vector.",
                     "To keep the same variable names and replace the original variables use replace = TRUE",
                     sep = "\n"))
})

test_that("lsd_column_names works",{
  expect_equal(names(deb_normalize_df(ex_df, replace = FALSE)),
               c("l", "s", "d", "l.1", "s.1", "d.1"))
  expect_equal(names(deb_normalize_df(ex_df, replace = FALSE, suffix = "_x")),
               c("l", "s", "d", "l_x", "s_x", "d_x"))
  expect_equal(names(deb_normalize_df(column_names, l = pounds, s = shillings, d = pence,
                                      replace = FALSE, suffix = "_x")),
               c("pounds", "shillings", "pence", "pounds_x", "shillings_x", "pence_x"))
  # Replace works with other columns present
  expect_equal(names(deb_normalize_df(cbind(transactions, ex_df), replace = TRUE)),
                     c("credit", "debit", "l", "s", "d"))
})

# Normalization #

test_that("normalization_df works", {
  expect_equal(deb_normalize_df(ex_df, replace = TRUE), df_answer)
  expect_equal(deb_normalize_df(ex_df, replace = TRUE, lsd_bases = c(20, 16)), df_ratio)
  expect_equal(deb_normalize_df(ex_df, replace = FALSE), cbind(ex_df, df_answer2))
  expect_equal(deb_normalize_df(na_df, replace = TRUE), na_answer)
})
