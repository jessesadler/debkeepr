context("test-checks.R")

x <- c(5, 84, 53)
ex_df <- data.frame(l = c(35, -10, 26.875, 12, 1),
                    s = c(50, -48, 84.365, 76, 19),
                    d = c(89, -181, 55, 205, 11))
character_df <- data.frame(ch = c("hello", "goodbye"),
                           n1 = c(6, 7),
                           n2 = c(3, 4))
column_names <- data.frame(pounds = c(37, -13, 31, 16),
                           shillings = c(17, -3, 6, 13),
                           pence = c(5, -1, 2.6, 1))
transactions <- data.frame(credit = sample(letters[1:5]),
                           debit = sample(letters[1:5]))
## lsd check ##
test_that("non-vector is an error", {
  expect_error(deb_normalize(ex_df),
               paste("lsd must be a list of class lsd, or an object that can be coerced to these classes,",
                     "       namely a numeric vector of length 3 or a list of such vectors.",
                     sep = "\n"))
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

## Error messages from bases_check ##
test_that("bases checks work", {
  expect_error(deb_normalize(x, bases = NULL),
               "bases must be a numeric vector")
  expect_error(deb_normalize(x, bases = c("t", "r")),
               "bases must be a numeric vector")
  expect_error(deb_normalize(x, bases = c(8, 2, 4)),
               "bases must be a numeric vector of length of 2")
  expect_error(deb_normalize(x, bases = 8),
               "bases must be a numeric vector of length of 2")
  expect_error(deb_normalize(x, bases = c(20, 0)),
               "Neither of the values in bases can be 0")
  expect_error(deb_normalize(x, bases = c(0, 0)),
               "Neither of the values in bases can be 0")
  expect_error(deb_normalize(x, bases = c(20, -12)),
               "The values in bases must both be positive")
  expect_error(deb_normalize(x, bases = c(-20, -12)),
               "The values in bases must both be positive")

  expect_error(deb_normalize_df(ex_df, bases = NULL),
               "bases must be a numeric vector")
  expect_error(deb_normalize_df(ex_df, bases = c(20, 0)),
               "Neither of the values in bases can be 0")
  expect_error(deb_normalize_df(ex_df, bases = c(20, -12)),
               "The values in bases must both be positive")
})

## Error messages from round_check ##
test_that("round check works", {
  expect_error(deb_normalize(x, round = "hello"),
               "round must be numeric")
  expect_error(deb_normalize(x, round = c(1, 3)),
               "round must be a numeric vector of length 1")

  expect_error(deb_normalize_df(ex_df, round = "hello"),
               "round must be numeric")
  expect_error(deb_normalize_df(ex_df, round = c(1, 3)),
               "round must be a numeric vector of length 1")
})


# Checks for data frames #
test_that("lsd_column_check work", {
  expect_error(deb_normalize_df(x, l, s, d),
               "df must be a data frame")
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
  expect_error(deb_normalize_df(ex_df, replace = "a"),
               "replace must be either TRUE or FALSE")
  expect_error(deb_normalize_df(ex_df, replace = c(TRUE, FALSE)),
               "replace must be a logical vector of length 1")
  expect_error(deb_normalize_df(ex_df, replace = FALSE, suffix = 1),
               "suffix must be a character vector")
  expect_error(deb_normalize_df(ex_df, replace = FALSE, suffix = c(".1", ".2")),
               "suffix must be a character vector of length 1")
  expect_error(deb_normalize_df(ex_df, replace = FALSE, suffix = ""),
               paste("suffix cannot be an empty character vector.",
                     "Use replace = TRUE to replace the original variables where this is an option in the function",
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
