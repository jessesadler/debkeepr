context("test-checks.R")

suppressPackageStartupMessages(library(tibble))

x <- c(5, 84, 53)
lsd_list <- deb_lsd(l = c(35, 12, 1),
                    s = c(10, 16, 19),
                    d = c(9, 5, 11))
with_null <- lsd_list
with_null[3] <- list(NULL)

tbl <- tibble(l = c(35, 12, 1),
              s = c(10, 16, 19),
              d = c(9, 5, 11))
tbl_lsd <- tibble(l = c(35, 12, 1),
                  s = c(10, 16, 19),
                  d = c(9, 5, 11),
                  lsd = lsd_list)

character_df <- data.frame(ch = c("hello", "goodbye"),
                           n1 = c(6, 7),
                           n2 = c(3, 4))
column_names <- data.frame(pounds = c(37, -13, 31, 16),
                           shillings = c(17, -3, 6, 13),
                           pence = c(5, -1, 2.6, 1))
transactions <- data.frame(credit = sample(letters[1:5]),
                           debit = sample(letters[1:5]))

## null check ##
test_that("null check turns null to NA vector", {
  expect_equal(null_check(x), x)
  expect_equal(null_check(lsd_list), lsd_list)
  expect_equal(null_check(with_null), deb_lsd(l = c(35, 12, NA),
                                              s = c(10, 16, NA),
                                              d = c(9, 5, NA)))
})

## lsd check ##
test_that("non-vector is an error", {
  expect_error(deb_normalize(tbl),
               paste("lsd must be a list of class lsd, or an object that can be coerced to this class,",
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
  expect_error(deb_normalize(x, bases = as.numeric(c(NA, NA))),
               "Neither of the values in bases can be NA")
  expect_error(deb_normalize(x, bases = c(20, 0)),
               "Neither of the values in bases can be 0")
  expect_error(deb_normalize(x, bases = c(0, 0)),
               "Neither of the values in bases can be 0")
  expect_error(deb_normalize(x, bases = c(20, -12)),
               "The values in bases must both be positive")
  expect_error(deb_normalize(x, bases = c(-20, -12)),
               "The values in bases must both be positive")
})

## Error messages from round_check ##
test_that("round check works", {
  expect_error(deb_normalize(x, round = "hello"),
               "round must be numeric")
  expect_error(deb_normalize(x, round = c(1, 3)),
               "round must be a numeric vector of length 1")
})


# Checks for data frames #
test_that("lsd_column_check work", {
  expect_error(deb_lsd_gather(x, l, s, d),
               "df must be a data frame")
  expect_error(deb_lsd_gather(tbl, pounds, shillings, pence),
               paste("Column names for l, s, and d must be provided if the",
                     "default names of l, s, and d are not present in the data frame",
                     sep = "\n"))
  expect_error(deb_lsd_gather(character_df, ch, n1, n2),
               "l must be a numeric variable")
  expect_error(deb_lsd_gather(character_df, n1, ch, n2),
               "s must be a numeric variable")
  expect_error(deb_lsd_gather(character_df, n1, n2, ch),
               "d must be a numeric variable")
})

test_that("suffix check", {
  expect_error(deb_lsd_spread(tbl_lsd, replace = "a"),
               "replace must be either TRUE or FALSE")
  expect_error(deb_lsd_spread(tbl_lsd, replace = c(TRUE, FALSE)),
               "replace must be a logical vector of length 1")
  expect_error(deb_lsd_spread(tbl_lsd, replace = FALSE, suffix = 1),
               "suffix must be a character vector")
  expect_error(deb_lsd_spread(tbl_lsd, replace = FALSE, suffix = c(".1", ".2")),
               "suffix must be a character vector of length 1")
  expect_error(deb_lsd_spread(tbl_lsd, replace = FALSE, suffix = ""),
               paste("suffix cannot be an empty character vector.",
                     "Use replace = TRUE to replace the original variables where this is an option in the function",
                     sep = "\n"))
})
