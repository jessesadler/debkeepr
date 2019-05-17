## Tests for checks.R ##

# suppressPackageStartupMessages(library(tibble))

# x <- c(5, 84, 53)
# lsd_list <- deb_lsd(l = c(35, 12, 1),
#                     s = c(10, 16, 19),
#                     d = c(9, 5, 11))
# with_null <- deb_as_lsd(list(c(5, 3, 4), c(4, 12, 4), NULL))
#
# tbl <- tibble(l = c(35, 12, 1),
#               s = c(10, 16, 19),
#               d = c(9, 5, 11))
# tbl_lsd <- tibble(l = c(35, 12, 1),
#                   s = c(10, 16, 19),
#                   d = c(9, 5, 11),
#                   lsd = lsd_list)
#
# character_df <- data.frame(ch = c("hello", "goodbye"),
#                            n1 = c(6, 7),
#                            n2 = c(3, 4))
# column_names <- data.frame(pounds = c(37, -13, 31, 16),
#                            shillings = c(17, -3, 6, 13),
#                            pence = c(5, -1, 2.6, 1))
# transactions <- data.frame(credit = sample(letters[1:5]),
#                            debit = sample(letters[1:5]))

## lsd check ##
test_that("non-numeric is an error", {
  expect_error(lsd_check("hello", 3, 4),
               "`l` must be a numeric vector")
  expect_error(lsd_check(3, "hello", 4),
               "`s` must be a numeric vector")
  expect_error(lsd_check(3, 4, "hello"),
               "`d` must be a numeric vector")
})

test_that("length of l, s, and d are same length, length 1, or length 0", {
  # Successful
  expect_invisible(lsd_check(l = 3, s = 4, d = 1))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = c(1, 3, 2)))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = 0))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = double()))

  # Errors
  expect_error(lsd_check(l = c(3, 5, 3),
                         s = c(4, 9),
                         d = 0),
               "`l`, `s`, and `d` must be vectors of equal length or length 1")
})

## bases check ##
test_that("bases is numeric vector of length 2", {
  # Successful
  expect_invisible(bases_check(c(20, 12)))
  expect_error(bases_check(NULL),
               "`bases` must be a numeric vector of length 2.")
  expect_error(bases_check(c("hello", "goodbye")),
               "`bases` must be a numeric vector of length 2.")
  expect_error(bases_check(1),
               "`bases` must be a numeric vector of length 2.")
  expect_error(bases_check(c(1, 3, 4)),
               "`bases` must be a numeric vector of length 2.")
})

test_that("bases does not have any missing values", {
  expect_error(bases_check(c(NA, 3)),
               "`bases` cannot be `NA`.")
  expect_error(bases_check(c(3, NA)),
               "`bases` cannot be `NA`.")
})

test_that("bases are natural numbers", {
  expect_error(bases_check(c(-12, -3)),
               "`bases` must be natural numbers greater than zero.")
  expect_error(bases_check(c(20, 0)),
               "`bases` must be natural numbers greater than zero.")
  expect_error(bases_check(c(20.5, 8.23)),
               "`bases` must be natural numbers greater than zero.")
})

## null check ##
# test_that("null check turns null to NA vector", {
#   expect_equal(null_check(x), x)
#   expect_equal(null_check(lsd_list), lsd_list)
#   expect_equal(null_check(with_null), deb_lsd(l = c(5, 4, NA),
#                                               s = c(3, 12, NA),
#                                               d = c(4, 4, NA)))
# })

# test_that("lsd_check allows lists with a NULL element", {
#   expect_silent(lsd_check(with_null))
# })

## Error messages from round_check ##
# test_that("round check works", {
#   expect_error(deb_normalize(x, round = "hello"),
#                "round must be numeric")
#   expect_error(deb_normalize(x, round = c(1, 3)),
#                "round must be a numeric vector of length 1")
# })


# Checks for data frames #
# test_that("lsd_column_check work", {
#   expect_error(deb_lsd_gather(x, l, s, d),
#                "df must be a data frame")
#   expect_error(deb_lsd_gather(tbl, pounds, shillings, pence),
#                paste("Column names for l, s, and d must be provided if the",
#                      "default names of l, s, and d are not present in the data frame",
#                      sep = "\n"))
#   expect_error(deb_lsd_gather(character_df, ch, n1, n2),
#                "l must be a numeric variable")
#   expect_error(deb_lsd_gather(character_df, n1, ch, n2),
#                "s must be a numeric variable")
#   expect_error(deb_lsd_gather(character_df, n1, n2, ch),
#                "d must be a numeric variable")
# })

# test_that("suffix check", {
#   expect_error(deb_lsd_spread(tbl_lsd, replace = "a"),
#                "replace must be either TRUE or FALSE")
#   expect_error(deb_lsd_spread(tbl_lsd, replace = c(TRUE, FALSE)),
#                "replace must be a logical vector of length 1")
#   expect_error(deb_lsd_spread(tbl_lsd, replace = FALSE, suffix = 1),
#                "suffix must be a character vector")
#   expect_error(deb_lsd_spread(tbl_lsd, replace = FALSE, suffix = c(".1", ".2")),
#                "suffix must be a character vector of length 1")
#   expect_error(deb_lsd_spread(tbl_lsd, replace = FALSE, suffix = ""),
#                paste("suffix cannot be an empty character vector.",
#                      "Use replace = TRUE to replace the original variables where this is an option in the function",
#                      sep = "\n"))
# })
