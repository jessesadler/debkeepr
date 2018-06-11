context("test-lsd-interest.R")

suppressPackageStartupMessages(library(tibble))

test_that("interest calculation works", {
  # with principal
  expect_equal(deb_interest(10, 14, 5, vector = TRUE),
               c(l = 11, s = 7, d = 9.812))
  # negative lsd
  expect_equal(deb_interest(-10, -14, -5, vector = TRUE),
               c(l = -11, s = -7, d = -9.812))
  # without principal
  expect_equal(deb_interest(10, 14, 5, vector = TRUE, with_principal = FALSE),
               c(l = 0, s = 13, d = 4.812))
  # duration
  expect_equal(deb_interest(10, 14, 5, vector = TRUE, with_principal = FALSE, duration = 5),
               deb_normalize(0, 5 * 13, 5 * 4.8125, vector = TRUE))
  # interest
  expect_equal(deb_interest(10, 14, 5, vector = TRUE, with_principal = FALSE, interest = 0.125),
               deb_normalize(0, 2 * 13, 2 * 4.8125, vector = TRUE))
  # round
  expect_equal(deb_interest(10, 14, 5, vector = TRUE, round = 4),
               c(l = 11, s = 7, d = 9.8125))
  expect_equal(deb_interest(10, 14, 5, vector = TRUE, round = 0),
               c(l = 11, s = 7, d = 10))
})

test_that("vectorization works", {
  expect_equal(deb_interest(c(10, 5), c(14, 8), c(5, 11), vector = TRUE),
                            list(c(l = 11, s = 7, d = 9.812),
                                 c(l = 5, s = 15, d = 8.688)))
  expect_equal(deb_interest(c(10, 5), c(14, 8), c(5, 11)),
               tibble::tibble(l = c(11, 5),
                              s = c(7, 15),
                              d = c(9.812, 8.688))
               )
})

test_that("checks work", {
  expect_error(deb_interest(10, 14, 5, interest = "t"),
               "interest must be numeric")
  expect_error(deb_interest(10, 14, 5, interest = c(0.6, 0.5)),
               "interest must be a numeric vector of length 1")
  expect_error(deb_interest(10, 14, 5, duration = "t"),
               "duration must be numeric")
  expect_error(deb_interest(10, 14, 5, duration = c(0.6, 0.5)),
               "duration must be a numeric vector of length 1")
  expect_error(deb_interest(10, 14, 5, with_principal = "t"),
               "with_principal must be logical, either TRUE or FALSE")
})

example1 <- tibble::tibble(l = 10, s = 14, d = 5)
example2 <- tibble::tibble(pounds = c(3, 5, 6, 2),
                           shillings = c(10, 18, 11, 16),
                           pence = c(9, 11, 10, 5))

test_that("deb_interest_mutate works", {
  expect_equal(deb_interest_mutate(example1),
               tibble::tibble(l = 10, s = 14, d = 5,
                              l.interest = 11, s.interest = 7, d.interest = 9.812))
  expect_equal(deb_interest_mutate(example1, with_principal = FALSE),
               tibble::tibble(l = 10, s = 14, d = 5,
                              l.interest = 0, s.interest = 13, d.interest = 4.812))
  expect_equal(deb_interest_mutate(example1, round = 0),
               tibble::tibble(l = 10, s = 14, d = 5,
                              l.interest = 11, s.interest = 7, d.interest = 10))
  expect_equal(ncol(deb_interest_mutate(example1)), 6)
  expect_equal(nrow(deb_interest_mutate(example2, l = pounds, s = shillings, d = pence)), 4)
})

test_that("naming for deb_interest_mutate works", {
  expect_equal(names(deb_interest_mutate(example1)),
               c("l", "s", "d", "l.interest", "s.interest", "d.interest"))
  expect_equal(names(deb_interest_mutate(example1, suffix = "_x")),
               c("l", "s", "d", "l_x", "s_x", "d_x"))
  expect_equal(names(deb_interest_mutate(example2, l = pounds, s = shillings, d = pence, suffix = "_x")),
               c("pounds", "shillings", "pence", "pounds_x", "shillings_x", "pence_x"))
  # Replace works
  expect_equal(names(deb_interest_mutate(example2, l = pounds, s = shillings, d = pence, replace = TRUE)),
               c("pounds", "shillings", "pence"))
})

test_that("checks work for mutate", {
  expect_error(deb_interest_mutate(example2), "Column names for l, s, and d must be provided if the
         default names of l, s, and d are not present in the data frame")
  expect_error(deb_interest_mutate(example1, suffix = 5), "suffix must be a character vector")
  expect_error(deb_interest_mutate(example1, suffix = c("hello", "goodbye")),
               "suffix must be a character vector of length 1")
})
