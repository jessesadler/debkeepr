context("test-lsd-interest.R")

suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))

x <- c(10, 3, 2)
y <- c(20, 5, 8)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
list1_b1 <- to_lsd(list1, b1)
list2_b2 <- to_lsd(list(x, y), b2)

tbl_b1 <- tibble(lsd = list1_b1)
tbl_b2 <- tibble(lsd = list2_b2)

test_that("interest checks work", {
  expect_error(deb_interest(x, interest = "t"),
               "interest must be numeric")
  expect_error(deb_interest(x, interest = c(0.6, 0.5)),
               "interest must be a numeric vector of length 1")
  expect_error(deb_interest(x, duration = "t"),
               "duration must be numeric")
  expect_error(deb_interest(x, duration = c(0.6, 0.5)),
               "duration must be a numeric vector of length 1")
  expect_error(deb_interest(x, with_principal = "t"),
               "with_principal must be logical, either TRUE or FALSE")
})

test_that("interest calculation works", {
  expect_equal(deb_interest(x),
               to_lsd(c(10, 15, 10.375), b1))
  expect_equal(deb_interest(x, with_principal = FALSE),
               deb_multiply(x, 1/16), b1)
  expect_equal(deb_interest(x, with_principal = FALSE, duration = 5),
               deb_multiply(x, 5/16), b1)
  expect_equal(deb_interest(x, round = 0),
               to_lsd(c(10, 15, 10), b1))
  expect_equal(deb_interest(x, interest = 0.10),
               to_lsd(c(11, 3, 5.8), b1))
  expect_equal(deb_interest(x, bases = b2),
               to_lsd(c(11, 0, 5.125), b2))
})

test_that("vectorization works", {
  expect_equal(deb_interest(list1),
               to_lsd(list(c(32, 8, 11.0625),
                           c(12, 8, 8.35),
                           c(-28, -5, -0.875)), b1))
  expect_equal(deb_interest(list1, with_principal = FALSE),
               to_lsd(list(c(1, 18, 2.0625),
                           c(0, 14, 7.55),
                           c(-1, -13, -2.875)), b1))
  expect_equal(deb_interest(list1, bases = b2, round = 0),
               to_lsd(list(c(33, 2, 4),
                           c(13, 7, 11),
                           c(-29, -1, -6)), b2))
})

test_that("deb_interest works with lsd objects", {
  expect_identical(deb_interest(x_b2),
                   deb_interest(x, bases = b2))
  expect_identical(deb_interest(list1_b1),
                   deb_interest(list1, bases = b1))
  expect_identical(deb_interest(list2_b2, round = 0),
                   deb_interest(list(x, y), bases = b2, round = 0))
})

test_that("deb_interest works with lsd column", {
  # mutated column is lsd
  expect_s3_class(mutate(tbl_b1, lsd = deb_interest(lsd))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_b2, lsd = deb_interest(lsd))$lsd),
               c(s = 8, d = 16))

  # mutated column is same as normal deb_interest
  expect_identical(mutate(tbl_b1, lsd = deb_interest(lsd, duration = 5))$lsd,
                   deb_interest(list1_b1, duration = 5))
  expect_identical(mutate(tbl_b2, lsd = deb_interest(lsd, interest = 0.08)),
                   tibble(lsd = deb_interest(list2_b2, interest = 0.08)))
})
