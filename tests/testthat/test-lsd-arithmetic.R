context("test-lsd-arithmetic.R")

suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))

x <- c(10, 3, 2)
y <- c(20, 5, 8)
neg <- c(-8, -16, -6)
dec <- c(5.85, 17.35, 10)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
list2 <- list(x, y, dec)
list1_b1 <- to_lsd(list1, b1)
list2_b2 <- to_lsd(list2, b2)

tbl_b1 <- tibble(lsd = list1_b1)
tbl_b2 <- tibble(lsd = list2_b2)

# Checks
test_that("arithmetic checks work", {
  expect_error(deb_multiply(c(9, 3, 6), x = "d"),
               "x must be a numeric vector")
  expect_error(deb_divide(c(9, 3, 6), x = "d"),
               "x must be a numeric vector")
  expect_error(deb_multiply(c(9, 3, 6), x = c(3, 5)),
               "x must be a numeric vector of length 1")
  expect_error(deb_divide(c(9, 3, 6), x = c(3, 5)),
               "x must be a numeric vector of length 1")
})

# Multiplication
test_that("lsd multiplication works", {
  expect_equal(deb_multiply(x, x = 3),
               to_lsd(c(30, 9, 6), b1))
  expect_equal(deb_multiply(x, x = -3),
               to_lsd(c(-30, -9, -6), b1))
  expect_equal(deb_multiply(neg, x = 3),
               to_lsd(c(-26, -9, -6), b1))
  expect_equal(deb_multiply(dec, x = 3),
               to_lsd(c(20, 5, 6.6), b1))
  expect_equal(deb_multiply(y, x = 3, bases = c(8, 16)),
               to_lsd(c(62, 0, 8), c(8, 16)))
  # Rounding and denarii base
  expect_equal(deb_multiply(c(405, 0, 0), x = 1/300),
               to_lsd(c(1, 7, 0), b1))
  expect_equal(deb_multiply(c(405, 0, 0), x = 0.0033333333),
               to_lsd(c(1, 7, 0), b1))
  expect_equal(deb_multiply(c(100, 5, 10), x = 1 / 3),
               to_lsd(c(33, 8, 7.33333), b1))
  expect_equal(deb_multiply(c(100, 5, 10), x = 1 / 3, round = 0),
               to_lsd(c(33, 8, 7), b1))
})

test_that("lsd multiplication is vectorized", {
  expect_equal(deb_multiply(list1, x = 3),
               to_lsd(list(c(91, 12, 3),
                           c(35, 2, 2.4),
                           c(-79, -15, -6)), b1))
  expect_equal(deb_multiply(list1, x = 3, bases = b2),
               to_lsd(list(c(93, 7, 11),
                           c(39, 3, 6.6),
                           c(-82, -2, -14)), b2))
  expect_equal(deb_multiply(list1, x = 3, round = 0),
               to_lsd(list(c(91, 12, 3),
                           c(35, 2, 2),
                           c(-79, -15, -6)), b1))
})

test_that("deb_multiply works with lsd objects", {
  expect_identical(deb_multiply(x_b2, x = 3),
                   deb_multiply(x, x = 3, bases = b2))
  expect_identical(deb_multiply(list1_b1, x = 3),
                   deb_multiply(list1, x = 3, bases = b1))
  expect_identical(deb_multiply(list2_b2, x = 3, round = 0),
                   deb_multiply(list2, x = 3, bases = b2, round = 0))
})

test_that("deb_multiply works with lsd column", {
  # mutate works
  expect_equal(ncol(mutate(tbl_b1, lsd2 = deb_multiply(lsd, x = 3))), 2)
  expect_equal(ncol(mutate(tbl_b1, lsd = deb_multiply(lsd, x = 3))), 1)

  # mutated column is lsd
  expect_s3_class(mutate(tbl_b1, lsd = deb_multiply(lsd, x = 3))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_b2, lsd = deb_multiply(lsd, x = 3))$lsd),
               c(s = 8, d = 16))

  # mutated column is same as normal deb_multiply
  expect_identical(mutate(tbl_b1, lsd = deb_multiply(lsd, x = 3))$lsd,
                   deb_multiply(list1_b1, x = 3))
  expect_identical(mutate(tbl_b2, lsd = deb_multiply(lsd, x = 3)),
                   tibble(lsd = deb_multiply(list2_b2, x = 3)))
})

# Division
test_that("lsd division works", {
  expect_equal(deb_divide(x, x = 3),
               to_lsd(c(3, 7, 8.66667), b1))
  expect_equal(deb_divide(neg, x = 3),
               to_lsd(c(-2, -18, -10), b1))
  expect_equal(deb_divide(x, x = 3, bases = b2),
               to_lsd(c(3, 3, 11.33333), b2))
  # Rounding and denarii base
  expect_equal(deb_divide(c(405, 0, 0), x = 300),
               to_lsd(c(1, 7, 0), b1))
  expect_equal(deb_divide(c(350, 8, 0), x = 6),
               to_lsd(c(58, 8, 0), b1))
  expect_equal(deb_divide(x, x = 3, round = 0),
               to_lsd(c(3, 7, 9), b1))
})

test_that("lsd division is vectorized", {
  expect_equal(deb_divide(list1, x = 2),
               to_lsd(list(c(15, 5, 4.5),
                           c(5, 17, 0.4),
                           c(-13, -5, -11)), b1))
  expect_equal(deb_divide(list1, x = 2, bases = b2),
               to_lsd(list(c(15, 5, 4.5),
                           c(6, 4, 9.1),
                           c(-13, -5, -13)), b2))
  expect_equal(deb_divide(list1, x = 2, round = 0),
               to_lsd(list(c(15, 5, 4),
                           c(5, 17, 0),
                           c(-13, -5, -11)), b1))
})

test_that("round argument works", {
  expect_equal(deb_divide(c(6, 8, 1), x = 3) %>% deb_multiply(x = 3),
               to_lsd(c(6, 8, 0.99999), b1))
  expect_equal(deb_divide(c(6, 8, 1), x = 3) %>% deb_multiply(x = 3, round = 4),
               to_lsd(c(6, 8, 1), b1))
})

test_that("deb_divide works with lsd objects", {
  expect_identical(deb_divide(x_b2, x = 3),
                   deb_divide(x, x = 3, bases = b2))
  expect_identical(deb_divide(list1_b1, x = 3),
                   deb_divide(list1, x = 3, bases = b1))
  expect_identical(deb_divide(list2_b2, x = 3, round = 0),
                   deb_divide(list2, x = 3, bases = b2, round = 0))
})

test_that("deb_divide works with lsd column", {
  # mutated column is lsd
  expect_s3_class(mutate(tbl_b1, lsd = deb_divide(lsd, x = 3))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_b2, lsd = deb_divide(lsd, x = 3))$lsd),
               c(s = 8, d = 16))

  # mutated column is same as normal deb_divide
  expect_identical(mutate(tbl_b1, lsd = deb_divide(lsd, x = 3))$lsd,
                   deb_divide(list1_b1, x = 3))
  expect_identical(mutate(tbl_b2, lsd = deb_divide(lsd, x = 3)),
                   tibble(lsd = deb_divide(list2_b2, x = 3)))
})

## Addition and subtraction ##

test_that("arithmetic_check2 works", {
  expect_silent(arithmetic_check2(list1, x))
  expect_silent(arithmetic_check2(list1, list(x)))
  expect_error(arithmetic_check2(list1, list(x, y)),
               "If lsd1 and lsd2 are both lists, they must be the same length, or one must be of length 1.")
})

test_that("lsd list length is checked", {
  # Check that length of lists are either the same or 1
  # This is necessary for use of map2
  expect_s3_class(deb_add(list1, list(x)), "lsd")
  expect_error(deb_add(list1, list(x, y)),
               "If lsd1 and lsd2 are both lists, they must be the same length, or one must be of length 1.")
  expect_s3_class(deb_subtract(list1, list(x)), "lsd")
  expect_error(deb_subtract(list1, list(x, y)),
               "If lsd1 and lsd2 are both lists, they must be the same length, or one must be of length 1.")
})

# Addition
test_that("lsd addition works", {
  expect_equal(deb_add(y, x),
               to_lsd(c(30, 8, 10), b1))
  expect_equal(deb_add(x, dec),
               to_lsd(c(16, 18, 4.2), b1))
  expect_equal(deb_add(x, dec, round = 0),
               to_lsd(c(16, 18, 4), b1))
  expect_equal(deb_add(x, c(5, 7, 15), bases = b2),
               to_lsd(c(16, 3, 1), b2))
})

test_that("lsd addition is vectorized", {
  expect_equal(deb_add(list1, list2),
               to_lsd(list(c(40, 13, 11),
                           c(31, 19, 8.8),
                           c(-19, -16, -7.8)), b1))
  expect_equal(deb_add(list1, x),
               to_lsd(list(c(40, 13, 11),
                           c(21, 17, 2.8),
                           c(-16, -8, -8)), b1))
  expect_equal(deb_add(x, list1), deb_add(list1, x))
  expect_equal(deb_add(x, list1, round = 0),
               to_lsd(list(c(40, 13, 11),
                           c(21, 17, 3),
                           c(-16, -8, -8)), b1))
  expect_equal(deb_add(list1, x, bases = b2),
               to_lsd(list(c(41, 5, 11),
                           c(23, 4, 4.2),
                           c(-17, 0, -8)), b2))
})

test_that("deb_add works with lsd objects", {
  expect_identical(deb_add(x_b2, y_b2), deb_add(x, y_b2))
  expect_identical(deb_add(list1_b1, x), deb_add(x, list1, b1))
  expect_identical(deb_add(x, list2_b2), deb_add(list2, x, b2))
})

test_that("deb_add works with lsd column", {
  # errors work
  expect_error(mutate(tbl_b1, lsd = deb_add(lsd, x_b2)),
               "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")

  # mutated column is lsd
  expect_s3_class(mutate(tbl_b1, lsd = deb_add(lsd, x))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_b2, lsd = deb_add(lsd, x_b2))$lsd),
               c(s = 8, d = 16))

  # mutated column is same as normal deb_add
  expect_identical(mutate(tbl_b1, lsd = deb_add(lsd, list1_b1))$lsd,
                   deb_add(list1_b1, list1_b1))
  expect_identical(mutate(tbl_b2, lsd = deb_add(lsd, list2_b2)),
                   tibble(lsd = deb_add(list2_b2, list2_b2)))
})

# Subtraction
test_that("lsd subtract works", {
  expect_equal(deb_subtract(y, x),
               to_lsd(c(10, 2, 6), b1))
  expect_equal(deb_subtract(x, y),
               to_lsd(c(-10, -2, -6), b1))
  expect_equal(deb_subtract(x, dec),
               to_lsd(c(3, 7, 11.8), b1))
  expect_equal(deb_subtract(x, dec, round = 0),
               to_lsd(c(3, 8, 0), b1))
  expect_equal(deb_subtract(x, c(5, 7, 15), bases = c(8, 16)),
               to_lsd(c(4, 3, 3), b2))
})

test_that("lsd subtract is vectorized", {
  expect_equal(deb_subtract(list1, list2),
               to_lsd(list(c(20, 7, 7),
                    c(-8, -11, -7.2),
                    c(-33, -7, -0.2)), b1))
  expect_equal(deb_subtract(list1, x),
               to_lsd(list(c(20, 7, 7),
                    c(1, 10, 10.8),
                    c(-36, -15, 0)), b1))
  expect_equal(deb_subtract(x, list1),
               to_lsd(list(c(-20, -7, -7),
                    c(-1, -10, -10.8),
                    c(36, 15, 0)), b1))
  expect_equal(deb_subtract(x, list1, round = 0, bases = b2),
               to_lsd(list(c(-20, -7, -7),
                    c(-2, -6, 0),
                    c(37, 6, 12)), b2))
})

test_that("deb_subtract works with lsd objects", {
  expect_identical(deb_subtract(x_b2, y_b2), deb_subtract(x, y_b2))
  expect_identical(deb_subtract(list1_b1, x), deb_subtract(list1, x, b1))
  expect_identical(deb_subtract(x, list2_b2), deb_subtract(x, list2, b2))
})

test_that("deb_subtract works with lsd column", {
  # errors work
  expect_error(mutate(tbl_b1, lsd = deb_subtract(lsd, x_b2)),
               "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")

  # mutated column is lsd
  expect_s3_class(mutate(tbl_b1, lsd = deb_subtract(lsd, x))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_b2, lsd = deb_subtract(lsd, x_b2))$lsd),
               c(s = 8, d = 16))

  # mutated column is same as normal deb_add
  expect_identical(mutate(tbl_b1, lsd = deb_subtract(lsd, list1_b1))$lsd,
                   deb_subtract(list1_b1, list1_b1))
  expect_identical(mutate(tbl_b2, lsd = deb_subtract(lsd, list2_b2)),
                   tibble(lsd = deb_subtract(list2_b2, list2_b2)))
})
