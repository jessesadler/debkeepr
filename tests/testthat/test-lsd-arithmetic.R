context("test-lsd-arithmetic.R")

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

ex_df <- data.frame(l = c(30, 10.725, -26, 405, 350),
                    s = c(10, 18.65, -11, 0, 8),
                    d = c(9, 11, -10, 0, 0))

answer_x3 <- data.frame(l = c(30, 10.725, -26, 405, 350),
                        s = c(10, 18.65, -11, 0, 8),
                        d = c(9, 11, -10, 0, 0),
                        l.1 = c(91, 35, -79, 1215, 1051),
                        s.1 = c(12, 2, -15, 0, 4),
                        d.1 = c(3, 2.4, -6, 0, 0))
answer_x3_replace <- data.frame(l = c(91, 35, -79, 1215, 1051),
                                s = c(12, 2, -15, 0, 4),
                                d = c(3, 2.4, -6, 0, 0))
answer_x03 <- data.frame(l = c(10, 3, -8, 135, 116),
                         s = c(3, 18, -17, 0, 16),
                         d = c(7, 0.26667, -3.33333, 0, 0))

answer_d2 <- data.frame(l = c(30, 10.725, -26, 405, 350),
                        s = c(10, 18.65, -11, 0, 8),
                        d = c(9, 11, -10, 0, 0),
                        l.1 = c(15, 5, -13, 202, 175),
                        s.1 = c(5, 17, -5, 10, 4),
                        d.1 = c(4.5, 0.4, -11, 0, 0))
answer_d2_replace <- data.frame(l = c(15, 5, -13, 202, 175),
                                s = c(5, 17, -5, 10, 4),
                                d = c(4.5, 0.4, -11, 0, 0))
answer_d6 <- data.frame(l = c(5, 1, -4, 67, 58),
                        s = c(1, 19, -8, 10, 8),
                        d = c(9.5, 0.13333, -7.66667, 0, 0))

na_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(NA, 11, -10))
na_replace <- data.frame(l = c(NA, 35, -79),
                         s = c(NA, 2, -15),
                         d = c(NA, 2.4, -6))

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

test_that("deb_multiply_mutate works", {
  expect_equal(ncol(deb_multiply_mutate(ex_df, l, s, d, x = 2)), 6)
  expect_equal(ncol(deb_multiply_mutate(ex_df, l, s, d, x = 2, replace = TRUE)), 3)
  expect_equal(deb_multiply_mutate(ex_df, l, s, d, x = 3), answer_x3)
  expect_equal(deb_multiply_mutate(ex_df, l, s, d, x = 3, replace = TRUE), answer_x3_replace)
  expect_equal(deb_multiply_mutate(ex_df, x = 0.3333333333, replace = TRUE), answer_x03)
  expect_equal(deb_multiply_mutate(ex_df, x = 0.3333333333, replace = TRUE, round = 0)[ , 3],
               c(7, 0, -3, 0, 0))
  # bases changes answer
  expect_false(identical(deb_multiply_mutate(ex_df, l, s, d, x = 3),
                         deb_multiply_mutate(ex_df, l, s, d, x = 3, bases = c(8, 16))))
  expect_equal(deb_multiply_mutate(na_df, replace = TRUE, x = 3), na_replace)
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

test_that("deb_divide_mutate works", {
  expect_equal(ncol(deb_divide_mutate(ex_df, l, s, d, x = 2)), 6)
  expect_equal(ncol(deb_divide_mutate(ex_df, l, s, d, x = 2, replace = TRUE)), 3)
  expect_equal(deb_divide_mutate(ex_df, l, s, d, x = 2), answer_d2)
  expect_equal(deb_divide_mutate(ex_df, l, s, d, x = 2, replace = TRUE), answer_d2_replace)
  expect_equal(deb_divide_mutate(ex_df, x = 6, replace = TRUE), answer_d6)
  expect_equal(deb_divide_mutate(ex_df, l, s, d, x = 2, round = 0, replace = TRUE)[ , 3],
               c(4, 0, -11, 0, 0))
  # bases changes answer
  expect_false(identical(deb_divide_mutate(ex_df, l, s, d, x = 3),
                         deb_divide_mutate(ex_df, l, s, d, x = 3, bases = c(8, 16))))
})

## Addition and subtraction ##

test_that("arithmetic_list_check works", {
  expect_silent(arithmetic_list_check(list1, x))
  expect_silent(arithmetic_list_check(list1, list(x)))
  expect_error(arithmetic_list_check(list1, list(x, y)),
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

test_that("deb_add_mutate works", {
  expect_equal(ncol(deb_add_mutate(ex_df, l, s, d, lsd = c(5, 15, 8))), 6)
  expect_equal(ncol(deb_add_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), replace = TRUE)), 3)
  expect_equal(deb_add_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), replace = TRUE),
               data.frame(l = c(36, 17, -20, 410, 356),
                          s = c(6, 9, -16, 15, 3),
                          d = c(5, 8.8, -2, 8, 8)))
  expect_equal(deb_add_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), round = 0, replace = TRUE)[ , 3],
               c(5, 9, -2, 8, 8))
  # bases changes answer
  expect_false(identical(deb_add_mutate(ex_df, l, s, d, lsd = c(5, 15, 8)),
                         deb_add_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), bases = c(8, 16))))
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

test_that("deb_subtraction_mutate works", {
  expect_equal(ncol(deb_subtract_mutate(ex_df, l, s, d, lsd = c(5, 15, 8))), 6)
  expect_equal(ncol(deb_subtract_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), replace = TRUE)), 3)
  expect_equal(deb_subtract_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), replace = TRUE),
               data.frame(l = c(24, 5, -32, 399, 344),
                          s = c(15, 18, -7, 4, 12),
                          d = c(1, 4.8, -6, 4, 4)))
  expect_equal(deb_subtract_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), round = 0, replace = TRUE)[ , 3],
               c(1, 5, -6, 4, 4))
  # bases changes answer
  expect_false(identical(deb_subtract_mutate(ex_df, l, s, d, lsd = c(5, 15, 8)),
                         deb_subtract_mutate(ex_df, l, s, d, lsd = c(5, 15, 8), bases = c(8, 16))))
})

test_that("lsd check works in add and subtract mutate", {
  expect_error(deb_add_mutate(ex_df, l, s, d, lsd = list(x, y)),
               "lsd must be a numeric vector")
  expect_error(deb_subtract_mutate(ex_df, l, s, d, lsd = list(x, y)),
               "lsd must be a numeric vector")
  expect_error(deb_add_mutate(ex_df, l, s, d, lsd = c(8, 16)),
               paste("lsd must be a vector of length of 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
  expect_error(deb_subtract_mutate(ex_df, l, s, d, lsd = c(8, 16)),
               paste("lsd must be a vector of length of 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
})
