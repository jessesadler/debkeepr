context("test-lsd-arithmetic.R")

ex_vector <- c(10, 3, 2)
ex_vector2 <- c(20, 5, 8)
neg_vector <- c(-8, -16, -6)
dec_vector <- c(5.85, 17.35, 10)

ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
ex_list2 <- list(ex_vector, dec_vector, ex_vector2)

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
  expect_equal(deb_multiply(ex_vector, x = 3),
               c(l = 30, s = 9, d = 6))
  expect_equal(deb_multiply(ex_vector, x = -3),
               c(l = -30, s = -9, d = -6))
  expect_equal(deb_multiply(neg_vector, x = 3),
               c(l = -26, s = -9, d = -6))
  expect_equal(deb_multiply(dec_vector, x = 3),
               c(l = 20, s = 5, d = 6.6))
  expect_equal(deb_multiply(ex_vector, x = 5.5),
               c(l = 55, s = 17, d = 5))
  expect_equal(deb_multiply(ex_vector2, x = 3, bases = c(8, 16)),
               c(l = 62, s = 0, d = 8))
  # Rounding and denarii base
  expect_equal(deb_multiply(c(405, 0, 0), x = 1/300),
               c(l = 1, s = 7, d = 0))
  expect_equal(deb_multiply(c(405, 0, 0), x = 0.0033333333),
               c(l = 1, s = 7, d = 0))
  expect_equal(deb_multiply(c(100, 5, 10), x = 1 / 3),
               c(l = 33, s = 8, d = 7.33333))
  expect_equal(deb_multiply(c(100, 5, 10), x = 1 / 3, round = 0),
               c(l = 33, s = 8, d = 7))
})

test_that("lsd multiplication is vectorized", {
  expect_equal(deb_multiply(ex_list, x = 3),
               list(c(l = 91, s = 12, d = 3),
                    c(l = 35, s = 2, d = 2.4),
                    c(l = -79, s = -15, d = -6)))
  expect_equal(deb_multiply(ex_list, x = 3, round = 0),
               list(c(l = 91, s = 12, d = 3),
                    c(l = 35, s = 2, d = 2),
                    c(l = -79, s = -15, d = -6)))
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
  expect_equal(deb_divide(ex_vector, x = 3),
               c(l = 3, s = 7, d = 8.66667))
  expect_equal(deb_divide(neg_vector, x = 3),
               c(l = -2, s = -18, d = -10))
  expect_equal(deb_divide(ex_vector, x = 3, bases = c(8, 16)),
               c(l = 3, s = 3, d = 11.33333))
  # Rounding and denarii base
  expect_equal(deb_divide(c(405, 0, 0), x = 300),
               c(l = 1, s = 7, d = 0))
  expect_equal(deb_divide(c(350, 8, 0), x = 6),
               c(l = 58, s = 8, d = 0))
  expect_equal(deb_divide(ex_vector, x = 3, round = 0),
               c(l = 3, s = 7, d = 9))
})

test_that("lsd division is vectorized", {
  expect_equal(deb_divide(ex_list, x = 2),
               list(c(l = 15, s = 5, d = 4.5),
                    c(l = 5, s = 17, d = 0.4),
                    c(l = -13, s = -5, d = -11)))
  expect_equal(deb_divide(ex_list, x = 2, round = 0),
               list(c(l = 15, s = 5, d = 4),
                    c(l = 5, s = 17, d = 0),
                    c(l = -13, s = -5, d = -11)))
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

test_that("round argument works", {
  expect_equal(deb_divide(c(6, 8, 1), x = 3) %>% deb_multiply(x = 3),
               c(l = 6, s = 8, d = 0.99999))
  expect_equal(deb_divide(c(6, 8, 1), x = 3) %>% deb_multiply(x = 3, round = 4),
               c(l = 6, s = 8, d = 1))
})

# Addition
test_that("lsd addition works", {
  expect_equal(deb_add(ex_vector2, ex_vector),
               c(l = 30, s = 8, d = 10))
  expect_equal(deb_add(ex_vector, dec_vector),
               c(l = 16, s = 18, d = 4.2))
  expect_equal(deb_add(ex_vector, dec_vector, round = 0),
               c(l = 16, s = 18, d = 4))
  expect_equal(deb_add(ex_vector, c(5, 7, 15), bases = c(8, 16)),
               c(l = 16, s = 3, d = 1))
})

test_that("lsd addition is vectorized", {
  expect_equal(deb_add(ex_list, ex_list2),
               list(c(l = 40, s = 13, d = 11),
                    c(l = 18, s = 9, d = 3),
                    c(l = -6, s = -6, d = -2)))
  expect_equal(deb_add(ex_list, ex_vector),
               list(c(l = 40, s = 13, d = 11),
                    c(l = 21, s = 17, d = 2.8),
                    c(l = -16, s = -8, d = -8)))
  expect_equal(deb_add(ex_vector, ex_list), deb_add(ex_list, ex_vector))
  expect_equal(deb_add(ex_vector, ex_list, round = 0),
               list(c(l = 40, s = 13, d = 11),
                    c(l = 21, s = 17, d = 3),
                    c(l = -16, s = -8, d = -8)))
  expect_equal(deb_add(ex_list, ex_vector, bases = c(8, 16)),
               list(c(l = 41, s = 5, d = 11),
                    c(l = 23, s = 4, d = 4.2),
                    c(l = -17, s = 0, d = -8)))
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
  expect_equal(deb_subtract(ex_vector2, ex_vector),
               c(l = 10, s = 2, d = 6))
  expect_equal(deb_subtract(ex_vector, ex_vector2),
               c(l = -10, s = -2, d = -6))
  expect_equal(deb_subtract(ex_vector, dec_vector),
               c(l = 3, s = 7, d = 11.8))
  expect_equal(deb_subtract(ex_vector, dec_vector, round = 0),
               c(l = 3, s = 8, d = 0))
  expect_equal(deb_subtract(ex_vector, c(5, 7, 15), bases = c(8, 16)),
               c(l = 4, s = 3, d = 3))
})

test_that("lsd subtract is vectorized", {
  expect_equal(deb_subtract(ex_list, ex_list2),
               list(c(l = 20, s = 7, d = 7),
                    c(l = 4, s = 18, d = 10.6),
                    c(l = -46, s = -17, d = -6)))
  expect_equal(deb_subtract(ex_list, ex_vector),
               list(c(l = 20, s = 7, d = 7),
                    c(l = 1, s = 10, d = 10.8),
                    c(l = -36, s = -15, d = 0)))
  expect_equal(deb_subtract(ex_vector, ex_list),
               list(c(l = -20, s = -7, d = -7),
                    c(l = -1, s = -10, d = -10.8),
                    c(l = 36, s = 15, d = 0)))
  expect_equal(deb_subtract(ex_vector, ex_list, round = 0),
               list(c(l = -20, s = -7, d = -7),
                    c(l = -1, s = -10, d = -11),
                    c(l = 36, s = 15, d = 0)))
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
  expect_error(deb_add_mutate(ex_df, l, s, d, lsd = list(ex_vector, ex_vector2)),
               "lsd must be a numeric vector")
  expect_error(deb_subtract_mutate(ex_df, l, s, d, lsd = list(ex_vector, ex_vector2)),
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
