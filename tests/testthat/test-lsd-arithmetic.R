context("test-lsd-arithmetic.R")

ex_vector <- c(10, 3, 2)
ex_vector2 <- c(20, 5, 8)
neg_vector <- c(-8, -16, -6)
dec_vector <- c(5.85, 17.35, 10)

ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
ex_list2 <- list(ex_vector, dec_vector, ex_vector2)

ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))

answer_x3 <- data.frame(l = c(30, 10.725, -26),
                        s = c(10, 18.65, -11),
                        d = c(9, 11, -10),
                        l.1 = c(91, 35, -79),
                        s.1 = c(12, 2, -15),
                        d.1 = c(3, 2.4, -6))
answer_x3_replace <- data.frame(l = c(91, 35, -79),
                                s = c(12, 2, -15),
                                d = c(3, 2.4, -6))

answer_d2 <- data.frame(l = c(30, 10.725, -26),
                        s = c(10, 18.65, -11),
                        d = c(9, 11, -10),
                        l.1 = c(15, 5, -13),
                        s.1 = c(5, 17, -5),
                        d.1 = c(4.5, 0.4, -11))
answer_d2_replace <- data.frame(l = c(15, 5, -13),
                                s = c(5, 17, -5),
                                d = c(4.5, 0.4, -11))

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
  expect_equal(deb_multiply(dec_vector, x = 3.33, round = 0),
               c(l = 22, s = 10, d = 2))
  expect_equal(deb_multiply(ex_vector2, x = 3, lsd_bases = c(8, 16)),
               c(l = 62, s = 0, d = 8))
})

test_that("lsd multiplication is vectorized", {
  expect_equal(deb_multiply(ex_list, 3),
               list(c(l = 91, s = 12, d = 3),
                    c(l = 35, s = 2, d = 2.4),
                    c(l = -79, s = -15, d = -6)))
})

test_that("deb_multiply_mutate works", {
  expect_equal(ncol(deb_multiply_mutate(ex_df, l, s, d, x = 2)), 6)
  expect_equal(ncol(deb_multiply_mutate(ex_df, l, s, d, x = 2, replace = TRUE)), 3)
  expect_equal(deb_multiply_mutate(ex_df, l, s, d, x = 3), answer_x3)
  expect_equal(deb_multiply_mutate(ex_df, l, s, d, x = 3, replace = TRUE), answer_x3_replace)
  expect_equal(deb_multiply_mutate(ex_df, l, s, d, x = 3, replace = TRUE, round = 0)[2, 3], 2)
  # lsd_bases changes answer
  expect_false(identical(deb_multiply_mutate(ex_df, l, s, d, x = 3),
                         deb_multiply_mutate(ex_df, l, s, d, x = 3, lsd_bases = c(8, 16))))
})

# Division
test_that("lsd division works", {
  expect_equal(deb_divide(ex_vector, x = 3),
               c(l = 3, s = 7, d = 8.667))
  expect_equal(deb_divide(neg_vector, x = 3),
               c(l = -2, s = -18, d = -10))
  expect_equal(deb_divide(dec_vector, x = 3),
               c(l = 2, s = 5, d = 0.733))
  expect_equal(deb_divide(dec_vector, x = 3, round = 0),
               c(l = 2, s = 5, d = 1))
  expect_equal(deb_divide(ex_vector, x = 3, lsd_bases = c(8, 16)),
               c(l = 3, s = 3, d = 11.333))
})

test_that("lsd division is vectorized", {
  expect_equal(deb_divide(ex_list, 2),
               list(c(l = 15, s = 5, d = 4.5),
                    c(l = 5, s = 17, d = 0.4),
                    c(l = -13, s = -5, d = -11)))
})

test_that("deb_divide_mutate works", {
  expect_equal(ncol(deb_divide_mutate(ex_df, l, s, d, x = 2)), 6)
  expect_equal(ncol(deb_divide_mutate(ex_df, l, s, d, x = 2, replace = TRUE)), 3)
  expect_equal(deb_divide_mutate(ex_df, l, s, d, x = 2), answer_d2)
  expect_equal(deb_divide_mutate(ex_df, l, s, d, x = 2, replace = TRUE), answer_d2_replace)
  expect_equal(deb_divide_mutate(ex_df, l, s, d, x = 2, replace = TRUE, round = 0)[2, 3], 0)
  # lsd_bases changes answer
  expect_false(identical(deb_divide_mutate(ex_df, l, s, d, x = 3),
                         deb_divide_mutate(ex_df, l, s, d, x = 3, lsd_bases = c(8, 16))))
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
               c(l = 3, s = 7, d = 12))
  expect_equal(deb_subtract(ex_vector, c(5, 7, 15), lsd_bases = c(8, 16)),
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
})
