context("test deb_normalize lsd.R")

## Error messages ##
test_that("non-numeric is an error", {
  expect_error(deb_normalize(9, 0, "t"),
               "d must be numeric")
})

test_that("round argument is numeric", {
  expect_error(deb_normalize(1, 34, 4, round = "t"),
               "round must be numeric")
})

test_that("vector argument is logical", {
  expect_error(deb_normalize(9, 0, 8, 2, 5),
               "vector must be logical, either TRUE or FALSE")
})

test_that("l, s, and d must be the same length", {
  expect_error(deb_normalize(l = c(9, 0), s = 8, d = 2),
               "l, s, and d must be numeric vectors of the same length")
})

## Different posible types of values ##
test_that("positive and negative normalization", {
  expect_equal(deb_librae(10, 25, 22), 11)
  expect_equal(deb_librae(-10, -25, -22), -11)
  expect_equal(deb_solidi(10, 25, 22), 6)
  expect_equal(deb_solidi(-10, -25, -22), -6)
  expect_equal(deb_denarii(10, 25, 22), 10)
  expect_equal(deb_denarii(-10, -25, -22), -10)
})

test_that("mixed positive and negative values", {
  expect_equal(deb_librae(10, -25, 22), 8)
  expect_equal(deb_librae(0, -25, 22), -1)
  expect_equal(deb_solidi(10, -25, 22), 16)
  expect_equal(deb_solidi(0, -25, 22), -3)
  expect_equal(deb_denarii(10, -25, 22), 10)
  expect_equal(deb_denarii(0, -25, 22), -2)
})

test_that("librae with decimal", {
  expect_equal(deb_decimal_check(10.7, 38, 22), c(10, 51, 34))
  expect_equal(deb_librae(10.7, 38, 22), 12)
  expect_equal(deb_librae(-10.7, -38, -22), -12)
  expect_equal(deb_solidi(10.7, 38, 22), 13)
  expect_equal(deb_solidi(-10.7, -38, -22), -13)
  expect_equal(deb_solidi(10.5, 38, 22), 9)
  expect_equal(deb_denarii(10.7, 38, 22), 10)
  expect_equal(deb_denarii(-10.7, -38, -22), -10)
})

test_that("solidi with decimal", {
  expect_equal(deb_decimal_check(10, 18.8, 32), c(10, 18, 41.6))
  expect_equal(deb_librae(10, 18.8, 32), 11)
  expect_equal(deb_librae(-10, -18.8, -32), -11)
  expect_equal(deb_solidi(10, 18.8, 32), 1)
  expect_equal(deb_solidi(-10, -18.8, -32), -1)
  expect_equal(deb_denarii(10, 18.8, 32), 5.6)
  expect_equal(deb_denarii(-10, -18.8, -32), -5.6)
})

test_that("round denarii decimal", {
  expect_equal(deb_denarii(10, 18.4254254, 10), 3.105)
  expect_equal(deb_denarii(10, 18.4254254, 10, round = 0), 3)
  expect_equal(deb_denarii(10, 18.4254254, 10, round = 5), 3.1051)
  expect_equal(deb_normalize(10, 18.4254254, 10), tibble::tibble(l = 10, s = 19, d = 3.105))
  expect_equal(deb_normalize(10, 18.4254254, 10, vector = TRUE, round = 0), c(l = 10, s = 19, d = 3))
  expect_equal(deb_normalize(10, 18.4254254, 10, vector = TRUE, round = 5), c(l = 10, s = 19, d = 3.1051))
})

test_that("it goes together in deb_normalize", {
  expect_equal(deb_normalize(10, 25, 22), tibble::tibble(l = 11, s = 6, d = 10))
  expect_equal(deb_normalize(10, 25, 22, vector = TRUE), c(l = 11, s = 6, d = 10))
  expect_equal(deb_normalize(10, -25, 22, vector = TRUE), c(l = 8, s = 16, d = 10))
  expect_equal(deb_normalize(0, -25, 22), tibble::tibble(l = -1, s = -3, d = -2))
  expect_equal(deb_normalize(10.7, 38, 22, vector = TRUE), c(l = 12, s = 13, d = 10))
  expect_equal(deb_normalize(10, 18.8, 32), tibble::tibble(l = 11, s = 1, d = 5.6))
})

## Vectorization ##

l_vector <- c(8, 8.325, -8.325, 10)
s_vector <- c(36, 36.85, -36.85, 36.325)
d_vector <- c(25, 25, -25, 18)

answer_tbl <- tibble::tibble(l = c(9, 10, -10, 11),
                             s = c(18, 5, -5, 17),
                             d = c(1, 5.2, -5.2, 9.9))
answer_list <- list(deb_normalize(8, 36, 25, vector = TRUE),
                    deb_normalize(8.325, 36.85, 25, vector = TRUE),
                    deb_normalize(-8.325, -36.85, -25, vector = TRUE),
                    deb_normalize(10, 36.325, 18, vector = TRUE))

test_that("vectorization works", {
  expect_equal(is.list(deb_decimal_check(l_vector, s_vector, d_vector)), TRUE)
  expect_equal(length(deb_decimal_check(l_vector, s_vector, d_vector)), 4)
  expect_equal(deb_librae(l_vector, s_vector, d_vector), c(9, 10, -10, 11))
  expect_equal(deb_solidi(l_vector, s_vector, d_vector), c(18, 5, -5, 17))
  expect_equal(deb_denarii(l_vector, s_vector, d_vector), c(1, 5.2, -5.2, 9.9))
  expect_equal(deb_denarii(l_vector, s_vector, d_vector, round = 0), c(1, 5, -5, 10))
})


test_that("vectorization of deb_normalize works", {
  # tibble output
  expect_equal(nrow(deb_normalize(l_vector, s_vector, d_vector)), 4)
  expect_equal(deb_normalize(l_vector, s_vector, d_vector), answer_tbl)
  # vector or list output
  expect_equal(length(deb_normalize(l_vector, s_vector, d_vector, vector = TRUE)), 4)
  expect_equal(is.list(deb_normalize(8, 35, 25, vector = TRUE)), FALSE)
  expect_equal(is.list(deb_normalize(l_vector, s_vector, d_vector, vector = TRUE)), TRUE)
  expect_equal(deb_normalize(l_vector, s_vector, d_vector, vector = TRUE), answer_list)
})
