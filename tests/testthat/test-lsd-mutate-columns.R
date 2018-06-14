context("test-lsd-mutate-columns.R")

## Different posible types of values ##
test_that("positive and negative normalization", {
  expect_equal(deb_librae(10, 25, 22), 11)
  expect_equal(deb_librae(-10, -25, -22), -11)
  expect_equal(deb_solidi(10, 25, 22), 6)
  expect_equal(deb_solidi(-10, -25, -22), -6)
  expect_equal(deb_denarii(10, 25, 22, round = 3), 10)
  expect_equal(deb_denarii(-10, -25, -22, round = 3), -10)
})

test_that("mixed positive and negative values", {
  expect_equal(deb_librae(10, -25, 22), 8)
  expect_equal(deb_librae(0, -25, 22), -1)
  expect_equal(deb_solidi(10, -25, 22), 16)
  expect_equal(deb_solidi(0, -25, 22), -3)
  expect_equal(deb_denarii(10, -25, 22, round = 3), 10)
  expect_equal(deb_denarii(0, -25, 22, round = 3), -2)
})

test_that("librae with decimal", {
  expect_equal(deb_librae(10.7, 38, 22), 12)
  expect_equal(deb_librae(-10.7, -38, -22), -12)
  expect_equal(deb_solidi(10.7, 38, 22), 13)
  expect_equal(deb_solidi(-10.7, -38, -22), -13)
  expect_equal(deb_solidi(10.5, 38, 22), 9)
  expect_equal(deb_denarii(10.7, 38, 22, round = 3), 10)
  expect_equal(deb_denarii(-10.7, -38, -22, round = 3), -10)
})

test_that("solidi with decimal", {
  expect_equal(deb_librae(10, 18.8, 32), 11)
  expect_equal(deb_librae(-10, -18.8, -32), -11)
  expect_equal(deb_solidi(10, 18.8, 32), 1)
  expect_equal(deb_solidi(-10, -18.8, -32), -1)
  expect_equal(deb_denarii(10, 18.8, 32, round = 3), 5.6)
  expect_equal(deb_denarii(-10, -18.8, -32, round = 3), -5.6)
})

test_that("round denarii decimal", {
  expect_equal(deb_denarii(10, 18.4254254, 10, round = 3), 3.105)
  expect_equal(deb_denarii(10, 18.4254254, 10, round = 0), 3)
  expect_equal(deb_denarii(10, 18.4254254, 10, round = 5), 3.1051)
})

l_vector <- c(8, 8.325, -8.325, 10)
s_vector <- c(36, 36.85, -36.85, 36.325)
d_vector <- c(25, 25, -25, 18)

test_that("vectorization works", {
  expect_equal(deb_librae(l_vector, s_vector, d_vector), c(9, 10, -10, 11))
  expect_equal(deb_solidi(l_vector, s_vector, d_vector), c(18, 5, -5, 17))
  expect_equal(deb_denarii(l_vector, s_vector, d_vector, round = 3), c(1, 5.2, -5.2, 9.9))
  expect_equal(deb_denarii(l_vector, s_vector, d_vector, round = 0), c(1, 5, -5, 10))
})
