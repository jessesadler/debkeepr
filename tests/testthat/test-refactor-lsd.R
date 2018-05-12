context("test refactor lsd.R")

test_that("non-numeric is an error", {
  expect_error(deb_refactor(9, 0, "t"),
               "d must be numeric")
})

test_that("positive and negative refactor", {
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
  expect_equal(deb_refactor(10, 18.4254254, 10), tibble::tibble(l = 10, s = 19, d = 3.105))
  expect_equal(deb_refactor(10, 18.4254254, 10, vector = TRUE, round = 0), c(l = 10, s = 19, d = 3))
  expect_equal(deb_refactor(10, 18.4254254, 10, vector = TRUE, round = 5), c(l = 10, s = 19, d = 3.1051))
})

test_that("it goes together in deb_refactor", {
  expect_equal(deb_refactor(10, 25, 22), tibble::tibble(l = 11, s = 6, d = 10))
  expect_equal(deb_refactor(10, 25, 22, vector = TRUE), c(l = 11, s = 6, d = 10))
  expect_equal(deb_refactor(10, -25, 22, vector = TRUE), c(l = 8, s = 16, d = 10))
  expect_equal(deb_refactor(0, -25, 22), tibble::tibble(l = -1, s = -3, d = -2))
  expect_equal(deb_refactor(10.7, 38, 22, vector = TRUE), c(l = 12, s = 13, d = 10))
  expect_equal(deb_refactor(10, 18.8, 32), tibble::tibble(l = 11, s = 1, d = 5.6))
})
