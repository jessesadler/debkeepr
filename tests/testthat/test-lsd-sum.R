context("test-deb-sum.R")

suppressPackageStartupMessages(library(dplyr))

example1 <- data.frame(group = c(1, 2, 1, 2),
                       l = c(3, 5, 6, 2),
                       s = c(10, 18, 11, 16),
                       d = c(9, 11, 10, 5))
example2 <- data.frame(group = c(1, 2, 1, 2),
                       pounds = c(3, 5, 6, 2),
                       shillings = c(10, 18, 11, 16),
                       pence = c(9, 11, 10, 5))
example_neg <- data.frame(group = c(1, 2, 1, 2),
                          l = c(-3, 5, -6, 2),
                          s = c(-10, 18, -11, 16),
                          d = c(-9, 11, -10, 5))
example_dec <- data.frame(group = c(1, 2, 1, 2),
                          l = c(8.425, 5, 0, 2),
                          s = c(0, 18, 11.365, 16),
                          d = c(0, 11, 0, 5))
example_error <- data.frame(l = c("j", "r", "s"),
                            s = c(10, 18, 11),
                            d = c(9, 11, 10))

test_that("basic functionality works", {
  expect_equal(nrow(deb_sum(example1, l, s, d)), 1)
  expect_equal(deb_sum(example1, l, s, d),
               data.frame(l = 18, s = 17, d = 11))
  expect_equal(deb_sum(example1, l, s, d, lsd_bases = c(20, 16)),
               data.frame(l = 18, s = 17, d = 3))
})

test_that("group_by works", {
  g <- example1 %>%
    group_by(group) %>%
    deb_sum(l, s, d)
  answer <- data.frame(group = c(1, 2),
                       l = c(10, 8),
                       s = c(2, 15),
                       d = c(7, 4))
  expect_equal(nrow(g), 2)
  expect_equal(g, answer)
})

test_that("column names can be different", {
  expect_equal(deb_sum(example2, l = pounds, s = shillings, d = pence),
               data.frame(pounds = 18, shillings = 17, pence = 11))
})

test_that("error is column names not present", {
  expect_error(deb_sum(example2),
               paste("Column names for l, s, and d must be provided if the",
                     "default names of l, s, and d are not present in the data frame",
                     sep = "\n"))
})

test_that("works with negative values", {
  expect_equal(deb_sum(example_neg, l, s, d),
               data.frame(l = -1, s = -7, d = -3))
  g <- example_neg %>%
    group_by(group) %>%
    deb_sum(l, s, d)
  answer <- data.frame(group = c(1, 2),
                   l = c(-10, 8),
                   s = c(-2, 15),
                   d = c(-7, 4))
  expect_equal(g, answer)
})

test_that("decimalization works", {
  expect_equal(deb_sum(example_dec, l, s, d),
               data.frame(l = 17, s = 15, d = 2.38))
  g <- example_dec %>%
    group_by(group) %>%
    deb_sum(l, s, d)
  answer <- data.frame(group = c(1, 2),
                       l = c(8, 8),
                       s = c(19, 15),
                       d = c(10.38, 4))
  expect_equal(deb_sum(example_dec, l, s, d, round = 0),
               data.frame(l = 17, s = 15, d = 2))
})

test_that("non-numeric is an error", {
  expect_error(deb_sum(example_error, l, s, d),
               "l must be a numeric variable")
  expect_error(deb_sum(example_error, d, l, s),
               "s must be a numeric variable")
  expect_error(deb_sum(example_error, s, d, l),
               "d must be a numeric variable")
})
