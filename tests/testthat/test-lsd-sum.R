context("test-lsd-sum.R")

suppressPackageStartupMessages(library(dplyr))

lsd_list <- list(c(12, 7, 9), c(5, 8, 11), c(3, 18, 5))
na_list <- list(c(12, 7, 9), c(5, 8, 11), c(3, 18, NA))

example1 <- data.frame(group = c(1, 2, 1, 2),
                       l = c(3, 5, 6, 2),
                       s = c(10, 18, 11, 16),
                       d = c(9, 11, 10, 5))
example2 <- data.frame(group = c(1, 2, 1, 2),
                       pounds = c(3, 5, 6, 2),
                       shillings = c(10, 18, 11, 16),
                       pence = c(9, 11, 10, 5))
example3 <- data.frame(l = c(1, 1, 1),
                       s = c(6, 6, 7),
                       d = c(3.33333333, 3.33333333, 5.33333333))
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
example_na <- data.frame(group = c(1, 2, 1, 2),
                         l = c(3, 5, 6, 2),
                         s = c(10, 18, 11, 16),
                         d = c(9, 11, 10, NA))

test_that("deb_sum works", {
  expect_equal(deb_sum(lsd_list),
               c(l = 21, s = 15, d = 1))
  expect_equal(deb_sum(lsd_list, c(-8, - 5, -5)),
               c(l = 13, s = 9, d = 8))
  expect_equal(deb_sum(lsd_list, c(8, 4, 9), c(6, 19, 10)),
               c(l = 36, s = 19, d = 8))
  expect_equal(deb_sum(lsd_list, lsd_bases = c(8, 16)),
               c(l = 24, s = 2, d = 9))
  expect_equal(deb_sum(c(1, 6, 3.33333333), c(1, 6, 3.33333333), c(1, 7, 5.33333333)),
               c(l = 4, s = 0, d = 0))
  expect_error(deb_sum(lsd_list, c("hello", "goodbye")),
               "lsd must be a numeric vector")
  expect_error(deb_sum(lsd_list, c(6, 3)),
               paste("lsd must be a vector of length of 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
})

test_that("deb_sum_df works on data frames", {
  expect_equal(nrow(deb_sum_df(example1, l, s, d)), 1)
  expect_equal(deb_sum_df(example1, l, s, d),
               data.frame(l = 18, s = 17, d = 11))
  expect_equal(deb_sum_df(example1, l, s, d, lsd_bases = c(20, 16)),
               data.frame(l = 18, s = 17, d = 3))
  expect_equal(deb_sum_df(example3),
               data.frame(l = 4, s = 0, d = 0))
})

test_that("na.rm works", {
  expect_equal(deb_sum(na_list),
               stats::setNames(as.numeric(c(NA, NA, NA)), c("l", "s", "d")))
  expect_equal(deb_sum(na_list, na.rm = TRUE),
               deb_sum(lsd_list[1:2]))
  expect_equal(deb_sum_df(example_na),
               data.frame(l = as.numeric(NA), s = as.numeric(NA), d = as.numeric(NA)))
  expect_equal(deb_sum_df(example_na, na.rm = TRUE),
               deb_sum_df(example1[1:3, ]))
})

test_that("group_by works", {
  g <- example1 %>%
    group_by(group) %>%
    deb_sum_df(l, s, d)
  answer <- data.frame(group = c(1, 2),
                       l = c(10, 8),
                       s = c(2, 15),
                       d = c(7, 4))
  expect_equal(nrow(g), 2)
  expect_equal(g, answer)
})

test_that("column names can be different", {
  expect_equal(deb_sum_df(example2, l = pounds, s = shillings, d = pence),
               data.frame(pounds = 18, shillings = 17, pence = 11))
})

test_that("error is column names not present", {
  expect_error(deb_sum_df(example2),
               paste("Column names for l, s, and d must be provided if the",
                     "default names of l, s, and d are not present in the data frame",
                     sep = "\n"))
})

test_that("works with negative values", {
  expect_equal(deb_sum_df(example_neg, l, s, d),
               data.frame(l = -1, s = -7, d = -3))
  g <- example_neg %>%
    group_by(group) %>%
    deb_sum_df(l, s, d)
  answer <- data.frame(group = c(1, 2),
                   l = c(-10, 8),
                   s = c(-2, 15),
                   d = c(-7, 4))
  expect_equal(g, answer)
})

test_that("decimalization works", {
  expect_equal(deb_sum_df(example_dec, l, s, d),
               data.frame(l = 17, s = 15, d = 2.38))
  g <- example_dec %>%
    group_by(group) %>%
    deb_sum_df(l, s, d)
  answer <- data.frame(group = c(1, 2),
                       l = c(8, 8),
                       s = c(19, 15),
                       d = c(10.38, 4))
})

test_that("non-numeric is an error", {
  expect_error(deb_sum_df(example_error, l, s, d),
               "l must be a numeric variable")
  expect_error(deb_sum_df(example_error, d, l, s),
               "s must be a numeric variable")
  expect_error(deb_sum_df(example_error, s, d, l),
               "d must be a numeric variable")
})
