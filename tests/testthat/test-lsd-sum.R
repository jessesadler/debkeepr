context("test-lsd-sum.R")

suppressPackageStartupMessages(library(dplyr))

x <- c(10, 3, 2)
y <- c(20, 5, 8)
neg <- c(-8, -16, -6)
dec <- c(5.85, 17.35, 10)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b1 <- to_lsd(x, b1)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(c(12, 7, 9), c(5, 8, 11), c(3, 18, 5))
na_list <- list(c(12, 7, 9), c(5, 8, 11), c(3, 18, NA))
list2 <- list(x, y, dec)
list1_b1 <- to_lsd(list1, b1)
list2_b2 <- to_lsd(list2, b2)
na_list_b1 <- to_lsd(na_list, b1)

df1 <- data.frame(group = c(1, 2, 1, 2),
                  l = c(3, 5, 6, 2),
                  s = c(10, 18, 11, 16),
                  d = c(9, 11, 10, 5))
df2 <- data.frame(group = c(1, 2, 1, 2),
                  pounds = c(3, 5, 6, 2),
                  shillings = c(10, 18, 11, 16),
                  pence = c(9, 11, 10, 5))
df3 <- data.frame(l = c(1, 1, 1),
                  s = c(6, 6, 7),
                  d = c(3.33333333, 3.33333333, 5.33333333))
df_neg <- data.frame(group = c(1, 2, 1, 2),
                     l = c(-3, 5, -6, 2),
                     s = c(-10, 18, -11, 16),
                     d = c(-9, 11, -10, 5))
df_dec <- data.frame(group = c(1, 2, 1, 2),
                     l = c(8.425, 5, 0, 2),
                     s = c(0, 18, 11.365, 16),
                     d = c(0, 11, 0, 5))
df_error <- data.frame(l = c("j", "r", "s"),
                       s = c(10, 18, 11),
                       d = c(9, 11, 10))
df_na <- data.frame(group = c(1, 2, 1, 2),
                    l = c(3, 5, 6, 2),
                    s = c(10, 18, 11, 16),
                    d = c(9, 11, 10, NA))

df_dec_b1 <- deb_as_lsd_mutate(df_dec, bases = b1, replace = TRUE)
df_dec_b2 <- deb_as_lsd_mutate(df_dec, lsd_column = b2, bases = b2, replace = TRUE)
df_na_b2 <- deb_as_lsd_mutate(df_na, lsd_column = lsd_na, bases = b2, replace = TRUE)
df_lsd <- cbind(df_dec_b1, df_dec_b2[2], df_na_b2[2])

test_that("deb_sum works", {
  expect_equal(deb_sum(list1),
               to_lsd(c(21, 15, 1), b1))
  expect_equal(deb_sum(list1, c(-8, - 5, -5)),
               to_lsd(c(13, 9, 8), b1))
  expect_equal(deb_sum(list1, c(8, 4, 9), c(6, 19, 10)),
               to_lsd(c(36, 19, 8), b1))
  expect_equal(deb_sum(list1, bases = b2),
               to_lsd(c(24, 2, 9), b2))
  expect_equal(deb_sum(c(1, 6, 3.33333333), c(1, 6, 3.33333333), c(1, 7, 5.33333333)),
               to_lsd(c(4, 0, 0), b1))
  expect_equal(deb_sum(c(1, 6, 3.33333333), c(1, 6, 3), c(1, 7, 6)),
               to_lsd(c(4, 0, 0.33333), b1))
  expect_equal(deb_sum(c(1, 6, 3.33333333), c(1, 6, 3), c(1, 7, 6), round = 0),
               to_lsd(c(4, 0, 0), b1))
  expect_error(deb_sum(list1, c("hello", "goodbye")),
               "lsd must be a numeric vector")
  expect_error(deb_sum(list1, c(6, 3)),
               paste("lsd must be a vector of length of 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
})

test_that("deb_sum works with lsd objects", {
  expect_identical(deb_sum(x_b2, y_b2, list2_b2),
                   deb_sum(x, y, list2, bases = b2))
  expect_identical(deb_sum(list2_b2),
                   deb_sum(list2, bases = b2))
  expect_identical(deb_sum(list1_b1, x, y, dec),
                   deb_sum(list1, x, y, dec, bases = b1))
  expect_identical(deb_sum(list2_b2, round = 0),
                   deb_sum(list2, bases = b2, round = 0))
  expect_error(deb_sum(x_b1, x_b2),
               "All objects of class lsd must have the same bases")
})

test_that("deb_sum_simple is same as deb_sum",{
  expect_equal(deb_sum(list1_b1), deb_sum_simple(list1_b1))
  expect_equal(deb_sum(na_list_b1), deb_sum_simple(na_list_b1))
  expect_equal(deb_sum(list2_b2, round = 0), deb_sum_simple(list2_b2, round = 0))
  expect_equal(deb_sum(na_list_b1, na.rm = TRUE),
               deb_sum_simple(na_list_b1, na.rm = TRUE))
})

test_that("deb_summarise works", {
  # Same answers as deb_sum
  expect_equal(deb_summarise(df_dec_b1, lsd)$lsd, deb_sum(df_dec_b1$lsd))
  expect_equal(deb_summarise(df_dec_b2, b2)$b2, deb_sum(df_dec_b2$b2))
  expect_equal(deb_summarise(df_dec_b1, lsd, round = 0)$lsd,
               deb_sum(df_dec_b1$lsd, round = 0))
  expect_equal(deb_summarise(df_na_b2, lsd_na)$lsd_na, deb_sum(df_na_b2$lsd_na))
  expect_equal(deb_summarise(df_na_b2, lsd_na, na.rm = TRUE)$lsd_na,
               deb_sum(df_na_b2$lsd_na, na.rm = TRUE))
})

test_that("deb_summarise works with multiple lsd columns", {
  expect_equal(nrow(deb_summarise(df_lsd, lsd, b2, lsd_na, na.rm = TRUE)), 1)
  expect_equal(ncol(deb_summarise(df_lsd, lsd, b2, lsd_na, na.rm = TRUE)), 3)
  expect_equal(ncol(deb_summarise(df_lsd, lsd, b2)), 2)
  expect_equal(deb_summarise(df_lsd, lsd, b2)[1], deb_summarise(df_lsd, lsd))
  expect_equal(deb_summarise(df_lsd, lsd, b2)[2], deb_summarise(df_lsd, b2))
})

test_that("deb_summarise works with group_by", {
  g <- df_dec_b1 %>%
    group_by(group) %>%
    deb_summarise(lsd)

  g2 <- df_lsd %>%
    group_by(group) %>%
    deb_summarise(lsd, b2, lsd_na)

  expect_equal(nrow(g), 2)
  expect_equal(ncol(g), 2)
  expect_equal(g$lsd, deb_as_lsd(list(c(8, 19, 10.38), c(8, 15, 4))))
  expect_equal(ncol(g2), 4)
})

test_that("deb_summarise error works", {
  expect_error(deb_summarise(df_dec_b1),
               "Names for lsd list columns must be provided.")
  expect_error(deb_summarise(df_dec_b1, group),
               "Variables to summarise must be list columns of class lsd.")
  expect_error(deb_summarise(df_dec_b1, lsd, group),
               "Variables to summarise must be list columns of class lsd.")
})

test_that("deb_sum_df works on data frames", {
  expect_equal(nrow(deb_sum_df(df1, l, s, d)), 1)
  expect_equal(deb_sum_df(df1, l, s, d),
               data.frame(l = 18, s = 17, d = 11))
  expect_equal(deb_sum_df(df1, l, s, d, bases = c(20, 16)),
               data.frame(l = 18, s = 17, d = 3))
  expect_equal(deb_sum_df(df3),
               data.frame(l = 4, s = 0, d = 0))
  expect_equal(deb_sum_df(df_dec, round = 0),
               data.frame(l = 17, s = 15, d = 2))
})

test_that("na.rm works", {
  expect_equal(deb_sum(na_list),
               to_lsd(as.numeric(c(NA, NA, NA)), b1))
  expect_equal(deb_sum(na_list, na.rm = TRUE),
               deb_sum(list1[1:2]))
  expect_equal(deb_sum_df(df_na),
               data.frame(l = as.numeric(NA), s = as.numeric(NA), d = as.numeric(NA)))
  expect_equal(deb_sum_df(df_na, na.rm = TRUE),
               deb_sum_df(df1[1:3, ]))
})

test_that("group_by works", {
  g <- df1 %>%
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
  expect_equal(deb_sum_df(df2, l = pounds, s = shillings, d = pence),
               data.frame(pounds = 18, shillings = 17, pence = 11))
})

test_that("error is column names not present", {
  expect_error(deb_sum_df(df2),
               paste("Column names for l, s, and d must be provided if the",
                     "default names of l, s, and d are not present in the data frame",
                     sep = "\n"))
})

test_that("works with negative values", {
  expect_equal(deb_sum_df(df_neg, l, s, d),
               data.frame(l = -1, s = -7, d = -3))
  g <- df_neg %>%
    group_by(group) %>%
    deb_sum_df(l, s, d)
  answer <- data.frame(group = c(1, 2),
                   l = c(-10, 8),
                   s = c(-2, 15),
                   d = c(-7, 4))
  expect_equal(g, answer)
})

test_that("decimalization works", {
  expect_equal(deb_sum_df(df_dec, l, s, d),
               data.frame(l = 17, s = 15, d = 2.38))
  g <- df_dec %>%
    group_by(group) %>%
    deb_sum_df(l, s, d)
  answer <- data.frame(group = c(1, 2),
                       l = c(8, 8),
                       s = c(19, 15),
                       d = c(10.38, 4))
})

test_that("non-numeric is an error", {
  expect_error(deb_sum_df(df_error, l, s, d),
               "l must be a numeric variable")
  expect_error(deb_sum_df(df_error, d, l, s),
               "s must be a numeric variable")
  expect_error(deb_sum_df(df_error, s, d, l),
               "d must be a numeric variable")
})
