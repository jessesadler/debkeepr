context("test-lsd-sum.R")

suppressPackageStartupMessages(library(tibble))
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

tbl_b1 <- tibble(group = c(1, 2, 1), b1 = list1_b1)
tbl_b2 <- tibble(group = c(1, 2, 1), b2 = list2_b2)
tbl_na <- tibble(group = c(1, 2, 1), na = na_list_b1)
tbl_lsd <- tibble(group = c(1, 2, 1), b1 = list1_b1, b2 = list2_b2, na = na_list_b1)

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

test_that("na.rm works", {
  expect_equal(deb_sum(na_list),
               to_lsd(as.numeric(c(NA, NA, NA)), b1))
  expect_equal(deb_sum(na_list, na.rm = TRUE),
               deb_sum(list1[1:2]))
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
  expect_equal(deb_summarise(tbl_b1, b1)$b1, deb_sum(tbl_b1$b1))
  expect_equal(deb_summarise(tbl_b2, b2)$b2, deb_sum(list2_b2))
  expect_equal(deb_summarise(tbl_b2, b2, round = 0)$b2,
               deb_sum(list2_b2, round = 0))
  expect_equal(deb_summarise(tbl_na, na)$na, deb_sum(na_list_b1))
  expect_equal(deb_summarise(tbl_na, na, na.rm = TRUE)$na,
               deb_sum(na_list_b1, na.rm = TRUE))
})

test_that("deb_summarise works with multiple lsd columns", {
  expect_equal(nrow(deb_summarise(tbl_lsd, b1, b2, na, na.rm = TRUE)), 1)
  expect_equal(ncol(deb_summarise(tbl_lsd, b1, b2, na, na.rm = TRUE)), 3)
  expect_equal(ncol(deb_summarise(tbl_lsd, b1, b2)), 2)
  expect_identical(deb_summarise(tbl_lsd, b1, b2)[1], deb_summarise(tbl_lsd, b1))
  expect_identical(deb_summarise(tbl_lsd, b1, b2)[2], deb_summarise(tbl_lsd, b2))
})

test_that("deb_summarise works with group_by", {
  g <- tbl_b1 %>%
    group_by(group) %>%
    deb_summarise(b1)

  g2 <- tbl_lsd %>%
    group_by(group) %>%
    deb_summarise(b1, b2, na)

  expect_equal(nrow(g), 2)
  expect_equal(ncol(g), 2)
  expect_equal(g$b1, deb_as_lsd(list(c(16, 6, 2), c(5, 8, 11))))
  expect_equal(ncol(g2), 4)
})

test_that("deb_summarise error works", {
  expect_error(deb_summarise(tbl_b1),
               "Names for lsd list columns must be provided.")
  expect_error(deb_summarise(tbl_b1, group),
               "Variables to summarise must be list columns of class lsd.")
  expect_error(deb_summarise(tbl_b1, b1, group),
               "Variables to summarise must be list columns of class lsd.")
})
