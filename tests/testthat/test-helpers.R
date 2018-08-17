context("test-helpers.R")

suppressPackageStartupMessages(library(tibble))

list1 <- to_lsd(list(c(35, 50, 89),
                  c(-10, -48, -181),
                  c(26.875, 84.1333, 55),
                  c(12, 76, 205)),
                  bases = c(20, 12))
list2 <- to_lsd(list(c(35, 50, 89),
                     c(-10, -48, -181),
                     c(26.875, 84.1333, 55),
                     c(12, 76, 205)),
                bases = c(8, 16))
tbl1 <- tibble::tibble(l = c(35, -10, 26.875, 12),
                         s = c(50, -48, 84.1333, 76),
                         d = c(89, -181, 55, 205))
tbl2 <- tibble::tibble(pounds = c(35, -10, 26.875, 12),
                          shillings = c(50, -48, 84.1333, 76),
                          pence = c(89, -181, 55, 205))
transactions <- tibble::tibble(credit = c("a", "b", "a", "c"),
                               debit = c("b", "a", "c", "a"),
                               l = c(35, -10, 26.875, 12),
                               s = c(50, -48, 84.1333, 76),
                               d = c(89, -181, 55, 205))

test_that("lsd_list checks", {
  expect_error(deb_list_to_df(c(5, 7, 8)),
               "lsd_list must be a list of numeric vectors")
  expect_error(deb_list_to_df(tbl1),
               "lsd_list must be a list of numeric vectors")
})

test_that("deb_list_to_df works",{
  expect_equal(is.data.frame(deb_list_to_df(list1)), TRUE)
  expect_equal(nrow(deb_list_to_df(list1)), 4)
  expect_equal(ncol(deb_list_to_df(list1)), 3)
  expect_equal(names(deb_list_to_df(list1)), c("l", "s", "d"))
  expect_equal(deb_list_to_df(list1), tbl1)
})

test_that("df checks", {
  expect_error(deb_df_to_list(list1), "df must be a data frame")
  expect_error(deb_df_to_list(transactions, credit, s, d), "l must be a numeric variable")
  expect_error(deb_df_to_list(transactions, l, credit, d), "s must be a numeric variable")
  expect_error(deb_df_to_list(transactions, l, s, credit), "d must be a numeric variable")
})

test_that("deb_df_to_list works",{
  expect_equal(is.list(deb_df_to_list(tbl1)), TRUE)
  expect_equal(length(deb_df_to_list(tbl1)), 4)
  expect_s3_class(deb_df_to_list(tbl1), "lsd")
  expect_equal(deb_df_to_list(tbl1), list1)
  expect_equal(deb_df_to_list(tbl1, bases = c(8, 16)), list2)
  expect_message(deb_df_to_list(transactions, l, s, d), "non-lsd variables were dropped")
  expect_equal(deb_df_to_list(transactions, l, s, d), list1)
  expect_equal(deb_df_to_list(tbl2, pounds, shillings, pence), list1)
})
