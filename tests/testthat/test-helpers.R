context("test-helpers.R")

suppressPackageStartupMessages(library(tibble))

ex_list <- list(c(l = 35, s = 50, d = 89),
                c(l = -10, s = -48, d = -181),
                c(l = 26.875, s = 84.1333, d = 55),
                c(l = 12, s = 76, d = 205))
ex_tbl <- tibble::tibble(l = c(35, -10, 26.875, 12),
                         s = c(50, -48, 84.1333, 76),
                         d = c(89, -181, 55, 205))
ex_tbl2 <- tibble::tibble(pounds = c(35, -10, 26.875, 12),
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
  expect_error(deb_list_to_df(ex_tbl),
               "lsd_list must be a list of numeric vectors")
})

test_that("deb_list_to_df works",{
  expect_equal(is.data.frame(deb_list_to_df(ex_list)), TRUE)
  expect_equal(nrow(deb_list_to_df(ex_list)), 4)
  expect_equal(ncol(deb_list_to_df(ex_list)), 3)
  expect_equal(names(deb_list_to_df(ex_list)), c("l", "s", "d"))
  expect_equal(deb_list_to_df(ex_list), ex_tbl)
})

test_that("df checks", {
  expect_error(deb_df_to_list(ex_list), "df must be a data frame")
  expect_error(deb_df_to_list(transactions, credit, s, d), "l must be a numeric variable")
  expect_error(deb_df_to_list(transactions, l, credit, d), "s must be a numeric variable")
  expect_error(deb_df_to_list(transactions, l, s, credit), "d must be a numeric variable")
})

test_that("deb_df_to_list works",{
  expect_equal(is.list(deb_df_to_list(ex_tbl)), TRUE)
  expect_equal(length(deb_df_to_list(ex_tbl)), 4)
  expect_equal(names(deb_df_to_list(ex_tbl)[[1]]), c("l", "s", "d"))
  expect_equal(deb_df_to_list(ex_tbl), ex_list)
  expect_message(deb_df_to_list(transactions, l, s, d), "non-lsd variables were dropped")
  expect_equal(deb_df_to_list(transactions, l, s, d), ex_list)
  expect_equal(deb_df_to_list(ex_tbl2, pounds, shillings, pence), ex_list)
})
