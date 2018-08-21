context("test-lsd-list-column.R")

suppressPackageStartupMessages(library(tibble))

df1 <- data.frame(l = c(3, 5, 6, 2),
                  s = c(10, 18, 11, 16),
                  d = c(9, 11, 10, 5))
df2 <- data.frame(pounds = c(3, 5, 6, 2),
                  shillings = c(10, 18, 11, 16),
                  pence = c(9, 11, 10, 5))
df_lsd <- deb_as_lsd_mutate(df1, replace = TRUE)
tbl <- tibble(l = c(3, 5, 6, 2),
              s = c(10, 18, 11, 16),
              d = c(9, 11, 10, 5))
lsd_list <- to_lsd(list(c(3, 10, 9),
                        c(5, 18, 11),
                        c(6, 11, 10),
                        c(2, 16, 5)),
                   bases = c(20, 12))
tbl_lsd1 <- tibble(lsd = lsd_list)
tbl_lsd2 <- tibble(data = lsd_list)

test_that("deb_as_lsd_mutate works", {
  expect_s3_class(deb_as_lsd_mutate(df1)$lsd, "lsd")
  expect_s3_class(deb_as_lsd_mutate(df2, pounds, shillings, pence)$lsd, "lsd")
  expect_s3_class(deb_as_lsd_mutate(tbl)$lsd, "lsd")
  expect_named(deb_as_lsd_mutate(df2, pounds, shillings, pence, lsd_column = data),
               c("pounds", "shillings", "pence", "data"))
  expect_s3_class(deb_as_lsd_mutate(df1, lsd_column = data)$data, "lsd")
  expect_equal(deb_bases(deb_as_lsd_mutate(df1, bases = c(8, 16))$lsd),
               c(s = 8, d = 16))
  expect_equal(ncol(deb_as_lsd_mutate(df1, replace = TRUE)), 1)
  expect_identical(deb_as_lsd_mutate(df1, replace = TRUE)$lsd,
                   lsd_list)
})

test_that("deb_from_lsd_mutate works", {
  expect_equal(ncol(deb_from_lsd_mutate(tbl_lsd1)), 4)
  expect_equal(ncol(deb_from_lsd_mutate(tbl_lsd2, lsd = data)), 4)
  expect_equal(ncol(deb_from_lsd_mutate(df_lsd)), 4)
  expect_named(deb_from_lsd_mutate(tbl_lsd1, lsd, pounds, shillings, pence),
               c("lsd", "pounds", "shillings", "pence"))
  expect_equal(ncol(deb_from_lsd_mutate(tbl_lsd1, replace = TRUE)), 3)
  expect_equal(deb_from_lsd_mutate(tbl_lsd1, replace = TRUE), tbl)
  expect_named(deb_from_lsd_mutate(deb_as_lsd_mutate(tbl)),
               c("l", "s", "d", "lsd", "l.1", "s.1", "d.1"))
  expect_equal(deb_from_lsd_mutate(deb_as_lsd_mutate(tbl, replace = TRUE), replace = TRUE), tbl)
})
