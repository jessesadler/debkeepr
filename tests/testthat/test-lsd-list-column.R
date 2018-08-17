context("test-lsd-list-column.R")

df1 <- data.frame(l = c(3, 5, 6, 2),
                  s = c(10, 18, 11, 16),
                  d = c(9, 11, 10, 5))
df2 <- data.frame(pounds = c(3, 5, 6, 2),
                  shillings = c(10, 18, 11, 16),
                  pence = c(9, 11, 10, 5))
lsd_list <- to_lsd(list(c(3, 10, 9),
                        c(5, 18, 11),
                        c(6, 11, 10),
                        c(2, 16, 5)),
                   bases = c(20, 12))

test_that("deb_as_lsd_mutate works", {
  expect_s3_class(deb_as_lsd_mutate(df1)$lsd, "lsd")
  expect_s3_class(deb_as_lsd_mutate(df2, pounds, shillings, pence)$lsd, "lsd")
  expect_named(deb_as_lsd_mutate(df2, pounds, shillings, pence, lsd_column = data),
               c("pounds", "shillings", "pence", "data"))
  expect_s3_class(deb_as_lsd_mutate(df1, lsd_column = data)$data, "lsd")
  expect_equal(deb_bases(deb_as_lsd_mutate(df1, bases = c(8, 16))$lsd),
               c(s = 8, d = 16))
  expect_equal(ncol(deb_as_lsd_mutate(df1, replace = TRUE)), 1)
})
