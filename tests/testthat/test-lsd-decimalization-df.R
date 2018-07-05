context("test-lsd-decimalization-df.R")

ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))
l_df <- data.frame(librae = deb_lsd_l(ex_list))
s_df <- data.frame(solidi = deb_lsd_s(ex_list))
d_df <- data.frame(denarii = deb_lsd_d(ex_list))

test_that("individual l, s, and d helper functions work", {
  expect_equal(decimalize_l(8, 10, 6), 8.525)
  expect_equal(decimalize_s(8, 10, 6), 170.5)
  expect_equal(decimalize_d(8, 10, 6), 2046)

  expect_equal(round(decimalize_l(8, 10, 6, lsd_bases = c(8, 16)), 3), 9.297)
  expect_equal(decimalize_s(8, 10, 6, lsd_bases = c(8, 16)), 74.375)
  expect_equal(decimalize_d(8, 10, 6, lsd_bases = c(8, 16)), 1190)
})

test_that("df pounds decimalization", {
  expect_equal(names(deb_lsd_l_mutate(ex_df)),
               c("l", "s", "d", "librae"))
  expect_equal(names(deb_lsd_l_mutate(ex_df, column_name = pounds)),
               c("l", "s", "d", "pounds"))
  expect_equal(deb_lsd_l_mutate(ex_df), cbind(ex_df, l_df))
})

test_that("df shillings decimalization", {
  expect_equal(names(deb_lsd_s_mutate(ex_df)),
               c("l", "s", "d", "solidi"))
  expect_equal(names(deb_lsd_s_mutate(ex_df, column_name = shillings)),
               c("l", "s", "d", "shillings"))
  expect_equal(deb_lsd_s_mutate(ex_df), cbind(ex_df, s_df))
})

test_that("df pence decimalization", {
  expect_equal(names(deb_lsd_d_mutate(ex_df)),
               c("l", "s", "d", "denarii"))
  expect_equal(names(deb_lsd_d_mutate(ex_df, column_name = pence)),
               c("l", "s", "d", "pence"))
  expect_equal(deb_lsd_d_mutate(ex_df), cbind(ex_df, d_df))
})
