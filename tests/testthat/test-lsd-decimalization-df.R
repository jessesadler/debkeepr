context("test-lsd-decimalization-df.R")

ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))
na_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(NA, 11, -10))
ex_df_names <- data.frame(pounds = c(30, 10.725, -26),
                          shillings = c(10, 18.65, -11),
                          pence = c(9, 11, -10))
l_df <- data.frame(librae = deb_lsd_l(ex_list))
s_df <- data.frame(solidi = deb_lsd_s(ex_list))
d_df <- data.frame(denarii = deb_lsd_d(ex_list))

## lsd to decimalized values ##

test_that("individual l, s, and d helper functions work", {
  expect_equal(decimalize_l(8, 10, 6), 8.525)
  expect_equal(decimalize_s(8, 10, 6), 170.5)
  expect_equal(decimalize_d(8, 10, 6), 2046)

  expect_equal(round(decimalize_l(8, 10, 6, lsd_bases = c(8, 16)), 3), 9.297)
  expect_equal(decimalize_s(8, 10, 6, lsd_bases = c(8, 16)), 74.375)
  expect_equal(decimalize_d(8, 10, 6, lsd_bases = c(8, 16)), 1190)
})

test_that("NA works", {
  expect_equal(deb_lsd_l_mutate(na_df)[1 ,],
               data.frame(l = 30, s = 10, d = as.numeric(NA), librae = as.numeric(NA)))
})

test_that("lsd df to decimalized pounds", {
  expect_equal(names(deb_lsd_l_mutate(ex_df)),
               c("l", "s", "d", "librae"))
  expect_equal(names(deb_lsd_l_mutate(ex_df_names, l = pounds, s = shillings, d = pence)),
               c("pounds", "shillings", "pence", "librae"))
  expect_equal(names(deb_lsd_l_mutate(ex_df, column_name = pounds)),
               c("l", "s", "d", "pounds"))
  expect_equal(deb_lsd_l_mutate(ex_df), cbind(ex_df, l_df))
})

test_that("lsd df to decimalized shillings", {
  expect_equal(names(deb_lsd_s_mutate(ex_df)),
               c("l", "s", "d", "solidi"))
  expect_equal(names(deb_lsd_s_mutate(ex_df_names, l = pounds, s = shillings, d = pence)),
               c("pounds", "shillings", "pence", "solidi"))
  expect_equal(names(deb_lsd_s_mutate(ex_df, column_name = shillings)),
               c("l", "s", "d", "shillings"))
  expect_equal(deb_lsd_s_mutate(ex_df), cbind(ex_df, s_df))
})

test_that("lsd df to decimalized pence", {
  expect_equal(names(deb_lsd_d_mutate(ex_df)),
               c("l", "s", "d", "denarii"))
  expect_equal(names(deb_lsd_d_mutate(ex_df_names, l = pounds, s = shillings, d = pence)),
               c("pounds", "shillings", "pence", "denarii"))
  expect_equal(names(deb_lsd_d_mutate(ex_df, column_name = pence)),
               c("l", "s", "d", "pence"))
  expect_equal(deb_lsd_d_mutate(ex_df), cbind(ex_df, d_df))
})

## Decimalized values to lsd ##
v_decimal <- c(327.873, 683.33333, 683.33333333, -645.865, 11.999999, NA)

df_decimal <- data.frame(decimal = v_decimal)
l_decimal <- data.frame(decimal = v_decimal,
                        l = c(327, 683, 683, -645, 11, as.numeric(NA)),
                        s = c(17, 6, 6, -17, 19, as.numeric(NA)),
                        d = c(5.52, 7.9992, 8, -3.6, 11.99976, as.numeric(NA)))
s_decimal <- data.frame(decimal = v_decimal,
                        l = c(16, 34, 34, -32, 0, as.numeric(NA)),
                        s = c(7, 3, 3, -5, 11, as.numeric(NA)),
                        d = c(10.476, 3.99996, 4, -10.38, 11.99999, as.numeric(NA)))
d_decimal <- data.frame(decimal = v_decimal,
                        l = c(1, 2, 2, -2, 0, as.numeric(NA)),
                        s = c(7, 16, 16, -13, 1, as.numeric(NA)),
                        d = c(3.873, 11.33333, 11.33333, -9.865, 0, as.numeric(NA)))

test_that("decimal column exists", {
  expect_error(deb_l_lsd_mutate(df_decimal, librae = hello),
               "librae column must exist in df")
  expect_error(deb_s_lsd_mutate(df_decimal, solidi = hello),
               "solidi column must exist in df")
  expect_error(deb_d_lsd_mutate(df_decimal, denarii = hello),
               "denarii column must exist in df")
})

id_df <- data.frame(id = letters[1:3])

test_that("decimal column is numeric", {
  expect_error(deb_l_lsd_mutate(id_df, librae = id),
               "librae must be numeric")
  expect_error(deb_s_lsd_mutate(id_df, solidi = id),
               "solidi must be numeric")
  expect_error(deb_d_lsd_mutate(id_df, denarii = id),
               "denarii must be numeric")
})

test_that("Decimalized values df to lsd", {
  expect_equal(deb_l_lsd_mutate(df_decimal, decimal), l_decimal)
  expect_false(identical(deb_l_lsd_mutate(df_decimal, decimal),
                         deb_l_lsd_mutate(df_decimal, decimal, lsd_bases = c(8, 16))))

  expect_equal(deb_s_lsd_mutate(df_decimal, decimal), s_decimal)
  expect_false(identical(deb_s_lsd_mutate(df_decimal, decimal),
                         deb_s_lsd_mutate(df_decimal, decimal, lsd_bases = c(8, 16))))

  expect_equal(deb_d_lsd_mutate(df_decimal, decimal), d_decimal)
  expect_false(identical(deb_d_lsd_mutate(df_decimal, decimal),
                         deb_d_lsd_mutate(df_decimal, decimal, lsd_bases = c(8, 16))))
})

test_that("column names change with lsd_column_names function", {
  # Can change names
  expect_equal(names(deb_l_lsd_mutate(df_decimal, decimal,
                                      l_column = "librae",
                                      s_column = "solidi",
                                      d_column = "denarii")),
               c("decimal", "librae", "solidi", "denarii"))
  # Suffix added if names already present
  expect_equal(names(deb_s_lsd_mutate(s_decimal, decimal)),
               c("decimal", "l", "s", "d", "l.1", "s.1", "d.1"))
  # Can change suffix
  expect_equal(names(deb_d_lsd_mutate(s_decimal, decimal, suffix = ".x")),
               c("decimal", "l", "s", "d", "l.x", "s.x", "d.x"))
})
