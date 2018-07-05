context("test-lsd-mutate.R")

## Test individual helper functions ##

test_that("separate librae functions work", {
  expect_equal(deb_librae_l(8), 8)
  expect_equal(deb_librae_l(-8), -8)
  expect_equal(deb_librae_l(8.325), 8)

  expect_equal(deb_librae_s(8), 0)
  expect_equal(deb_librae_s(8.325), 6)
  expect_equal(deb_librae_s(8.5), 10)
  expect_equal(deb_librae_s(-8.325), -6)

  expect_equal(deb_librae_d(8), 0)
  expect_equal(deb_librae_d(8.325), 6)
  expect_equal(deb_librae_d(-8.325), -6)
  expect_equal(deb_librae_d(8.35678, round = 4), 1.6272)
  expect_equal(deb_librae_d(8.35678, round = 0), 2)
})

# Test vectorization
l_vector <- c(8, 8.325, -8.325, 5.425, 4.5678)
# Answers
ll_solution <- c(8, 8, -8, 5, 4)
ls_solution <- c(0, 6, -6, 8, 11)
ld_solution <- c(0, 6, -6, 6, 4.272)
ld_solution_round <- c(0, 6, -6, 6, 4)

test_that("librae functions are vectorized", {
  expect_equal(deb_librae_l(l_vector), ll_solution)
  expect_equal(deb_librae_s(l_vector), ls_solution)
  expect_equal(deb_librae_d(l_vector), ld_solution)
  expect_equal(deb_librae_d(l_vector, round = 0), ld_solution_round)
})

test_that("separate solidi functions work", {
  expect_equal(deb_solidi_l(166), 8)
  expect_equal(deb_solidi_l(-166), -8)
  expect_equal(deb_solidi_l(166.5), 8)

  expect_equal(deb_solidi_s(166), 6)
  expect_equal(deb_solidi_s(166.5), 6)
  expect_equal(deb_solidi_s(-166.5), -6)

  expect_equal(deb_solidi_d(166), 0)
  expect_equal(deb_solidi_d(166.5), 6)
  expect_equal(deb_solidi_d(-166.5), -6)
  expect_equal(deb_solidi_d(166.35678, round = 5), 4.28136)
  expect_equal(deb_solidi_d(166.35678, round = 0), 4)
})

# Test vectorization
s_vector <- c(166, -166, 166.5, 236.35678, -354.845)
# Answers
sl_solution <- c(8, -8, 8, 11, -17)
ss_solution <- c(6, -6, 6, 16, -14)
sd_solution <- c(0, 0, 6, 4.281, -10.140)
sd_solution_round <- c(0, 0, 6, 4, -10)

test_that("solidi functions are vectorized", {
  expect_equal(deb_solidi_l(s_vector), sl_solution)
  expect_equal(deb_solidi_s(s_vector), ss_solution)
  expect_equal(deb_solidi_d(s_vector), sd_solution)
  expect_equal(deb_solidi_d(s_vector, round = 0), sd_solution_round)
})

test_that("separate denarii functions work", {
  expect_equal(deb_denarii_l(1998), 8)
  expect_equal(deb_denarii_l(-1998), -8)

  expect_equal(deb_denarii_s(1998), 6)
  expect_equal(deb_denarii_s(-1998), -6)

  expect_equal(deb_denarii_d(1998), 6)
  expect_equal(deb_denarii_d(-1998), -6)
  expect_equal(deb_denarii_d(1998.85), 6.85)
})

# Test vectorization
d_vector <- c(1998, -1998, 387, -5378)
# Answers
dl_solution <- c(8, -8, 1, -22)
ds_solution <- c(6, -6, 12, -8)
dd_solution <- c(6, -6, 3, -2)

test_that("denarii functions are vectorized", {
  expect_equal(deb_denarii_l(d_vector), dl_solution)
  expect_equal(deb_denarii_s(d_vector), ds_solution)
  expect_equal(deb_denarii_d(d_vector), dd_solution)
})

## Test mutate functions ##

l_df <- data.frame(pounds = l_vector)
s_df <- data.frame(shillings = s_vector)
d_df <- data.frame(pence = d_vector)

l_solution_df <- data.frame(pounds = l_vector,
                            l = ll_solution,
                            s = ls_solution,
                            d = ld_solution)
s_solution_df <- data.frame(shillings = s_vector,
                            l = sl_solution,
                            s = ss_solution,
                            d = sd_solution)
d_solution_df <- data.frame(pence = d_vector,
                            l = dl_solution,
                            s = ds_solution,
                            d = dd_solution)

test_that("mutate functions work", {
  expect_equal(deb_l_mutate(l_df, pounds, l, s, d), l_solution_df)
  expect_equal(deb_l_mutate(l_df, pounds, l, s, d, round = 0)[5, 4], 4)
  expect_false(identical(deb_l_mutate(l_df, pounds, l, s, d),
                         deb_l_mutate(l_df, pounds, l, s, d, lsd_bases = c(8, 16))))

  expect_equal(deb_s_mutate(s_df, shillings), s_solution_df)
  expect_equal(deb_s_mutate(s_df, shillings, round = 0)[ , 4], c(0, 0, 6, 4 , -10))
  expect_false(identical(deb_s_mutate(s_df, shillings),
                         deb_s_mutate(s_df, shillings, lsd_bases = c(8, 16))))

  expect_equal(deb_d_mutate(d_df, pence), d_solution_df)
  expect_false(identical(deb_d_mutate(d_df, pence),
                         deb_d_mutate(d_df, pence, lsd_bases = c(8, 16))))
})

test_that("column names change with lsd_column_names function", {
  # Can change names
  expect_equal(names(deb_l_mutate(l_df, pounds,
                            l_column = "librae", s_column = "solidi", d_column = "denarii")),
               c("pounds", "librae", "solidi", "denarii"))
  # Suffix added if names already present
  expect_equal(names(deb_s_mutate(s_solution_df, shillings)),
               c("shillings", "l", "s", "d", "l.1", "s.1", "d.1"))
  # Can change suffix
  expect_equal(names(deb_d_mutate(d_solution_df, pence, suffix = ".x")),
               c("pence", "l", "s", "d", "l.x", "s.x", "d.x"))
})

test_that("lsd_column_names check works", {
  expect_error(deb_l_mutate(l_df, pounds, suffix = 7),
               "suffix must be a character vector")
  expect_error(deb_l_mutate(l_df, pounds, suffix = c("hello", "you")),
               "suffix must be a character vector of length 1")
})

test_that("decimal column exists", {
  expect_error(deb_l_mutate(l_df, librae = hello),
               "librae column must exist the in df")
  expect_error(deb_s_mutate(s_df, solidi = hello),
               "solidi column must exist the in df")
  expect_error(deb_d_mutate(d_df, denarii = hello),
               "denarii column must exist the in df")
})

id_df <- add_column(l_df, id = letters[1:5])

test_that("decimal column is numeric", {
  expect_error(deb_l_mutate(id_df, librae = id),
               "librae must be numeric")
  expect_error(deb_s_mutate(id_df, solidi = id),
               "solidi must be numeric")
  expect_error(deb_d_mutate(id_df, denarii = id),
               "denarii must be numeric")
})
