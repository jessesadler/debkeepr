context("test-lsd-transformations.R")

# These are all based on deb_normalize
test_that("decimalized to lsd works", {
  # deb_l_lsd
  expect_equal(deb_l_lsd(8, vector = TRUE), c(l = 8, s = 0, d = 0))
  expect_equal(deb_l_lsd(-8, vector = TRUE), c(l = -8, s = 0, d = 0))
  expect_equal(deb_l_lsd(8.325, vector = TRUE), c(l = 8, s = 6, d = 6))

  # deb_s_lsd
  expect_equal(deb_s_lsd(123, vector = TRUE), c(l = 6, s = 3, d = 0))
  expect_equal(deb_s_lsd(-123, vector = TRUE), c(l = -6, s = -3, d = 0))
  expect_equal(deb_s_lsd(123.325, vector = TRUE), c(l = 6, s = 3, d = 3.9))

  # deb_d_lsd
  expect_equal(deb_d_lsd(1339, vector = TRUE), c(l = 5, s = 11, d = 7))
  expect_equal(deb_d_lsd(-1339, vector = TRUE), c(l = -5, s = -11, d = -7))
  expect_equal(deb_d_lsd(1339.25, vector = TRUE), c(l = 5, s = 11, d = 7.25))
})

test_that("vectorization works for separate l, s, d to lsd", {
  expect_equal(nrow(deb_l_lsd(c(8.325, -8.725))), 2)
  expect_equal(length(deb_l_lsd(c(8.325, -8.725), vector = TRUE)), 2)
  expect_equal(deb_l_lsd(c(8.325, -8.725), vector = TRUE),
              list(c(l = 8, s = 6, d = 6), c(l = -8, s = -14, d = -6)))

  expect_equal(nrow(deb_s_lsd(c(123, -123.325))), 2)
  expect_equal(length(deb_s_lsd(c(123, -123.325), vector = TRUE)), 2)
  expect_equal(deb_s_lsd(c(123, -123.325), vector = TRUE),
               list(c(l = 6, s = 3, d = 0), c(l = -6, s = -3, d = -3.9)))

  expect_equal(nrow(deb_d_lsd(c(1339, -1339))), 2)
  expect_equal(length(deb_d_lsd(c(1339, -1339), vector = TRUE)), 2)
  expect_equal(deb_d_lsd(c(1339, -1339), vector = TRUE),
               list(c(l = 5, s = 11, d = 7), c(l = -5, s = -11, d = -7)))
})

# Reverse of above
test_that("lsd to decimalized l, s, and d works", {
  # deb_lsd_l
  expect_equal(deb_lsd_l(8, 6, 6), 8.325)
  expect_equal(deb_lsd_l(-8, -6, -6), -8.325)
  expect_equal(deb_lsd_l(8.325, 6, 6), 8.65)

  # deb_lsd_s
  expect_equal(deb_lsd_s(8, 6, 6), 166.5)
  expect_equal(deb_lsd_s(-8, -6, -6), -166.5)
  expect_equal(deb_lsd_s(8.325, 6, 6), 173)

  # deb_lsd_d
  expect_equal(deb_lsd_d(8, 6, 6), 1998)
  expect_equal(deb_lsd_d(-8, -6, -6), -1998)
  expect_equal(deb_lsd_d(8.325, 6, 6), 2076)
})

l_vector <- c(8, 4, 6)
s_vector <- c(87, 35, 64)
d_vector <- c(46, 58, 96)

test_that("vectorization works for lsd to decimalized l, s, d", {
  # Round numbers to make get numbers to be equivalent
  expect_equal(round(deb_lsd_l(l_vector, s_vector, d_vector), 3),
               c(12.542, 5.992, 9.6))
  expect_equal(round(deb_lsd_s(l_vector, s_vector, d_vector), 3),
               c(250.833, 119.833, 192))
  expect_equal(deb_lsd_d(l_vector, s_vector, d_vector),
               c(3010, 1438, 2304))
})

test_that("non-numeric values are not accepted", {
  # decimal to lsd
  expect_error(deb_l_lsd("j"), "l must be numeric")
  expect_error(deb_s_lsd("r"), "s must be numeric")
  expect_error(deb_d_lsd("s"), "d must be numeric")

  # lsd to decimal
  expect_error(deb_lsd_l("j", 3, 2), "l must be numeric")
  expect_error(deb_lsd_s(3, "r", 2), "s must be numeric")
  expect_error(deb_lsd_d(3, 3, "s"), "d must be numeric")
})
