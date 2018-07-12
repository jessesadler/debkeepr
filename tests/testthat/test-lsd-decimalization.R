context("test-lsd-decimalization.R")

# These are all based on deb_normalize
test_that("decimalized to lsd works", {
  # deb_l_lsd
  expect_equal(deb_l_lsd(8), c(l = 8, s = 0, d = 0))
  expect_equal(deb_l_lsd(-8), c(l = -8, s = 0, d = 0))
  expect_equal(deb_l_lsd(8.325), c(l = 8, s = 6, d = 6))
  expect_equal(deb_l_lsd(8.325, lsd_bases = c(8, 16)), c(l = 8, s = 2, d = 9.6))

  # deb_s_lsd
  expect_equal(deb_s_lsd(123), c(l = 6, s = 3, d = 0))
  expect_equal(deb_s_lsd(-123), c(l = -6, s = -3, d = 0))
  expect_equal(deb_s_lsd(123.325), c(l = 6, s = 3, d = 3.9))
  expect_equal(deb_s_lsd(123.325, lsd_bases = c(8, 16)), c(l = 15, s = 3, d = 5.2))

  # deb_d_lsd
  expect_equal(deb_d_lsd(1339), c(l = 5, s = 11, d = 7))
  expect_equal(deb_d_lsd(-1339), c(l = -5, s = -11, d = -7))
  expect_equal(deb_d_lsd(1339.25), c(l = 5, s = 11, d = 7.25))
  expect_equal(deb_d_lsd(1339.25, lsd_bases = c(8, 16)), c(l = 10, s = 3, d = 11.25))
})

test_that("vectorization works for separate l, s, d to lsd", {
  expect_equal(length(deb_l_lsd(c(8.325, -8.725))), 2)
  expect_equal(deb_l_lsd(c(8.325, -8.725)),
              list(c(l = 8, s = 6, d = 6), c(l = -8, s = -14, d = -6)))

  expect_equal(length(deb_s_lsd(c(123, -123.325))), 2)
  expect_equal(deb_s_lsd(c(123, -123.325)),
               list(c(l = 6, s = 3, d = 0), c(l = -6, s = -3, d = -3.9)))

  expect_equal(length(deb_d_lsd(c(1339, -1339))), 2)
  expect_equal(deb_d_lsd(c(1339, -1339)),
               list(c(l = 5, s = 11, d = 7), c(l = -5, s = -11, d = -7)))
})

# Reverse of above
test_that("lsd to decimalized l, s, and d works", {
  # deb_lsd_l
  expect_equal(deb_lsd_l(c(8, 6, 6)), 8.325)
  expect_equal(deb_lsd_l(c(-8, -6, -6)), -8.325)
  expect_equal(deb_lsd_l(c(8.325, 6, 6)), 8.65)
  expect_equal(deb_lsd_l(c(8, 4, 8), lsd_bases = c(8, 16)), 8.5625)

  # deb_lsd_s
  expect_equal(deb_lsd_s(c(8, 6, 6)), 166.5)
  expect_equal(deb_lsd_s(c(-8, -6, -6)), -166.5)
  expect_equal(deb_lsd_s(c(8.325, 6, 6)), 173)
  expect_equal(deb_lsd_s(c(8, 6, 6), lsd_bases = c(8, 16)), 70.375)

  # deb_lsd_d
  expect_equal(deb_lsd_d(c(8, 6, 6)), 1998)
  expect_equal(deb_lsd_d(c(-8, -6, -6)), -1998)
  expect_equal(deb_lsd_d(c(8.325, 6, 6)), 2076)
  expect_equal(deb_lsd_d(c(8, 6, 6), lsd_bases = c(8, 16)), 1126)
})

ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))

test_that("vectorization works for lsd to decimalized l, s, d", {
  # Round numbers to make numbers to be equivalent
  expect_equal(round(deb_lsd_l(ex_list), 3),
               c(30.538, 11.703, -26.592))
  expect_equal(round(deb_lsd_s(ex_list), 3),
               c(610.750, 234.067, -531.833))
  expect_equal(deb_lsd_d(ex_list),
               c(7329.0, 2808.8, -6382.0))
})

test_that("non-numeric values are not accepted", {
  # decimal to lsd
  expect_error(deb_l_lsd("j"), "l must be numeric")
  expect_error(deb_s_lsd("r"), "s must be numeric")
  expect_error(deb_d_lsd("s"), "d must be numeric")
})
