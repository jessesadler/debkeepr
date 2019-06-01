## Test decimalization ##

x <- c(10, 3, 2)
y <- c(20, 5, 8)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b1 <- to_lsd(x, b1)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
list2 <- list(x, y)
list1_b1 <- to_lsd(list1, b1)
list2_b2 <- to_lsd(list2, b2)

tbl_b1 <- tibble(lsd = list1_b1)
tbl_b2 <- tibble(lsd = list2_b2)

l1 <- c(8, 8.325, -8.1275)
s1 <- c(123, 123.325, -123.5)
d1 <- c(1339, 1339.25, -1339)

tbl_lsd <- tibble(l = l1, s = s1, d = d1)

# lsd to decimlaized
test_that("lsd to decimalized l, s, and d works", {
  # deb_lsd_l
  expect_equal(deb_lsd_l(c(8, 6, 6)), 8.325)
  expect_equal(deb_lsd_l(c(-8, -6, -6)), -8.325)
  expect_equal(deb_lsd_l(c(8.325, 6, 6)), 8.65)
  expect_equal(deb_lsd_l(c(8, 4, 8), bases = b2), 8.5625)

  # deb_lsd_s
  expect_equal(deb_lsd_s(c(8, 6, 6)), 166.5)
  expect_equal(deb_lsd_s(c(-8, -6, -6)), -166.5)
  expect_equal(deb_lsd_s(c(8.325, 6, 6)), 173)
  expect_equal(deb_lsd_s(c(8, 6, 6), bases = b2), 70.375)

  # deb_lsd_d
  expect_equal(deb_lsd_d(c(8, 6, 6)), 1998)
  expect_equal(deb_lsd_d(c(-8, -6, -6)), -1998)
  expect_equal(deb_lsd_d(c(8.325, 6, 6)), 2076)
  expect_equal(deb_lsd_d(c(8, 6, 6), bases = b2), 1126)
})

test_that("vectorization works for lsd to decimalized l, s, d", {
  # Round numbers to make numbers to be equivalent
  expect_equal(round(deb_lsd_l(list1), 3),
               c(30.538, 11.703, -26.592))
  expect_equal(round(deb_lsd_s(list1), 3),
               c(610.750, 234.067, -531.833))
  expect_equal(deb_lsd_d(list1),
               c(7329.0, 2808.8, -6382.0))
})

test_that("decimalization works with lsd objects", {
  # deb_lsd_l
  expect_identical(deb_lsd_l(x_b2),
                   deb_lsd_l(x, bases = b2))
  expect_identical(deb_lsd_l(list1_b1),
                   deb_lsd_l(list1, bases = b1))
  expect_identical(deb_lsd_l(list2_b2),
                   deb_lsd_l(list2, bases = b2))

  # deb_lsd_s
  expect_identical(deb_lsd_s(x_b2),
                   deb_lsd_s(x, bases = b2))
  expect_identical(deb_lsd_s(list1_b1),
                   deb_lsd_s(list1, bases = b1))
  expect_identical(deb_lsd_s(list2_b2),
                   deb_lsd_s(list2, bases = b2))

  # deb_lsd_d
  expect_identical(deb_lsd_d(x_b2),
                   deb_lsd_d(x, bases = b2))
  expect_identical(deb_lsd_d(list1_b1),
                   deb_lsd_d(list1, bases = b1))
  expect_identical(deb_lsd_d(list2_b2),
                   deb_lsd_d(list2, bases = b2))
})

test_that("decimalization works with lsd column", {
  # mutated column is same as normal decimalization

  # deb_lsd_l
  expect_identical(mutate(tbl_b1, l = deb_lsd_l(lsd))$l,
                   deb_lsd_l(list1_b1))
  expect_identical(mutate(tbl_b2, l = deb_lsd_l(lsd)),
                   tibble(lsd = list2_b2,
                          l = deb_lsd_l(list2_b2)))

  # deb_lsd_s
  expect_identical(mutate(tbl_b1, s = deb_lsd_s(lsd))$s,
                   deb_lsd_s(list1_b1))
  expect_identical(mutate(tbl_b2, s = deb_lsd_s(lsd)),
                   tibble(lsd = list2_b2,
                          s = deb_lsd_s(list2_b2)))

  # deb_lsd_d
  expect_identical(mutate(tbl_b1, d = deb_lsd_d(lsd))$d,
                   deb_lsd_d(list1_b1))
  expect_identical(mutate(tbl_b2, d = deb_lsd_d(lsd)),
                   tibble(lsd = list2_b2,
                          d = deb_lsd_d(list2_b2)))
})

# Decimals to lsd
test_that("non-numeric values are not accepted", {
  expect_error(deb_l_lsd("j"), "l must be numeric")
  expect_error(deb_s_lsd("r"), "s must be numeric")
  expect_error(deb_d_lsd("s"), "d must be numeric")
})

test_that("decimalized to lsd works", {
  # deb_l_lsd
  expect_equal(deb_l_lsd(8), to_lsd(c(8, 0, 0), b1))
  expect_equal(deb_l_lsd(-8), to_lsd(c(-8, 0, 0), b1))
  expect_equal(deb_l_lsd(8.325), to_lsd(c(8, 6, 6), b1))
  expect_equal(deb_l_lsd(8.325, bases = b2), to_lsd(c(8, 2, 9.6), b2))
  expect_equal(deb_l_lsd(8.1275), to_lsd(c(8, 2, 6.6), b1))
  expect_equal(deb_l_lsd(8.1275, round = 0), to_lsd(c(8, 2, 7), b1))

  # deb_s_lsd
  expect_equal(deb_s_lsd(123), to_lsd(c(6, 3, 0), b1))
  expect_equal(deb_s_lsd(-123), to_lsd(c(-6, -3, 0), b1))
  expect_equal(deb_s_lsd(123.325), to_lsd(c(6, 3, 3.9), b1))
  expect_equal(deb_s_lsd(123.325, bases = b2), to_lsd(c(15, 3, 5.2), b2))
  expect_equal(deb_s_lsd(123.325, round = 0), to_lsd(c(6, 3, 4), b1))

  # deb_d_lsd
  expect_equal(deb_d_lsd(1339), to_lsd(c(5, 11, 7), b1))
  expect_equal(deb_d_lsd(-1339), to_lsd(c(-5, -11, -7), b1))
  expect_equal(deb_d_lsd(1339.25), to_lsd(c(5, 11, 7.25), b1))
  expect_equal(deb_d_lsd(1339.25, bases = b2), to_lsd(c(10, 3, 11.25), b2))
  expect_equal(deb_d_lsd(1339.25, round = 0), to_lsd(c(5, 11, 7), b1))
})

test_that("vectorization works for separate l, s, d to lsd", {
  expect_equal(length(deb_l_lsd(c(8.325, -8.725))), 2)
  expect_equal(deb_l_lsd(c(8.325, -8.725)),
               to_lsd(list(c(8, 6, 6), c(-8, -14, -6)), b1))
  expect_equal(deb_l_lsd(c(8.325, -8.725), bases = b2),
               to_lsd(list(c(8, 2, 9.6), c(-8, -5, -12.8)), b2))
  expect_equal(deb_l_lsd(c(8.1275, -8.333), round = 0),
               to_lsd(list(c(8, 2, 7), c(-8, -6, -8)), b1))

  expect_equal(length(deb_s_lsd(c(123, -123.325))), 2)
  expect_equal(deb_s_lsd(c(123, -123.325)),
               to_lsd(list(c(6, 3, 0), c(-6, -3, -3.9)), b1))
  expect_equal(deb_s_lsd(c(123, -123.325), bases = b2),
               to_lsd(list(c(15, 3, 0), c(-15, -3, -5.2)), b2))
  expect_equal(deb_s_lsd(c(123, -123.325), round = 0),
               to_lsd(list(c(6, 3, 0), c(-6, -3, -4)), b1))

  expect_equal(length(deb_d_lsd(c(1339, -1339))), 2)
  expect_equal(deb_d_lsd(c(1339, -1339)),
               to_lsd(list(c(5, 11, 7), c(-5, -11, -7)), b1))
  expect_equal(deb_d_lsd(c(1339, -1339), bases = b2),
               to_lsd(list(c(10, 3, 11), c(-10, -3, -11)), b2))
  expect_equal(deb_d_lsd(c(5.25, 5.75), round = 0),
               to_lsd(list(c(0, 0, 5), c(0, 0, 6)), b1))
})

test_that("decimalization works with lsd column", {
  # mutated column is same as normal decimalization

  # deb_l_lsd
  expect_s3_class(mutate(tbl_lsd, lsd = deb_l_lsd(l))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_lsd, lsd = deb_l_lsd(l, b2))$lsd),
               c(s = 8, d = 16))
  expect_equal(mutate(tbl_lsd, lsd = deb_l_lsd(l, b2))$lsd,
               deb_l_lsd(l1, b2))

  # deb_s_lsd
  expect_s3_class(mutate(tbl_lsd, lsd = deb_s_lsd(l))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_lsd, lsd = deb_s_lsd(l, b2))$lsd),
               c(s = 8, d = 16))
  expect_equal(mutate(tbl_lsd, lsd = deb_s_lsd(l, b2))$lsd,
               deb_s_lsd(l1, b2))

  # deb_d_lsd
  expect_s3_class(mutate(tbl_lsd, lsd = deb_d_lsd(l))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_lsd, lsd = deb_d_lsd(l, b2))$lsd),
               c(s = 8, d = 16))
  expect_equal(mutate(tbl_lsd, lsd = deb_d_lsd(l, b2))$lsd,
               deb_d_lsd(l1, b2))
})
