## Test normalization ##

suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(dplyr))

x <- c(5, 84, 53)
neg <- c(-5, -84, -53)
dec <- c(5.875, 84.325, 55)
mix <- c(5, -67, -35)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b2 <- to_lsd(x, b2)
dec_b2 <- to_lsd(dec, b2)

list1 <- list(c(35, 50, 89),
              c(-10, -48, -181),
              c(26.875, 84.365, 55),
              c(12, 76, 205))
list2 <- list(x, dec, neg)
list1_b1 <- to_lsd(list1, b1)
list1_b2 <- to_lsd(list1, b2)
list2_b2 <- to_lsd(list2, b2)

tbl_b1 <- tibble(lsd = list1_b1)
tbl_b2 <- tibble(lsd = list1_b2)


## lsd_decimal ##
test_that("lsd_decimal_check", {
  expect_equal(lsd_decimal_check(dec, bases = c(20, 12)), c(5, 101, 64.9))
  expect_equal(lsd_decimal_check(-dec, bases = c(20, 12)), c(5, 101, 64.9))
  expect_equal(lsd_decimal_check(c(8.5, 7, 0), bases = c(20, 12)), c(8, 17, 0))
  expect_equal(lsd_decimal_check(c(8.5, 7, 0), bases = c(16, 12)), c(8, 15, 0))
  expect_equal(lsd_decimal_check(c(8, 29.875, 30), bases = c(20, 12)), c(8, 29, 40.5))
  expect_equal(lsd_decimal_check(c(NA, 3, 4)), c(NA, NA, NA))
})

## Normalization of lsd ##
test_that("it goes together in deb_normalize", {
  expect_equal(deb_normalize(x), to_lsd(c(9, 8, 5), b1))
  expect_equal(deb_normalize(neg), to_lsd(c(-9, -8, -5), b1))
  expect_equal(deb_normalize(dec), to_lsd(c(10, 6, 4.9), b1))
  expect_equal(deb_normalize(mix), to_lsd(c(1, 10, 1), b1))
  expect_equal(deb_normalize(c(NA, 4, 5)),
               to_lsd(as.numeric(c(NA, NA, NA)), b1))
  # rounding and avoid d being equal to base of d
  expect_equal(deb_normalize(c(1, 19, 11.999999)), to_lsd(c(2, 0, 0), b1))
  expect_equal(deb_normalize(c(1, 18, 23.999999)), to_lsd(c(2, 0, 0), b1))
  expect_equal(deb_normalize(c(-1, -19, -11.999999)), to_lsd(c(-2, 0, 0), b1))
  expect_equal(deb_normalize(c(1, 19, 11.99999)), to_lsd(c(1, 19, 11.99999), b1))
  expect_equal(deb_normalize(c(1, 7, 15.999999), b2),
               to_lsd(c(2, 0, 0), c(8, 16)))
  expect_equal(deb_normalize(c(1, 5, 11 + 2/3), round = 0), to_lsd(c(1, 6, 0), b1))
})

## Vectorization ##
test_that("vectorization works", {
  expect_equal(is.list(deb_normalize(list1)), TRUE)
  expect_equal(length(deb_normalize(list1)), 4)
  expect_equal(deb_normalize(list1),
               to_lsd(list(c(37, 17, 5),
                           c(-13, -3, -1),
                           c(31, 6, 5.38),
                           c(16, 13, 1)), b1))
  expect_equal(deb_normalize(list1, bases = c(20, 16)),
               to_lsd(list(c(37, 15, 9),
                           c(-12, -19, -5),
                           c(31, 5, 4.84),
                           c(16, 8, 13)), c(20, 16)))
  expect_false(identical(deb_normalize(list1_b1), deb_normalize(list1_b2)))
  expect_equal(deb_normalize(list1, round = 0)[3], to_lsd(c(31, 6, 5), b1))
})

## bases ##
test_that("different basess work", {
  expect_equal(deb_normalize(x, bases = c(20, 12)),
               to_lsd(c(9, 8, 5), b1))
  expect_equal(deb_normalize(x_b2),
               to_lsd(c(15, 7, 5), b2))
  expect_equal(deb_normalize(x, bases = c(20, 16)),
               to_lsd(c(9, 7, 5), bases = c(20, 16)))
  expect_equal(deb_normalize(neg, b2),
               to_lsd(c(-15, -7, -5), b2))
})

## lsd and lsd_list class
test_that("works with lsd class", {
  # Creates lsd class
  expect_s3_class(deb_normalize(x), "lsd")
  expect_s3_class(deb_normalize(list1), "lsd")
  # Maintains lsd class
  expect_s3_class(deb_normalize(x_b2), "lsd")
  expect_s3_class(deb_normalize(list1_b1), "lsd")

  # Works with a list of length 1
  expect_s3_class(deb_normalize(list(x)), "lsd")

  # Use and maintain base attribute
  expect_equal(deb_bases(deb_normalize(x_b2)), c(s = 8, d = 16))
  expect_equal(deb_normalize(x_b2),
               deb_normalize(x, bases = b2))
  expect_equal(deb_bases(deb_normalize(list1_b2)), c(s = 8, d = 16))
  expect_equal(deb_normalize(list1_b2),
               deb_normalize(list1, bases = b2))
})

test_that("normalize works with lsd column", {
  # mutate works
  expect_equal(ncol(mutate(tbl_b1, lsd2 = deb_normalize(lsd))), 2)
  expect_equal(ncol(mutate(tbl_b1, lsd = deb_normalize(lsd))), 1)

  # mutated column is lsd
  expect_s3_class(mutate(tbl_b1, lsd = deb_normalize(lsd))$lsd, "lsd")
  expect_equal(deb_bases(mutate(tbl_b2, lsd = deb_normalize(lsd))$lsd),
               c(s = 8, d = 16))

  # mutated column is same as normal deb_normalize
  expect_identical(mutate(tbl_b1, lsd = deb_normalize(lsd))$lsd,
                   deb_normalize(list1_b1))
  expect_identical(mutate(tbl_b2, lsd = deb_normalize(lsd)),
                   tibble(lsd = deb_normalize(list1_b2)))
})
