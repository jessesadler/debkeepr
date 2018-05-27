context("test-lsd-arithmetic.R")

test_that("lsd multiplication works", {
  expect_equal(deb_multiply(x = 3, 10, 3, 2, vector = TRUE),
               c(l = 30, s = 9, d = 6))
  expect_equal(deb_multiply(x = -3, 10, 3, 2, vector = TRUE),
               c(l = -30, s = -9, d = -6))
  expect_equal(deb_multiply(x = 10, 10, 3, 2, vector = TRUE),
               c(l = 101, s = 11, d = 8))
  expect_equal(deb_multiply(x = 5.5, 10, 3, 2, vector = TRUE),
               c(l = 55, s = 17, d = 5))
})

test_that("lsd division works", {
  expect_equal(deb_divide(x = 3, 9, 3, 6, vector = TRUE),
               c(l = 3, s = 1, d = 2))
  expect_equal(deb_divide(x = -3, 9, 3, 6, vector = TRUE),
               c(l = -3, s = -1, d = -2))
  expect_equal(deb_divide(x = 3, 104, 33, 52, vector = TRUE, round = 5),
               c(l = 35, s = 5, d = 9.33333))
})

test_that("arithmetic checks work", {
  expect_error(deb_multiply(x = "d", 9, 3, 6),
               "x must be a numeric vector")
  expect_error(deb_divide(x = "d", 9, 3, 6),
               "x must be a numeric vector")
  expect_error(deb_multiply(x = c(3, 5), 9, 3, 6),
               "x must be a numeric vector of length 1")
  expect_error(deb_divide(x = c(3, 5), 9, 3, 6),
               "x must be a numeric vector of length 1")
})

v1 <- c(15, 19, 8)
v2 <- c(8, 6, 11)
v3 <- c(32, 36, 45)
v4 <- c(12, 64, 86)
v5 <- c(12, 5)
lsd_list1 <- list(v1, v3)
lsd_list2 <- list(v2, v4)

test_that("lsd subtract works", {
  expect_equal(deb_subtract(lsd1 = v1, lsd2 = v2, vector = TRUE),
               c(l = 7, s = 12, d = 9))
  expect_equal(deb_subtract(lsd1 = v2, lsd2 = v1, vector = TRUE),
               c(l = -7, s = -12, d = -9))
  expect_equal(deb_subtract(lsd1 = v3, lsd2 = v4, vector = TRUE),
               c(l = 18, s = 8, d = 7))
})

test_that("lsd subtract is vectorized", {
  expect_equal(is.list(deb_subtract(lsd_list1, lsd_list2, vector = TRUE)),
               TRUE)
  expect_equal(length(deb_subtract(lsd_list1, lsd_list2, vector = TRUE)),
               2)
  expect_equal(nrow(deb_subtract(lsd_list1, lsd_list2, vector = FALSE)),
               2)
  expect_equal(deb_subtract(lsd_list1, lsd_list2, vector = TRUE),
              list(deb_subtract(v1, v2, vector = TRUE),
                   deb_subtract(v3, v4, vector = TRUE)))
})

test_that("lsd subtract checks work", {
  expect_error(deb_subtract(lsd_list1, list(v1, v5)),
               "The vectors in lsd1 and lsd2 must all be length 3")
  expect_error(deb_subtract(lsd_list1, v1),
               "lsd1 and lsd2 must either be both vectors or lists")
  expect_error(deb_subtract(v1, c("a", "b", "c")),
               "lsd1 and lsd2 must be numeric vectors or lists of numeric vectors")
  expect_error(deb_subtract(list(v1, c("a", "b", "c")), lsd_list1),
               "lsd1 and lsd2 must be numeric vectors or lists of numeric vectors")
  expect_error(deb_subtract(v1, v5),
               "lsd1 and lsd2 must be vectors of length 3")
  expect_warning(deb_subtract(v1, c(-8, 9, 7)),
                 "Negative values are present in lsd1 or lsd2")
  expect_warning(deb_subtract(lsd_list1, list(c(10, 4, 6), c(-8, 9, 7))),
                 "Negative values are present in lsd1 or lsd2")
})
