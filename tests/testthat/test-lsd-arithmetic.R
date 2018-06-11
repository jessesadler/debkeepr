context("test-lsd-arithmetic.R")

df <- data.frame(l = c(30, 10, 26, 12),
                 s = c(10, 18, 11, 16),
                 d = c(9, 11, 10, 5))
answer_x2 <- data.frame(l = c(30, 10, 26, 12),
                        s = c(10, 18, 11, 16),
                        d = c(9, 11, 10, 5),
                        l.1 = c(61, 21, 53, 25),
                        s.1 = c(1, 17, 3, 12),
                        d.1 = c(6, 10, 8, 10))
answer_d2 <- data.frame(l = c(15, 5, 13, 6),
                        s = c(5, 9, 5, 8),
                        d = c(4.5, 5.5, 11, 2.5))

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

test_that("deb_multiply_mutate works", {
  expect_equal(ncol(deb_multiply_mutate(df, l, s, d, x = 2)), 6)
  expect_equal(ncol(deb_multiply_mutate(df, l, s, d, x = 2, replace = TRUE)), 3)
  expect_equal(deb_multiply_mutate(df, l, s, d, x = 2), answer_x2)
})

test_that("lsd division works", {
  expect_equal(deb_divide(x = 3, 9, 3, 6, vector = TRUE),
               c(l = 3, s = 1, d = 2))
  expect_equal(deb_divide(x = -3, 9, 3, 6, vector = TRUE),
               c(l = -3, s = -1, d = -2))
  expect_equal(deb_divide(x = 3, 104, 33, 52, vector = TRUE, round = 5),
               c(l = 35, s = 5, d = 9.33333))
})

test_that("deb_divide_mutate works", {
  expect_equal(ncol(deb_divide_mutate(df, l, s, d, x = 2)), 6)
  expect_equal(ncol(deb_divide_mutate(df, l, s, d, x = 2, replace = TRUE)), 3)
  expect_equal(deb_divide_mutate(df, l, s, d, x = 2, replace = TRUE), answer_d2)
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
