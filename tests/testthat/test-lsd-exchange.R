context("test-lsd-exchange.R")

suppressPackageStartupMessages(library(purrr))

ex_vector <- c(10, 3, 2)
ex_vector2 <- c(20, 5, 8)
neg_vector <- c(-8, -16, -6)
dec_vector <- c(5.85, 17.35, 10)

ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
ex_list2 <- list(ex_vector, dec_vector, ex_vector2)

ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))

exchange_30 <- data.frame(l = c(45, 17, -39),
                          s = c(16, 11, -17),
                          d = c(1.5, 1.2, -9))
rate_list <- list(c(0, 33, 4), c(0, 30, 0), c(0, 40, 0))

test_that("exchange_rate_check works", {
  expect_error(deb_exchange(ex_vector, rate_per_shillings = "a"),
               "rate_per_shillings must be numeric")
  expect_error(deb_exchange_mutate(ex_df, l, s, d, rate_per_shillings = "a"),
               "rate_per_shillings must be numeric")
  expect_error(deb_exchange(ex_vector, rate_per_shillings = c(31, 32)),
               "rate_per_shillings must be a numeric vector of length 1")
  expect_error(deb_exchange_mutate(ex_df, l, s, d, rate_per_shillings = c(31, 32)),
               "rate_per_shillings must be a numeric vector of length 1")
})

test_that("deb_exchange works", {
  expect_equal(deb_exchange(ex_vector, rate_per_shillings = 24),
               deb_multiply(ex_vector, x = 24/20))
  expect_equal(deb_exchange(ex_vector, rate_per_shillings = 30),
               c(l = 15, s = 4, d = 9))
  expect_equal(deb_exchange(ex_vector, rate_per_shillings = 30 + 3/12),
               c(l = 15, s = 7, d = 3.475))
  expect_equal(deb_exchange(ex_vector, rate_per_shillings = 30, lsd_bases = c(8, 16)),
               c(l = 38, s = 7, d = 11.5))
})

test_that("deb_exchange is vectorized", {
  expect_equal(deb_exchange(ex_list, rate_per_shillings = 30),
               list(c(l = 45, s = 16, d = 1.5),
                    c(l = 17, s = 11, d = 1.2),
                    c(l = -39, s = -17, d = -9)))
})

test_that("deb_exchange_mutate works", {
  expect_equal(ncol(deb_exchange_mutate(ex_df, l, s, d, rate_per_shillings = 30)), 6)
  expect_equal(deb_exchange_mutate(ex_df, rate_per_shillings = 30, replace = TRUE), exchange_30)
  expect_false(identical(deb_exchange_mutate(ex_df, rate_per_shillings = 30),
                         deb_exchange_mutate(ex_df, rate_per_shillings = 30, lsd_bases = c(8, 16))))
})

test_that("normalized_to_sd helper works",{
  expect_equal(normalized_to_sd(c(1, 11, 0)), c(0, 31, 0))
})

test_that("normalized_to_d helper works",{
  expect_equal(normalized_to_d(c(1, 11, 6)), c(0, 0, 378))
})

test_that("deb_exchange_rate works", {
  expect_equal(deb_exchange_rate(c(166, 13, 4), c(100, 0, 0)),
               c(l = 0, s = 12, d = 0))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 4)),
               c(l = 0, s = 33, d = 4))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 2, 10), lsd_bases = c(8, 16)),
               c(l = 0, s = 13, d = 4.9))
  expect_equal(deb_exchange_rate(c(20, 10, 8), c(10, 5, 4), lsd_bases = c(40, 24)),
               c(l = 0, s = 20, d = 0))
  expect_equal(deb_exchange_rate(c(166, 13, 4), c(100, 0, 0), output = "pence"),
               c(l = 0, s = 0, d = 144))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 4), output = "pence"),
               c(l = 0, s = 0, d = 400))
  expect_equal(deb_exchange_rate(c(166, 13, 4), c(100, 0, 0), output = "pounds"),
               c(l = 0, s = 12, d = 0))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 4), output = "pounds"),
               c(l = 1, s = 13, d = 4))
})

test_that("deb_exchange_rate is vectorized", {
  expect_equal(length(deb_exchange_rate(ex_list, ex_list2)), 3)
  expect_equal(purrr::map(deb_exchange_rate(ex_list, ex_list2), ~ round(., 0)),
               list(c(l = 0, s = 6, d = 8),
                    c(l = 0, s = 11, d = 7),
                    c(l = 0, s = -15, d = -3)))
})

test_that("deb_invert_rate works", {
  expect_equal(deb_invert_rate(c(0, 33, 4)), c(l = 0, s = 12, d = 0))
  expect_equal(deb_invert_rate(c(0, 12, 0)), c(l = 0, s = 33, d = 4))
  expect_equal(deb_invert_rate(c(0, 12, 0), output = "pence"),
               c(l = 0, s = 0, d = 400))
  expect_equal(deb_invert_rate(c(0, 12, 0), output = "pounds"),
               c(l = 1, s = 13, d = 4))
})

test_that("deb_invert_rate is vectorized", {
  expect_equal(length(deb_invert_rate(rate_list)), 3)
  expect_equal(deb_invert_rate(rate_list),
               list(c(l = 0, s = 12, d = 0),
                    c(l = 0, s = 13, d = 4),
                    c(l = 0, s = 10, d = 0)))
  expect_equal(deb_invert_rate(rate_list, lsd_bases = c(40, 12)),
               list(c(l = 0, s = 48, d = 0),
                    c(l = 0, s = 53, d = 4),
                    c(l = 0, s = 40, d = 0)))
})

## Error messages from exchange_rate_check ##
test_that("non-vector is an error", {
  expect_error(deb_invert_rate(data.frame(a = c(1:4), b = c(5:8))),
               "exchange_rate must be either a numeric vector or list of numeric vectors")
})

test_that("non-numeric is an error", {
  expect_error(deb_invert_rate(c("hello", "goodbye")),
               "exchange_rate must be a numeric vector")
  expect_error(deb_invert_rate(list(c("hello", "goodbye"), c(TRUE, FALSE))),
               "exchange_rate must be a list of numeric vectors")
})

test_that("length of exchange_rate is 3", {
  expect_error(deb_invert_rate(c(10, 9, 3, 5)),
               paste("exchange_rate must be a vector of length of 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
  expect_error(deb_invert_rate(list(c(10, 9, 3, 5), c(6, 3), c(4, 6, 8))),
               paste("exchange_rate must be a list of numeric vectors of length 3.",
                     "There must be a value for pounds, shillings, and pence.",
                     sep = "\n"))
})
