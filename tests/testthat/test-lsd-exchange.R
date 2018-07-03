context("test-lsd-exchange.R")

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
  expect_equal(deb_exchange(ex_vector, rate_per_shillings = 30 + 3/12, round = 0),
               c(l = 15, s = 7, d = 3))
  expect_equal(deb_exchange(ex_vector, rate_per_shillings = 30, lsd_ratio = c(8, 16)),
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
                         deb_exchange_mutate(ex_df, rate_per_shillings = 30, lsd_ratio = c(8, 16))))
})

test_that("normalized_to_sd helper works",{
  expect_equal(normalized_to_sd(c(1, 11, 0)), c(0, 31, 0))
})

test_that("deb_rate_per_shilling works", {
  expect_equal(deb_rate_per_shilling(c(166, 13, 4), c(100, 0, 0)),
               c(l = 0, s = 33, d = 4))
  expect_equal(deb_rate_per_shilling(c(100, 0, 0), c(166, 13, 4)),
               c(l = 0, s = 12, d = 0))
  expect_equal(deb_rate_per_shilling(c(166, 13, 4), c(100, 0, 0), lsd_ratio = c(8, 16)),
               c(l = 0, s = 13, d = 5.333))
})

test_that("deb_rate_per_shilling is vectorized", {
  expect_equal(length(deb_rate_per_shilling(ex_list, ex_list2)), 3)
  expect_equal(deb_rate_per_shilling(ex_list, ex_list2, round = 0),
               list(c(l = 0, s = 60, d = 1),
                    c(l = 0, s = 34, d = 8),
                    c(l = 0, s = -26, d = -3)))
})
