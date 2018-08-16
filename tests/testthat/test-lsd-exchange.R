context("test-lsd-exchange.R")

x <- c(10, 3, 2)
y <- c(20, 5, 8)
neg <- c(-8, -16, -6)
dec <- c(5.85, 17.35, 10)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b1 <- to_lsd(x, b1)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
list2 <- list(x, y, dec)
lsd_list1 <- to_lsd(list1, b2)
lsd_list2 <- list(x_b2, y_b2, dec)
lsd_list3 <- list(x_b1, x_b2, y_b2)

ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))

exchange_30 <- data.frame(l = c(45, 17, -39),
                          s = c(16, 11, -17),
                          d = c(1.5, 1.2, -9))
rate_list <- list(c(0, 33, 4), c(0, 30, 0), c(0, 40, 0))

test_that("exchange_rate_check works", {
  expect_error(deb_exchange(x, shillings_rate = "a"),
               "shillings_rate must be numeric")
  expect_error(deb_exchange_mutate(ex_df, l, s, d, shillings_rate = "a"),
               "shillings_rate must be numeric")
  expect_error(deb_exchange(x, shillings_rate = c(31, 32)),
               "shillings_rate must be a numeric vector of length 1")
  expect_error(deb_exchange_mutate(ex_df, l, s, d, shillings_rate = c(31, 32)),
               "shillings_rate must be a numeric vector of length 1")
})

test_that("deb_exchange works", {
  expect_equal(deb_exchange(x, shillings_rate = 24),
               deb_multiply(x, x = 24/20))
  expect_equal(deb_exchange(x, shillings_rate = 30),
               to_lsd(c(15, 4, 9), b1))
  expect_equal(deb_exchange(x, shillings_rate = 30 + 3/12),
               to_lsd(c(15, 7, 3.475), b1))
  expect_equal(deb_exchange(x, shillings_rate = 30 + 3/12, round = 0),
               to_lsd(c(15, 7, 3), b1))
  expect_equal(deb_exchange(x, shillings_rate = 30, bases = c(8, 16)),
               to_lsd(c(38, 7, 11.5), b2))
})

test_that("deb_exchange is vectorized", {
  expect_equal(deb_exchange(list1, shillings_rate = 30),
               to_lsd(list(c(45, 16, 1.5),
                           c(17, 11, 1.2),
                           c(-39, -17, -9)), b1))
  expect_equal(deb_exchange(list1, shillings_rate = 16, bases = b2),
               to_lsd(list(c(62, 5, 2),
                           c(26, 2, 4.4),
                           c(-54, -7, -4)), b2))
  expect_equal(deb_exchange(list1, shillings_rate = 30, round = 0),
               to_lsd(list(c(45, 16, 2),
                           c(17, 11, 1),
                           c(-39, -17, -9)), b1))
})

test_that("deb_exchange works with lsd objects", {
  expect_identical(deb_exchange(x_b2, shillings_rate = 12),
                   deb_exchange(x, shillings_rate = 12, bases = b2))
  expect_identical(deb_exchange(lsd_list1, shillings_rate = 12),
                   deb_exchange(list1, shillings_rate = 12, bases = b2))
  expect_identical(deb_exchange(lsd_list2, shillings_rate = 12, round = 0),
                   deb_exchange(list2, shillings_rate = 12, bases = b2, round = 0))
  expect_error(deb_exchange(lsd_list3, shillings_rate = 12),
               "All lsd vectors in a list must have the same bases")
})

test_that("deb_exchange_mutate works", {
  expect_equal(ncol(deb_exchange_mutate(ex_df, l, s, d, shillings_rate = 30)), 6)
  expect_equal(deb_exchange_mutate(ex_df, shillings_rate = 30, replace = TRUE), exchange_30)
  expect_equal(deb_exchange_mutate(ex_df, shillings_rate = 30, round = 0)[ , 6],
               c(2, 1, -9))
  expect_false(identical(deb_exchange_mutate(ex_df, shillings_rate = 30),
                         deb_exchange_mutate(ex_df, shillings_rate = 30, bases = c(8, 16))))
})

test_that("normalized_to_sd helper works",{
  expect_equal(normalized_to_sd(c(1, 11, 0), b1), c(0, 31, 0))
  expect_equal(normalized_to_sd(to_lsd(c(1, 11, 0), b1), b1), to_lsd(c(0, 31, 0), b1))
  expect_equal(normalized_to_sd(list(x, y), b1),
               to_lsd(list(c(0, 203, 2),
                           c(0, 405, 8)), b1))
})

test_that("normalized_to_d helper works",{
  expect_equal(normalized_to_d(c(1, 11, 6), b1), c(0, 0, 378))
  expect_equal(normalized_to_d(to_lsd(c(1, 11, 6), b1), b1), to_lsd(c(0, 0, 378), b1))
  expect_equal(normalized_to_d(list(x, y), bases = b1),
               to_lsd(list(c(0, 0, 2438),
                           c(0, 0, 4868)), b1))
})

test_that("deb_exchange_rate works", {
  expect_equal(deb_exchange_rate(c(166, 13, 4), c(100, 0, 0)),
               to_lsd(c(0, 12, 0), b1))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 4)),
               to_lsd(c(0, 33, 4), b1))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 0), round = 0),
               to_lsd(c(0, 33, 4), b1))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 2, 10), bases = c(8, 16)),
               to_lsd(c(0, 13, 4.9), b2))
  expect_equal(deb_exchange_rate(c(20, 10, 8), c(10, 5, 4), bases = c(40, 24)),
               to_lsd(c(0, 20, 0), c(40, 24)))
  expect_equal(deb_exchange_rate(c(166, 13, 4), c(100, 0, 0), output = "pence"),
               to_lsd(c(0, 0, 144), b1))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 4), output = "pence"),
               to_lsd(c(0, 0, 400), b1))
  expect_equal(deb_exchange_rate(c(166, 13, 4), c(100, 0, 0), output = "pounds"),
               to_lsd(c(0, 12, 0), b1))
  expect_equal(deb_exchange_rate(c(100, 0, 0), c(166, 13, 4), output = "pounds"),
               to_lsd(c(1, 13, 4), b1))
})

test_that("deb_exchange_rate is vectorized", {
  expect_equal(length(deb_exchange_rate(list1, list2)), 3)
  expect_equal(deb_exchange_rate(list1, list2, round = 0),
               to_lsd(list(c(0, 6, 8),
                           c(0, 34, 8),
                           c(0, -5, -1)), b1))
  expect_equal(deb_exchange_rate(list1, list2, bases = b2, round = 0),
               to_lsd(list(c(0, 2, 10),
                           c(0, 12, 9),
                           c(0, -2, -6)), b2))
})

test_that("deb_exchange_rate works with lsd objects", {
  expect_identical(deb_exchange_rate(x_b2, y_b2), deb_exchange_rate(x, y_b2))
  expect_identical(deb_exchange_rate(lsd_list1, x), deb_exchange_rate(list1, x, bases = b2))
  expect_identical(deb_exchange_rate(x, lsd_list2), deb_exchange_rate(x, list2, bases = b2))
  expect_error(deb_exchange_rate(lsd_list3, lsd_list1),
               "All lsd vectors in a list must have the same bases")
})

test_that("deb_invert_rate works", {
  expect_equal(deb_invert_rate(c(0, 33, 4)), to_lsd(c(0, 12, 0), b1))
  expect_equal(deb_invert_rate(c(0, 12, 0)), to_lsd(c(0, 33, 4), b1))
  expect_equal(deb_invert_rate(c(0, 33, 0), round = 0), to_lsd(c(0, 12, 1), b1))
  expect_equal(deb_invert_rate(c(0, 12, 0), output = "pence"),
               to_lsd(c(0, 0, 400), b1))
  expect_equal(deb_invert_rate(c(0, 12, 0), output = "pounds"),
               to_lsd(c(1, 13, 4), b1))
  expect_equal(deb_invert_rate(c(0, 12, 0), bases = b2), to_lsd(c(0, 5, 5.33333), b2))
})

test_that("deb_invert_rate is vectorized", {
  expect_equal(length(deb_invert_rate(rate_list)), 3)
  expect_equal(deb_invert_rate(rate_list),
               to_lsd(list(c(0, 12, 0),
                      c(0, 13, 4),
                      c(0, 10, 0)), b1))
  expect_equal(deb_invert_rate(rate_list, bases = c(40, 12)),
               to_lsd(list(c(0, 48, 0),
                      c(0, 53, 4),
                      c(0, 40, 0)), c(40, 12)))
  expect_equal(deb_invert_rate(rate_list, bases = c(20, 16), round = 0),
               to_lsd(list(c(0, 12, 0),
                      c(0, 13, 5),
                      c(0, 10, 0)), c(20, 16)))
})

test_that("deb_invert_rate works with lsd objects", {
  expect_identical(deb_invert_rate(x_b2),
                   deb_invert_rate(x, bases = b2))
  expect_identical(deb_invert_rate(lsd_list1),
                   deb_invert_rate(list1, bases = b2))
  expect_identical(deb_invert_rate(lsd_list2, round = 0),
                   deb_invert_rate(list2, bases = b2, round = 0))
  expect_error(deb_invert_rate(lsd_list3),
               "All lsd vectors in a list must have the same bases")
})

## Error messages from exchange_rate_check ##
test_that("non-vector is an error", {
  expect_error(deb_invert_rate(data.frame(a = c(1:4), b = c(5:8))),
               paste("exchange_rate must be a vector of class lsd, a list of class lsd_list,",
                     "       or an object that can be coerced to these classes, namely",
                     "       a numeric vector or list of numeric vectors", sep = "\n"))
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
