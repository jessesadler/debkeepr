context("test-lsd-interest.R")

x <- c(10, 3, 2)
y <- c(20, 5, 8)
b1 <- c(20, 12)
b2 <- c(8, 16)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))
list1_b1 <- to_lsd(list1, b1)
list2_b2 <- to_lsd(list(x, y), b2)

ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))

test_that("interest checks work", {
  expect_error(deb_interest(x, interest = "t"),
               "interest must be numeric")
  expect_error(deb_interest(x, interest = c(0.6, 0.5)),
               "interest must be a numeric vector of length 1")
  expect_error(deb_interest(x, duration = "t"),
               "duration must be numeric")
  expect_error(deb_interest(x, duration = c(0.6, 0.5)),
               "duration must be a numeric vector of length 1")
  expect_error(deb_interest(x, with_principal = "t"),
               "with_principal must be logical, either TRUE or FALSE")
})

test_that("interest calculation works", {
  expect_equal(deb_interest(x),
               to_lsd(c(10, 15, 10.375), b1))
  expect_equal(deb_interest(x, with_principal = FALSE),
               deb_multiply(x, 1/16), b1)
  expect_equal(deb_interest(x, with_principal = FALSE, duration = 5),
               deb_multiply(x, 5/16), b1)
  expect_equal(deb_interest(x, round = 0),
               to_lsd(c(10, 15, 10), b1))
  expect_equal(deb_interest(x, interest = 0.10),
               to_lsd(c(11, 3, 5.8), b1))
  expect_equal(deb_interest(x, bases = b2),
               to_lsd(c(11, 0, 5.125), b2))
})

test_that("vectorization works", {
  expect_equal(deb_interest(list1),
               to_lsd(list(c(32, 8, 11.0625),
                           c(12, 8, 8.35),
                           c(-28, -5, -0.875)), b1))
  expect_equal(deb_interest(list1, with_principal = FALSE),
               to_lsd(list(c(1, 18, 2.0625),
                           c(0, 14, 7.55),
                           c(-1, -13, -2.875)), b1))
  expect_equal(deb_interest(list1, bases = b2, round = 0),
               to_lsd(list(c(33, 2, 4),
                           c(13, 7, 11),
                           c(-29, -1, -6)), b2))
})

test_that("deb_interest works with lsd objects", {
  expect_identical(deb_interest(x_b2),
                   deb_interest(x, bases = b2))
  expect_identical(deb_interest(list1_b1),
                   deb_interest(list1, bases = b1))
  expect_identical(deb_interest(list2_b2, round = 0),
                   deb_interest(list(x, y), bases = b2, round = 0))
})

test_that("interest mutate works",{
  expect_equal(deb_interest_mutate(ex_df, replace = TRUE),
               data.frame(l = c(32, 12, -28),
                          s = c(8, 8, -5),
                          d = c(11.0625, 8.35, -0.875)))
  expect_equal(deb_interest_mutate(ex_df, with_principal = FALSE, replace = TRUE),
               data.frame(l = c(1, 0, -1),
                          s = c(18, 14, -13),
                          d = c(2.0625, 7.55, -2.875)))
  expect_equal(deb_interest_mutate(ex_df, replace = TRUE, round = 0),
               data.frame(l = c(32, 12, -28),
                          s = c(8, 8, -5),
                          d = c(11, 8, -1)))
  expect_equal(deb_interest_mutate(ex_df, with_principal = FALSE,
                                   replace = TRUE, duration = 5, interest = 0.10),
               deb_multiply_mutate(ex_df, replace = TRUE, x = 0.50))
  expect_false(identical(deb_interest_mutate(ex_df),
                         deb_interest_mutate(ex_df, bases = c(8, 16))))
})
