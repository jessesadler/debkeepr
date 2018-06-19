context("test-lsd-interest.R")

ex_vector <- c(10, 3, 2)
ex_list <- list(c(30, 10, 9), c(10.725, 18.65, 11), c(-26, -11, -10))

ex_df <- data.frame(l = c(30, 10.725, -26),
                    s = c(10, 18.65, -11),
                    d = c(9, 11, -10))

test_that("interest checks work", {
  expect_error(deb_interest(ex_vector, interest = "t"),
               "interest must be numeric")
  expect_error(deb_interest(ex_vector, interest = c(0.6, 0.5)),
               "interest must be a numeric vector of length 1")
  expect_error(deb_interest(ex_vector, duration = "t"),
               "duration must be numeric")
  expect_error(deb_interest(ex_vector, duration = c(0.6, 0.5)),
               "duration must be a numeric vector of length 1")
  expect_error(deb_interest(ex_vector, with_principal = "t"),
               "with_principal must be logical, either TRUE or FALSE")
})

test_that("interest calculation works", {
  expect_equal(deb_interest(ex_vector),
               c(l = 10, s = 15, d = 10.375))
  expect_equal(deb_interest(ex_vector, with_principal = FALSE),
               deb_multiply(ex_vector, 1/16))
  expect_equal(deb_interest(ex_vector, with_principal = FALSE, duration = 5),
               deb_multiply(ex_vector, 5/16))
  expect_equal(deb_interest(ex_vector, interest = 0.10),
               c(l = 11, s = 3, d = 5.8))
})

test_that("vectorization works", {
  expect_equal(deb_interest(ex_list),
               list(c(l = 32, s = 8, d = 11.062),
                    c(l = 12, s = 8, d = 8.35),
                    c(l = -28, s = -5, d = -0.875)))
  expect_equal(deb_interest(ex_list, with_principal = FALSE),
               list(c(l = 1, s = 18, d = 2.062),
                    c(l = 0, s = 14, d = 7.55),
                    c(l = -1, s = -13, d = -2.875)))
})

test_that("interest mutate works",{
  expect_equal(deb_interest_mutate(ex_df, replace = TRUE),
               data.frame(l = c(32, 12, -28),
                          s = c(8, 8, -5),
                          d = c(11.062, 8.35, -0.875)))
  expect_equal(deb_interest_mutate(ex_df, with_principal = FALSE, replace = TRUE),
               data.frame(l = c(1, 0, -1),
                          s = c(18, 14, -13),
                          d = c(2.062, 7.55, -2.875)))
  expect_equal(deb_interest_mutate(ex_df, with_principal = FALSE,
                                   replace = TRUE, duration = 5, interest = 0.10),
               deb_multiply_mutate(ex_df, replace = TRUE, x = 0.50))
})
