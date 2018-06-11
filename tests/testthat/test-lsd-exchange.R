context("test-lsd-exchange.R")

df <- data.frame(l = c(30, 10, 26, 12),
                 s = c(10, 18, 11, 16),
                 d = c(9, 11, 10, 5))
exchange_30 <- data.frame(l = c(45, 16, 39, 19),
                          s = c(16, 8, 17, 4),
                          d = c(1.5, 4.5, 9, 7.5))
answer_30 <- data.frame(l = c(30, 10, 26, 12),
                        s = c(10, 18, 11, 16),
                        d = c(9, 11, 10, 5),
                        l.exchange = c(45, 16, 39, 19),
                        s.exchange = c(16, 8, 17, 4),
                        d.exchange = c(1.5, 4.5, 9, 7.5))
df2 <- data.frame(pounds = c(30, 10, 26, 12),
                  shillings = c(10, 18, 11, 16),
                  pence = c(9, 11, 10, 5))
exchange_302 <- data.frame(pounds = c(45, 16, 39, 19),
                           shillings = c(16, 8, 17, 4),
                           pence = c(1.5, 4.5, 9, 7.5))

test_that("exchange_rate_check works", {
  expect_error(deb_exchange(6, 10, 7, rate_per_solidi = "a"),
               "rate_per_solidi must be numeric")
  expect_error(deb_exchange_mutate(df, l, s, d, rate_per_solidi = "a"),
               "rate_per_solidi must be numeric")
  expect_error(deb_exchange(6, 10, 7, rate_per_solidi = c(31, 32)),
               "rate_per_solidi must be a numeric vector of length 1")
  expect_error(deb_exchange_mutate(df, l, s, d, rate_per_solidi = c(31, 32)),
               "rate_per_solidi must be a numeric vector of length 1")
})

test_that("deb_exchange works", {
  expect_equal(deb_exchange(8, 10, 6, rate_per_solidi = 24),
               deb_multiply(x = 24/20, 8, 10, 6))
  expect_equal(deb_exchange(8, 10, 6, rate_per_solidi = 30, vector = TRUE),
               c(l = 12, s = 15, d = 9))
  expect_equal(deb_exchange(8, 10, 6, rate_per_solidi = 15, vector = TRUE),
               c(l = 6, s = 7, d = 10.5))
  expect_equal(deb_exchange(8, 10, 6, rate_per_solidi = 31 + 3/12),
               deb_exchange(8, 10, 6, rate_per_solidi = 31.25))
  expect_equal(deb_exchange(8, 10, 6, rate_per_solidi = 30.5, vector = TRUE),
               c(l = 13, s = 0, d = 0.15))
})

test_that("deb_exchange_mutate works", {
  expect_equal(ncol(deb_exchange_mutate(df, l, s, d, rate_per_solidi = 30)), 6)
  expect_equal(deb_exchange_mutate(df, l, s, d, rate_per_solidi = 30), answer_30)
  expect_equal(deb_exchange_mutate(df, l, s, d, rate_per_solidi = 30,
                                   replace = TRUE),
               exchange_30)
  expect_equal(deb_exchange_mutate(df2, pounds, shillings, pence,
                                   rate_per_solidi = 30,
                                   replace = TRUE),
               exchange_302)

})
