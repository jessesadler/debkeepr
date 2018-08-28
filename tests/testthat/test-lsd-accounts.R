context("test-lsd-accounts.R")

suppressPackageStartupMessages(library(tibble))

b1 <- c(20, 12)
b2 <- c(8, 16)
tbl_b1 <- tibble(credit = c("a", "b", "a", "c"),
                 debit = c("b", "a", "c", "a"),
                 lsd = deb_lsd(l = c(10, 10, 7, 9),
                               s = c(15, 15, 11, 2),
                               d = c(6, 6, 8.25, 11.5)))
tbl_b2 <- tibble(from = c("a", "b", "a", "c"),
                 to = c("b", "a", "c", "a"),
                 data = deb_lsd(l = c(10, 10, 7, 9),
                                s = c(15, 15, 11, 2),
                                d = c(6, 6, 8.25, 11.5),
                                bases = b2))
tbl_na <- tibble(credit = c("a", "b", "a", "c", "a"),
                 debit = c("b", "a", "c", "a", "b"),
                 lsd = deb_lsd(l = c(10, 10, 7, 9, NA),
                               s = c(15, 15, 11, 2, 4),
                               d = c(6, 6, 8.25, 11.5, 6)))
tbl_na2 <- tibble(credit = c("a", "b", "a", "c", "c"),
                  debit = c("b", "a", "c", "a", "a"),
                  lsd = deb_lsd(l = c(10, 10, 7, 9, NA),
                                s = c(15, 15, 11, 2, 4),
                                d = c(6, 6, 8.25, 11.5, 6)))
set.seed(240)
big_tbl <- tibble(credit = sample(letters[1:4], 15, replace = TRUE),
                  debit = sample(letters[1:4], 15, replace = TRUE),
                  lsd = deb_lsd(l = sample(1:30, 15, replace = TRUE),
                                s = sample(1:19, 15, replace = TRUE),
                                d = sample(1:11, 15, replace = TRUE)))
relation_v <- c("credit", "debit", "current")

summary_b1 <- tibble(account_id = rep(c("a", "b", "c"), each = 3),
                     relation = rep(relation_v, 3),
                     lsd = deb_lsd(l = c(18, 19, -1, 10, 10, 0, 9, 7, 1),
                                   s = c(7, 18, -11, 15, 15, 0, 2, 11, 11),
                                   d = c(2.25, 5.5, -3.25, 6, 6, 0, 11.5, 8.25, 3.25)))
summary_b2 <- tibble(account_id = rep(c("a", "b", "c"), each = 3),
                     relation = rep(relation_v, 3),
                     data = deb_lsd(l = c(20, 21, 0, 11, 11, 0, 9, 8, 0),
                                    s = c(2, 2, -7, 7, 7, 0, 2, 3, 7),
                                    d = c(14.25, 1.5, -3.25, 6, 6, 0, 11.5, 8.25, 3.25),
                                    bases = b2))

# credit_check makes checks for all lsd-account functions
test_that("credit_check works", {
  expect_error(deb_account(b1),
               "df must be a data frame")
  expect_error(deb_account(tbl_b1),
               "argument \"account_id\" is missing, with no default")
  expect_error(deb_account(tbl_b1, account_id = "d"),
               "account_id must be a value present in the credit and/or debit variables")
  expect_error(deb_account(tbl_b1, "a", credit = credit, debit = lsd),
               "credit and debit variables must be of the same class")
  expect_error(deb_account(tbl_b2, "a", lsd = data),
               paste("Column names for credit and/or debit must be provided if",
                     "the default names of credit and/or debit are not present in the data frame",
                     sep = "\n"))
  expect_error(deb_account(tbl_b2, "a", credit = from, debit = to),
               paste("Column name for lsd list column must be provided,",
                     "if the default name of lsd is not present in df.",
                     sep = " "))
  expect_error(deb_account(tbl_b1, "a", lsd = credit),
               "lsd must be an lsd list column")
})

## deb_account ##
test_that("deb_account works", {
  expect_identical(deb_account(tbl_b1, "a"),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(18, 19, -1),
                                        s = c(7, 18, -11),
                                        d = c(2.25, 5.5, -3.25))))
  expect_identical(deb_account(tbl_b1, "a", round = 0),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(18, 19, -1),
                                        s = c(7, 18, -11),
                                        d = c(2, 6, -4))))
  expect_identical(deb_account(big_tbl, "a"),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(23, 22, 1),
                                        s = c(4, 3, 0),
                                        d = c(2, 8, 6))))
  expect_identical(deb_account(tbl_b2, "a", from, to, data),
                   tibble(relation = relation_v,
                          data = deb_lsd(l = c(20, 21, 0),
                                         s = c(2, 2, -7),
                                         d = c(14.25, 1.5, -3.25),
                                         bases = b2)))
  expect_identical(deb_account(tbl_na, "a", na.rm = TRUE),
                   deb_account(tbl_b1, "a"))
  expect_identical(deb_account(tbl_na, "a", na.rm = FALSE),
                   tibble(relation = relation_v,
                          lsd = deb_as_lsd(list(as.numeric(c(NA, NA, NA)),
                                                c(19, 18, 5.5),
                                                as.numeric(c(NA, NA, NA))))))
})

test_that("deb_account_summary works", {
  expect_equal(nrow(deb_account_summary(tbl_b1)), 9)
  expect_identical(deb_account_summary(tbl_b1), summary_b1)
  expect_identical(deb_account_summary(tbl_b2, from, to, data), summary_b2)

  # One set of deb_account_summary is equal to deb_account for that account
  expect_identical(deb_account_summary(tbl_b1)[7:9, 2:3],
                   deb_account(tbl_b1, "c"))
  expect_identical(deb_account_summary(tbl_b2, from, to, data)[7:9, 2:3],
                   deb_account(tbl_b2, "c", from, to, data))
  # Round
  expect_identical(deb_account_summary(tbl_b1, round = 0)[7:9, 2:3],
                   deb_account(tbl_b1, "c", round = 0))
  # Deal with NA values
  expect_identical(deb_account_summary(tbl_b1),
                   deb_account_summary(tbl_na, na.rm = TRUE))
  expect_false(identical(deb_account_summary(tbl_b1), deb_account_summary(tbl_na)))
  expect_identical(deb_account_summary(tbl_na)[4:6, 2:3],
                   deb_account(tbl_na, "b"))
})

test_that("deb_credit works", {
  expect_equal(nrow(deb_credit(tbl_b1)), 3)
  expect_identical(deb_credit(tbl_b1),
                   summary_b1[c(1, 4, 7), c(1, 3)])
  expect_identical(deb_credit(tbl_b2, from, data),
                   summary_b2[c(1, 4, 7), c(1, 3)])
  # Round
  expect_identical(deb_credit(tbl_b1, round = 0)[3 , 2],
                   deb_account(tbl_b1, "c", round = 0)[1, 2])
  # Deal with NA values
  expect_identical(deb_credit(tbl_na, na.rm = TRUE),
                   deb_credit(tbl_b1))
  expect_identical(deb_credit(tbl_na),
                   deb_account_summary(tbl_na)[c(1, 4, 7), c(1, 3)])
})

test_that("deb_debit works", {
  expect_equal(nrow(deb_debit(tbl_b1)), 3)
  expect_identical(deb_debit(tbl_b1),
                   summary_b1[c(2, 5, 8), c(1, 3)])
  expect_identical(deb_debit(tbl_b2, to, data),
                   summary_b2[c(2, 5, 8), c(1, 3)])
  # Round
  expect_identical(deb_debit(tbl_b1, round = 0)[3 , 2],
                   deb_account(tbl_b1, "c", round = 0)[2, 2])
  # Deal with NA values
  expect_identical(deb_debit(tbl_na, na.rm = TRUE),
                   deb_debit(tbl_b1))
  expect_identical(deb_debit(tbl_na),
                   deb_account_summary(tbl_na)[c(2, 5, 8), c(1, 3)])
})

test_that("deb_current works", {
  # Values come from deb_account_summary.
  expect_equal(nrow(deb_current(tbl_b1)), 3)
  expect_equal(nrow(deb_current(big_tbl)), 4)
  expect_identical(deb_current(tbl_b1),
                   summary_b1[c(3, 6, 9), c(1, 3)])
  expect_identical(deb_current(tbl_b2, from, to, data),
                   summary_b2[c(3, 6, 9), c(1, 3)])

  # Deal with NA values
  expect_identical(deb_current(tbl_na, na.rm = TRUE),
                   deb_current(tbl_b1))
  expect_identical(deb_current(tbl_na),
                   deb_account_summary(tbl_na)[c(3, 6, 9), c(1, 3)])
})

test_that("deb_open works", {
  # Values come from deb_account_summary.
  expect_equal(nrow(deb_open(tbl_b1)), 2)
  expect_equal(nrow(deb_open(big_tbl)), 4)
  expect_identical(deb_open(tbl_b1),
                   summary_b1[c(3, 9), c(1, 3)])
  expect_identical(deb_open(tbl_b2, from, to, data),
                   summary_b2[c(3, 9), c(1, 3)])
  # Deal with NA values
  expect_identical(deb_open(tbl_na, na.rm = TRUE),
                   deb_open(tbl_b1))
  # Rows with NA are excluded
  expect_equal(nrow(deb_open(tbl_na)), 1)
})

test_that("deb_balance works", {
  expect_equal(nrow(deb_balance(tbl_b1)), 2)
  expect_identical(deb_balance(tbl_b1),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(1, 1),
                                        s = c(11, 11),
                                        d = c(3.25, 3.25))))
  expect_identical(deb_balance(tbl_b2, from, to, data),
                   tibble(relation = c("credit", "debit"),
                          data = deb_lsd(l = c(0, 0),
                                        s = c(7, 7),
                                        d = c(3.25, 3.25),
                                        bases = b2)))
  expect_identical(deb_balance(big_tbl),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(54, 54),
                                        s = c(8, 8),
                                        d = c(9, 9))))
  # Deal with NA values
  expect_identical(deb_balance(tbl_na, na.rm = TRUE),
                   deb_balance(tbl_b1))
  expect_identical(deb_balance(tbl_na),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_as_lsd(list(c(1, 11, 3.25),
                                                as.numeric(c(NA, NA, NA))))))
  expect_identical(deb_balance(tbl_na2),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_as_lsd(list(as.numeric(c(NA, NA, NA)),
                                                as.numeric(c(NA, NA, NA))))))
})
