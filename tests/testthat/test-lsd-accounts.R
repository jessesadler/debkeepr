context("test-lsd-accounts.R")

suppressPackageStartupMessages(library(tibble))

b1 <- c(20, 12)
b2 <- c(8, 16)
tbl_b1 <- tibble(credit = c("a", "b", "a", "c", "d", "c"),
                 debit = c("b", "a", "c", "a", "a", "e"),
                 lsd = deb_lsd(l = c(10, 10, 7, 9, 15, 12),
                               s = c(15, 15, 11, 2, 0, 10),
                               d = c(6, 6, 8.25, 11.5, 9, 4)))
tbl_b2 <- tibble(from = c("a", "b", "a", "c", "d", "c"),
                 to = c("b", "a", "c", "a", "a", "e"),
                 data = deb_lsd(l = c(10, 10, 7, 9, 15, 12),
                                s = c(15, 15, 11, 2, 0, 10),
                                d = c(6, 6, 8.25, 11.5, 9, 4),
                                bases = b2))
# NA, and account "e's" only credit is NA
tbl_na <- tibble(credit = c("a", "b", "a", "c", "d", "c", "e", "a"),
                 debit = c("b", "a", "c", "a", "a", "e", "b", "b"),
                 lsd = deb_lsd(l = c(10, 10, 7, 9, 15, 12, NA, NA),
                               s = c(15, 15, 11, 2, 0, 10, 8, NA),
                               d = c(6, 6, 8.25, 11.5, 9, 4, 6, NA)))
# balance is NA, NA
tbl_na2 <- tibble(credit = c("a", "b", "a", "c", "c"),
                 debit = c("b", "a", "c", "a", "a"),
                 lsd = deb_lsd(l = c(10, 10, 7, 9, NA),
                               s = c(15, 15, 11, 2, 4),
                               d = c(6, 6, 8.25, 11.5, 6)))
# balance is value, NA
tbl_na3 <- tibble(credit = c("a", "b", "a", "c", "c"),
                  debit = c("b", "a", "c", "a", "a"),
                  lsd = deb_lsd(l = c(10, 15, 7, 9, NA),
                                s = c(15, 117, 11, 2, 4),
                                d = c(6, 8, 8.25, 11.5, 6)))
# totally balanced if na.rm = TRUE
tbl_balanced <- tibble(credit = c("a", "b", "a", "c", "b"),
                       debit = c("b", "a", "c", "a", "a"),
                       lsd = deb_lsd(l = c(10, 10, 7, 7, NA),
                                     s = c(15, 15, 11, 11, NA),
                                     d = c(6, 6, 8, 8, NA)))
relation_v <- c("credit", "debit", "current")

summary_b1 <- tibble(account_id = letters[1:5],
                     credit = deb_lsd(l = c(18, 10, 21, 15, 0),
                                   s = c(7, 15, 13, 0, 0),
                                   d = c(2.25, 6, 3.5, 9, 0)),
                     debit = deb_lsd(l = c(34, 10, 7, 0, 12),
                                     s = c(19, 15, 11, 0, 10),
                                     d = c(2.5, 6, 8.25, 0, 4)),
                     current = deb_lsd(l = c(-16, 0, 14, 15, -12),
                                       s = c(-12, 0, 1, 0, -10),
                                       d = c(-0.25, 0, 7.25, 9, -4)))
summary_b2 <- tibble(account_id = letters[1:5],
                     credit = deb_lsd(l = c(20, 11, 22, 15, 0),
                                      s = c(2, 7, 4, 0, 0),
                                      d = c(14.25, 6, 15.5, 9, 0),
                                      bases = b2),
                     debit = deb_lsd(l = c(36, 11, 8, 0, 13),
                                     s = c(2, 7, 3, 0, 2),
                                     d = c(10.5, 6, 8.25, 0, 4),
                                     bases = b2),
                     current = deb_lsd(l = c(-15, 0, 14, 15, -13),
                                       s = c(-7, 0, 1, 0, -2),
                                       d = c(-12.25, 0, 7.25, 9, -4),
                                       bases = b2))
summary_round <- tibble(account_id = c("a", "c"),
                        credit = deb_lsd(l = c(18, 21),
                                         s = c(7, 13),
                                         d = c(2, 4)),
                        debit = deb_lsd(l = c(34, 7),
                                        s = c(19, 11),
                                        d = c(2, 8)),
                        current = deb_lsd(l = c(-16, 14),
                                          s = c(-12, 1),
                                          d = c(0, 8)))

# credit_check makes checks for all lsd-account functions
test_that("credit_check works", {
  expect_error(deb_account(b1),
               "df must be a data frame")
  expect_error(deb_account(tbl_b1),
               "argument \"account_id\" is missing, with no default")
  expect_error(deb_account(tbl_b1, account_id = "x"),
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
                          lsd = deb_lsd(l = c(18, 34, -16),
                                        s = c(7, 19, -12),
                                        d = c(2.25, 2.5, -0.25))))
  expect_identical(deb_account(tbl_b1, "a", round = 0),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(18, 34, -16),
                                        s = c(7, 19, -12),
                                        d = c(2, 2, 0))))
  # only one type of transaction
  expect_identical(deb_account(tbl_b1, "d"),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(15, 0, 15),
                                        s = c(0, 0, 0),
                                        d = c(9, 0, 9))))
  expect_identical(deb_account(tbl_b1, "e"),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(0, 12, -12),
                                        s = c(0, 10, -10),
                                        d = c(0, 4, -4))))
  # different bases
  expect_identical(deb_account(tbl_b2, "a", from, to, data),
                   tibble(relation = relation_v,
                          data = deb_lsd(l = c(20, 36, -15),
                                         s = c(2, 2, -7),
                                         d = c(14.25, 10.5, -12.25),
                                         bases = b2)))
  # NA
  expect_identical(deb_account(tbl_na, "a", na.rm = TRUE),
                   deb_account(tbl_b1, "a"))
  expect_identical(deb_account(tbl_na, "a", na.rm = FALSE),
                   tibble(relation = relation_v,
                          lsd = deb_as_lsd(list(as.numeric(c(NA, NA, NA)),
                                                c(34, 19, 2.5),
                                                as.numeric(c(NA, NA, NA))))))
})

test_that("deb_account_summary works", {
  expect_equal(nrow(deb_account_summary(tbl_b1)), 5)
  expect_identical(deb_account_summary(tbl_b1), summary_b1)
  expect_identical(deb_account_summary(tbl_b2, from, to, data), summary_b2)

  # Round
  expect_identical(deb_account_summary(tbl_b1, round = 0)[c(1, 3), ],
                   summary_round)
  # Deal with NA values
  expect_identical(deb_account_summary(tbl_b1),
                   deb_account_summary(tbl_na, na.rm = TRUE))
  expect_identical(deb_account_summary(tbl_na)[ , 4],
                   tibble(current = deb_lsd(l = c(NA, NA, 14, 15, -12),
                                            s = c(NA, NA, 1, 0, -10),
                                            d = c(NA, NA, 7.25, 9, -4))))
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
