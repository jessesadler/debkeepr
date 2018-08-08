context("test-lsd-accounts.R")

suppressPackageStartupMessages(library(tibble))

example1 <- tibble::tibble(credit = c("a", "b", "a", "c"),
                           debit = c("b", "a", "c", "a"),
                           l = c(10, 10, 7, 9),
                           s = c(15, 15, 11, 2),
                           d = c(6, 6, 8.25, 11.5))
example2 <- tibble::tibble(from = c("a", "b", "a", "c"),
                           to = c("b", "a", "c", "a"),
                           pounds = c(10, 10, 7, 9),
                           shillings = c(15, 15, 11, 2),
                           pence = c(6, 6, 8.25, 11.5))
example_na <- tibble::tibble(credit = c("a", "b", "a", "c", "a"),
                             debit = c("b", "a", "c", "a", "b"),
                             l = c(10, 10, 7, 9, NA),
                             s = c(15, 15, 11, 2, 4),
                             d = c(6, 6, 8.25, 11.5, 6))

relation_v <- c("credit", "debit", "current")

summary_answer <- tibble::tibble(account_id = rep(c("a", "b", "c"), each = 3),
                                 relation = rep(relation_v, 3),
                                 l = c(18, 19, -1, 10, 10, 0, 9, 7, 1),
                                 s = c(7, 18, -11, 15, 15, 0, 2, 11, 11),
                                 d = c(2.25, 5.5, -3.25, 6, 6, 0, 11.5, 8.25, 3.25))

set.seed(240)
example3 <- tibble::tibble(credit = sample(letters[1:4], 15, replace = TRUE),
                           debit = sample(letters[1:4], 15, replace = TRUE),
                           l = sample(1:30, 15, replace = TRUE),
                           s = sample(1:19, 15, replace = TRUE),
                           d = sample(1:11, 15, replace = TRUE))
set.seed(240)
example4 <- tibble::tibble(from = sample(letters[1:4], 15, replace = TRUE),
                           to = sample(letters[1:4], 15, replace = TRUE),
                           pounds = sample(1:30, 15, replace = TRUE),
                           shillings = sample(1:19, 15, replace = TRUE),
                           pence = sample(1:11, 15, replace = TRUE))

# credit_check makes checks for all lsd-account functions
test_that("credit_check works", {
  expect_error(deb_account(example1),
               "argument \"account_id\" is missing, with no default")
  expect_error(deb_account(example1, account_id = "d"),
               "account_id must be a value present in the credit and/or debit variables")
  expect_error(deb_account(example1, "a", credit = credit, debit = l),
               "credit and debit variables must be of the same class")
  expect_error(deb_account(example2, "a"),
               paste("Column names for credit and/or debit must be provided if",
                     "the default names of credit and/or debit are not present in the data frame",
                     sep = "\n"))
  expect_error(deb_account(example2, "a", credit = from, debit = to),
               paste("Column names for l, s, and d must be provided if the",
                     "default names of l, s, and d are not present in the data frame",
                     sep = "\n"))
})

## deb_account ##
test_that("deb_account works", {
  expect_equal(deb_account(example1, "a"),
               tibble::tibble(relation = relation_v,
                              l = c(18, 19, -1),
                              s = c(7, 18, -11),
                              d = c(2.25, 5.5, -3.25)))
  expect_equal(deb_account(example1, "a", round = 0),
               tibble::tibble(relation = relation_v,
                              l = c(18, 19, -1),
                              s = c(7, 18, -11),
                              d = c(2, 6, -4)))
  expect_equal(deb_account(example3, "a"),
               tibble::tibble(relation = relation_v,
                              l = c(23, 22, 1),
                              s = c(4, 3, 0),
                              d = c(2, 8, 6)))
  expect_false(identical(deb_account(example1, "a"),
                         deb_account(example1, "a", bases = c(8, 16))))
  expect_equal(deb_account(example1, "a", na.rm = TRUE),
               deb_account(example1, "a"))
})

test_that("deb_account accepts different column names", {
  expect_equal(names(deb_account(example2, "a", from, to, pounds, shillings, pence)),
               c("relation", "pounds", "shillings", "pence"))
  expect_equal(deb_account(example2, "a", from, to, pounds, shillings, pence),
               tibble::tibble(relation = relation_v,
                              pounds = c(18, 19, -1),
                              shillings = c(7, 18, -11),
                              pence = c(2.25, 5.5, -3.25)))
  expect_equal(deb_account(example4, "a", from, to, pounds, shillings, pence),
               tibble::tibble(relation = relation_v,
                              pounds = c(23, 22, 1),
                              shillings = c(4, 3, 0),
                              pence = c(2, 8, 6)))
})

test_that("deb_account_summary works", {
  expect_equal(nrow(deb_account_summary(example1)), 9)
  expect_equal(deb_account_summary(example1), summary_answer)
  expect_false(identical(deb_account_summary(example1),
                         deb_account_summary(example1, bases = c(8, 16))))
  # Accepts different names
  expect_equal(names(deb_account_summary(example2, from, to, pounds, shillings, pence)),
               c("account_id", "relation", "pounds", "shillings", "pence"))
  # One set of deb_account_summary is equal to deb_account for that account
  expect_equal(deb_account_summary(example1)[7:9, 2:5],
               deb_account(example1, "c"))
  expect_equal(deb_account_summary(example4, from, to, pounds, shillings, pence)[7:9, 2:5],
               deb_account(example4, "c", from, to, pounds, shillings, pence))
  # Round
  expect_equal(deb_account_summary(example1, round = 0)[ , 5],
               tibble(d = c(2, 6, -4, 6, 6, 0, 0, 8, 4)))
  # Deal with NA values
  expect_false(identical(deb_account_summary(example_na), deb_account_summary(example1)))
  expect_equal(deb_account_summary(example_na)[1, 3:5],
               tibble::tibble(l = as.numeric(NA), s = as.numeric(NA), d = as.numeric(NA)))
  expect_equal(deb_account_summary(example_na, na.rm = TRUE), deb_account_summary(example1))
})

test_that("deb_credit works", {
  expect_equal(nrow(deb_credit(example1)), 3)
  expect_equal(deb_credit(example1),
               summary_answer[c(1, 4, 7), c(1, 3, 4, 5)])
  expect_equal(deb_credit(example1, round = 0)[ , 4],
               tibble(d = c(2, 6, 0)))
  expect_false(identical(deb_credit(example1),
                         deb_credit(example1, bases = c(8, 16))))
  # Accepts different names
  expect_equal(names(deb_credit(example2, from, pounds, shillings, pence)),
               c("account_id", "pounds", "shillings", "pence"))
  # Deal with NA values
  expect_false(identical(deb_credit(example_na), deb_credit(example1)))
  expect_equal(deb_credit(example_na)[1, 2:4],
               tibble::tibble(l = as.numeric(NA), s = as.numeric(NA), d = as.numeric(NA)))
  expect_equal(deb_credit(example_na, na.rm = TRUE), deb_credit(example1))
})

test_that("deb_debit works", {
  expect_equal(nrow(deb_debit(example1)), 3)
  expect_equal(deb_debit(example1),
               summary_answer[c(2, 5, 8), c(1, 3, 4, 5)])
  expect_equal(deb_debit(example1, round = 0)[ , 4],
               tibble(d = c(6, 6, 8)))
  expect_false(identical(deb_debit(example1),
                         deb_debit(example1, bases = c(8, 16))))
  # Accepts different names
  expect_equal(names(deb_debit(example2, to, pounds, shillings, pence)),
               c("account_id", "pounds", "shillings", "pence"))
  # Deal with NA values
  expect_false(identical(deb_debit(example_na), deb_debit(example1)))
  expect_equal(deb_debit(example_na)[2, 2:4],
               tibble::tibble(l = as.numeric(NA), s = as.numeric(NA), d = as.numeric(NA)))
  expect_equal(deb_debit(example_na, na.rm = TRUE), deb_debit(example1))
})

test_that("deb_current works", {
  # Values come from deb_account_summary.
  # Do not need to test them.
  expect_equal(nrow(deb_current(example1)), 3)
  expect_equal(nrow(deb_current(example3)), 4)
  expect_false(identical(deb_current(example3),
                         deb_current(example3, bases = c(8, 16))))
  expect_equal(names(deb_current(example1)),
               c("account_id", "l", "s", "d"))
  # Deal with NA values
  expect_false(identical(deb_current(example_na), deb_current(example1)))
  expect_equal(deb_current(example_na)[1, 2:4],
               tibble::tibble(l = as.numeric(NA), s = as.numeric(NA), d = as.numeric(NA)))
  expect_equal(deb_current(example_na, na.rm = TRUE), deb_current(example1))
})

test_that("deb_open works", {
  # Values come from deb_account_summary.
  # Do not need to test them.
  expect_equal(nrow(deb_open(example1)), 2)
  expect_equal(nrow(deb_open(example3)), 4)
  expect_false(identical(deb_open(example3),
                         deb_open(example3, bases = c(8, 16))))
  # Deal with NA values
  expect_false(identical(deb_open(example_na), deb_open(example1)))
  # Rows with NA are excluded
  expect_equal(nrow(deb_open(example_na)), 1)
  expect_equal(deb_open(example_na, na.rm = TRUE), deb_open(example1))
})

test_that("deb_balance works", {
  expect_equal(nrow(deb_balance(example1)), 2)
  expect_false(identical(deb_balance(example1),
                         deb_balance(example1, bases = c(8, 16))))
  expect_equal(names(deb_balance(example1)),
                     c("relation", "l", "s", "d"))
  expect_equal(deb_balance(example1),
               tibble::tibble(relation = c("credit", "debit"),
                              l = c(1, 1),
                              s = c(11, 11),
                              d = c(3.25, 3.25)))
  expect_equal(deb_balance(example1, round = 0),
               tibble::tibble(relation = c("credit", "debit"),
                              l = c(1, 1),
                              s = c(11, 11),
                              d = c(4, 4)))
  expect_equal(deb_balance(example3),
               tibble::tibble(relation = c("credit", "debit"),
                              l = c(54, 54),
                              s = c(8, 8),
                              d = c(9, 9)))
  # Deal with NA values
  expect_false(identical(deb_balance(example_na), deb_balance(example1)))
  expect_equal(deb_balance(example_na)[2, 2:4],
               tibble::tibble(l = 0, s = 0, d = 0))
  expect_equal(deb_balance(example_na, na.rm = TRUE), deb_balance(example1))
})
