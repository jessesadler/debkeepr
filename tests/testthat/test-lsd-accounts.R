## Test accounts functions ##

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
# balance has values
tbl_na3 <- tibble(credit = c("a", "b", "a", "c", "d", "c", "a"),
                 debit = c("b", "a", "c", "a", "a", "e", "b"),
                 lsd = deb_lsd(l = c(10, 10, 7, 9, 15, 12, NA),
                               s = c(15, 15, 11, 2, 0, 10, NA),
                               d = c(6, 6, 8.25, 11.5, 9, 4, NA)))
# totally balanced if na.rm = TRUE
tbl_balanced <- tibble(credit = c("a", "b", "a", "c"),
                       debit = c("b", "a", "c", "a"),
                       lsd = deb_lsd(l = c(10, 10, 7, 7),
                                     s = c(15, 15, 11, 11),
                                     d = c(6, 6, 8, 8)))
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
summary_na2 <- tibble(account_id = c("a", "b", "c"),
                      credit = deb_lsd(l = c(18, 10, NA),
                                       s = c(7, 15, NA),
                                       d = c(2.25, 6, NA)),
                      debit = deb_lsd(l = c(NA, 10, 7),
                                      s = c(NA, 15, 11),
                                      d = c(NA, 6, 8.25)),
                      current = deb_lsd(l = c(NA, 0, NA),
                                        s = c(NA, 0, NA),
                                        d = c(NA, 0, NA)))

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
  expect_identical(deb_account(tbl_b1, "c"),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(21, 7, 14),
                                        s = c(13, 11, 1),
                                        d = c(3.5, 8.25, 7.25))))
  expect_identical(deb_account(tbl_b1, "c", round = 0),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(21, 7, 14),
                                        s = c(13, 11, 1),
                                        d = c(4, 8, 8))))
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
                          lsd = deb_lsd(l = c(NA, 34, NA),
                                        s = c(NA, 19, NA),
                                        d = c(NA, 2.5, NA))))
  expect_identical(deb_account(tbl_na, "b", na.rm = FALSE),
                   tibble(relation = relation_v,
                          lsd = deb_lsd(l = c(10, NA, NA),
                                        s = c(15, NA, NA),
                                        d = c(6, NA, NA))))
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
  expect_identical(deb_account_summary(tbl_na2), summary_na2)

})

test_that("deb_credit works", {
  expect_equal(nrow(deb_credit(tbl_b1)), 5)
  expect_equal(ncol(deb_credit(tbl_b1)), 2)
  expect_named(deb_credit(tbl_b2, from, to, data),
               c("account_id", "data"))
  # credit is same as deb_account_summary credit column
  expect_identical(deb_credit(tbl_b1)[[2]],
                   summary_b1[[2]])
  expect_identical(deb_credit(tbl_b2, from, to, data)[[2]],
                   summary_b2[[2]])
  # Round
  expect_identical(deb_credit(tbl_b1, round = 0)[[2]],
                   deb_account_summary(tbl_b1, round = 0)[[2]])
  # Deal with NA values
  expect_identical(deb_credit(tbl_na, na.rm = TRUE),
                   deb_credit(tbl_b1))
  expect_identical(deb_credit(tbl_na2)[[2]],
                   summary_na2[[2]])
})

test_that("deb_debit works", {
  expect_equal(nrow(deb_debit(tbl_b1)), 5)
  expect_equal(ncol(deb_debit(tbl_b1)), 2)
  expect_named(deb_debit(tbl_b2, from, to, data),
               c("account_id", "data"))
  # debit is same as deb_account_summary debit column
  expect_identical(deb_debit(tbl_b1)[[2]],
                   summary_b1[[3]])
  expect_identical(deb_debit(tbl_b2, from, to, data)[[2]],
                   summary_b2[[3]])
  # Round
  expect_identical(deb_debit(tbl_b1, round = 0)[[2]],
                   deb_account_summary(tbl_b1, round = 0)[[3]])
  # Deal with NA values
  expect_identical(deb_debit(tbl_na, na.rm = TRUE),
                   deb_debit(tbl_b1))
  expect_identical(deb_debit(tbl_na2)[[2]],
                   summary_na2[[3]])
})

test_that("deb_current works", {
  expect_equal(nrow(deb_current(tbl_b1)), 5)
  expect_equal(ncol(deb_current(tbl_b1)), 2)
  expect_named(deb_current(tbl_b2, from, to, data),
               c("account_id", "data"))
  # current is same as deb_account_summary current column
  expect_identical(deb_current(tbl_b1)[[2]],
                   summary_b1[[4]])
  expect_identical(deb_current(tbl_b2, from, to, data)[[2]],
                   summary_b2[[4]])
  # all zeros
  expect_identical(deb_current(tbl_balanced),
                   tibble(account_id = c("a", "b", "c"),
                          lsd = deb_lsd(l = c(0, 0, 0),
                                        s = c(0, 0, 0),
                                        d = c(0, 0, 0))))
  # Round
  expect_identical(deb_current(tbl_b1, round = 0)[[2]],
                   deb_account_summary(tbl_b1, round = 0)[[4]])
  # Deal with NA values
  expect_identical(deb_current(tbl_na, na.rm = TRUE),
                   deb_current(tbl_b1))
  expect_identical(deb_current(tbl_na2)[[2]],
                   summary_na2[[4]])
})

test_that("deb_open works", {
  expect_equal(nrow(deb_open(tbl_b1)), 4)
  expect_equal(ncol(deb_open(tbl_b1)), 2)
  expect_named(deb_open(tbl_b2, from, to, data),
               c("account_id", "data"))
  # open is same as deb_account_summary current column without 0s
  expect_identical(deb_open(tbl_b1)[[2]],
                   summary_b1[[4]][c(1, 3:5)])
  expect_identical(deb_open(tbl_b2, from, to, data)[[2]],
                   summary_b2[[4]][c(1, 3:5)])
  # all zeros returns empty tibble
  expect_equal(nrow(deb_open(tbl_balanced)), 0)
  # Round
  expect_identical(deb_open(tbl_b1, round = 0)[[2]],
                   deb_account_summary(tbl_b1, round = 0)[[4]][c(1, 3:5)])
  # Deal with NA values
  expect_identical(deb_open(tbl_na, na.rm = TRUE),
                   deb_open(tbl_b1))
  expect_identical(deb_open(tbl_na)[[2]],
                   summary_b1[[4]][3:4])
  # No positive or negative values and NA returns empty tibble
  expect_equal(nrow(deb_open(tbl_na2)), 0)
  expect_identical(deb_open(tbl_na2, na.rm = TRUE),
                   tibble(account_id = c("a", "c"),
                          lsd = deb_lsd(l = c(-1, 1),
                                        s = c(-11, 11),
                                        d = c(-3.25, 3.25))))
})

test_that("deb_balance works", {
  expect_equal(nrow(deb_balance(tbl_b1)), 2)
  expect_equal(ncol(deb_balance(tbl_b1)), 2)
  expect_named(deb_balance(tbl_b2, from, to, data),
               c("relation", "data"))
  # correct values
  expect_identical(deb_balance(tbl_b1),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(29, 29),
                                        s = c(2, 2),
                                        d = c(4.25, 4.25))))
  expect_identical(deb_balance(tbl_b2, from, to, data),
                   tibble(relation = c("credit", "debit"),
                          data = deb_lsd(l = c(29, 29),
                                         s = c(2, 2),
                                         d = c(0.25, 0.25),
                                        bases = b2)))
  # all zeros
  expect_identical(deb_balance(tbl_balanced),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(0, 0),
                                        s = c(0, 0),
                                        d = c(0, 0))))
  # Round: round 0 has different values, round 1 has same
  expect_identical(deb_balance(tbl_b1, round = 0),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(29, 29),
                                        s = c(2, 2),
                                        d = c(5, 4))))
  expect_identical(deb_balance(tbl_b1, round = 1),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(29, 29),
                                        s = c(2, 2),
                                        d = c(4.3, 4.3))))
  # Deal with NA values
  expect_identical(deb_balance(tbl_na, na.rm = TRUE),
                   deb_balance(tbl_b1))
  expect_identical(deb_balance(tbl_na),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(29, NA),
                                        s = c(2, NA),
                                        d = c(4.25, NA))))
  expect_identical(deb_balance(tbl_na2),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_as_lsd(list(as.numeric(c(NA, NA, NA)),
                                                as.numeric(c(NA, NA, NA))))))
  expect_identical(deb_balance(tbl_na3),
                   tibble(relation = c("credit", "debit"),
                          lsd = deb_lsd(l = c(29, 12),
                                        s = c(2, 10),
                                        d = c(4.25, 4))))
  expect_identical(deb_balance(tbl_na3, na.rm = TRUE),
                   deb_balance(tbl_b1))
})
