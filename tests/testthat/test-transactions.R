## Test transaction functions ##

# Problems with comparing tibblesb / data frames
# In addition, all.equal() and identical() acting differently


# Set up ------------------------------------------------------------------

suppressPackageStartupMessages(library(tibble))

mut <- function(x, lsd = TRUE) {
  if (lsd) {
    x[["dec"]] <- deb_as_decimal(x[["lsd"]])
  } else {
    x[["dec"]] <- deb_as_decimal(x[["data"]])
  }
  x[c(1, 3)]
}

b1 <- c(20, 12)
b2 <- c(8, 16)
l <- c(10, 10, 7, 9, 15, 12)
s <- c(15, 15, 11, 2, 0, 10)
d <- c(6, 6, 8, 11, 9, 4)
cred <- c("a", "b", "a", "c", "d", "c")
deb <- c("b", "a", "c", "a", "a", "e")
lsd_b1 <- deb_lsd(l, s, d)
lsd_b2 <- deb_lsd(l, s, d, bases = b2)
dec_b1 <- deb_as_decimal(lsd_b1)
dec_b2 <- deb_as_decimal(lsd_b2)

# Account d and e only have one type of transaction
# Tests no NAs path in deb_account_summary
tbl_b1 <- tibble(credit = cred, debit = deb,
                 lsd = lsd_b1,
                 dec = dec_b1)
df_b1 <- as.data.frame(tbl_b1)
# Different bases and non-default column names
tbl_b2 <- tibble(from = cred, to = deb,
                 data = lsd_b2,
                 dec = dec_b2)
df_b2 <- as.data.frame(tbl_b2)

# totally balanced
# Tests all accounts present path in deb_account_summary
balanced <- tibble(credit = c("a", "b", "a", "c"),
                   debit = c("b", "a", "c", "a"),
                   lsd = deb_lsd(l = c(10, 10, 7, 7),
                                 s = c(15, 15, 11, 11),
                                 d = c(6, 6, 8, 8)))
balanced[["dec"]] <- deb_as_decimal(balanced[["lsd"]])

# NA, and account "e's" only credit is NA
# Tests else path in deb_account_summary
tbl_na <- tibble(credit = c(cred, "e", "a"), debit = c(deb, "b", "b"),
                 lsd = c(lsd_b1, NA, NA),
                 dec = c(dec_b1, NA, NA))
# NAs but all accounts have credit and debit
# Tests all accounts present path in deb_account_summary
tbl_na2 <- tibble(credit = c("a", "b", "a", "c", "c"),
                  debit = c("b", "a", "c", "a", "a"),
                  lsd = deb_lsd(l = c(10, 10, 7, 9, NA),
                                s = c(15, 15, 11, 2, NA),
                                d = c(6, 6, 8, 11, NA)))
tbl_na2[["dec"]] <- deb_as_decimal(tbl_na2[["lsd"]])

relation_v <- c("credit", "debit", "current")

s_b1 <- tibble(account_id = letters[1:5],
               credit = deb_lsd(l = c(18, 10, 21, 15, 0),
                                s = c(7, 15, 13, 0, 0),
                                d = c(2, 6, 3, 9, 0)),
               debit = deb_lsd(l = c(34, 10, 7, 0, 12),
                               s = c(19, 15, 11, 0, 10),
                               d = c(2, 6, 8, 0, 4)),
               current = deb_lsd(l = c(-16, 0, 14, 15, -12),
                                 s = c(-12, 0, 1, 0, -10),
                                 d = c(0, 0, 7, 9, -4)))
sdec_b1 <- tibble(account_id = letters[1:5],
                  credit = deb_decimal(s_b1$credit),
                  debit = deb_decimal(s_b1$debit),
                  current = deb_decimal(s_b1$current))
s_b2 <- tibble(account_id = letters[1:5],
               credit = deb_lsd(l = c(20, 11, 22, 15, 0),
                                s = c(2, 7, 4, 0, 0),
                                d = c(14, 6, 15, 9, 0),
                                bases = b2),
               debit = deb_lsd(l = c(36, 11, 8, 0, 13),
                               s = c(2, 7, 3, 0, 2),
                               d = c(10, 6, 8, 0, 4),
                               bases = b2),
               current = deb_lsd(l = c(-15, 0, 14, 15, -13),
                                 s = c(-7, 0, 1, 0, -2),
                                 d = c(-12, 0, 7, 9, -4),
                                 bases = b2))
sdec_b2 <- tibble(account_id = letters[1:5],
                  credit = deb_decimal(s_b2$credit),
                  debit = deb_decimal(s_b2$debit),
                  current = deb_decimal(s_b2$current))
s_balanced <- tibble(account_id = letters[1:3],
                     credit = deb_lsd(l = c(18, 10, 7),
                                      s = c(7, 15, 11),
                                      d = c(2, 6, 8)),
                     debit = deb_lsd(l = c(18, 10, 7),
                                     s = c(7, 15, 11),
                                     d = c(2, 6, 8)),
                     current = deb_lsd(l = c(0, 0, 0),
                                       s = c(0, 0, 0),
                                       d = c(0, 0, 0)))
sdec_balanced <- tibble(account_id = letters[1:3],
                        credit = deb_decimal(s_balanced$credit),
                        debit = deb_decimal(s_balanced$debit),
                        current = deb_decimal(s_balanced$current))
s_na <- tibble(account_id = letters[1:5],
               credit = deb_lsd(l = c(NA, 10, 21, 15, NA),
                                s = c(NA, 15, 13, 0, NA),
                                d = c(NA, 6, 3, 9, NA)),
               debit = deb_lsd(l = c(34, NA, 7, 0, 12),
                               s = c(19, NA, 11, 0, 10),
                               d = c(2, NA, 8, 0, 4)),
               current = deb_lsd(l = c(NA, NA, 14, 15, NA),
                                 s = c(NA, NA, 1, 0, NA),
                                 d = c(NA, NA, 7, 9, NA)))
sdec_na <- tibble(account_id = letters[1:5],
                  credit = deb_decimal(s_na$credit),
                  debit = deb_decimal(s_na$debit),
                  current = deb_decimal(s_na$current))
s_na2 <- tibble(account_id = c("a", "b", "c"),
                credit = deb_lsd(l = c(18, 10, NA),
                                 s = c(7, 15, NA),
                                 d = c(2, 6, NA)),
                debit = deb_lsd(l = c(NA, 10, 7),
                                s = c(NA, 15, 11),
                                d = c(NA, 6, 8)),
                current = deb_lsd(l = c(NA, 0, NA),
                                  s = c(NA, 0, NA),
                                  d = c(NA, 0, NA)))
sdec_na2 <- tibble(account_id = letters[1:3],
                   credit = deb_decimal(s_na2$credit),
                   debit = deb_decimal(s_na2$debit),
                   current = deb_decimal(s_na2$current))


# Checks ------------------------------------------------------------------

error1 <- "`lsd` must be provided if the default is not present in `df`."
error2 <- paste0("Column names for `credit` and `debit` must be provided",
                 " if the default names are not present in `df`.")
error3 <- "`credit` and `debit` must be of the same type."
error4 <- "`account_id` must be a value present in `credit` and/or `debit`."
error5 <- "`lsd` must be either of type <deb_lsd> or <deb_decimal>."

# transaction_check makes checks for all transaction functions
test_that("transaction_check works", {
  # transaction_check with tibble
  expect_error(deb_account(b1), "`df` must be a data frame.")
  expect_error(deb_account(tbl_b2, "a", credit = from, debit = to), error1)
  expect_error(deb_account(tbl_b2, "a", lsd = data), error2)
  expect_error(deb_account(tbl_b1, "a", credit = credit, debit = lsd),
               error3)
  expect_error(deb_account(tbl_b1)) # uses default error
  expect_error(deb_account(tbl_b1, account_id = "x"), error4)
  # deb_ptype_check
  expect_error(deb_account(tbl_b1, "a", lsd = credit), error5)

  # transaction_check with data frames
  expect_error(deb_account(df_b2, "a", credit = from, debit = to), error1)
  expect_error(deb_account(df_b2, "a", lsd = data), error2)
  expect_error(deb_account(df_b1, "a", credit = credit, debit = lsd),
               error3)
  expect_error(deb_account(df_b1)) # uses default error
  expect_error(deb_account(df_b1, account_id = "x"), error4)
  # deb_ptype_check
  expect_error(deb_account(df_b1, "a", lsd = credit), error5)
})


# deb_account -------------------------------------------------------------

test_that("deb_account works", {
  expect_equal(nrow(deb_account(tbl_b1, "c")), 3L)
  expect_equal(ncol(deb_account(tbl_b1, "c")), 2L)
  expect_equal(nrow(deb_account(df_b1, "c")), 3L)
  expect_equal(ncol(deb_account(df_b1, "c")), 2L)

  res1 <- deb_lsd(l = c(21, 7, 14),
                  s = c(13, 11, 1),
                  d = c(3, 8, 7))
  res2 <- deb_as_decimal(res1)
  expect_equal(deb_account(tbl_b1, "c")[["lsd"]], res1)
  expect_equal(deb_account(tbl_b1, "c", lsd = dec)[["dec"]], res2)
  expect_equal(deb_account(df_b1, "c")[["lsd"]], res1)

  # only one type of transaction
  res3 <- deb_lsd(l = c(15, 0, 15),
                  s = c(0, 0, 0),
                  d = c(9, 0, 9))
  res4 <- deb_as_decimal(res3)

  expect_equal(deb_account(tbl_b1, "d")[["lsd"]], res3)
  expect_equal(deb_account(tbl_b1, "d", lsd = dec)[["dec"]], res4)

  res5 <- tibble(relation = relation_v,
                 lsd = deb_lsd(l = c(0, 12, -12),
                               s = c(0, 10, -10),
                               d = c(0, 4, -4)))
  res6 <- mut(res5)
  expect_identical(deb_account(tbl_b1, "e"), res5)
  expect_identical(deb_account(tbl_b1, "e", lsd = dec), res6)

  # different bases and column names
  res7 <- tibble(relation = relation_v,
                 data = deb_lsd(l = c(20, 36, -15),
                                s = c(2, 2, -7),
                                d = c(14, 10, -12),
                                bases = b2))
  res8 <- mut(res7, FALSE)
  expect_identical(deb_account(tbl_b2, "a", from, to, data), res7)
  expect_identical(deb_account(tbl_b2, "a", from, to, dec), res8)

  # NA
  expect_identical(deb_account(tbl_na, "a", na.rm = TRUE),
                   deb_account(tbl_b1, "a"))
  expect_identical(deb_account(tbl_na, "a", lsd = dec, na.rm = TRUE),
                   deb_account(tbl_b1, "a", lsd = dec))

  res9 <- tibble(relation = relation_v,
                 lsd = deb_lsd(l = c(NA, 34, NA),
                               s = c(NA, 19, NA),
                               d = c(NA, 2, NA)))
  res10 <- mut(res9)
  expect_identical(deb_account(tbl_na, "a", na.rm = FALSE), res9)
  expect_equal(deb_account(tbl_na, "a", lsd = dec, na.rm = FALSE)[["dec"]],
               deb_as_decimal(res9[["lsd"]]))

  res11 <- tibble(relation = relation_v,
                  lsd = deb_lsd(l = c(10, NA, NA),
                                s = c(15, NA, NA),
                                d = c(6, NA, NA)))
  res12 <- mut(res11)
  expect_identical(deb_account(tbl_na, "b", lsd = dec, na.rm = FALSE), res12)
})


# deb_account_summary -----------------------------------------------------

test_that("deb_account_summary works", {
  expect_equal(nrow(deb_account_summary(tbl_b1)), 5L)
  expect_equal(nrow(deb_account_summary(df_b1)), 5L)
  expect_identical(deb_account_summary(tbl_b1), s_b1)
  # Trouble getting tibbles to be equal with floating point
  expect_equal(deb_account_summary(tbl_b1, lsd = dec)[["current"]],
               deb_as_decimal(s_b1[["current"]]))
  expect_identical(deb_account_summary(df_b1), s_b1)
  expect_identical(deb_account_summary(tbl_b2, from, to, data), s_b2)
  expect_identical(deb_account_summary(balanced), s_balanced)
  expect_identical(deb_account_summary(balanced, lsd = dec), sdec_balanced)

  # na.rm works
  expect_identical(deb_account_summary(tbl_na, na.rm = TRUE), s_b1)
  res1 <- deb_account_summary(tbl_na, lsd = dec, na.rm = TRUE)
  expect_equal(res1[["current"]], sdec_b1[["current"]])
  # Deal with NA values
  expect_identical(deb_account_summary(tbl_na), s_na)
  res2 <- deb_account_summary(tbl_na, lsd = dec)
  expect_equal(res2[["current"]], sdec_na[["current"]])
  # Ensure it checks that all accounts in both sides
  expect_equal(nrow(deb_account_summary(tbl_na, debit, credit)), 5L)
  expect_identical(deb_account_summary(tbl_na2), s_na2)
  expect_identical(deb_account_summary(tbl_na2, lsd = dec), sdec_na2)
})


# Credit, debit, current, and open ----------------------------------------

test_that("deb_credit works", {
  expect_equal(nrow(deb_credit(tbl_b1)), 5L)
  expect_equal(ncol(deb_credit(tbl_b1)), 2L)
  expect_named(deb_credit(tbl_b2, from, to, data), c("account_id", "data"))
  # credit is same as deb_account_summary credit column
  expect_identical(deb_credit(tbl_b1)[[2]], s_b1[[2]])
  expect_equal(deb_credit(tbl_b1, lsd = dec)[[2]], sdec_b1[[2]])
  expect_identical(deb_credit(tbl_b2, from, to, data)[[2]], s_b2[[2]])
  expect_identical(deb_credit(df_b1)[[2]], s_b1[[2]])
  expect_identical(deb_credit(balanced)[[2]], s_balanced[[2]])
  expect_identical(deb_credit(balanced, lsd = dec)[[2]], sdec_balanced[[2]])
  # Deal with NA values
  expect_identical(deb_credit(tbl_na, na.rm = TRUE), deb_credit(tbl_b1))
  expect_identical(deb_credit(tbl_na)[[2]], s_na[[2]])
  expect_identical(deb_credit(tbl_na, debit, credit)[[2]], s_na[[3]])
  expect_identical(deb_credit(tbl_na2)[[2]], s_na2[[2]])
})


test_that("deb_debit works", {
  expect_equal(nrow(deb_debit(tbl_b1)), 5L)
  expect_equal(ncol(deb_debit(tbl_b1)), 2L)
  expect_named(deb_debit(tbl_b2, from, to, data),
               c("account_id", "data"))
  # debit is same as deb_account_summary debit column
  expect_identical(deb_debit(tbl_b1)[[2]], s_b1[[3]])
  expect_equal(deb_debit(tbl_b1, lsd = dec)[[2]], sdec_b1[[3]])
  expect_identical(deb_debit(tbl_b2, from, to, data)[[2]], s_b2[[3]])
  expect_identical(deb_debit(df_b1)[[2]], s_b1[[3]])
  expect_identical(deb_debit(balanced)[[2]], s_balanced[[3]])
  expect_identical(deb_debit(balanced, lsd = dec)[[2]], sdec_balanced[[3]])
  # Deal with NA values
  expect_identical(deb_debit(tbl_na, na.rm = TRUE), deb_debit(tbl_b1))
  expect_identical(deb_debit(tbl_na)[[2]], s_na[[3]])
  expect_identical(deb_debit(tbl_na, debit, credit)[[2]], s_na[[2]])
  expect_identical(deb_debit(tbl_na2)[[2]], s_na2[[3]])
})

test_that("deb_current works", {
  expect_equal(nrow(deb_current(tbl_b1)), 5)
  expect_equal(ncol(deb_current(tbl_b1)), 2)
  expect_named(deb_current(tbl_b2, from, to, data),
               c("account_id", "data"))
  # current is same as deb_account_summary current column
  expect_identical(deb_current(tbl_b1)[[2]], s_b1[[4]])
  expect_equal(deb_current(tbl_b1, lsd = dec)[[2]], sdec_b1[[4]])
  expect_identical(deb_current(tbl_b2, from, to, data)[[2]], s_b2[[4]])
  expect_identical(deb_current(df_b1)[[2]], s_b1[[4]])
  # all zeros
  expect_identical(deb_current(balanced),
                   tibble(account_id = c("a", "b", "c"),
                          lsd = deb_lsd(l = c(0, 0, 0),
                                        s = c(0, 0, 0),
                                        d = c(0, 0, 0))))
  # Deal with NA values
  expect_identical(deb_current(tbl_na, na.rm = TRUE), deb_current(tbl_b1))
  expect_identical(deb_current(tbl_na)[[2]], s_na[[4]])
  expect_identical(deb_current(tbl_na2)[[2]], s_na2[[4]])
})

test_that("deb_open works", {
  expect_equal(nrow(deb_open(tbl_b1)), 4)
  expect_equal(ncol(deb_open(tbl_b1)), 2)
  expect_named(deb_open(tbl_b2, from, to, data),
               c("account_id", "data"))
  # open is same as deb_account_summary current column without 0s
  expect_identical(deb_open(tbl_b1)[[2]], s_b1[[4]][c(1, 3:5)])
  expect_equal(deb_open(tbl_b1, lsd = dec)[[2]], sdec_b1[[4]][c(1, 3:5)])
  expect_identical(deb_open(tbl_b2, from, to, data)[[2]], s_b2[[4]][c(1, 3:5)])
  expect_identical(deb_open(df_b1)[[2]], s_b1[[4]][c(1, 3:5)])
  # all zeros returns empty tibble
  expect_equal(nrow(deb_open(balanced)), 0)
  # Deal with NA values
  expect_identical(deb_open(tbl_na, na.rm = TRUE), deb_open(tbl_b1))
  expect_identical(deb_open(tbl_na)[[2]], s_na[[4]])
  expect_identical(deb_open(tbl_na2)[[2]],
                   deb_lsd(c(NA, NA), c(NA, NA), c(NA, NA)))
})


# Balance -----------------------------------------------------------------

test_that("deb_balance works", {
  expect_equal(nrow(deb_balance(tbl_b1)), 2)
  expect_equal(ncol(deb_balance(tbl_b1)), 2)
  expect_named(deb_balance(tbl_b2, from, to, data),
               c("relation", "data"))
  # correct values
  rel <- c("credit", "debit")
  res1 <- deb_lsd(c(29, -29), c(2, -2), c(4, -4))
  expect_identical(deb_balance(tbl_b1)[[2]], res1)
  expect_equal(deb_balance(tbl_b1, lsd = dec)[[2]], deb_as_decimal(res1))
  expect_identical(deb_balance(tbl_b2, from, to, data)[[2]],
                   deb_lsd(c(29, -29), c(2, -2), c(0, 0), b2))
  expect_identical(deb_balance(df_b1)[[2]], res1)
  # all zeros
  expect_identical(deb_balance(balanced),
                   tibble(relation = rel,
                          lsd = deb_lsd(c(0, 0), c(0, 0), c(0, 0))))
  # Deal with NA values: Any NAs lead to both being NA
  res2 <- tibble(relation = rel,
                 lsd = deb_lsd(c(NA, NA), c(NA, NA), c(NA, NA)))
  expect_identical(deb_balance(tbl_na, na.rm = TRUE), deb_balance(tbl_b1))
  expect_identical(deb_balance(tbl_na), res2)
  expect_identical(deb_balance(tbl_na2), res2)
})
