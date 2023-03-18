## Test transaction functions ##

# transaction_check makes checks for all transaction functions
test_that("transaction_check works: Error messages", {
  x <- tibble::tibble(y = 1, from = "a", to = "b", data = deb_lsd(1, 1, 1))

  # transaction_check with tibble
  expect_snapshot(deb_account("hello", "a"), error = TRUE)
  expect_snapshot(deb_account(x, "a", credit = from, debit = to), error = TRUE)
  expect_snapshot(deb_account(x, "a", lsd = data), error = TRUE)
  expect_snapshot(error = TRUE,
    deb_account(x, "a", credit = from, debit = y, lsd = data))
  expect_snapshot(error = TRUE,
    deb_account(x, credit = from, debit = to, lsd = data))
  expect_snapshot(error = TRUE,
    deb_account(x, account_id = NULL, credit = from, debit = to, lsd = data))
  expect_snapshot(error = TRUE,
    deb_account(x, account_id = "x", credit = from, debit = to, lsd = data))
  # deb_ptype_check
  expect_snapshot(error = TRUE,
    deb_account(x, "a", lsd = y, credit = from, debit = to))
})


# deb_account -------------------------------------------------------------

test_that("deb_account works", {
  # Data
  l <- c(10, 10, 7, 9, 15, 12)
  s <- c(15, 15, 11, 2, 0, 10)
  d <- c(6, 6, 8, 11, 9, 4.5)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c"),
    debit = c("b", "a", "c", "a", "a", "e"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    dec = deb_as_decimal(lsd),
    tetra = deb_as_tetra(lsd, f = 4),
    dec_tetra = deb_as_decimal(tetra),
    lsd_alt = deb_lsd(l, s, d, c(50, 16))
  )
  df <- as.data.frame(tbl)

  # Check that the function works
  expect_equal(dim(deb_account(tbl, "c", lsd = lsd)), c(3L, 2L))
  expect_equal(dim(deb_account(df, "c", lsd = lsd)), c(3L, 2L))
  expect_equal(names(deb_account(tbl, "c", lsd = lsd)), c("relation", "lsd"))
  expect_equal(deb_account(tbl, "c", lsd = lsd)[["relation"]],
               c("credit", "debit", "current"))
  # Works with other deb-types
  expect_equal(dim(deb_account(tbl, "c", lsd = dec)), c(3L, 2L))
  expect_equal(dim(deb_account(tbl, "c", lsd = tetra)), c(3L, 2L))
  expect_equal(dim(deb_account(tbl, "c", lsd = dec_tetra)), c(3L, 2L))
  # Different credit and debit column names
  expect_identical(deb_account(tbl, "c", from, to, lsd),
                   deb_account(tbl, "c", lsd = lsd))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_account(df, "c", lsd = tetra)))

  # Correct outcome
  res1 <- deb_lsd(l = c(21, 7, 14),
                  s = c(13, 11, 1),
                  d = c(3.5, 8, 7.5))
  res2 <- deb_as_tetra(res1, f = 4)

  expect_equal(deb_account(tbl, "c", lsd = lsd)[["lsd"]], res1)
  expect_equal(deb_account(df, "c", lsd = lsd)[["lsd"]], res1)
  expect_equal(deb_account(tbl, "c", lsd = dec)[["dec"]],
               deb_as_decimal(res1))
  expect_equal(deb_account(tbl, "c", lsd = tetra)[["tetra"]], res2)
  expect_equal(deb_account(tbl, "c", lsd = dec_tetra)[["dec_tetra"]],
               deb_as_decimal(res2))
  # Only one type of transaction: credit
  res3 <- deb_lsd(l = c(15, 0, 15),
                  s = c(0, 0, 0),
                  d = c(9, 0, 9))
  res4 <- deb_as_tetra(res3, f = 4)

  expect_equal(deb_account(tbl, "d", lsd = lsd)[["lsd"]], res3)
  expect_equal(deb_account(tbl, "d", lsd = dec)[["dec"]],
               deb_as_decimal(res3))
  expect_equal(deb_account(tbl, "d", lsd = tetra)[["tetra"]], res4)
  expect_equal(deb_account(tbl, "d", lsd = dec_tetra)[["dec_tetra"]],
               deb_as_decimal(res4))
  # Only one type of transaction: debit
  res5 <- deb_lsd(l = c(0, 12, -12),
                  s = c(0, 10, -10),
                  d = c(0, 4.5, -4.5))
  res6 <- deb_as_tetra(res5, f = 4)

  expect_equal(deb_account(tbl, "e", lsd = lsd)[["lsd"]], res5)
  expect_equal(deb_account(tbl, "e", lsd = dec)[["dec"]],
                   deb_as_decimal(res5))
  expect_equal(deb_account(tbl, "e", lsd = tetra)[["tetra"]], res6)
  expect_equal(deb_account(tbl, "e", lsd = dec_tetra)[["dec_tetra"]],
               deb_as_decimal(res6))
  # Different bases
  res7 <- deb_lsd(l = c(17, 34, -16),
                  s = c(26, 18, -41),
                  d = c(14, 10, -12),
                  bases = c(50, 16))
  expect_equal(deb_account(tbl, "a", lsd = lsd_alt)[["lsd_alt"]], res7)
})

test_that("deb_account works with NAs", {
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "e", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b", "b"),
    lsd = deb_lsd(l, s, d),
    dec = deb_as_decimal(lsd),
    tetra = deb_as_tetra(lsd, f = 4),
    dec_tetra = deb_as_decimal(tetra),
    lsd_alt = deb_lsd(l, s, d, c(50, 16))
  )

  # Default: na.rm = FALSE
  res1 <- deb_lsd(l = c(NA, 34, NA),
                  s = c(NA, 19, NA),
                  d = c(NA, 2, NA))
  res2 <- deb_as_tetra(res1, f = 4)

  expect_equal(deb_account(tbl, "a", lsd = lsd)[["lsd"]], res1)
  expect_equal(deb_account(tbl, "a", lsd = tetra)[["tetra"]], res2)

  # Remove NA: na.rm = TRUE (same as removing rows with NA)
  expect_identical(deb_account(tbl, "a", lsd = lsd, na.rm = TRUE),
                   deb_account(tbl[1:6, ], "a", lsd = lsd))
  expect_identical(deb_account(tbl, "a", lsd = tetra, na.rm = TRUE),
                   deb_account(tbl[1:6, ], "a", lsd = tetra))
})


# deb_account_summary -----------------------------------------------------

test_that("deb_account_summary works with all accounts present in cred and deb", {
  # Helper function to cast type
  switch_type <- function(x, type) {
    x %>%
      dplyr::mutate(dplyr::across(2:4, type))
  }

  # Data
  # All accounts have a debit and credit
  l <- c(10, 10, 7, 9, 15, 12)
  s <- c(15, 15, 11, 2, 0, 10)
  d <- c(6, 6, 8, 11, 9, 4.5)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "b", "c"),
    debit = c("b", "a", "c", "a", "a", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    dec = deb_as_decimal(lsd),
    tetra = deb_as_tetra(lsd, f = 4),
    dec_tetra = deb_as_decimal(tetra),
    lsd_alt = deb_lsd(l, s, d, c(50, 16))
  )
  df <- as.data.frame(tbl)
  res1 <- tibble::tibble(
    account_id = c("a", "b", "c"),
    credit  = deb_lsd(c(18, 25, 21), c(7, 16, 13), c(2, 3, 3.5)),
    debit   = deb_lsd(c(34, 23, 7), c(19, 5, 11), c(2, 10.5, 8)),
    current = deb_lsd(c(-16, 2, 14), c(-12, 10, 1), c(0, 4.5, 7.5))
  )
  res2 <- switch_type(res1, ~ deb_as_tetra(.x, f = 4))

  # Check that the function works
  expect_equal(dim(deb_account_summary(tbl, lsd = lsd)), c(3L, 4L))
  expect_equal(dim(deb_account_summary(df, lsd = lsd)), c(3L, 4L))
  expect_equal(names(deb_account_summary(tbl, lsd = lsd)),
               c("account_id", "credit", "debit", "current"))
  expect_equal(deb_account_summary(tbl, lsd = lsd)[["account_id"]],
               c("a", "b", "c"))
  # Works with other deb-types
  expect_equal(dim(deb_account_summary(tbl, lsd = dec)), c(3L, 4L))
  expect_equal(dim(deb_account_summary(tbl, lsd = tetra)), c(3L, 4L))
  expect_equal(dim(deb_account_summary(tbl, lsd = dec_tetra)), c(3L, 4L))
  # Different credit and debit column names
  expect_identical(deb_account_summary(tbl, from, to, lsd),
                   deb_account_summary(tbl, lsd = lsd))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_account_summary(df, lsd = tetra)))

  # Correct outcomes
  expect_equal(deb_account_summary(tbl, lsd = lsd), res1)
  expect_equal(deb_account_summary(df, lsd = lsd), as.data.frame(res1))
  expect_equal(deb_account_summary(tbl, lsd = dec),
               switch_type(res1, deb_as_decimal))
  expect_equal(deb_account_summary(tbl, lsd = tetra), res2)
  expect_equal(deb_account_summary(tbl, lsd = dec_tetra),
               switch_type(res2, deb_as_decimal))

  # Different bases
  res3 <- tibble::tibble(
    account_id = c("a", "b", "c"),
    credit  = deb_lsd(c(17, 25, 21), c(26, 15, 12), c(14, 15, 15.5), c(50, 16)),
    debit   = deb_lsd(c(34, 22, 7), c(18, 25, 11), c(10, 10.5, 8), c(50, 16)),
    current = deb_lsd(c(-16, 2, 14), c(-41, 40, 1), c(-12, 4.5, 7.5), c(50, 16))
  )
  expect_equal(deb_account_summary(tbl, lsd = lsd_alt), res3)

  # With NAs
  tbl_na <- tibble::tibble(
    credit = c("a", "b", "a", "c", "b", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "b", "c"),
    lsd = deb_lsd(c(l, NA), c(s, NA), c(d, NA))
  )
  expect_equal(deb_account_summary(tbl_na)[["current"]],
               deb_lsd(c(NA, 2, NA), c(NA, 10, NA), c(NA, 4.5, NA)))
  expect_equal(deb_account_summary(tbl_na, na.rm = TRUE),
               deb_account_summary(tbl))

})

test_that("deb_account_summary works when an account only has cred or deb", {
  # Only need to test one type because function switches
  # to deb_decimal and then back for speed.

  # Data
  # With NAs: tests else statement: Need to distinguish between NAs and 0s
  # Credit: account "a" has NA; no credit for account "e"
  # Debit:  account "b" has NA: no debit for account "d"
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    lsd = deb_lsd(l, s, d)
  )

  # Correct outcome
  res1 <- tibble::tibble(
    account_id = c("a", "b", "c", "d", "e"),
    credit  = deb_lsd(c(18, 10, 21, 15, 0),
                      c(7, 15, 13, 0 , 0),
                      c(2, 6, 3.5, 9, 0)),
    debit   = deb_lsd(c(34, 10, 7, 0, 12),
                      c(19, 15, 11, 0, 10),
                      c(2, 6, 8, 0, 4.5)),
    current = deb_lsd(c(-16, 0, 14, 15, -12),
                      c(-12, 0, 1, 0, -10),
                      c(0, 0, 7.5, 9, -4.5))
  )
  expect_equal(deb_account_summary(tbl, na.rm = TRUE), res1)
  expect_equal(deb_account_summary(tbl, na.rm = TRUE),
               deb_account_summary(tbl[1:6, ]))

  res2 <- deb_lsd(c(NA, NA, 14, 15, -12),
                  c(NA, NA, 1, 0, -10),
                  c(NA, NA, 7.5, 9, -4.5))
  expect_equal(deb_account_summary(tbl)[["current"]], res2)
})



# deb_credit and deb_debit ------------------------------------------------

test_that("deb_credit works", {
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    dec = deb_as_decimal(lsd),
    tetra = deb_as_tetra(lsd, f = 4),
    dec_tetra = deb_as_decimal(tetra),
    lsd_alt = deb_lsd(l, s, d, c(50, 16))
  )
  df <- as.data.frame(tbl)

  # Function works
  expect_equal(dim(deb_credit(tbl[1:4, ])), c(3L, 2L)) # all present
  expect_equal(dim(deb_credit(df[1:4, ])), c(3L, 2L)) # all present
  expect_equal(dim(deb_credit(tbl[1:6, ])), c(5L, 2L)) # Missing credit/debit
  expect_equal(dim(deb_credit(tbl)), c(5L, 2L)) # Missing credit/debit and NA
  expect_equal(names(deb_credit(tbl)), c("account_id", "lsd"))
  expect_equal(names(deb_credit(tbl, from, to, lsd = tetra)),
               c("account_id", "tetra"))
  expect_equal(deb_credit(tbl)[["account_id"]], c("a", "b", "c", "d", "e"))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_credit(df, lsd = dec)))

  # All accounts present in credit and debit
  res1 <- deb_lsd(c(18, 10, 9), c(7, 15, 2), c(2, 6, 11))
  res2 <- deb_as_tetra(res1, f = 4)
  res3 <- deb_lsd(c(17, 10, 9), c(26, 15, 2), c(14, 6, 11),
                  bases = c(50, 16))

  expect_equal(deb_credit(tbl[1:4, ], lsd = lsd)[["lsd"]], res1)
  expect_equal(deb_credit(df[1:4, ], lsd = lsd)[["lsd"]], res1)
  # Same as credit column in deb_account_summary()
  expect_equal(deb_credit(tbl[1:4, ])[["lsd"]],
               deb_account_summary(tbl[1:4, ])[["credit"]])
  expect_equal(deb_credit(tbl[1:4, ], lsd = dec)[["dec"]],
               deb_as_decimal(res1))
  expect_equal(deb_credit(tbl[1:4, ], lsd = tetra)[["tetra"]], res2)
  expect_equal(deb_credit(tbl[1:4, ], lsd = dec_tetra)[["dec_tetra"]],
               deb_as_decimal(res2))
  # Different bases
  expect_equal(deb_credit(tbl[1:4, ], lsd = lsd_alt)[["lsd_alt"]], res3)

  # Missing account in credit and/or debit but no NA
  res4 <- deb_lsd(c(18, 10, 21, 15, 0), c(7, 15, 13, 0 , 0), c(2, 6, 3.5, 9, 0))

  expect_equal(deb_credit(tbl[1:6, ])[["lsd"]], res4)
  expect_equal(deb_credit(tbl[1:6, ], lsd = tetra)[["tetra"]],
               deb_account_summary(tbl[1:6, ], lsd = tetra)[["credit"]])

  # Missing account in credit and/or debit and NAs
  res5 <- deb_lsd(c(NA, 10, 21, 15, 0), c(NA, 15, 13, 0 , 0), c(NA, 6, 3.5, 9, 0))

  expect_equal(deb_credit(tbl)[["lsd"]], res5)
  expect_equal(deb_credit(tbl, lsd = tetra)[["tetra"]],
               deb_account_summary(tbl, lsd = tetra)[["credit"]])
})


test_that("deb_debit works", {
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    dec = deb_as_decimal(lsd),
    tetra = deb_as_tetra(lsd, f = 4),
    dec_tetra = deb_as_decimal(tetra),
    lsd_alt = deb_lsd(l, s, d, c(50, 16))
  )
  df <- as.data.frame(tbl)

  # Function works
  expect_equal(dim(deb_debit(tbl[1:4, ])), c(3L, 2L)) # all present
  expect_equal(dim(deb_debit(df[1:4, ])), c(3L, 2L)) # all present
  expect_equal(dim(deb_debit(tbl[1:6, ])), c(5L, 2L)) # Missing credit/debit
  expect_equal(dim(deb_debit(tbl)), c(5L, 2L)) # Missing credit/debit and NA
  expect_equal(names(deb_debit(tbl)), c("account_id", "lsd"))
  expect_equal(names(deb_debit(tbl, from, to, lsd = tetra)),
               c("account_id", "tetra"))
  expect_equal(deb_debit(tbl)[["account_id"]], c("a", "b", "c", "d", "e"))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_debit(df, lsd = dec)))

  # All accounts present in credit and debit
  res1 <- deb_lsd(c(19, 10, 7), c(18, 15, 11), c(5, 6, 8))
  res2 <- deb_as_tetra(res1, f = 4)
  res3 <- deb_lsd(c(19, 10, 7), c(18, 15, 11), c(1, 6, 8),
                  bases = c(50, 16))

  expect_equal(deb_debit(tbl[1:4, ], lsd = lsd)[["lsd"]], res1)
  expect_equal(deb_debit(df[1:4, ], lsd = lsd)[["lsd"]], res1)
  # Same as debit column in deb_account_summary()
  expect_equal(deb_debit(tbl[1:4, ])[["lsd"]],
               deb_account_summary(tbl[1:4, ])[["debit"]])
  expect_equal(deb_debit(tbl[1:4, ], lsd = dec)[["dec"]],
               deb_as_decimal(res1))
  expect_equal(deb_debit(tbl[1:4, ], lsd = tetra)[["tetra"]], res2)
  expect_equal(deb_debit(tbl[1:4, ], lsd = dec_tetra)[["dec_tetra"]],
               deb_as_decimal(res2))
  # Different bases
  expect_equal(deb_debit(tbl[1:4, ], lsd = lsd_alt)[["lsd_alt"]], res3)

  # Missing account in credit and/or debit but no NA
  res4 <- deb_lsd(c(34, 10, 7, 0, 12), c(19, 15, 11, 0 , 10), c(2, 6, 8, 0, 4.5))

  expect_equal(deb_debit(tbl[1:6, ])[["lsd"]], res4)
  expect_equal(deb_debit(tbl[1:6, ], lsd = tetra)[["tetra"]],
               deb_account_summary(tbl[1:6, ], lsd = tetra)[["debit"]])

  # Missing account in credit and/or debit and NAs
  res5 <- deb_lsd(c(34, NA, 7, 0, 12), c(19, NA, 11, 0 , 10), c(2, NA, 8, 0, 4.5))

  expect_equal(deb_debit(tbl)[["lsd"]], res5)
  expect_equal(deb_debit(tbl, lsd = tetra)[["tetra"]],
               deb_account_summary(tbl, lsd = tetra)[["debit"]])
})


# deb_current and deb_open ------------------------------------------------

test_that("deb_current works                                                                                                                                                                                                   ", {
  # deb_current uses deb_account_summary, so only minimal testing is necessary
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    tetra = deb_as_tetra(lsd, f = 4)
  )
  df <- as.data.frame(tbl)

  # Function works
  expect_equal(dim(deb_current(tbl[1:6, ])), c(5L, 2L))
  expect_equal(dim(deb_current(df[1:6, ])), c(5L, 2L))
  expect_equal(names(deb_current(tbl[1:6, ])), c("account_id", "lsd"))
  expect_equal(names(deb_current(tbl[1:6, ], from, to, lsd = tetra)),
               c("account_id", "tetra"))
  expect_equal(deb_current(tbl[1:6, ])[["account_id"]],
               c("a", "b", "c", "d", "e"))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_current(df, lsd = tetra)))

  # Correct outcome
  res1 <-  deb_lsd(c(-16, 0, 14, 15, -12),
                   c(-12, 0, 1, 0, -10),
                   c(0, 0, 7.5, 9, -4.5))
  res2 <- replace(res1, c(1, 2), NA)

  expect_equal(deb_current(tbl[1:6, ])[["lsd"]], res1)
  expect_equal(deb_current(df[1:6, ])[["lsd"]], res1)
  expect_equal(deb_current(tbl[1:6, ], lsd = tetra)[["tetra"]],
               deb_account_summary(tbl[1:6, ], lsd = tetra)[["current"]])
  expect_equal(deb_current(tbl)[["lsd"]], res2)
})

test_that("deb_open works", {
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    tetra = deb_as_tetra(lsd, f = 4)
  )
  df <- as.data.frame(tbl)

  # Function works
  expect_equal(dim(deb_open(tbl[1:6, ])), c(4L, 2L))
  expect_equal(dim(deb_open(df[1:6, ])), c(4L, 2L))
  expect_equal(dim(deb_open(tbl)), c(5L, 2L)) # Includes all accounts with NA
  expect_equal(names(deb_open(tbl[1:6, ])), c("account_id", "lsd"))
  expect_equal(names(deb_open(tbl[1:6, ], from, to, lsd = tetra)),
               c("account_id", "tetra"))
  expect_equal(deb_open(tbl[1:6, ])[["account_id"]],
               c("a", "c", "d", "e"))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_open(df, lsd = tetra)))

  # Correct outcome
  res1 <-  deb_lsd(c(-16, 14, 15, -12),
                   c(-12, 1, 0, -10),
                   c(0, 7.5, 9, -4.5))
  res2 <- vec_c(NA, replace(res1, 1, NA))

  expect_equal(deb_open(tbl[1:6, ])[["lsd"]], res1)
  expect_equal(deb_open(df[1:6, ])[["lsd"]], res1)
  # Same as current without 0 for account b
  expect_equal(deb_open(tbl[1:6, ], lsd = tetra)[["tetra"]],
               deb_account_summary(tbl[1:6, ], lsd = tetra)[["current"]][-2])
  # NAs
  expect_equal(deb_open(tbl)[["lsd"]], res2)
  # Balanced data frame returns empty tibble
  expect_equal(nrow(deb_open(tbl[1:2, ])), 0)
  expect_equal(names(deb_open(tbl[1:2, ])), c("account_id", "lsd"))
})


# Balance -----------------------------------------------------------------

test_that("deb_balance works", {
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    tetra = deb_as_tetra(lsd, f = 4)
  )
  df <- as.data.frame(tbl)

  # Function works
  expect_equal(dim(deb_balance(tbl[1:6, ])), c(2L, 2L))
  expect_equal(dim(deb_balance(df[1:6, ])), c(2L, 2L))
  expect_equal(names(deb_balance(tbl[1:6, ])), c("relation", "lsd"))
  expect_equal(names(deb_balance(tbl[1:6, ], from, to, lsd = tetra)),
               c("relation", "tetra"))
  expect_equal(deb_balance(tbl[1:6, ])[["relation"]],
               c("credit", "debit"))
  # Returns data frame
  expect_false(tibble::is_tibble(deb_balance(df, lsd = tetra)))

  # Correct outcome
  res <- tibble::tibble(relation = c("credit", "debit"),
                        lsd = deb_lsd(c(29, -29), c(2, -2), c(4.5, -4.5)))
  expect_equal(deb_balance(tbl[1:6, ], lsd = lsd), res)
  expect_equal(deb_balance(df[1:6, ], lsd = lsd), as.data.frame(res))
  expect_equal(deb_balance(tbl[1:6, ], lsd = tetra)[["tetra"]],
               deb_tetra(c(29, -29), c(2, -2), c(4, -4), c(2, -2)))
  # NAs
  expect_equal(deb_balance(tbl, lsd = lsd)[["lsd"]],
               vec_init(deb_lsd(), 2))
  expect_equal(deb_balance(tbl, lsd = tetra)[["tetra"]],
               vec_init(deb_tetra(), 2))
  # Balanced
  expect_equal(deb_balance(tbl[1:2, ], lsd = lsd)[["lsd"]],
               rep(deb_as_lsd(0), 2))
  expect_equal(deb_balance(tbl[1:2, ], lsd = tetra)[["tetra"]],
               rep(deb_as_tetra(0), 2))
})
