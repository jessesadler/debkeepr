## Test arithmetic with deb_lsd and deb_decimal ##

lsd1 <- deb_lsd(1, 16, 9)
lsd2 <- deb_lsd(5, 6, 8)
lsd3 <- c(lsd1, lsd2, NA, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))
lsd4 <- c(lsd1, lsd2, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))
bases2 <- c(50, 16)
lsd_bases <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases = bases2)
lsd_round <- deb_lsd(5, 19, 11.8755)
neg_round <- deb_lsd(-5, -19, -11.8755)
multi_decimal <- deb_lsd(2, 3.3, 2.2)
round1 <- deb_lsd(6, 0, 0)
round2 <- deb_lsd(-6, 0, 0)
finite <- deb_lsd(c(Inf, NaN, 1), c(Inf, NaN, 2), c(Inf, NaN, 3))
dec1 <- deb_decimal(1.8375)
dec2 <- deb_decimal(36.75, unit = "s")
dec3 <- deb_decimal(c(1.8375, NA, 5.225, 3.2875, 1.1125))
dec4 <- deb_decimal(c(1.8375, 5.225, 3.2875, 1.1125))

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> objects.")


# vec_math ----------------------------------------------------------------
test_that("vec_math has error message for unimplemented functions", {
  expect_error(prod(lsd4), "not implemented.$")
  expect_error(sin(lsd4), "not implemented.$")
})


# Math group --------------------------------------------------------------
test_that("sum and mean with deb_lsd work", {
  expect_equal(sum(lsd3), deb_lsd(NA, NA, NA))
  expect_equal(sum(lsd3, na.rm = TRUE), deb_lsd(18, 11, 5))
  expect_equal(sum(deb_lsd(c(1, 1), c(2, 1), c(3, NA)), na.rm = TRUE),
               deb_lsd(1, 2, 3))
  expect_equal(sum(lsd1, lsd2), deb_lsd(7, 3, 5))
  expect_equal(sum(lsd_bases), deb_lsd(6, 23, 1, bases = bases2))
  expect_equal(mean(c(lsd1, lsd2)), deb_lsd(3, 11, 8.5))
  expect_equal(mean(lsd3, na.rm = TRUE), deb_lsd(4, 12, 10.25))
  expect_equal(mean(deb_lsd(c(1, 5), c(42, 30), c(13, 15), bases = bases2)),
               deb_lsd(3, 36, 14, bases2))
  # Mean only takes first object
  expect_equal(mean(lsd4, lsd3), deb_lsd(4, 12, 10.25))
  # Error with different bases
  expect_error(sum(lsd3, lsd_bases), bases_error)
})

test_that("sum and mean work with deb_decimal", {
  expect_equal(sum(dec1, deb_decimal(1.5)), deb_decimal(3.3375))
  expect_equal(sum(dec3, na.rm = TRUE),
               deb_decimal(sum(as.numeric(dec3), na.rm = TRUE)))
  # Errors
  expect_error(sum(dec1, deb_decimal(1.5, bases = bases2)), bases_error)
  expect_error(sum(dec1, dec2),
    "`unit` attributes must be equal to combine <deb_decimal> objects.")
  expect_equal(mean(dec3, na.rm = TRUE), deb_decimal(2.865625))
})

test_that("sum works with deb-style objects and numeric", {
  expect_equal(sum(lsd1, dec1), deb_lsd(3, 13, 6))
  expect_equal(sum(lsd1, dec1), sum(lsd1, dec2))
  expect_equal(sum(lsd3, 1.8375, 3, na.rm = TRUE), deb_lsd(23, 8, 2))
  expect_equal(sum(dec3, 3.5, na.rm = TRUE),
               deb_decimal(sum(1.8375, 5.225, 3.2875, 1.1125, 3.5)))
})

test_that("cumulative functions work", {
  # cumsum
  expect_equal(cumsum(deb_lsd(rep(1, 5), rep(1, 5), rep(1, 5))),
               deb_lsd(1:5, 1:5, 1:5))
  expect_equal(cumsum(lsd4),
               c(lsd1, sum(lsd4[1:2]), sum(lsd4[1:3]), sum(lsd4[1:4])))
  expect_equal(as.numeric(cumsum(dec4)), cumsum(as.numeric(dec4)))
  # cummin
  expect_equal(cummin(lsd4), deb_lsd(rep(1, 4), rep(16, 4), rep(9, 4)))
  expect_equal(cummin(dec4), c(rep(dec4[[1]], 3), dec4[[4]]))
  # cummax
  expect_equal(cummax(lsd4), c(lsd1, lsd2, lsd2, lsd4[[4]]))
  expect_equal(cummax(dec4), c(dec1, rep(dec4[[2]], 3)))
})

test_that("finite, infinite and NaN checks work with deb_lsd", {
  expect_equal(is.finite(lsd4), c(T, T, T, T))
  expect_equal(is.infinite(lsd4), c(F, F, F, F))
  expect_equal(is.nan(lsd4), c(F, F, F, F))
  expect_equal(is.finite(finite), c(F, F, T))
  expect_equal(is.infinite(finite), c(T, F, F))
  expect_equal(is.nan(finite), c(F, T, F))
})

# Round family with deb_lsd -----------------------------------------------

test_that("round family works with deb_lsd", {
  # round
  expect_equal(round(lsd_round), round1)
  expect_equal(round(neg_round), round2)
  expect_equal(round(deb_lsd(5, 49, 15.6, bases2)), deb_lsd(6, 0, 0, bases2))
  expect_equal(round(lsd_round, 3), deb_lsd(5, 19, 11.876))
  expect_equal(round(multi_decimal), round(deb_normalize(multi_decimal)))
  # signif
  expect_equal(signif(lsd_round, 3), deb_lsd(5, 19, 11.9))
  expect_equal(signif(lsd_round, 2), round1)
  # ceiling
  expect_equal(ceiling(lsd_round), round1)
  expect_equal(ceiling(neg_round), deb_lsd(-5, -19, -11))
  expect_equal(ceiling(multi_decimal), ceiling(deb_normalize(multi_decimal)))
  # floor
  expect_equal(floor(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(floor(neg_round), round2)
  expect_equal(floor(multi_decimal), floor(deb_normalize(multi_decimal)))
  # trunc
  expect_equal(trunc(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(trunc(neg_round), deb_lsd(-5, -19, -11))
  expect_equal(trunc(multi_decimal), trunc(deb_normalize(multi_decimal)))
})

test_that("abs works with deb_lsd objects", {
  expect_equal(abs(lsd_round), lsd_round)
  expect_equal(abs(neg_round), lsd_round)
})

# deb_lsd arithmetic operators --------------------------------------------
test_that("Arithmetic operators work with two deb_lsd objects", {
  # plus
  expect_equal(lsd1 + lsd2, deb_lsd(7, 3, 5))
  expect_equal(lsd_bases + deb_lsd(2, 10, 5, bases2),
               deb_lsd(c(3, 7), c(26, 16), c(14, 13), bases2))
  expect_error(lsd1 + lsd_bases, bases_error)
  # minus
  expect_equal(lsd1 - lsd2, deb_lsd(-3, -9, -11))
  expect_equal(lsd2 - lsd1, deb_lsd(3, 9, 11))
  # division
  expect_equal(deb_lsd(10, 13, 4) / lsd2, 2)
  # Incompatible op
  expect_error(lsd1 * lsd2)
})

test_that("Arithmetic operators work with deb_lsd and numeric", {
  # deb_lsd and numeric
  expect_equal(lsd2 * 3, deb_lsd(16, 0, 0))
  expect_equal(lsd_bases * 2, deb_lsd(c(2, 10), c(33, 13), c(2, 0), bases2))
  expect_equal(lsd2 / 2, deb_lsd(2, 13, 4))
  expect_error(lsd1 + 3)
  # numeric and deb_lsd
  expect_equal(3 * lsd2, deb_lsd(16, 0, 0))
  expect_error(3 / lsd1)
})


# deb_decimal arithmetic operators ----------------------------------------

test_that("Arithmetic operators work with two deb_decimal objects", {
  expect_equal(dec1 + deb_decimal(1.5), deb_decimal(3.3375))
  expect_equal(dec1 - deb_decimal(1.5), deb_decimal(0.3375))
  expect_equal(deb_decimal(3.25) / deb_decimal(6.5), 0.5)
  # Errors
  expect_error(dec1 + dec2,
    "`unit` attributes must be equal to combine <deb_decimal> objects.")
  expect_error(dec1 + deb_decimal(1.5, bases = bases2), bases_error)
  expect_error(dec1 * dec3)
})

test_that("Arithmetic operators work with deb_lsd and numeric", {
  # deb_decimal and numeric
  expect_equal(dec1 + 1.5, deb_decimal(3.3375))
  expect_equal(dec1 - 1.5, deb_decimal(0.3375))
  expect_equal(dec2 * 2, deb_decimal(73.5, unit = "s"))
  expect_equal(deb_decimal(2.5)^2, deb_decimal(6.25))
  expect_equal(dec2 / 3, deb_decimal(12.25, unit = "s"))
  expect_equal(dec2 %% 3, deb_decimal(0.75, unit = "s"))
  expect_equal(dec2 %/% 3, deb_decimal(12, unit = "s"))
  # numeric and deb_decimal
  expect_equal(1.5 + dec1, deb_decimal(3.3375))
  expect_equal(1.5 - dec1, deb_decimal(-0.3375))
  expect_equal(2 * dec2, deb_decimal(73.5, unit = "s"))
  expect_error(2 / dec2)
})


# Unary operators ---------------------------------------------------------

test_that("unary operators work", {
  expect_equal(-lsd_round, neg_round)
  expect_equal(+lsd_bases, lsd_bases)
  expect_equal(-dec1, deb_decimal(-1.8375))
  expect_equal(+dec2, dec2)
})

# deb_lsd and deb_decimal arithmetic operators ----------------------------

test_that("Arithmetic operators work with deb_lsd and deb_decimal", {
  # deb_lsd and deb_decimal
  expect_equal(lsd2 + dec1, deb_lsd(7, 3, 5))
  expect_equal(lsd2 + dec2, deb_lsd(7, 3, 5))
  expect_equal(lsd_bases + deb_decimal(2.5, bases = bases2),
               deb_lsd(c(3, 7), c(41, 31), c(9, 8), bases2))
  expect_equal(lsd2 - dec1, deb_lsd(3, 9, 11))
  expect_equal(lsd2 / deb_decimal(2 + 2 / 3), 2)
  # deb_decimal and deb_lsd
  expect_equal(dec1 + lsd2, deb_lsd(7, 3, 5))
  expect_equal(dec1 - lsd2, deb_lsd(-3, -9, -11))
  expect_equal(deb_decimal(10 + 2 / 3) / lsd2, 2)
  # Errors
  expect_error(lsd_bases + dec1, bases_error)
  expect_error(dec1 + lsd_bases, bases_error)
  expect_error(lsd1 * dec1)
  expect_error(dec1 * lsd1)
})
