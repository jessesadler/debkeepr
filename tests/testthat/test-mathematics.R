## Test mathematical functions ##

bases2 <- c(50, 16)
bases3 <- c(20, 12, 4)
bases4 <- c(50, 16, 8)

lsd1 <- deb_lsd(1, 16, 9)
lsd2 <- deb_lsd(5, 6, 8)
lsd3 <- c(lsd1, lsd2, NA, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))
lsd4 <- c(lsd1, lsd2, deb_lsd(c(3, 7), c(12, 15), c(1, 11)))

lsd_bases <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases = bases2)
lsd_round <- deb_lsd(5, 19, 11.8755)
neg_round <- deb_lsd(-5, -19, -11.8755)
multi_decimal <- deb_lsd(2, 3.3, 2.2)
round1 <- deb_lsd(6, 0, 0)
round2 <- deb_lsd(-6, 0, 0)
finite <- deb_lsd(c(Inf, NaN, 1), c(Inf, NaN, 2), c(Inf, NaN, 3))

dec_l <- deb_decimal(1.8375)
dec_s <- deb_decimal(36.75, unit = "s")
dec_d <- deb_decimal(552, unit = "d")
dec2 <- deb_decimal(c(1.8375, NA, 5.225, 3.2875, 1.1125))
dec3 <- deb_decimal(c(1.8375, 5.225, 3.2875, 1.1125))
dec_l_alt <- deb_decimal(1.8375, bases = bases2)

tetra1 <- deb_tetra(1, 16, 9, 3)
tetra2 <- deb_tetra(5, 12, 3, 3)
tetra3 <- c(tetra1, tetra2, NA, deb_tetra(c(3, 2), c(12, 8), c(4, 6), c(2, 3)))
tetra_alt <- deb_tetra(c(1, 5), c(10, 36),
                       c(3, 8), c(4, 6),
                       bases = bases4)
tetra_round <- deb_tetra(5, 19, 11, 3.8755)
tetra_round_neg <- deb_tetra(-5, -19, -11, -3.8755)
tetra_multi_decimal <- deb_tetra(2, 3.3, 2.2, 1.8)
tetra_finite <- deb_tetra(c(Inf, NaN, 1), c(Inf, NaN, 2),
                          c(Inf, NaN, 3), c(Inf, NaN, 3))

dec_tetra_l <- deb_decimal(1.840625, bases = bases3)
dec_tetra_s <- deb_decimal(36.8125, unit = "s", bases = bases3)
dec_tetra_d <- deb_decimal(441.75, unit = "d", bases = bases3)
dec_tetra_f <- deb_decimal(1767, unit = "f", bases = bases3)

dec_tetra <- deb_decimal(c(36.8125, 112.3125, 72.375, 48.5625), unit = "s", bases = bases3)
dec_tetra_alt <- deb_decimal(c(1.840625, 5.615625, 3.618750, 2.428125), bases = bases4)
dec_tetra_f_alt <- deb_decimal(7708, unit = "f", bases = bases4)


bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.")


# vec_math ----------------------------------------------------------------
test_that("vec_math has error message for unimplemented functions", {
  expect_error(prod(lsd4), "not implemented.$")
  expect_error(sin(lsd4), "not implemented.$")
  expect_error(prod(tetra3), "not implemented.$")
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
  expect_equal(sum(dec_l, deb_decimal(1.5)), deb_decimal(3.3375))
  expect_equal(sum(dec_s, deb_decimal(20, "s")), deb_decimal(56.75, "s"))
  expect_equal(sum(dec_d, deb_decimal(48, "d")), deb_decimal(600, "d"))
  expect_equal(sum(dec2, na.rm = TRUE),
               deb_decimal(sum(as.numeric(dec2), na.rm = TRUE)))
  expect_equal(mean(dec2, na.rm = TRUE), deb_decimal(2.865625))
  # Different units work
  expect_equal(sum(dec_l, dec_s), deb_decimal(3.675))
  expect_equal(sum(dec_s, dec_d, dec_l), deb_decimal(5.975))
  expect_equal(sum(dec_d, dec_s), deb_decimal(82.75, unit = "s"))
  # Tetra bases
  expect_equal(sum(dec_tetra), deb_decimal(270.0625, "s", bases3))
  expect_equal(sum(dec_tetra_l, dec_tetra_f), dec_tetra_l * 2)
  expect_equal(sum(dec_tetra_alt), deb_decimal(13.503125, bases = bases4))
  # Errors
  expect_error(sum(dec_l, deb_decimal(1.5, bases = bases2)), bases_error)

})

test_that("sum and mean with deb_tetra work", {
  expect_equal(sum(tetra3), deb_tetra(NA, NA, NA, NA))
  expect_equal(sum(tetra3, na.rm = TRUE), deb_tetra(13, 10, 0, 3))
  expect_equal(sum(tetra1, tetra2), deb_tetra(7, 9, 1, 2))
  expect_equal(sum(tetra_alt), deb_tetra(6, 46, 12, 2, bases4))
  expect_equal(mean(tetra3), deb_tetra(NA, NA, NA, NA))
  expect_equal(mean(tetra3, na.rm = TRUE), deb_tetra(3, 7, 6, 0.75))
  expect_equal(mean(tetra_alt), deb_tetra(3, 23, 6, 1, bases4))
  # Error with different bases
  expect_error(sum(tetra1, tetra_alt), bases_error)
})

test_that("sum works with deb-style vectors and numeric", {
  expect_equal(sum(lsd1, dec_l), deb_lsd(3, 13, 6))
  expect_equal(sum(lsd1, dec_l), sum(lsd1, dec_s))
  expect_equal(sum(lsd3, 1.8375, 3, na.rm = TRUE), deb_lsd(23, 8, 2))
  expect_equal(sum(dec2, 3.5, na.rm = TRUE),
               deb_decimal(sum(1.8375, 5.225, 3.2875, 1.1125, 3.5)))
  expect_equal(sum(lsd1, tetra1), deb_lsd(3, 13, 6.75))
  expect_equal(sum(tetra1, dec_tetra_s), deb_tetra(3, 13, 7, 2))
  expect_equal(sum(tetra_alt, dec_tetra_f_alt), deb_tetra(8, 6, 15, 6, bases4))
  expect_equal(sum(c(tetra3, 4.5), na.rm = TRUE), deb_tetra(18, 0, 0, 3))
  expect_equal(sum(c(tetra_alt, 4.5)), deb_tetra(11, 21, 12, 2, bases4))
  # Error
  expect_error(sum(lsd1, dec_tetra), bases_error)
})

test_that("mean works with deb-style vectors and numeric", {
  expect_equal(mean(c(tetra1, lsd1, dec_l)), deb_lsd(1, 16, 9.25))
  expect_equal(mean(c(lsd_bases, tetra_alt, dec_l_alt, 5)),
               deb_lsd(3, 18, 9.875, bases2))
})

test_that("cumulative functions work", {
  # cumsum
  expect_equal(cumsum(deb_lsd(rep(1, 5), rep(1, 5), rep(1, 5))),
               deb_lsd(1:5, 1:5, 1:5))
  expect_equal(cumsum(lsd4),
               c(lsd1, sum(lsd4[1:2]), sum(lsd4[1:3]), sum(lsd4[1:4])))
  expect_equal(as.numeric(cumsum(dec3)), cumsum(as.numeric(dec3)))
  expect_equal(cumsum(deb_tetra(rep(1, 3), rep(1, 3), rep(1, 3), rep(1, 3))),
               deb_tetra(1:3, 1:3, 1:3, 1:3))
  # cummin
  expect_equal(cummin(lsd4), deb_lsd(rep(1, 4), rep(16, 4), rep(9, 4)))
  expect_equal(cummin(dec3), c(rep(dec3[[1]], 3), dec3[[4]]))
  expect_equal(cummin(tetra3[-3]), rep(tetra1, 4))
  # cummax
  expect_equal(cummax(lsd4), c(lsd1, lsd2, lsd2, lsd4[[4]]))
  expect_equal(cummax(dec3), c(dec_l, rep(dec3[[2]], 3)))
  expect_equal(cummax(tetra3[-3]), c(tetra1, rep(tetra2, 3)))
})

test_that("finite, infinite and NaN checks work with deb_lsd and deb_tetra", {
  # lsd
  expect_equal(is.finite(lsd4), c(T, T, T, T))
  expect_equal(is.infinite(lsd4), c(F, F, F, F))
  expect_equal(is.nan(lsd4), c(F, F, F, F))
  expect_equal(is.finite(finite), c(F, F, T))
  expect_equal(is.infinite(finite), c(T, F, F))
  expect_equal(is.nan(finite), c(F, T, F))
  # tetra
  expect_equal(is.finite(tetra3), c(T, T, F, T, T))
  expect_equal(is.infinite(tetra3), c(F, F, F, F, F))
  expect_equal(is.nan(tetra_finite), c(F, T, F))
  expect_equal(is.finite(tetra_finite), c(F, F, T))
  expect_equal(is.infinite(tetra_finite), c(T, F, F))
})

# Round family with deb_lsd -----------------------------------------------

test_that("round family works with deb_lsd and deb_tetra", {
  # round
  expect_equal(round(lsd_round), round1)
  expect_equal(round(neg_round), round2)
  expect_equal(round(deb_lsd(5, 49, 15.6, bases2)), deb_lsd(6, 0, 0, bases2))
  expect_equal(round(lsd_round, 3), deb_lsd(5, 19, 11.876))
  expect_equal(round(multi_decimal), round(deb_normalize(multi_decimal)))
  expect_equal(round(tetra_round), deb_tetra(6, 0, 0, 0))
  expect_equal(round(tetra_round, 2), deb_tetra(5, 19, 11, 3.88))
  expect_equal(round(tetra_multi_decimal), round(deb_normalize(tetra_multi_decimal)))
  # signif
  expect_equal(signif(lsd_round, 3), deb_lsd(5, 19, 11.9))
  expect_equal(signif(lsd_round, 2), round1)
  expect_equal(signif(tetra_round, 3), deb_tetra(5, 19, 11, 3.88))
  expect_equal(signif(tetra_round, 1), deb_tetra(6, 0, 0, 0))
  # ceiling
  expect_equal(ceiling(lsd_round), round1)
  expect_equal(ceiling(neg_round), deb_lsd(-5, -19, -11))
  expect_equal(ceiling(multi_decimal), ceiling(deb_normalize(multi_decimal)))
  expect_equal(ceiling(tetra_round), deb_tetra(6, 0, 0, 0))
  expect_equal(ceiling(tetra_round_neg), deb_tetra(-5, -19, -11, -3))
  # floor
  expect_equal(floor(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(floor(neg_round), round2)
  expect_equal(floor(multi_decimal), floor(deb_normalize(multi_decimal)))
  expect_equal(floor(tetra_round), deb_tetra(5, 19, 11, 3))
  expect_equal(floor(tetra_round_neg), deb_tetra(-6, 0, 0, 0))
  # trunc
  expect_equal(trunc(lsd_round), deb_lsd(5, 19, 11))
  expect_equal(trunc(neg_round), deb_lsd(-5, -19, -11))
  expect_equal(trunc(multi_decimal), trunc(deb_normalize(multi_decimal)))
  expect_equal(trunc(tetra_round), deb_tetra(5, 19, 11, 3))
  expect_equal(trunc(tetra_round_neg), deb_tetra(-5, -19, -11, -3))
})

test_that("abs works with deb_lsd vectors", {
  expect_equal(abs(lsd_round), lsd_round)
  expect_equal(abs(neg_round), lsd_round)
  expect_equal(abs(tetra_round), tetra_round)
  expect_equal(abs(tetra_round_neg), tetra_round)
})
