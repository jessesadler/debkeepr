## Test equality and comparison ##

bases2 <- c(50, 16)
bases3 <- c(20, 12, 4)
bases4 <- c(50, 16, 8)

lsd1 <- deb_lsd(1, 2, 3)
lsd2 <- deb_lsd(5, 6, 8)
lsd_alt <- deb_lsd(1, 2, 3, bases2)
normalize <- c(lsd1, lsd2, NA, deb_lsd(c(3, 2), c(40, 84), c(80, 65)))
dec1 <- deb_decimal(1.1125)
dec2 <- deb_decimal(8.825)
dec3 <- deb_decimal(c(1.1125, NA, 5.225, 3.2875, 1.1125))
dec_s <- deb_decimal(22.25, "s")
dec_s2 <- deb_decimal(176.5, "s")
dec_d <- deb_decimal(267, "d")
dec_d2 <- deb_decimal(2118, "d")
dec_alt <- deb_decimal(1.1125, bases = bases2)
dec_tetra <- deb_decimal(1.1125, bases = bases3)
dec_tetra_alt <- deb_decimal(1.1125, bases = bases4)
tetra1 <- deb_tetra(1, 2, 3, 2)
tetra2 <- deb_tetra(5, 6, 7, 3)
tetra3 <- c(tetra1, tetra2)
tetra_alt <- deb_tetra(1, 2, 3, 2, bases4)


# Equality ----------------------------------------------------------------

test_that("Equality works with deb_lsd", {
  expect_true(lsd1 == deb_lsd(1, 2, 3))
  expect_false(lsd1 == lsd2)
  expect_true(lsd1 != lsd2)
  expect_true(lsd1 == deb_lsd(0, 20, 27)) # normalization
  expect_equal(unique(normalize), normalize[c(1:3, 5)])
  expect_true(anyDuplicated(normalize))
  expect_equal(is.na(normalize), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # Error with different bases
  expect_snapshot(lsd1 == lsd_alt, error = TRUE)
})

test_that("Equality works with deb_decimal", {
  expect_true(dec1 == deb_decimal(1.1125))
  expect_false(dec1 == dec2)
  expect_true(dec1 != dec2)
  expect_true(dec1 == dec_tetra)
  expect_equal(dec3 == dec_s, c(TRUE, NA, FALSE, FALSE, TRUE))
  # Different units
  expect_true(dec1 == dec_s)
  expect_true(dec1 == dec_d)
  expect_true(dec_s == dec_d)
  expect_false(dec1 != dec_s)
  # Functions
  expect_equal(unique(dec3), dec3[-5])
  expect_true(anyDuplicated(dec3))
  expect_equal(is.na(dec3), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # Error with different bases
  expect_snapshot(dec1 == dec_alt, error = TRUE)
  expect_snapshot(dec_tetra == dec_tetra_alt, error = TRUE)
  expect_snapshot(dec1 == dec_tetra_alt, error = TRUE)
})


test_that("Equality works with deb_tetra", {
  expect_true(tetra1 == deb_tetra(1, 2, 3, 2))
  expect_false(tetra1 == tetra2)
  expect_true(tetra1 != tetra2)
  expect_true(tetra1 == deb_tetra(0, 20, 25, 10)) # normalization
  expect_equal(unique(c(tetra1, tetra2, tetra1)), c(tetra1, tetra2))
  expect_true(anyDuplicated(c(tetra1, tetra2, tetra1)))
  expect_equal(is.na(c(tetra1, deb_tetra(NA, NA, NA, NA))), c(FALSE, TRUE))
  # Error with different bases
  expect_snapshot(tetra1 == tetra_alt, error = TRUE)
})

test_that("Equality works with mixed types", {
  expect_true(lsd1 == dec1)
  expect_true(lsd1 == deb_tetra(1, 2, 3, 0))
  expect_true(deb_tetra(1, 2, 3, 0) == dec1)
  # Error with different bases
  expect_snapshot(lsd1 == tetra_alt, error = TRUE)
  expect_snapshot(lsd1 == dec_tetra_alt, error = TRUE)
})

# Comparison --------------------------------------------------------------

test_that("Comparison logical operators work", {
  # deb_lsd
  expect_true(lsd2 > lsd1)
  expect_true(lsd2 <= deb_lsd(4, 26, 8))
  expect_false(lsd2 > deb_lsd(4, 26, 8))
  expect_true(lsd2 > dec1)
  expect_true(lsd2 > 5)
  expect_false(lsd2 < 5)

  # deb_decimal
  expect_true(dec1 < dec2)
  expect_true(dec1 < dec_s2)
  expect_true(dec1 < dec_d2)
  expect_true(dec_s < dec_d2)
  expect_true(dec2 > 5)
  expect_false(dec2 < 5)

  # deb_tetra
  expect_true(tetra2 > tetra1)
  expect_true(tetra1 <= deb_tetra(0, 20, 27, 4))
  expect_false(tetra2 > deb_tetra(4, 26, 8, 12))
  expect_true(lsd2 > tetra1)
  expect_true(tetra2 > deb_decimal(86.925, "s", bases3))
  expect_true(tetra2 > 5)
  expect_false(tetra2 < 5)

  # Mixed types
  expect_true(tetra2 > lsd1)
  expect_true(dec_s2 > lsd1)
  expect_true(dec2 > tetra1)
  expect_true(tetra2 > dec_tetra)

  # Error with different bases
  expect_snapshot(lsd1 < lsd_alt, error = TRUE)
  expect_snapshot(dec1 < dec_alt, error = TRUE)
  expect_snapshot(tetra1 < tetra_alt, error = TRUE)
  expect_snapshot(lsd1 < tetra_alt, error = TRUE)
  expect_snapshot(lsd1 < dec_tetra_alt, error = TRUE)
  expect_snapshot(dec_d < dec_tetra_alt, error = TRUE)
})

test_that("Comparison functions work", {
  # median and quantile not implemented yet

  # deb_lsd
  expect_equal(min(normalize, na.rm = TRUE), lsd1)
  expect_equal(max(normalize, na.rm = TRUE), deb_lsd(2, 84, 65))
  expect_equal(range(normalize, na.rm = TRUE), c(lsd1, deb_lsd(2, 84, 65)))
  expect_equal(sort(normalize), normalize[c(1, 2, 4, 5)])

  # deb_decimal
  expect_equal(min(dec3, na.rm = TRUE), dec1)
  expect_equal(max(dec3, na.rm = TRUE), deb_decimal(5.225))
  expect_equal(range(dec3, na.rm = TRUE), c(dec1, deb_decimal(5.225)))
  expect_equal(sort(dec3), dec3[c(1, 1, 4, 3)])

  # deb_tetra
  expect_equal(min(tetra3), tetra1)
  expect_equal(max(tetra3), tetra2)
  expect_equal(range(tetra3), tetra3)
  expect_equal(sort(c(tetra2, tetra3)), c(tetra3, tetra2))
})
