## Test equality and comparison ##

# Equality ----------------------------------------------------------------

test_that("Equality works with deb_lsd", {
  # Data
  lsd1 <- deb_lsd(1, 2, 3)
  lsd2 <- deb_lsd(5, 6, 8)
  lsd_alt <- deb_lsd(1, 2, 3, c(50, 16))
  normalize <- c(lsd1, lsd2, NA, deb_lsd(c(3, 2), c(40, 84), c(80, 65)))

  # Tests
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
  # Data
  dec1 <- deb_decimal(1.1125)
  dec2 <- deb_decimal(c(1.1125, NA, 5.225, 3.2875, 1.1125))

  # Tests
  expect_true(dec1 == deb_decimal(1.1125))
  expect_false(dec1 == deb_decimal(8.825))
  expect_true(dec1 != deb_decimal(8.825))
  expect_true(dec1 == deb_decimal(1.1125, bases = c(20, 12, 4)))
  expect_equal(dec2 == deb_decimal(22.25, "s"), c(TRUE, NA, FALSE, FALSE, TRUE))
  # Different units
  expect_true(dec1 == deb_decimal(22.25, "s"))
  expect_true(dec1 == deb_decimal(267, "d"))
  expect_true(deb_decimal(22.25, "s") == deb_decimal(267, "d"))
  expect_false(dec1 != deb_decimal(22.25, "s"))
  # Functions
  expect_equal(unique(dec2), dec2[-5])
  expect_true(anyDuplicated(dec2))
  expect_equal(is.na(dec2), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # Error with different bases
  expect_snapshot(error = TRUE,
    dec1 == deb_decimal(1.1125, bases = c(50, 16)))
  expect_snapshot(error = TRUE,
    deb_decimal(1.1125, bases = c(20, 12, 4)) == deb_decimal(1.1125, bases = c(50, 16, 8)))
  expect_snapshot(error = TRUE,
    dec1 == deb_decimal(1.1125, bases = c(50, 16, 8)))
})


test_that("Equality works with deb_tetra", {
  # Data
  tetra1 <- deb_tetra(1, 2, 3, 2)
  tetra2 <- deb_tetra(5, 6, 7, 3)
  tetra_alt <- deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8))

  # Tests
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
  # Data
  lsd1 <- deb_lsd(1, 2, 3)
  dec1 <- deb_decimal(1.1125)


  # Tests
  expect_true(lsd1 == dec1)
  expect_true(lsd1 == deb_tetra(1, 2, 3, 0))
  expect_true(deb_tetra(1, 2, 3, 0) == dec1)
  # Error with different bases
  expect_snapshot(error = TRUE,
    lsd1 == deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8)))
  expect_snapshot(error = TRUE,
    lsd1 == deb_decimal(1.1125, bases = c(50, 16, 8)))
})

# Comparison --------------------------------------------------------------

test_that("Comparison logical operators work", {
  # Data
  lsd1 <- deb_lsd(1, 2, 3)
  lsd2 <- deb_lsd(5, 6, 8)
  tetra1 <- deb_tetra(1, 2, 3, 2)
  tetra2 <- deb_tetra(5, 6, 7, 3)

  # deb_lsd
  expect_true(lsd2 > lsd1)
  expect_true(lsd2 <= deb_lsd(4, 26, 8))
  expect_false(lsd2 > deb_lsd(4, 26, 8))
  expect_true(lsd2 > deb_decimal(1.1125))
  expect_true(lsd2 > 5)
  expect_false(lsd2 < 5)

  # deb_decimal
  expect_true(deb_decimal(1.1125) < deb_decimal(8.825))
  expect_true(deb_decimal(1.1125) < deb_decimal(176.5, "s"))
  expect_true(deb_decimal(1.1125) < deb_decimal(2118, "d"))
  expect_true(deb_decimal(22.25, "s") < deb_decimal(2118, "d"))
  expect_true(deb_decimal(8.825) > 5)
  expect_false(deb_decimal(8.825) < 5)

  # deb_tetra
  expect_true(tetra2 > tetra1)
  expect_true(tetra1 <= deb_tetra(0, 20, 27, 4))
  expect_false(tetra2 > deb_tetra(4, 26, 8, 12))
  expect_true(lsd2 > tetra1)
  expect_true(tetra2 > deb_decimal(86.925, "s", c(20, 12, 4)))
  expect_true(tetra2 > 5)
  expect_false(tetra2 < 5)

  # Mixed types
  expect_true(tetra2 > lsd1)
  expect_true(deb_decimal(176.5, "s") > lsd1)
  expect_true(deb_decimal(8.825) > tetra1)
  expect_true(tetra2 > deb_decimal(1.1125, bases = c(20, 12, 4)))

  # Error with different bases
  expect_snapshot(error = TRUE,
    lsd1 < deb_lsd(1, 2, 3, bases = c(50, 16)))
  expect_snapshot(error = TRUE,
    deb_decimal(1.1125) < deb_decimal(1.1125, bases = c(50, 16)))
  expect_snapshot(error = TRUE,
    tetra1 < deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8)))
  expect_snapshot(error = TRUE,
    lsd1 < deb_tetra(1, 2, 3, 2, bases = c(50, 16, 8)))
  expect_snapshot(error = TRUE,
    lsd1 < deb_decimal(1.1125, bases = c(50, 16, 8)))
  expect_snapshot(error = TRUE,
    deb_decimal(267, "d") < deb_decimal(1.1125, bases = c(50, 16, 8)))
})

test_that("Comparison functions work", {
  # Data
  lsd <- deb_lsd(c(1, 5, NA, 3, 2), c(2, 6, NA, 40, 84), c(3, 8, NA, 80, 65))
  dec <- deb_decimal(c(1.1125, NA, 5.225, 3.2875, 1.1125))
  tetra <- deb_tetra(c(1, 5), c(2, 6), c(3, 7), c(2, 3))

  # median and quantile not implemented yet

  # deb_lsd
  expect_equal(min(lsd, na.rm = TRUE), lsd[[1]])
  expect_equal(max(lsd, na.rm = TRUE), lsd[[5]])
  expect_equal(range(lsd, na.rm = TRUE), c(lsd[[1]], lsd[[5]]))
  expect_equal(sort(lsd), lsd[c(1, 2, 4, 5)])

  # deb_decimal
  expect_equal(min(dec, na.rm = TRUE), dec[[1]])
  expect_equal(max(dec, na.rm = TRUE), dec[[3]])
  expect_equal(range(dec, na.rm = TRUE), c(dec[[1]], dec[[3]]))
  expect_equal(sort(dec), dec[c(1, 1, 4, 3)])

  # deb_tetra
  expect_equal(min(tetra), tetra[[1]])
  expect_equal(max(tetra), tetra[[2]])
  expect_equal(range(tetra), tetra)
  expect_equal(sort(c(tetra[[2]], tetra)), c(tetra, tetra[[2]]))
})
