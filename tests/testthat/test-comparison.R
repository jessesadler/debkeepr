## Test equality and comparison ##

lsd1 <- deb_lsd(1, 2, 3)
lsd2 <- deb_lsd(5, 6, 8)
normalize <- c(lsd1, lsd2, NA, deb_lsd(c(3, 2), c(40, 84), c(80, 65)))
decimal1 <- deb_decimal(1.1125)
decimal2 <- deb_decimal(8.825)
decimal3 <- deb_decimal(c(1.1125, NA, 5.225, 3.2875, 1.1125))

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> objects.")

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
  expect_error(lsd1 == deb_lsd(5, 6, 8, bases = c(20, 16)),
               bases_error)
})

test_that("Equality works with deb_decimal", {
  expect_true(decimal1 == deb_decimal(1.1125))
  expect_false(decimal1 == decimal2)
  expect_true(decimal1 != decimal2)
  expect_equal(unique(decimal3), decimal3[-5])
  expect_true(anyDuplicated(decimal3))
  expect_equal(is.na(decimal3), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # Error with different bases
  expect_error(decimal1 == deb_decimal(1.1125, bases = c(24, 12)),
               bases_error)
})


# Comparison --------------------------------------------------------------

test_that("Comparison logical operators work", {
  # deb_lsd
  expect_true(lsd2 > lsd1)
  expect_true(lsd2 <= deb_lsd(4, 26, 8))
  expect_false(lsd2 > deb_lsd(4, 26, 8))
  expect_true(lsd2 > decimal1)
  expect_true(lsd2 > 5)
  expect_false(lsd2 < 5)

  # deb_decimal
  expect_true(decimal1 < decimal2)
  expect_true(decimal2 > 5)
  expect_false(decimal2 < 5)

  # Error with different bases
  expect_error(lsd1 < deb_lsd(15, 6, 8, bases = c(20, 16)), bases_error)
  expect_error(decimal1 < deb_decimal(11.125, bases = c(24, 12)), bases_error)
})

test_that("Comparison functions work", {
  # median and quantile not implemented yet
  expect_equal(min(normalize, na.rm = TRUE), lsd1)
  expect_equal(max(normalize, na.rm = TRUE), deb_lsd(2, 84, 65))
  expect_equal(range(normalize, na.rm = TRUE), c(lsd1, deb_lsd(2, 84, 65)))
  expect_equal(sort(normalize), normalize[c(1, 2, 4, 5)])

  expect_equal(min(decimal3, na.rm = TRUE), decimal1)
  expect_equal(max(decimal3, na.rm = TRUE), deb_decimal(5.225))
  expect_equal(range(decimal3, na.rm = TRUE), c(decimal1, deb_decimal(5.225)))
  expect_equal(sort(decimal3), decimal3[c(1, 1, 4, 3)])
})
