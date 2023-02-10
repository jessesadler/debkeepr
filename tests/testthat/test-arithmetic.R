## Test arithmetic with deb_lsd, deb_tetra, and deb_decimal ##

# deb_lsd arithmetic operators --------------------------------------------
test_that("Arithmetic operators work with two deb_lsd vectors", {
  # Bases
  bases2 <- c(50, 16)
  # Data
  lsd1 <- deb_lsd(1, 16, 9)
  lsd2 <- deb_lsd(5, 6, 8)
  lsd_alt <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases2)

  # plus
  expect_equal(lsd1 + lsd2, deb_lsd(7, 3, 5))
  expect_equal(lsd_alt + deb_lsd(2, 10, 5, bases2),
               deb_lsd(c(3, 7), c(26, 16), c(14, 13), bases2))
  # minus
  expect_equal(lsd1 - lsd2, deb_lsd(-3, -9, -11))
  expect_equal(lsd2 - lsd1, deb_lsd(3, 9, 11))
  expect_equal(lsd_alt - deb_lsd(2, 4, 5, bases2),
               deb_lsd(c(0, 3), c(-37, 2), c(-12, 3), bases2))
  # division
  expect_equal(deb_lsd(10, 13, 4) / lsd2, 2)
  expect_equal(as.numeric(1 / deb_lsd(0, 36, 10)),
               deb_lsd(1, 0, 0) / deb_lsd(0, 36, 10))
  expect_equal(deb_lsd(2, 33, 2, bases2) / lsd_alt[[1]], 2)
  # Errors
  expect_snapshot(lsd1 + lsd_alt, error = TRUE)
  expect_snapshot_error(lsd1 * lsd2)
})

test_that("Arithmetic operators work with deb_lsd and numeric", {
  # Bases and data
  bases2 <- c(50, 16)
  lsd_alt <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases2)

  # deb_lsd and numeric
  expect_equal(deb_lsd(5, 6, 8) * 3, deb_lsd(16, 0, 0))
  expect_equal(lsd_alt[[1]] / 2, deb_lsd(0, 33, 4.5, bases2))
  # numeric and deb_lsd
  expect_equal(3 * deb_lsd(5, 6, 8), deb_lsd(16, 0, 0))
  expect_equal(1 / deb_lsd(0, 40, 0, bases2), deb_lsd(1, 12, 8, bases2))
  # Errors
  expect_snapshot_error(deb_lsd(5, 6, 8) + 3)
  expect_snapshot_error(3 %% deb_lsd(5, 6, 8))
})


# deb_decimal arithmetic operators ----------------------------------------

test_that("Arithmetic operators work with two deb_decimal vectors", {
  # Bases
  bases2 <- c(50, 16)

  expect_equal(deb_decimal(1.5, bases = bases2) + deb_decimal(1.5, bases = bases2),
               deb_decimal(3, bases = bases2))
  expect_equal(deb_decimal(1.8375) - deb_decimal(1.5), deb_decimal(0.3375))
  expect_equal(deb_decimal(3.25) / deb_decimal(6.5), 0.5)
  # Different units
  expect_equal(deb_decimal(1.8375) + deb_decimal(36.75, unit = "s"), deb_decimal(3.675))
  expect_equal(
    deb_decimal(1.5, bases = bases2) - deb_decimal(36.75, unit = "s", bases2),
    deb_decimal(0.765, bases = bases2))
  # Errors
  expect_snapshot(deb_decimal(1.8375) + deb_decimal(1.5, bases = bases2), error = TRUE)
  expect_snapshot_error(deb_decimal(1.8375) * deb_decimal(1.8375))
})

test_that("Arithmetic ops work with deb_decimal vectors with tetra bases", {
  # Bases
  bases3 <- c(20, 12, 4)
  bases4 <- c(50, 16, 8)
  # Data
  dec_tetra_l <- deb_decimal(1.840625, bases = bases3)
  dec_tetra_f_alt <- deb_decimal(7708, unit = "f", bases4)

  expect_equal(dec_tetra_l + dec_tetra_l, dec_tetra_l * 2)
  expect_equal(dec_tetra_f_alt - deb_decimal(225, "f", bases4),
               deb_decimal(7483, "f", bases4))
  expect_equal(dec_tetra_l / deb_decimal(3.68125, bases = bases3), 0.5)
  # Different units
  expect_equal(dec_tetra_l + deb_decimal(36.8125, unit = "s", bases3),
               dec_tetra_l * 2)
  expect_equal(deb_decimal(1.5, bases = bases4) - dec_tetra_f_alt,
               deb_as_decimal(0.295625, bases = bases4))
  expect_equal(dec_tetra_l / deb_decimal(73.625, "s", bases3), 0.5)
  # Errors
  expect_snapshot(dec_tetra_l + deb_decimal(1.5, bases = bases4), error = TRUE)
  expect_snapshot_error(dec_tetra_l * dec_tetra_l)
})

test_that("Arithmetic ops work with deb_decimal vectors with mixed bases", {
  # Data
  dec_l <- deb_decimal(1.8375)
  dec_l_alt <- deb_decimal(1.5, bases = c(50, 16))
  dec_tetra_l <- deb_decimal(1.840625, bases = c(20, 12, 4))

  expect_equal(dec_l + dec_tetra_l, deb_decimal(3.678125))
  expect_equal(dec_tetra_l + dec_l, deb_decimal(3.678125))
  expect_equal(dec_l - dec_tetra_l, deb_decimal(-0.003125))
  expect_equal(dec_tetra_l - dec_l, deb_decimal(0.003125))
  expect_equal(dec_tetra_l / deb_decimal(73.625, "s"), 0.5)
  expect_equal(deb_decimal(73.625, "s") / dec_tetra_l, 2)
  # Errors
  expect_snapshot(dec_l_alt + dec_tetra_l, error = TRUE)
  expect_snapshot(dec_tetra_l - dec_l_alt, error = TRUE)
})

test_that("Arithmetic operators work with deb_decimal and numeric", {
  # Data
  x <- deb_decimal(1.8375)
  y <- deb_decimal(36.75, unit = "s")

  # deb_decimal and numeric
  expect_equal(x + 1.5, deb_decimal(3.3375))
  expect_equal(x - 1.5, deb_decimal(0.3375))
  expect_equal(y * 2, deb_decimal(73.5, unit = "s"))
  expect_equal(deb_decimal(2.5)^2, deb_decimal(6.25))
  expect_equal(y / 3, deb_decimal(12.25, unit = "s"))
  expect_equal(y %% 3, deb_decimal(0.75, unit = "s"))
  expect_equal(y %/% 3, deb_decimal(12, unit = "s"))
  # numeric and deb_decimal
  expect_equal(1.5 + x, deb_decimal(3.3375))
  expect_equal(1.5 - x, deb_decimal(-0.3375))
  expect_equal(1 / deb_decimal(0.6), deb_decimal(1 + 2 / 3))
  expect_equal(2 * y, deb_decimal(73.5, unit = "s"))
  # Errors
  expect_snapshot_error(1 %% x)
})

test_that("Arithmetic operators work with tetra deb_decimal and numeric", {
  # Bases and data
  bases3 <- c(20, 12, 4)
  x <- deb_decimal(1.840625, bases = bases3)
  y <- deb_decimal(36.8125, unit = "s", bases3)

  # deb_decimal and numeric
  expect_equal(x + 1.5, deb_decimal(3.340625, bases = bases3))
  expect_equal(x - 1.5, deb_decimal(0.340625, bases = bases3))
  expect_equal(y * 2, deb_decimal(73.625, "s", bases3))
  expect_equal(deb_decimal(2.5, bases = bases3)^2,
               deb_decimal(6.25, bases = bases3))
  expect_equal(y / 2, deb_decimal(18.40625, "s", bases3))
  expect_equal(y %% 2, deb_decimal(0.8125, "s", bases3))
  expect_equal(y %/% 2, deb_decimal(18, "s", bases3))
  # numeric and deb_decimal
  expect_equal(1.5 + x, deb_decimal(3.340625, bases = bases3))
  expect_equal(1.5 - x, deb_decimal(-0.340625, bases = bases3))
  expect_equal(1 / deb_decimal(0.6, bases = bases3),
               deb_decimal(1 + 2 / 3, bases = bases3))
  expect_equal(2 * y, deb_decimal(73.625, "s", bases3))
  # Errors
  expect_snapshot_error(1 %% x)
})

# deb_tetra arithmetic operators ------------------------------------------
test_that("Arithmetic operators work with two deb_tetra vectors", {
  # Bases and data
  bases4 <- c(50, 16, 8)
  tetra1 <- deb_tetra(1, 16, 9, 3)
  tetra2 <- deb_tetra(5, 6, 8, 2)
  tetra_alt <- deb_tetra(c(1, 5), c(10, 36),
                         c(3, 8), c(4, 6),
                         bases4)

  # plus
  expect_equal(tetra1 + tetra2, deb_tetra(7, 3, 6, 1))
  expect_equal(tetra_alt + deb_tetra(2, 20, 10, 6, bases4),
               deb_tetra(c(3, 8), c(30, 7), c(14, 3), c(2, 4), bases4))
  # minus
  expect_equal(tetra1 - tetra2, deb_tetra(-3, -9, -10, -3))
  expect_equal(tetra2 - tetra1, deb_tetra(3, 9, 10, 3))
  expect_equal(tetra_alt[[2]] - tetra_alt[[1]], deb_tetra(4, 26, 5, 2, bases4))
  # division
  expect_equal(deb_tetra(10, 13, 5, 0) / tetra2, 2)
  expect_equal(deb_tetra(2, 20, 7, 0, bases4) / tetra_alt[[1]], 2)
  expect_equal(as.numeric(1 / deb_tetra(0, 36, 10, 2)),
               deb_tetra(1, 0, 0, 0) / deb_tetra(0, 36, 10, 2))
  # Errors
  expect_snapshot(tetra1 + tetra_alt, error = TRUE)
  expect_snapshot_error(tetra1 * tetra2)
})

test_that("Arithmetic operators work with deb_tetra and numeric", {
  # Data
  x <- deb_tetra(5, 6, 8, 2)
  y <- deb_tetra(c(1, 5), c(10, 36), c(3, 8), c(4, 6), c(50, 16, 8))

  # deb_tetra and numeric
  expect_equal(x * 3, deb_tetra(16, 0, 1, 2))
  expect_equal(y / 2,
               deb_tetra(c(0, 2), c(30, 43), c(1, 4), c(6, 3), c(50, 16, 8)))
  # numeric and deb_tetra
  expect_equal(3 * x, deb_tetra(16, 0, 1, 2))
  expect_equal(1 / deb_tetra(0, 40, 0, 0, c(50, 16, 8)),
               deb_tetra(1, 12, 8, 0, c(50, 16, 8)))
  # Errors
  expect_snapshot_error(x + 3)
  expect_snapshot_error(3 %% x)
})

# Unary operators ---------------------------------------------------------

test_that("unary operators work", {
  expect_equal(-deb_lsd(1, 16, 9), deb_lsd(-1, -16, -9))
  expect_equal(+deb_lsd(5, 6, 8), deb_lsd(5, 6, 8))
  expect_equal(-deb_decimal(1.8375), deb_decimal(-1.8375))
  expect_equal(+deb_decimal(36.75, unit = "s"), deb_decimal(36.75, unit = "s"))
  expect_equal(-deb_tetra(1, 16, 9, 3), deb_tetra(-1, -16, -9, -3))
  expect_equal(+deb_tetra(1, 16, 9, 3), deb_tetra(1, 16, 9, 3))
})

# deb_lsd and deb_decimal arithmetic operators ----------------------------

test_that("Arithmetic operators work with deb_lsd and deb_decimal", {
  # Data
  lsd <- deb_lsd(5, 6, 8)
  lsd_alt <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), c(50, 16))
  dec <- deb_decimal(1.8375)
  dec_tetra <- deb_decimal(1.840625, bases = c(20, 12, 4))

  # deb_lsd and deb_decimal
  expect_equal(lsd + dec, deb_lsd(7, 3, 5))
  expect_equal(lsd + deb_decimal(36.75, unit = "s"), deb_lsd(7, 3, 5))
  expect_equal(lsd_alt + deb_decimal(2.5, bases = c(50, 16)),
               deb_lsd(c(3, 7), c(41, 31), c(9, 8), c(50, 16)))
  expect_equal(lsd - dec, deb_lsd(3, 9, 11))
  expect_equal(lsd / deb_decimal(2 + 2 / 3), 2)
  # deb_decimal and deb_lsd
  expect_equal(dec + lsd, deb_lsd(7, 3, 5))
  expect_equal(dec - lsd, deb_lsd(-3, -9, -11))
  expect_equal(deb_decimal(10 + 2 / 3) / lsd, 2)
  # deb_lsd and deb_decimal with tetra bases
  expect_equal(lsd + dec_tetra, deb_lsd(7, 3, 5.75))
  expect_equal(lsd - deb_decimal(1767, unit = "f", c(20, 12, 4)),
               deb_lsd(3, 9, 10.25))
  expect_equal(dec_tetra + lsd, deb_lsd(7, 3, 5.75))
  expect_equal(lsd / deb_decimal(2 + 2 / 3, bases = c(20, 12, 4)), 2)

  # Errors
  expect_snapshot(lsd_alt + dec, error = TRUE)
  expect_snapshot(dec + lsd_alt, error = TRUE)
  expect_snapshot(lsd + deb_decimal(1.5, bases = c(50, 16, 8)), error = TRUE)
  expect_snapshot(dec_tetra + lsd_alt, error = TRUE)
  expect_snapshot_error(lsd * dec)
  expect_snapshot_error(dec * lsd)
})

# deb_lsd and deb_tetra arithmetic operators ------------------------------

test_that("Arithmetic operators work with deb_lsd and deb_tetra", {
  # Data
  lsd <- deb_lsd(1, 16, 9)
  lsd_alt <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), c(50, 16))
  tetra <- deb_tetra(1, 16, 9, 3)
  tetra_alt <- deb_tetra(c(1, 5), c(10, 36),
                         c(3, 8), c(4, 6),
                         c(50, 16, 8))

  # deb_lsd and deb_tetra
  expect_equal(lsd + tetra, deb_lsd(3, 13, 6.75))
  expect_equal(lsd_alt - tetra_alt,
               deb_lsd(c(0, 0), c(6, -30), c(5.5, -0.75), c(50, 16)))
  expect_equal(lsd / deb_tetra(0, 18, 4, 2), 2)
  # deb_tetra and deb_lsd
  expect_equal(tetra + lsd, deb_lsd(3, 13, 6.75))
  expect_equal(tetra_alt - lsd_alt,
               deb_lsd(c(0, 0), c(-6, 30), c(-5.5, 0.75), c(50, 16)))
  expect_equal(deb_tetra(1, 2, 3, 2) / deb_lsd(2, 4, 7), 0.5)
  # Errors
  expect_snapshot(lsd_alt + tetra, error = TRUE)
  expect_snapshot(tetra + lsd_alt, error = TRUE)
  expect_snapshot_error(lsd * tetra)
  expect_snapshot_error(tetra * lsd)
})


# deb_tetra and deb_decimal arithmetic operators --------------------------

test_that("Arithmetic operators work with deb_tetra and deb_decimal", {
  # Bases
  bases3 <- c(20, 12, 4)
  bases4 <- c(50, 16, 8)

  x <- deb_tetra(1, 16, 9, 3)
  y <- deb_tetra(5, 6, 8, 2)
  tetra_alt <- deb_tetra(c(1, 5), c(10, 36), c(3, 8), c(4, 6),
                         bases4)
  dec <- deb_decimal(1.840625, bases = bases3)

  # deb_tetra and deb_decimal
  expect_equal(y + dec, deb_tetra(7, 3, 6, 1))
  expect_equal(y + deb_decimal(36.8125, unit = "s", bases3),
               deb_tetra(7, 3, 6, 1))
  expect_equal(tetra_alt - deb_decimal(1.5, bases = bases4),
               deb_tetra(c(0, 4), c(-14, 11), c(-12, 8), c(-4, 6), bases4))
  expect_equal(x / deb_decimal(0.9203125, bases = bases3), 2)
  # deb_decimal and deb_tetra
  expect_equal(deb_decimal(1767, unit = "f", bases3) + y,
               deb_tetra(7, 3, 6, 1))
  expect_equal(deb_decimal(1.5, bases = bases4) - tetra_alt,
               deb_tetra(c(0, -4), c(14, -11), c(12, -8), c(4, -6), bases4))
  expect_equal(deb_decimal(0.9203125, bases = bases3) / x, 0.5)
  # deb_tetra and deb_decimal with lsd bases
  expect_equal(x + deb_decimal(1.8375), deb_tetra(3, 13, 6, 3))
  expect_equal(deb_decimal(36.75, unit = "s") + x,
               deb_tetra(3, 13, 6, 3))
  expect_equal(deb_decimal(0.9203125) / x, 0.5)

  # Errors
  expect_snapshot(tetra_alt + dec, error = TRUE)
  expect_snapshot(dec + tetra_alt, error = TRUE)
  expect_snapshot(tetra_alt + deb_decimal(1.8375), error = TRUE)
  expect_snapshot(deb_decimal(1.5, bases = c(50, 16)) - x, error = TRUE)
  expect_snapshot_error(x * dec)
  expect_snapshot_error(dec * x)
})
