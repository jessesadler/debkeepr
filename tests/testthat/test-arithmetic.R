## Test arithmetic with deb_lsd, deb_tetra, and deb_decimal ##

bases2 <- c(50, 16)
bases3 <- c(20, 12, 4)
bases4 <- c(50, 16, 8)

lsd1 <- deb_lsd(1, 16, 9)
lsd2 <- deb_lsd(5, 6, 8)
lsd_alt <- deb_lsd(c(1, 5), c(16, 6), c(9, 8), bases = bases2)

dec_l <- deb_decimal(1.8375)
dec_s <- deb_decimal(36.75, unit = "s")
dec_d <- deb_decimal(552, unit = "d")
dec_l_alt <- deb_decimal(1.5, bases = bases2)
dec_s_alt <- deb_decimal(36.75, unit = "s", bases = bases2)

tetra1 <- deb_tetra(1, 16, 9, 3)
tetra2 <- deb_tetra(5, 6, 8, 2)
tetra_alt <- deb_tetra(c(1, 5), c(10, 36),
                       c(3, 8), c(4, 6),
                       bases = bases4)

dec_tetra_l <- deb_decimal(1.840625, bases = bases3)
dec_tetra_s <- deb_decimal(36.8125, unit = "s", bases = bases3)
dec_tetra_d <- deb_decimal(441.75, unit = "d", bases = bases3)
dec_tetra_f <- deb_decimal(1767, unit = "f", bases = bases3)
dec_tetra_l_alt <- deb_decimal(1.5, bases = bases4)
dec_tetra_f_alt <- deb_decimal(7708, unit = "f", bases = bases4)


# deb_lsd arithmetic operators --------------------------------------------
test_that("Arithmetic operators work with two deb_lsd vectors", {
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
  # deb_lsd and numeric
  expect_equal(lsd2 * 3, deb_lsd(16, 0, 0))
  expect_equal(lsd_alt * 2, deb_lsd(c(2, 10), c(33, 13), c(2, 0), bases2))
  expect_equal(lsd2 / 2, deb_lsd(2, 13, 4))
  expect_equal(lsd_alt[[1]] / 2, deb_lsd(0, 33, 4.5, bases2))
  # numeric and deb_lsd
  expect_equal(3 * lsd2, deb_lsd(16, 0, 0))
  expect_equal(2 * lsd_alt, deb_lsd(c(2, 10), c(33, 13), c(2, 0), bases2))
  expect_equal(1 / deb_lsd(0, 12, 0), deb_lsd(1, 13, 4))
  expect_equal(1 / deb_lsd(0, 40, 0, bases2), deb_lsd(1, 12, 8, bases2))
  # Errors
  expect_snapshot_error(lsd1 + 3)
  expect_snapshot_error(3 %% lsd1)
})


# deb_decimal arithmetic operators ----------------------------------------

test_that("Arithmetic operators work with two deb_decimal vectors", {
  expect_equal(dec_l + deb_decimal(1.5), deb_decimal(3.3375))
  expect_equal(dec_l_alt + dec_l_alt, deb_decimal(3, bases = bases2))
  expect_equal(dec_l - deb_decimal(1.5), deb_decimal(0.3375))
  expect_equal(dec_s_alt - deb_decimal(20.25, "s", bases2),
               deb_decimal(16.5, "s", bases2))
  expect_equal(deb_decimal(3.25) / deb_decimal(6.5), 0.5)
  expect_equal(dec_l_alt / deb_decimal(3, bases = bases2), 0.5)
  # Different units
  expect_equal(dec_l + dec_s, deb_decimal(3.675))
  expect_equal(dec_d + dec_l, deb_decimal(4.1375))
  expect_equal(dec_l_alt + dec_s_alt, deb_decimal(2.235, bases = bases2))
  expect_equal(dec_s - dec_l, deb_decimal(0))
  expect_equal(dec_d + dec_s, deb_decimal(82.75, unit = "s"))
  expect_equal(dec_l_alt - dec_s_alt, deb_decimal(0.765, bases = bases2))
  # Errors
  expect_snapshot(dec_l + deb_decimal(1.5, bases = bases2), error = TRUE)
  expect_snapshot_error(dec_l * dec_s)
})

test_that("Arithmetic ops work with deb_decimal vectors with tetra bases", {
  expect_equal(dec_tetra_l + dec_tetra_l, dec_tetra_l * 2)
  expect_equal(dec_tetra_f_alt + dec_tetra_f_alt, dec_tetra_f_alt * 2)
  expect_equal(dec_tetra_l - deb_decimal(1.5, bases = bases3),
               deb_decimal(0.340625, bases = bases3))
  expect_equal(dec_tetra_f_alt - deb_decimal(225, "f", bases4),
               deb_decimal(7483, "f", bases4))
  expect_equal(dec_tetra_l / deb_decimal(3.68125, bases = bases3), 0.5)
  expect_equal(dec_tetra_l_alt / deb_decimal(3, bases = bases4), 0.5)
  # Different units
  expect_equal(dec_tetra_l + dec_tetra_s, dec_tetra_l * 2)
  expect_equal(dec_tetra_f + dec_tetra_l, dec_tetra_l * 2)
  expect_equal(dec_tetra_l_alt + dec_tetra_f_alt,
               deb_as_decimal(2.704375, bases = bases4))
  expect_equal(dec_tetra_s - dec_tetra_d, deb_decimal(0, "s", bases3))
  expect_equal(dec_tetra_l_alt - dec_tetra_f_alt,
               deb_as_decimal(0.295625, bases = bases4))
  expect_equal(dec_tetra_l / deb_decimal(73.625, "s", bases3), 0.5)
  expect_equal(dec_tetra_l_alt / deb_decimal(150, "s", bases4), 0.5)
  # Errors
  expect_snapshot(dec_tetra_l + deb_decimal(1.5, bases = bases4), error = TRUE)
  expect_snapshot_error(dec_tetra_l * dec_tetra_s)
})

test_that("Arithmetic ops work with deb_decimal vectors with mixed bases", {
  expect_equal(dec_l + dec_tetra_l, deb_decimal(3.678125))
  expect_equal(dec_tetra_l + dec_l, deb_decimal(3.678125))
  expect_equal(dec_l - dec_tetra_s, deb_decimal(-0.003125))
  expect_equal(dec_tetra_f - dec_l, deb_decimal(0.003125))
  expect_equal(dec_tetra_l / deb_decimal(73.625, "s"), 0.5)
  expect_equal(deb_decimal(73.625, "s") / dec_tetra_l, 2)
  # Errors
  expect_snapshot(dec_l_alt + dec_tetra_f, error = TRUE)
  expect_snapshot(dec_tetra_f - dec_l_alt, error = TRUE)
})

test_that("Arithmetic operators work with deb_decimal and numeric", {
  # deb_decimal and numeric
  expect_equal(dec_l + 1.5, deb_decimal(3.3375))
  expect_equal(dec_l - 1.5, deb_decimal(0.3375))
  expect_equal(dec_s * 2, deb_decimal(73.5, unit = "s"))
  expect_equal(deb_decimal(2.5)^2, deb_decimal(6.25))
  expect_equal(dec_s / 3, deb_decimal(12.25, unit = "s"))
  expect_equal(dec_s %% 3, deb_decimal(0.75, unit = "s"))
  expect_equal(dec_s %/% 3, deb_decimal(12, unit = "s"))
  # numeric and deb_decimal
  expect_equal(1.5 + dec_l, deb_decimal(3.3375))
  expect_equal(1.5 - dec_l, deb_decimal(-0.3375))
  expect_equal(1 / deb_decimal(0.6), deb_decimal(1 + 2 / 3))
  expect_equal(2 * dec_s, deb_decimal(73.5, unit = "s"))
  # Errors
  expect_snapshot_error(1 %% dec_l)
})

test_that("Arithmetic operators work with tetra deb_decimal and numeric", {
  # deb_decimal and numeric
  expect_equal(dec_tetra_l + 1.5, deb_decimal(3.340625, bases = bases3))
  expect_equal(dec_tetra_l - 1.5, deb_decimal(0.340625, bases = bases3))
  expect_equal(dec_tetra_s * 2, deb_decimal(73.625, "s", bases3))
  expect_equal(deb_decimal(2.5, bases = bases3)^2,
               deb_decimal(6.25, bases = bases3))
  expect_equal(dec_tetra_s / 2, deb_decimal(18.40625, "s", bases3))
  expect_equal(dec_tetra_s %% 2, deb_decimal(0.8125, "s", bases3))
  expect_equal(dec_tetra_s %/% 2, deb_decimal(18, "s", bases3))
  # numeric and deb_decimal
  expect_equal(1.5 + dec_tetra_l, deb_decimal(3.340625, bases = bases3))
  expect_equal(1.5 - dec_tetra_l, deb_decimal(-0.340625, bases = bases3))
  expect_equal(1 / deb_decimal(0.6, bases = bases3),
               deb_decimal(1 + 2 / 3, bases = bases3))
  expect_equal(2 * dec_tetra_s, deb_decimal(73.625, "s", bases3))
  # Errors
  expect_snapshot_error(1 %% dec_tetra_l)
})

# deb_tetra arithmetic operators ------------------------------------------
test_that("Arithmetic operators work with two deb_tetra vectors", {
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
  # deb_tetra and numeric
  expect_equal(tetra2 * 3, deb_tetra(16, 0, 1, 2))
  expect_equal(tetra_alt * 2,
               deb_tetra(c(2, 11), c(20, 23), c(7, 1), c(0, 4), bases4))
  expect_equal(tetra2 / 2, deb_tetra(2, 13, 4, 1))
  expect_equal(tetra_alt / 2,
               deb_tetra(c(0, 2), c(30, 43), c(1, 4), c(6, 3), bases4))
  # numeric and deb_tetra
  expect_equal(3 * tetra2, deb_tetra(16, 0, 1, 2))
  expect_equal(2 * tetra_alt[[1]], deb_tetra(2, 20, 7, 0, bases4))
  expect_equal(1 / deb_tetra(0, 12, 0, 0), deb_tetra(1, 13, 4, 0))
  expect_equal(1 / deb_tetra(0, 40, 0, 0, bases4),
               deb_tetra(1, 12, 8, 0, bases4))
  # Errors
  expect_snapshot_error(tetra1 + 3)
  expect_snapshot_error(3 %% tetra1)
})

# Unary operators ---------------------------------------------------------

test_that("unary operators work", {
  expect_equal(-lsd1, deb_lsd(-1, -16, -9))
  expect_equal(+lsd_alt, lsd_alt)
  expect_equal(-dec_l, deb_decimal(-1.8375))
  expect_equal(+dec_s, dec_s)
  expect_equal(-tetra1, deb_tetra(-1, -16, -9, -3))
  expect_equal(+tetra_alt, tetra_alt)
})

# deb_lsd and deb_decimal arithmetic operators ----------------------------

test_that("Arithmetic operators work with deb_lsd and deb_decimal", {
  # deb_lsd and deb_decimal
  expect_equal(lsd2 + dec_l, deb_lsd(7, 3, 5))
  expect_equal(lsd2 + dec_s, deb_lsd(7, 3, 5))
  expect_equal(lsd_alt + deb_decimal(2.5, bases = bases2),
               deb_lsd(c(3, 7), c(41, 31), c(9, 8), bases2))
  expect_equal(lsd2 - dec_l, deb_lsd(3, 9, 11))
  expect_equal(lsd2 / deb_decimal(2 + 2 / 3), 2)
  expect_equal(lsd1 / deb_decimal(73.5, unit = "s"), 0.5)
  # deb_decimal and deb_lsd
  expect_equal(dec_l + lsd2, deb_lsd(7, 3, 5))
  expect_equal(dec_l - lsd2, deb_lsd(-3, -9, -11))
  expect_equal(deb_decimal(10 + 2 / 3) / lsd2, 2)
  expect_equal(deb_decimal(73.5, unit = "s") / lsd1, 2)
  # deb_lsd and deb_decimal with tetra bases
  expect_equal(lsd2 + dec_tetra_l, deb_lsd(7, 3, 5.75))
  expect_equal(lsd2 + dec_tetra_f, deb_lsd(7, 3, 5.75))
  expect_equal(dec_tetra_l + lsd2, deb_lsd(7, 3, 5.75))
  expect_equal(lsd2 / deb_decimal(2 + 2 / 3, bases = bases3), 2)

  # Errors
  expect_snapshot(lsd_alt + dec_l, error = TRUE)
  expect_snapshot(dec_l + lsd_alt, error = TRUE)
  expect_snapshot(lsd1 + dec_tetra_l_alt, error = TRUE)
  expect_snapshot(dec_tetra_l + lsd_alt, error = TRUE)
  expect_snapshot_error(lsd1 * dec_l)
  expect_snapshot_error(dec_l * lsd1)
})

# deb_lsd and deb_tetra arithmetic operators ------------------------------

test_that("Arithmetic operators work with deb_lsd and deb_tetra", {
  # deb_lsd and deb_tetra
  expect_equal(lsd1 + tetra1, deb_lsd(3, 13, 6.75))
  expect_equal(lsd_alt + tetra_alt,
               deb_lsd(c(2, 10), c(26, 43), c(12.5, 0.75), bases2))
  expect_equal(lsd2 - tetra1, deb_lsd(3, 9, 10.25))
  expect_equal(lsd_alt - tetra_alt,
               deb_lsd(c(0, 0), c(6, -30), c(5.5, -0.75), bases2))
  expect_equal(lsd1 / deb_tetra(0, 18, 4, 2), 2)
  # deb_tetra and deb_lsd
  expect_equal(tetra1 + lsd1, deb_lsd(3, 13, 6.75))
  expect_equal(tetra_alt + lsd_alt,
                deb_lsd(c(2, 10), c(26, 43), c(12.5, 0.75), bases2))
  expect_equal(tetra1 - lsd2, deb_lsd(-3, -9, -10.25))
  expect_equal(tetra_alt - lsd_alt,
               deb_lsd(c(0, 0), c(-6, 30), c(-5.5, 0.75), bases2))
  expect_equal(deb_tetra(1, 2, 3, 2) / deb_lsd(2, 4, 7), 0.5)
  # Errors
  expect_snapshot(lsd_alt + tetra1, error = TRUE)
  expect_snapshot(tetra1 + lsd_alt, error = TRUE)
  expect_snapshot_error(lsd1 * tetra1)
  expect_snapshot_error(tetra1 * lsd1)
})


# deb_tetra and deb_decimal arithmetic operators --------------------------

test_that("Arithmetic operators work with deb_tetra and deb_decimal", {
  # deb_tetra and deb_decimal
  expect_equal(tetra2 + dec_tetra_l, deb_tetra(7, 3, 6, 1))
  expect_equal(tetra2 + dec_tetra_s, deb_tetra(7, 3, 6, 1))
  expect_equal(tetra_alt + deb_decimal(1.11875, bases = bases4),
               deb_tetra(c(2, 6), c(16, 42), c(2, 7), c(4, 6), bases4))
  expect_equal(tetra2 - dec_tetra_l, deb_tetra(3, 9, 10, 3))
  expect_equal(tetra_alt - dec_tetra_l_alt,
               deb_tetra(c(0, 4), c(-14, 11), c(-12, 8), c(-4, 6), bases4))
  expect_equal(tetra1 / deb_decimal(0.9203125, bases = bases3), 2)
  expect_equal(tetra_alt[[1]] / deb_decimal(120.4375, "s", bases4), 0.5)
  # deb_decimal and deb_tetra
  expect_equal(dec_tetra_f + tetra2, deb_tetra(7, 3, 6, 1))
  expect_equal(deb_decimal(1.11875, bases = bases4) + tetra_alt,
               deb_tetra(c(2, 6), c(16, 42), c(2, 7), c(4, 6), bases4))
  expect_equal(dec_tetra_d - tetra2, deb_tetra(-3, -9, -10, -3))
  expect_equal(dec_tetra_l_alt - tetra_alt,
               deb_tetra(c(0, -4), c(14, -11), c(12, -8), c(4, -6), bases4))
  expect_equal(deb_decimal(0.9203125, bases = bases3) / tetra1, 0.5)
  expect_equal(deb_decimal(120.4375, "s", bases4) / tetra_alt[[1]], 2)
  # deb_tetra and deb_decimal with lsd bases
  expect_equal(tetra1 + dec_l, deb_tetra(3, 13, 6, 3))
  expect_equal(dec_s + tetra1, deb_tetra(3, 13, 6, 3))
  expect_equal(deb_decimal(0.9203125) / tetra1, 0.5)

  # Errors
  expect_snapshot(tetra_alt + dec_tetra_l, error = TRUE)
  expect_snapshot(dec_tetra_l + tetra_alt, error = TRUE)
  expect_snapshot(tetra_alt + dec_l, error = TRUE)
  expect_snapshot(dec_l_alt - tetra1, error = TRUE)
  expect_snapshot_error(tetra1 * dec_tetra_l)
  expect_snapshot_error(dec_tetra_l * tetra1)
})
