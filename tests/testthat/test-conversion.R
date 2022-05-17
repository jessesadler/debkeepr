## Test bases and unit conversions ##

bases1 <- c(20, 12)
bases2 <- c(40, 24)
bases3 <- c(20, 12, 4)
bases4 <- c(40, 24, 8)

lsd1 <- deb_lsd(5, 6, 8)
lsd2 <- deb_lsd(5, 13, 8, bases2)

dec_l <- deb_decimal(16 / 3)
dec_s <- deb_decimal(106 + 2 / 3, unit = "s")
dec_d <- deb_decimal(1280, unit = "d")
dec_l_alt <- deb_decimal(16 / 3, bases = bases2)
dec_s_alt <- deb_decimal(213 + 1 / 3, unit = "s", bases = bases2)
dec_d_alt <- deb_decimal(5120, unit = "d", bases = bases2)

dec_tetra_l <- deb_decimal(1.840625, bases = bases3)
dec_tetra_s <- deb_decimal(36.8125, "s", bases3)
dec_tetra_d <- deb_decimal(441.75, "d", bases3)
dec_tetra_f <- deb_decimal(1767, "f", bases3)

dec_lsd_l <- deb_decimal(1.840625)
dec_lsd_s <- deb_decimal(36.8125, "s")
dec_lsd_d <- deb_decimal(441.75, "d")

dec_tetra_l_alt <- deb_decimal(1.840625, bases = bases4)
dec_tetra_s_alt <- deb_decimal(73.625, "s", bases4)
dec_tetra_d_alt <- deb_decimal(1767, "d", bases4)
dec_tetra_f_alt <- deb_decimal(14136, "f", bases4)

dec_lsd_l_alt <- deb_decimal(1.840625, bases = bases2)
dec_lsd_s_alt <- deb_decimal(73.625, "s", bases2)
dec_lsd_d_alt <- deb_decimal(1767, "d", bases2)

tetra1 <- deb_tetra(1, 16, 9, 3)
tetra2 <- deb_tetra(1, 33, 15, 0, bases4)

# Bases conversion --------------------------------------------------------

test_that("Bases conversion works with deb_lsd vectors", {
  expect_equal(deb_convert_bases(lsd1, to = bases1), lsd1)
  expect_equal(deb_convert_bases(lsd1, to = bases2), lsd2)
  expect_equal(deb_convert_bases(lsd2, to = bases1), lsd1)
  expect_equal(as.numeric(deb_convert_bases(lsd2, to = bases1)),
               as.numeric(lsd1))
  # Errors
  expect_snapshot_error(deb_convert_bases("a", bases1))
  expect_snapshot(deb_convert_bases(lsd1, bases3), error = TRUE)
  expect_snapshot(deb_convert_bases(lsd1, to = c(NA, 20)), error = TRUE)
  expect_snapshot(deb_convert_bases(lsd1, to = c(-10, -20)), error = TRUE)
})

test_that("Bases conversion works with deb_decimal vectors", {
  # lsd
  expect_equal(deb_convert_bases(dec_l, to = bases1), dec_l)
  expect_equal(deb_convert_bases(dec_l, to = bases2), dec_l_alt)
  expect_equal(deb_convert_bases(dec_s, to = bases2), dec_s_alt)
  expect_equal(deb_as_lsd(deb_convert_bases(dec_s, to = bases2)), lsd2)
  expect_equal(deb_convert_bases(dec_d, to = bases2), dec_d_alt)
  expect_equal(deb_as_lsd(deb_convert_bases(dec_d, to = bases2)), lsd2)
  # tetra
  expect_equal(deb_convert_bases(dec_tetra_l, to = bases3), dec_tetra_l)
  expect_equal(deb_convert_bases(dec_tetra_l, to = bases4), dec_tetra_l_alt)
  expect_equal(deb_convert_bases(dec_tetra_s_alt, to = bases3), dec_tetra_s)
  expect_equal(deb_convert_bases(dec_tetra_d_alt, to = bases3), dec_tetra_d)
  expect_equal(deb_convert_bases(dec_tetra_f, to = bases4), dec_tetra_f_alt)
  # lsd to tetra
  expect_equal(deb_convert_bases(dec_lsd_l, to = bases3), dec_tetra_l)
  expect_equal(deb_convert_bases(dec_lsd_l, to = bases4), dec_tetra_l_alt)
  expect_equal(deb_convert_bases(dec_lsd_s_alt, to = bases3), dec_tetra_s)
  expect_equal(deb_convert_bases(dec_lsd_d_alt, to = bases3), dec_tetra_d)
  # tetra to lsd
  expect_equal(deb_convert_bases(dec_tetra_l, to = bases1), dec_lsd_l)
  expect_equal(deb_convert_bases(dec_tetra_l, to = bases2), dec_lsd_l_alt)
  expect_equal(deb_convert_bases(dec_tetra_s_alt, to = bases1), dec_lsd_s)
  expect_equal(deb_convert_bases(dec_tetra_d_alt, to = bases1), dec_lsd_d)
  expect_equal(deb_convert_bases(dec_tetra_f, to = bases2), dec_lsd_d_alt)
  # Show that the values are actually equal so calculations are correct
  expect_true(deb_as_lsd(deb_as_tetra(dec_tetra_l_alt)) == dec_lsd_d_alt)
  expect_true(deb_as_lsd(deb_as_tetra(dec_tetra_f)) == dec_lsd_d)
})

test_that("Bases conversion works with deb_tetra vectors", {
  expect_equal(deb_convert_bases(tetra1, to = bases3), tetra1)
  expect_equal(deb_convert_bases(tetra1, to = bases4), tetra2)
  expect_equal(deb_convert_bases(tetra2, to = bases3), tetra1)
  expect_equal(as.numeric(deb_convert_bases(tetra2, to = bases3)),
               as.numeric(tetra1))
  # Errors
  expect_snapshot(deb_convert_bases(tetra1, bases2), error = TRUE)
  expect_snapshot(deb_convert_bases(tetra1, to = c(NA, 20, 12)), error = TRUE)
  expect_snapshot(deb_convert_bases(tetra1, to = c(-10, -20, -12)), error = TRUE)
})

# Unit conversion ---------------------------------------------------------

test_that("Unit conversion works", {
  # lsd
  expect_equal(deb_convert_unit(dec_l, to = "l"), dec_l)
  expect_equal(deb_convert_unit(dec_l, to = "s"), dec_s)
  expect_equal(deb_convert_unit(dec_l, to = "d"), dec_d)
  expect_equal(deb_convert_unit(dec_s_alt, to = "d"), dec_d_alt)
  expect_equal(deb_convert_unit(dec_s_alt, to = "l"), dec_l_alt)
  expect_equal(deb_convert_unit(dec_d_alt, to = "l"), dec_l_alt)
  expect_equal(deb_convert_unit(dec_d_alt, to = "s"), dec_s_alt)
  expect_equal(deb_as_lsd(deb_convert_unit(dec_d_alt, to = "l")), lsd2)
  # tetra
  expect_equal(deb_convert_unit(dec_tetra_l, to = "l"), dec_tetra_l)
  expect_equal(deb_convert_unit(dec_tetra_l, to = "s"), dec_tetra_s)
  expect_equal(deb_convert_unit(dec_tetra_l, to = "d"), dec_tetra_d)
  expect_equal(deb_convert_unit(dec_tetra_l, to = "f"), dec_tetra_f)
  expect_equal(deb_convert_unit(dec_tetra_s, to = "l"), dec_tetra_l)
  expect_equal(deb_convert_unit(dec_tetra_s, to = "d"), dec_tetra_d)
  expect_equal(deb_convert_unit(dec_tetra_s, to = "f"), dec_tetra_f)
  expect_equal(deb_convert_unit(dec_tetra_d, to = "l"), dec_tetra_l)
  expect_equal(deb_convert_unit(dec_tetra_d, to = "s"), dec_tetra_s)
  expect_equal(deb_convert_unit(dec_tetra_d, to = "f"), dec_tetra_f)
  expect_equal(deb_convert_unit(dec_tetra_f, to = "l"), dec_tetra_l)
  expect_equal(deb_convert_unit(dec_tetra_f, to = "s"), dec_tetra_s)
  expect_equal(deb_convert_unit(dec_tetra_f, to = "d"), dec_tetra_d)

  # Errors
  expect_snapshot_error(deb_convert_unit(lsd1, to = "l"))
  expect_snapshot_error(deb_convert_unit(dec_l, to = "hello"))
  expect_snapshot_error(deb_convert_unit(dec_l, to = "f"))
})
