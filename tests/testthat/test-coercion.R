## Test coercion with deb_lsd, deb_decimal, and deb_tetra ##

library(vctrs)

bases2 <- c(50, 16)
bases3 <- c(20, 12, 4)
bases4 <- c(50, 16, 8)

lsd <- deb_lsd(1, 2, 3)
lsd2 <- deb_lsd(4, 5, 6)
lsd_alt <- deb_lsd(1, 2, 3, bases2)
lsd_tetra <- deb_lsd(1, 16, 9.75)

dec <- deb_decimal(c(1.35, 2.5))
dec2 <- deb_decimal(3.25)
dec3 <- deb_decimal(c(1.35, 2.5, 3.25))
dec_s <- deb_decimal(c(27, 50), unit = "s")
dec_s2 <- deb_decimal(65, unit = "s")
dec_s3 <- deb_decimal(c(27, 50, 65), unit = "s")
dec_d <- deb_decimal(c(324, 600), unit = "d")
dec_d2 <- deb_decimal(780, unit = "d")
dec_d3 <- deb_decimal(c(324, 600, 780), unit = "d")

tetra <- deb_tetra(1, 2, 3, 4)
tetra2 <- deb_tetra(5, 6, 7, 8)

dec_tetra_l <- deb_decimal(1.840625, bases = bases3)
dec_tetra_s <- deb_decimal(36.8125, unit = "s", bases = bases3)
dec_tetra_d <- deb_decimal(441.75, unit = "d", bases = bases3)
dec_tetra_f <- deb_decimal(1767, unit = "f", bases = bases3)
dec_tetra_l_alt <- deb_decimal(1.5, bases = bases4)
dec_tetra_f_alt <- deb_decimal(7708, unit = "f", bases = bases4)

# deb_lsd coercion --------------------------------------------------------

test_that("deb_lsd coerces to itself", {
  # Successful
  expect_equal(vec_ptype_common(deb_lsd(), deb_lsd()), deb_lsd())
  expect_length(vec_c(lsd, lsd2), 2)
  expect_equal(vec_c(lsd, lsd2), deb_lsd(c(1, 4), c(2, 5), c(3, 6)))
  expect_length(c(lsd, lsd2), 2)
  expect_equal(c(lsd, lsd2), deb_lsd(c(1, 4), c(2, 5), c(3, 6)))
  # Errors
  expect_snapshot(vec_c(lsd, lsd_alt), error = TRUE)
  expect_snapshot(c(lsd, lsd_alt), error = TRUE)
})

test_that("double coerces to deb_lsd", {
  expect_equal(vec_ptype_common(deb_lsd(), double()), deb_lsd())
  expect_length(vec_c(lsd, 1.5), 2)
  expect_equal(vec_c(lsd, 1.5), deb_lsd(c(1, 1), c(2, 10), c(3, 0)))
  expect_equal(vec_c(1.5, lsd), deb_lsd(c(1, 1), c(10, 2), c(0, 3)))
  expect_length(c(lsd, 1.5), 2)
  expect_equal(c(lsd, 1.5), deb_lsd(c(1, 1), c(2, 10), c(3, 0)))
})

test_that("integer coerces to deb_lsd", {
  expect_equal(vec_ptype_common(deb_lsd(), integer()), deb_lsd())
  expect_length(vec_c(lsd, 1L), 2)
  expect_equal(vec_c(lsd, 1L), deb_lsd(c(1, 1), c(2, 0), c(3, 0)))
  expect_equal(vec_c(1L, lsd), deb_lsd(c(1, 1), c(0, 2), c(0, 3)))
  expect_length(c(lsd, 1L), 2)
  expect_equal(c(lsd, 1L), deb_lsd(c(1, 1), c(2, 0), c(3, 0)))
})

test_that("deb_lsd coercion works with NA", {
  expect_equal(vec_c(NA, lsd), deb_lsd(c(NA, 1), c(NA, 2), c(NA, 3)))
  expect_equal(c(lsd, NA), deb_lsd(c(1, NA), c(2, NA), c(3, NA)))
})

test_that("incompatible types do not work", {
  expect_snapshot_error(c(lsd, "hello"))
  expect_snapshot_error(c(lsd, TRUE))
})

# deb_decimal coercion ----------------------------------------------------

test_that("deb_decimal coerces to itself", {
  # Successful
  expect_equal(vec_ptype_common(deb_decimal(), deb_decimal()),
               deb_decimal())
  expect_length(vec_c(dec, dec2), 3)
  expect_equal(vec_c(dec, dec2), dec3)
  expect_length(c(dec, dec2), 3)
  expect_equal(c(dec, dec2), dec3)
  expect_equal(c(dec_s, dec_s2), dec_s3)
  expect_equal(c(dec_d, dec_d2), dec_d3)
  # Tetra
  expect_equal(vec_c(deb_decimal(1.5, bases = bases3),
                     deb_decimal(3.5, bases = bases3)),
               deb_decimal(c(1.5, 3.5), bases = bases3))

  # With different units: follows hierarchy
  expect_equal(deb_unit(c(dec, dec_s)), deb_unit(c(dec_s, dec)))
  expect_equal(deb_unit(c(dec, dec_d)), deb_unit(c(dec_d, dec)))
  expect_equal(deb_unit(c(dec_d, dec_s)), deb_unit(c(dec_s, dec_d)))
  expect_equal(c(dec_s, dec2), dec3)
  expect_equal(c(dec_d, dec2), dec3)
  expect_equal(c(dec_d, dec_s2), dec_s3)
  expect_equal(c(dec_tetra_d, dec_tetra_f), c(dec_tetra_d, dec_tetra_d))
  expect_equal(c(dec_tetra_f, dec_tetra_d), c(dec_tetra_d, dec_tetra_d))
  expect_equal(c(dec_tetra_f, dec_tetra_s), c(dec_tetra_s, dec_tetra_s))
  expect_equal(c(dec_tetra_f, dec_tetra_l), c(dec_tetra_l, dec_tetra_l))

  # With mixed tetra and lsd bases
  expect_equal(deb_bases(c(dec, dec_tetra_l)), deb_bases(c(dec_tetra_l, dec)))
  expect_equal(deb_bases(c(dec, dec_tetra_s)), deb_bases(c(dec_tetra_s, dec)))
  expect_equal(deb_bases(c(dec, dec_tetra_l)), c(s = 20L, d = 12L))
  expect_equal(deb_bases(c(dec_tetra_l, dec)), c(s = 20L, d = 12L))
  expect_equal(c(dec, dec_tetra_f), c(dec, 1.840625))
  expect_equal(c(dec_s, dec_tetra_f), c(dec_s, 36.8125))
  expect_equal(c(dec_d, dec_tetra_f), c(dec_d, 441.75))

  # Errors
  expect_snapshot(vec_c(dec, deb_decimal(4.5, bases = bases2)), error = TRUE)
  expect_snapshot(c(dec, deb_decimal(4.5, bases = bases2)), error = TRUE)
  expect_snapshot(vec_c(deb_decimal(1.5, bases = bases4),
                        deb_decimal(1.5, bases = bases3)), error = TRUE)
  expect_snapshot(c(dec, dec_tetra_l_alt), error = TRUE)
  expect_snapshot(c(dec_tetra_l_alt, dec), error = TRUE)

})

test_that("double coerces to deb_decimal", {
  expect_equal(vec_ptype_common(deb_decimal(), double()), deb_decimal())
  expect_length(vec_c(dec, 4.5), 3)
  expect_equal(vec_c(dec, 4.5), deb_decimal(c(1.35, 2.5, 4.5)))
  expect_equal(vec_c(4.5, dec), deb_decimal(c(4.5, 1.35, 2.5)))
  expect_length(c(dec, 4.5), 3)
  expect_equal(c(dec, 4.5), deb_decimal(c(1.35, 2.5, 4.5)))
})

test_that("integer coerces to deb_decimal", {
  expect_equal(vec_ptype_common(deb_decimal(), integer()), deb_decimal())
  expect_length(vec_c(dec, 4L), 3)
  expect_equal(vec_c(dec, 4L), deb_decimal(c(1.35, 2.5, 4)))
  expect_equal(vec_c(4L, dec), deb_decimal(c(4, 1.35, 2.5)))
  expect_length(c(dec, 4L), 3)
  expect_equal(c(dec, 4L), deb_decimal(c(1.35, 2.5, 4)))
})

test_that("deb_decimal coercion works with NA", {
  expect_equal(vec_c(NA, dec), deb_decimal(c(NA, 1.35, 2.5)))
  expect_equal(c(dec, NA), deb_decimal(c(1.35, 2.5, NA)))
})

test_that("incompatible types do not work", {
  expect_snapshot_error(c(dec, "hello"))
  expect_snapshot_error(c(dec, TRUE))
})


# deb_tetra coercion ------------------------------------------------------

test_that("deb_tetra coerces to itself", {
  # Successful
  expect_equal(vec_ptype_common(deb_tetra(), deb_tetra()), deb_tetra())
  expect_length(vec_c(tetra, tetra2), 2)
  expect_equal(vec_c(tetra, tetra2),
               deb_tetra(c(1, 5), c(2, 6), c(3, 7), c(4, 8)))
  expect_length(c(tetra, tetra2), 2)
  expect_equal(c(tetra, tetra2),
               deb_tetra(c(1, 5), c(2, 6), c(3, 7), c(4, 8)))
  # Errors
  expect_snapshot(vec_c(tetra, deb_tetra(1, 2, 3, 4, bases = bases4)),
                  error = TRUE)
  expect_snapshot(c(tetra, deb_tetra(1, 2, 3, 4, bases = bases4)),
                  error = TRUE)
})

test_that("double coerces to deb_tetra", {
  expect_equal(vec_ptype_common(deb_tetra(), double()), deb_tetra())
  expect_length(vec_c(tetra, 1.5), 2)
  expect_equal(vec_c(tetra, 1.5),
               deb_tetra(c(1, 1), c(2, 10), c(3, 0), c(4, 0)))
  expect_equal(vec_c(1.5, tetra),
               deb_tetra(c(1, 1), c(10, 2), c(0, 3), c(0, 4)))
  expect_length(c(tetra, 1.5), 2)
  expect_equal(c(tetra, 1.5),
               deb_tetra(c(1, 1), c(2, 10), c(3, 0), c(4, 0)))
})

test_that("integer coerces to deb_tetra", {
  expect_equal(vec_ptype_common(deb_tetra(), integer()), deb_tetra())
  expect_length(vec_c(tetra, 1L), 2)
  expect_equal(vec_c(tetra, 1L), deb_tetra(c(1, 1), c(2, 0), c(3, 0), c(4, 0)))
  expect_equal(vec_c(1L, tetra), deb_tetra(c(1, 1), c(0, 2), c(0, 3), c(0, 4)))
  expect_length(c(tetra, 1L), 2)
  expect_equal(c(tetra, 1L), deb_tetra(c(1, 1), c(2, 0), c(3, 0), c(4, 0)))
})

test_that("deb_tetra coercion works with NA", {
  expect_equal(vec_c(NA, tetra),
               deb_tetra(c(NA, 1), c(NA, 2), c(NA, 3), c(NA, 4)))
  expect_equal(c(tetra, NA),
               deb_tetra(c(1, NA), c(2, NA), c(3, NA), c(4, NA)))
})

test_that("incompatible types do not work", {
  expect_snapshot_error(c(tetra, "hello"))
  expect_snapshot_error(c(tetra, TRUE))
})

# deb_lsd and deb_decimal coercion ----------------------------------------

test_that("deb_decimal coerces to deb_lsd", {
  # deb_decimal with lsd bases
  expect_equal(vec_c(lsd, dec2), deb_lsd(c(1, 3), c(2, 5), c(3, 0)))
  expect_equal(c(lsd, dec2), deb_lsd(c(1, 3), c(2, 5), c(3, 0)))
  expect_equal(vec_c(dec2, lsd), deb_lsd(c(3, 1), c(5, 2), c(0, 3)))
  # deb_decimal with tetra bases
  expect_equal(vec_c(lsd, dec_tetra_l), c(lsd, lsd_tetra))
  expect_equal(vec_c(lsd_alt, dec_tetra_f_alt),
               c(lsd_alt, deb_lsd(1, 10, 3.5, bases2)))
  # Errors
  expect_snapshot(vec_c(lsd, deb_decimal(5.5, bases = bases2)), error = TRUE)
  expect_snapshot(vec_c(dec, lsd_alt), error = TRUE)
  expect_snapshot(vec_c(lsd, dec_tetra_l_alt), error = TRUE)
})


# deb_lsd and deb_tetra coercion ------------------------------------------

test_that("deb_tetra coerces to deb_lsd", {
  expect_equal(vec_c(lsd, tetra),
               deb_lsd(c(1, 1), c(2, 2), c(3, 4)))
  expect_equal(c(lsd, tetra),
               deb_lsd(c(1, 1), c(2, 2), c(3, 4)))
  expect_equal(vec_c(tetra, lsd),
               deb_lsd(c(1, 1), c(2, 2), c(4, 3)))

  expect_snapshot(vec_c(lsd, deb_tetra(1, 2, 3, 4, bases4)), error = TRUE)
  expect_snapshot(vec_c(tetra, lsd_alt), error = TRUE)
})


# deb_tetra and deb_decimal coercion --------------------------------------

test_that("deb_decimal coerces to deb_tetra", {
  # deb_decimal with tetra bases
  expect_equal(vec_c(tetra, deb_decimal(1.5, bases = bases3)),
               deb_tetra(c(1, 1), c(2, 10), c(3, 0), c(4, 0)))
  expect_equal(c(tetra, deb_decimal(1.5, bases = bases3)),
               deb_tetra(c(1, 1), c(2, 10), c(3, 0), c(4, 0)))
  expect_equal(vec_c(deb_decimal(1.5, bases = bases3), tetra),
               deb_tetra(c(1, 1), c(10, 2), c(0, 3), c(0, 4)))
  # deb_decimal with lsd bases
  expect_equal(vec_c(tetra, dec),
               deb_tetra(c(1, 1, 2), c(2, 7, 10), c(3, 0, 0), c(4, 0, 0)))
  expect_equal(vec_c(dec, tetra),
               deb_tetra(c(1, 2, 1), c(7, 10, 2), c(0, 0, 3), c(0, 0, 4)))
  # Errors
  expect_snapshot(vec_c(tetra, deb_decimal(1.5, bases = bases2)), error = TRUE)
  expect_snapshot(vec_c(tetra, deb_decimal(1.5, bases = c(20, 12, 8))),
                  error = TRUE)
})
