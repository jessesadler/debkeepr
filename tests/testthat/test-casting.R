## Test casting with deb_lsd, deb_decimal, and deb_tetra ##

library(vctrs)

bases2 <- c(50, 16)
bases3 <- c(20, 12, 4)
bases4 <- c(50, 16, 8)

lsd <- deb_lsd(c(NA, 2, 3), c(NA, 4, 5), c(NA, 6, 9))
lsd1 <- deb_lsd(1, 2, 3)
lsd_alt <- deb_lsd(1, 25, 4, bases = bases2)
lsd3 <- deb_lsd(c(1, 2, 3), c(2, 4, 5), c(3, 6, 9))

dec <- deb_decimal(c(NA, 2.225, 3.2875))
dec_l <- deb_decimal(1.1125)
dec_bases <- deb_decimal(1.505, bases = bases2)
dec_s <- deb_decimal(22.25, unit = "s")
dec_s2 <- deb_decimal(c(NA, 15, 25), unit = "s")
dec_s3 <- deb_decimal(c(22.25, 15, 25), unit = "s")
dec_d <- deb_decimal(267, unit = "d")
dec_d2 <- deb_decimal(c(NA, 180, 300), unit = "d")
dec_d3 <- deb_decimal(c(267, 180, 300), unit = "d")
dec3 <- deb_decimal(c(1.1125, 2.225, 3.2875))

tetra <- deb_tetra(c(NA, 1), c(NA, 2), c(NA, 4), c(NA, 2))
tetra1 <- deb_tetra(1, 2, 4, 2)
tetra_alt <- deb_tetra(1, 10, 3, 4, bases = bases4)

dec_tetra <- deb_decimal(c(1.840625, 1.11875), bases = bases3)
dec_tetra_alt <- deb_decimal(c(60.21875, 85.9375), bases = bases4)
dec_l_tetra <- deb_decimal(1.11875, bases = bases3)
dec_s_tetra <- deb_decimal(22.375, unit = "s", bases = bases3)
dec_d_tetra <- deb_decimal(268.5, unit = "d", bases = bases3)
dec_f_tetra <- deb_decimal(1074, unit = "f", bases = bases3)
dec_s_tetra_alt <- deb_decimal(60.21875, unit = "s", bases = bases4)

bases_error <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.")
mixed_error <- paste0("s and d units of the `bases` attributes must be ",
                      "equal to combine <deb_lsd> and <deb_tetra> vectors.")

x <- deb_lsd(l = 0:3, s = 4:7, d = 8:11)
y <- list(c(0, 4, 8),
          c(1, 5, 9),
          c(2, 6, 10),
          c(3, 7, 11))

z <- deb_tetra(l = 0:3, s = 4:7, d = 8:11, f = 1:4)
tetra_list <- list(c(0, 4, 8, 1),
                   c(1, 5, 9, 2),
                   c(2, 6, 10, 3),
                   c(3, 7, 11, 4))

# Test with vec_cast ------------------------------------------------------

test_that("vec_cast works for deb_lsd", {
  # deb_lsd to deb_lsd: checks for equal bases
  expect_equal(vec_cast(lsd, deb_lsd()), lsd)
  expect_equal(vec_cast(lsd_alt, deb_lsd(bases = bases2)), lsd_alt)
  expect_error(vec_cast(lsd_alt, deb_lsd()), bases_error)

  # deb_lsd with double and integer
  expect_equal(vec_cast(lsd1, numeric()), 1.1125)
  expect_equal(vec_cast(1.1125, deb_lsd()), lsd1)
  expect_equal(vec_cast(1:3, deb_lsd()), deb_lsd(1:3, 0, 0))
  expect_error(vec_cast(lsd, integer()))
  # Allow cast from numeric prototypes
  expect_equal(vec_cast(numeric(), deb_lsd()), deb_lsd())
  expect_equal(vec_cast(integer(), deb_lsd()), deb_lsd())
  # deb_lsd to character
  expect_equal(vec_cast(lsd, character()), c(NA, "2:4s:6d", "3:5s:9d"))
  # NA and incompatible cast from boilerplate
  expect_equal(vec_cast(NA, deb_lsd()), deb_lsd(NA, NA, NA))
  expect_error(vec_cast(factor("hello"), deb_lsd()))
})

test_that("vec_cast works for deb_decimal", {
  # deb_decimal to deb_decimal: checks for equal bases
  expect_equal(vec_cast(dec, deb_decimal()), dec)
  expect_equal(vec_cast(dec_bases, deb_decimal(bases = bases2)),
               dec_bases)
  expect_equal(vec_cast(dec_s, deb_decimal(unit = "s")), dec_s)
  expect_equal(vec_cast(dec_d, deb_decimal(unit = "d")), dec_d)
  expect_error(vec_cast(dec_bases, deb_decimal()), bases_error)
  # Convert units: see also test-conversion
  expect_equal(vec_cast(dec_l, deb_decimal(unit = "s")), dec_s)
  expect_equal(vec_cast(dec_l, deb_decimal(unit = "d")), dec_d)
  expect_equal(vec_cast(dec_s, deb_decimal()), dec_l)
  expect_equal(vec_cast(dec_s, deb_decimal(unit = "d")), dec_d)
  expect_equal(vec_cast(dec_d, deb_decimal()), dec_l)
  expect_equal(vec_cast(dec_d, deb_decimal(unit = "s")), dec_s)
  # Convert mixed bases
  expect_equal(vec_cast(dec_l, deb_decimal(bases = bases3)),
               deb_decimal(1.1125, bases = bases3))
  expect_equal(vec_cast(dec_s_tetra, deb_decimal(unit = "s")),
               deb_decimal(22.375, unit = "s"))
  expect_equal(vec_cast(dec_s_tetra, deb_decimal()),
               deb_decimal(1.11875, unit = "l"))
  expect_error(vec_cast(dec_l, deb_decimal(bases = bases4)), mixed_error)
  expect_error(vec_cast(dec_s_tetra_alt, deb_decimal()), mixed_error)

  # deb_decimal with double and integer
  expect_equal(vec_cast(dec, numeric()), c(NA, 2.225, 3.2875))
  expect_equal(vec_cast(1.1125, deb_decimal()), dec_l)
  expect_equal(vec_cast(1:3, deb_decimal()), deb_decimal(1:3))
  expect_error(vec_cast(dec, integer()))
  # deb_lsd to character
  expect_equal(vec_cast(dec, character()), c(NA, "2.225", "3.2875"))
  # NA and incompatible cast from boilerplate
  expect_equal(vec_cast(NA, deb_decimal()), deb_decimal(NA))
  expect_error(vec_cast(factor("hello"), deb_decimal()))
})

test_that("vec_cast works for deb_decimal with tetra bases", {
  # With same unit
  expect_equal(vec_cast(dec_s_tetra, deb_decimal(unit = "s", bases = bases3)),
               dec_s_tetra)
  # Convert units
  expect_equal(vec_cast(dec_l_tetra, deb_decimal(unit = "s", bases = bases3)),
               dec_s_tetra)
  expect_equal(vec_cast(dec_l_tetra, deb_decimal(unit = "d", bases = bases3)),
               dec_d_tetra)
  expect_equal(vec_cast(dec_s_tetra, deb_decimal(unit = "l", bases = bases3)),
               dec_l_tetra)
  expect_equal(vec_cast(dec_s_tetra, deb_decimal(unit = "d", bases = bases3)),
               dec_d_tetra)
  expect_equal(vec_cast(dec_d_tetra, deb_decimal(unit = "l", bases = bases3)),
               dec_l_tetra)
  expect_equal(vec_cast(dec_d_tetra, deb_decimal(unit = "s", bases = bases3)),
               dec_s_tetra)
  # Convert units with farthing
  expect_equal(vec_cast(dec_l_tetra, deb_decimal(unit = "f", bases = bases3)),
               dec_f_tetra)
  expect_equal(vec_cast(dec_s_tetra, deb_decimal(unit = "f", bases = bases3)),
               dec_f_tetra)
  expect_equal(vec_cast(dec_d_tetra, deb_decimal(unit = "f", bases = bases3)),
               dec_f_tetra)
  expect_equal(vec_cast(dec_f_tetra, deb_decimal(unit = "l", bases = bases3)),
               dec_l_tetra)
  expect_equal(vec_cast(dec_f_tetra, deb_decimal(unit = "s", bases = bases3)),
               dec_s_tetra)
  expect_equal(vec_cast(dec_f_tetra, deb_decimal(unit = "d", bases = bases3)),
               dec_d_tetra)

  # Error of trying to convert lsd decimal to farthing
  expect_error(vec_cast(dec_l, deb_decimal(unit = "f")),
               "bases` must be a numeric vector of length 3 to use `'f'` unit.")
})

test_that("vec_cast works for deb_tetra", {
  # deb_tetra to deb_tetra: checks for equal bases
  expect_equal(vec_cast(tetra, deb_tetra()), tetra)
  expect_equal(vec_cast(tetra_alt, deb_tetra(bases = bases4)), tetra_alt)
  expect_error(vec_cast(tetra_alt, deb_tetra()), bases_error)

  # deb_lsd with double and integer
  expect_equal(vec_cast(tetra1, numeric()),
               1 + 2 / 20 + 4 / 240 + 2 / 960)
  expect_equal(vec_cast(1.11875, deb_tetra()), tetra1)
  expect_equal(vec_cast(1:3, deb_tetra()), deb_tetra(1:3, 0, 0, 0))
  expect_error(vec_cast(tetra, integer()))
  # Allow cast from numeric prototypes
  expect_equal(vec_cast(numeric(), deb_tetra()), deb_tetra())
  expect_equal(vec_cast(integer(), deb_tetra()), deb_tetra())
  # deb_tetra to character
  expect_equal(vec_cast(tetra, character()), c(NA, "1:2s:4d:2f"))
  # NA and incompatible cast from boilerplate
  expect_equal(vec_cast(NA, deb_tetra()), deb_tetra(NA, NA, NA, NA))
  expect_error(vec_cast(factor("hello"), deb_tetra()))
})

test_that("vec_cast works with lists",{
  # list to deb_lsd
  expect_identical(vec_cast(y, deb_lsd()), x)
  expect_error(vec_cast(c(y, 5), deb_lsd()),
               "`x` must be a list of numeric vectors of length 3.")
  expect_false(identical(vec_cast(y, deb_lsd()), lsd))
  # list to deb_decimal
  expect_equal(vec_cast(y, deb_decimal()), deb_as_decimal(x))
  expect_error(vec_cast(c(y, 5), deb_decimal()))
  # list to deb_tetra
  expect_identical(vec_cast(tetra_list, deb_tetra()), z)
  expect_error(vec_cast(c(tetra_list, 5), deb_tetra()),
               "`x` must be a list of numeric vectors of length 4.")
  # deb_lsd to list
  expect_identical(vec_cast(x, list()), y)
  # deb_tetra to list
  expect_identical(vec_cast(z, list()), tetra_list)
  # deb_as_list
  expect_error(deb_as_list(dec))
  expect_identical(deb_as_list(x), y)
  expect_identical(deb_as_list(z), tetra_list)
})

test_that("vec_cast works between deb_lsd and deb_decimal", {
  # Successful
  expect_equal(vec_cast(dec, deb_lsd()), lsd)
  expect_equal(vec_cast(lsd, deb_decimal()), dec)
  # Units dealt with correctly
  expect_equal(vec_cast(dec_s, deb_lsd()), lsd1)
  # Alt bases and units work if provided to prototype
  expect_equal(vec_cast(dec_bases, deb_lsd(bases = bases2)),
               lsd_alt)
  expect_equal(vec_cast(lsd_alt, deb_decimal(bases = bases2)),
               dec_bases)
  expect_equal(vec_cast(dec_s, deb_decimal(unit = "s")), dec_s)
  # deb_decimal with tetra bases to lsd
  expect_equal(vec_cast(dec_l_tetra, deb_lsd()), deb_lsd(1, 2, 4.5))
  expect_equal(vec_cast(dec_s_tetra, deb_lsd()), deb_lsd(1, 2, 4.5))
  expect_equal(vec_cast(dec_d_tetra, deb_lsd()), deb_lsd(1, 2, 4.5))
  expect_equal(vec_cast(dec_f_tetra, deb_lsd()), deb_lsd(1, 2, 4.5))
  expect_equal(vec_cast(dec_s_tetra_alt, deb_lsd(bases = bases2)),
               deb_lsd(1, 10, 3.5, bases2))
  # deb_lsd to deb_decimal with tetra bases
  expect_equal(vec_cast(lsd1, deb_decimal(bases = bases3)),
               deb_decimal(1.1125, bases = bases3))
  expect_equal(vec_cast(lsd1, deb_decimal(unit = "f", bases = bases3)),
               deb_decimal(1068, "f", bases3))

  # Errors when x has different bases or units and default if not changed
  expect_error(vec_cast(lsd_alt, deb_lsd()), bases_error)
  expect_error(vec_cast(dec_bases, deb_decimal()), bases_error)
  expect_error(vec_cast(dec_bases, deb_lsd()), bases_error)
  expect_error(vec_cast(lsd_alt, deb_decimal()), bases_error)
  expect_error(vec_cast(dec_s_tetra_alt, deb_lsd()), mixed_error)
  expect_error(vec_cast(lsd1, deb_decimal(bases = bases4)), mixed_error)
})

test_that("vec_cast works between deb_lsd and deb_tetra", {
  # Successful
  expect_equal(vec_cast(tetra1, deb_lsd()),
               deb_lsd(1, 2, 4.5))
  expect_equal(vec_cast(deb_lsd(1, 2, 3.75), deb_tetra()),
               deb_tetra(1, 2, 3, 3))
  # With length more than 1
  expect_equal(vec_cast(lsd, deb_tetra()),
               deb_tetra(c(NA, 2, 3),
                         c(NA, 4, 5),
                         c(NA, 6, 9),
                         c(NA, 0, 0)))
  # Alt bases work if provided to prototype
  expect_equal(vec_cast(tetra_alt, deb_lsd(bases = bases2)),
               deb_lsd(1, 10, 3.5, bases = bases2))
  expect_equal(vec_cast(deb_lsd(1, 10, 3.5, bases = bases2),
                        deb_tetra(bases = bases4)),
               tetra_alt)
  # Errors when x has different bases than default if not changed
  expect_error(vec_cast(tetra_alt, deb_tetra()), bases_error)
  expect_error(vec_cast(tetra_alt, deb_lsd()), mixed_error)
  expect_error(vec_cast(lsd_alt, deb_tetra()), mixed_error)
})

test_that("vec_cast works between deb_decimal and deb_tetra", {
  # Successful
  expect_equal(vec_cast(tetra1, deb_decimal(bases = bases3)),
               dec_l_tetra)
  expect_equal(vec_cast(tetra1, deb_decimal(unit = "s", bases = bases3)),
               dec_s_tetra)
  expect_equal(vec_cast(tetra1, deb_decimal(unit = "d", bases = bases3)),
               dec_d_tetra)
  expect_equal(vec_cast(tetra1, deb_decimal(unit = "f", bases = bases3)),
               dec_f_tetra)
  expect_equal(vec_cast(dec_l_tetra, deb_tetra()), tetra1)
  expect_equal(vec_cast(dec_s_tetra, deb_tetra()), tetra1)
  expect_equal(vec_cast(dec_d_tetra, deb_tetra()), tetra1)
  expect_equal(vec_cast(dec_f_tetra, deb_tetra()), tetra1)
  # With length more than 1
  expect_equal(vec_cast(deb_decimal(c(1.11875, 2.234375, 3.2875), bases = bases3),
                        deb_tetra()),
               deb_tetra(1:3, c(2, 4, 5), c(4, 8, 9), c(2, 1, 0)))
  # deb_decimal with lsd bases to tetra
  expect_equal(vec_cast(dec_l, deb_tetra()), deb_tetra(1, 2, 3, 0))
  expect_equal(vec_cast(dec_s, deb_tetra()), deb_tetra(1, 2, 3, 0))
  expect_equal(vec_cast(dec_d, deb_tetra()), deb_tetra(1, 2, 3, 0))
  expect_equal(vec_cast(dec_bases, deb_tetra(bases = bases4)),
               deb_tetra(1, 25, 4, 0, bases4))
  # deb_tetra to deb_decimal with lsd bases
  expect_equal(vec_cast(tetra1, deb_decimal()), deb_decimal(1.11875))

  # Error
  expect_error(vec_cast(dec_bases, deb_tetra()), mixed_error)
  expect_error(vec_cast(dec_l_tetra, deb_tetra(bases = bases4)),
               bases_error)
})

# Test casting methods ----------------------------------------------------

test_that("deb_as_lsd works", {
  expect_equal(deb_as_lsd(lsd_alt), lsd_alt)
  expect_equal(deb_as_lsd(dec), lsd)
  expect_equal(deb_as_lsd(dec_d), lsd1)
  expect_equal(deb_as_lsd(deb_decimal(75.25, unit = "s", bases = bases2)),
               lsd_alt)
  expect_equal(deb_as_lsd(dec_l_tetra), deb_lsd(1, 2, 4.5))
  expect_equal(deb_as_lsd(dec_f_tetra), deb_lsd(1, 2, 4.5))
  expect_equal(deb_as_lsd(dec_s_tetra_alt), deb_lsd(1, 10, 3.5, bases2))
  expect_equal(deb_as_lsd(tetra1), deb_lsd(1, 2, 4.5))
  expect_equal(deb_as_lsd(1.1125), lsd1)
  expect_equal(deb_as_lsd(1.505, bases = bases2), lsd_alt)
  expect_equal(deb_as_lsd(NA), deb_lsd(NA, NA, NA))
  expect_error(deb_as_lsd(factor("hello")))
  expect_identical(deb_as_lsd(y), x)
})

test_that("deb_as_decimal works", {
  expect_equal(deb_as_decimal(dec_bases), dec_bases)
  expect_equal(deb_as_decimal(dec_s), dec_s)
  expect_equal(deb_as_decimal(lsd), dec)
  expect_equal(deb_as_decimal(lsd1, unit = "s"), dec_s)
  expect_equal(deb_as_decimal(lsd1, unit = "d"), dec_d)
  expect_equal(deb_as_decimal(lsd_alt), dec_bases)
  expect_equal(deb_as_decimal(tetra1), dec_l_tetra)
  expect_equal(deb_as_decimal(tetra_alt, unit = "s"), dec_s_tetra_alt)
  expect_equal(deb_as_decimal(1.1125), dec_l)
  expect_equal(deb_as_decimal(1.505, bases = bases2), dec_bases)
  expect_equal(deb_as_decimal(22.25, unit = "s"), dec_s)
  expect_equal(deb_as_decimal(NA), deb_decimal(NA))
  expect_error(deb_as_decimal(factor("hello")))
  expect_identical(deb_as_decimal(y), deb_as_decimal(x))
})

test_that("deb_as_tetra works", {
  expect_equal(deb_as_tetra(tetra_alt), tetra_alt)
  expect_equal(deb_as_tetra(lsd1, f = 4), deb_tetra(1, 2, 3, 0))
  expect_equal(deb_as_tetra(lsd_alt, f = 8),
               deb_tetra(1, 25, 4, 0, bases = bases4))
  expect_error(deb_as_tetra(lsd_alt))
  expect_equal(deb_as_tetra(dec_l_tetra), tetra1)
  expect_equal(deb_as_tetra(dec_f_tetra), tetra1)
  expect_equal(deb_as_tetra(dec_s_tetra_alt), tetra_alt)
  expect_equal(deb_as_tetra(dec_l, f = 4), deb_tetra(1, 2, 3, 0))
  expect_equal(deb_as_tetra(1.11875), tetra1)
  expect_equal(deb_as_tetra(1 + 10 / 50 + 3 / (50 * 16) + 4 / (50 * 16 * 8),
                            bases = bases4),
               tetra_alt)
  expect_equal(deb_as_tetra(NA), deb_tetra(NA, NA, NA, NA))
  expect_error(deb_as_tetra(factor("hello")))
  expect_identical(deb_as_tetra(tetra_list), z)
  # Error with no f unit
  expect_error(deb_as_tetra(lsd1))
  expect_error(deb_as_tetra(dec_l))

})

# Test assignment subsetting ----------------------------------------------

test_that("assignment subsetting works", {
  # deb_lsd
  lsd[[1]] <- lsd1
  expect_equal(lsd, lsd3)
  lsd[[1]] <- 1.1125
  expect_equal(lsd, lsd3)
  lsd[[1]] <- NA
  expect_equal(lsd, lsd)

  # deb_decimal
  dec[[1]] <- dec_l
  expect_equal(dec, dec3)
  dec[[1]] <- 1.1125
  expect_equal(dec, dec3)
  dec[[1]] <- NA
  expect_equal(dec, dec)

  # deb_decimal with different units
  dec[[1]] <- dec_s
  expect_equal(dec, dec3)
  dec[[1]] <- dec_d
  expect_equal(dec, dec3)
  dec_s2[[1]] <- dec_l
  expect_equal(dec_s2, dec_s3)
  dec_s2[[1]] <- dec_d
  expect_equal(dec_s2, dec_s3)
  dec_d2[[1]] <- dec_l
  expect_equal(dec_d2, dec_d3)
  dec_d2[[1]] <- dec_s
  expect_equal(dec_d2, dec_d3)

  # deb_decimal with tetra bases
  dec_tetra[[1]] <- dec_l_tetra
  expect_equal(dec_tetra, c(dec_l_tetra, dec_l_tetra))
  dec_tetra[[1]] <- dec_s_tetra
  expect_equal(dec_tetra, c(dec_l_tetra, dec_l_tetra))
  dec_tetra[[1]] <- dec_d_tetra
  expect_equal(dec_tetra, c(dec_l_tetra, dec_l_tetra))
  dec_tetra[[1]] <- dec_f_tetra
  expect_equal(dec_tetra, c(dec_l_tetra, dec_l_tetra))

  # deb_decimal with mixed bases
  dec[[1]] <- dec_f_tetra
  expect_equal(dec, deb_decimal(c(1.11875, 2.225, 3.2875)))
  dec_tetra[[1]] <- dec_d
  expect_equal(dec_tetra, deb_decimal(c(1.1125, 1.11875), bases = bases3))

  # deb_tetra
  tetra[[1]] <- tetra1
  expect_equal(tetra, c(tetra1, tetra1))
  tetra[[1]] <- 1.11875
  expect_equal(tetra, c(tetra1, tetra1))
  tetra[[1]] <- NA
  expect_equal(tetra, vec_c(NA, tetra1))

  # deb_lsd and deb_decimal
  lsd[[1]] <- dec_l
  expect_equal(lsd, lsd3)
  lsd[[1]] <- dec_s
  expect_equal(lsd, lsd3)
  dec[[1]] <- lsd1
  expect_equal(dec, dec3)
  lsd[[1]] <- dec_l_tetra
  expect_equal(lsd, c(deb_lsd(1, 2, 4.5), lsd[2:3]))
  dec_tetra[[1]] <- lsd1
  expect_equal(dec_tetra, deb_decimal(c(1.1125, 1.11875), bases = bases3))

  # deb_lsd and deb_tetra
  lsd[[1]] <- tetra1
  expect_equal(lsd, c(deb_lsd(1, 2, 4.5), lsd[2:3]))
  tetra[[1]] <- lsd1
  expect_equal(tetra, c(deb_tetra(1, 2, 3, 0), tetra1))

  # deb_decimal and deb_tetra
  tetra[[1]] <- dec_l_tetra
  expect_equal(tetra, c(tetra1, tetra1))
  tetra[[1]] <- dec_s_tetra
  expect_equal(tetra, c(tetra1, tetra1))
  tetra[[1]] <- dec_d_tetra
  expect_equal(tetra, c(tetra1, tetra1))
  tetra[[1]] <- dec_f_tetra
  expect_equal(tetra, c(tetra1, tetra1))
  dec_tetra[[1]] <- tetra1
  expect_equal(dec_tetra, c(dec_l_tetra, dec_l_tetra))

  # Errors due to attribute mismatches
  expect_error(lsd[[1]] <- lsd_alt, bases_error)
  expect_error(lsd[[1]] <- dec_bases, bases_error)
  expect_error(dec[[1]] <- dec_bases, bases_error)
  expect_error(dec[[1]] <- lsd_alt, bases_error)
  expect_error(tetra[[1]] <- tetra_alt, bases_error)

  # Mixed bases errors
  expect_error(lsd[[1]] <- tetra_alt, mixed_error)
  expect_error(tetra_alt[[1]] <- lsd, mixed_error)
  expect_error(dec[[1]] <- dec_s_tetra_alt, mixed_error)
  expect_error(dec_tetra_alt[[1]] <- dec_l, mixed_error)
  expect_error(lsd[[1]] <- dec_s_tetra_alt, mixed_error)
  expect_error(dec_tetra_alt[[1]] <- lsd1, mixed_error)
})
