## Test deb_decimal class ##

x <- c(1.125, 2.5, 3.3)
y <- deb_decimal(x)
tetra <- c(20, 12, 4)

test_that("new_decimal works", {
  expect_equal(length(new_decimal()), 0)
  expect_equal(class(new_decimal()),
               c("deb_decimal", "vctrs_vctr", "double"))
  expect_equal(length(new_decimal(x)), 3)
  # With tetra
  expect_equal(length(new_decimal(x, bases = c(20L, 12L, 4L))), 3)
  # Error if bases are not length 2 or 3
  expect_error(new_decimal(x, bases = c(20L, 12L, 8L, 4L)),
               "`bases` must be a numeric vector of length 2 for lsd or 3 for tetra.")
})

test_that("deb_decimal works", {
  # Prototype
  expect_equal(length(deb_decimal()), 0)
  expect_equal(class(deb_decimal()),
               c("deb_decimal", "vctrs_vctr", "double"))
  # Basics
  expect_equal(class(deb_decimal(x)),
               c("deb_decimal", "vctrs_vctr", "double"))
  expect_true(deb_is_decimal(y))
  expect_false(deb_is_decimal(3))
  # NA
  expect_true(is.na(deb_decimal(NA)))
  expect_equal(is.na(deb_decimal(c(1.25, 3, NA))), c(FALSE, FALSE, TRUE))
  # Data is correct
  expect_equal(as.numeric(y), x)
  expect_equal(deb_unit(y), "l")
  expect_equal(deb_unit(deb_decimal(x, unit = "s")), "s")
  expect_equal(deb_unit(deb_decimal(x, unit = "d")), "d")
  expect_error(deb_decimal(x, unit = "hello"))
  expect_equal(deb_bases(y), c(s = 20L, d = 12L))
  expect_equal(deb_bases(deb_decimal(x, bases = c(60, 16))),
               c(s = 60L, d = 16L))
  # With tetra
  expect_equal(length(deb_decimal(x, bases = tetra)), 3)
  expect_equal(deb_unit(deb_decimal(x, unit = "f", bases = tetra)), "f")
  # Error with "f" unit
  expect_error(deb_decimal(x, unit = "f"),
               "`bases` must be a numeric vector of length 3 to use `'f'` unit.")
})

test_that("deb_decimal prints", {
  expect_that(print(y), prints_text())
  expect_that(print(deb_decimal(NA)), prints_text())
  expect_equal(vctrs::vec_ptype_abbr(y), "l[20s:12d]")
  expect_equal(vctrs::vec_ptype_abbr(deb_decimal(1, unit = "s",
                                                 bases = c(60, 16))),
               "s[60s:16d]")
  expect_equal(unit_word(deb_decimal(1, "s")), "solidus")
  expect_equal(unit_word(deb_decimal(1, "d")), "denarius")
  # With tetra
  expect_that(print(deb_decimal(x, bases = tetra)), prints_text())
  expect_equal(unit_word(deb_decimal(1, "f", tetra)), "farthing")
  expect_equal(vctrs::vec_ptype_abbr(deb_decimal(1, bases = tetra)),
               "l[20s:12d:4f]")
  expect_equal(vctrs::vec_ptype_abbr(deb_decimal(1, unit = "f", bases = tetra)),
               "f[20s:12d:4f]")
})
