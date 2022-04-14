## Test deb_tetra class ##

x <- deb_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(2, 4, 6))

test_that("new_tetra works", {
  expect_equal(length(new_tetra()), 0)
  expect_equal(class(new_tetra()), c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))
  expect_equal(length(new_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(2, 4, 6))), 3)
})

test_that("deb_tetra works", {
  # Prototype
  expect_equal(length(deb_tetra()), 0)
  expect_equal(class(deb_tetra()), c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))
  # Basics
  expect_equal(class(deb_tetra(1, 2, 3, 4)),
               c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))
  expect_true(deb_is_tetra(x))
  expect_false(deb_is_tetra(3))
  # NA
  expect_true(is.na(deb_tetra(3, 4, NA, 1)))
  expect_equal(is.na(deb_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(2, 4, NA))),
               c(FALSE, FALSE, TRUE))
  # Data is correct
  expect_equal(vctrs::field(x, "l"), c(1, 2, 3))
  expect_equal(vctrs::field(x, "s"), c(4, 5, 6))
  expect_equal(vctrs::field(x, "d"), c(7, 8, 9))
  expect_equal(vctrs::field(x, "f"), c(2, 4, 6))
  expect_equal(deb_bases(x), c(s = 20L, d = 12L, f = 4L))
  expect_equal(deb_bases(deb_tetra(1, 2, 3, 4, bases = c(60, 16, 8))),
               c(s = 60L, d = 16L, f = 8L))
})

test_that("deb_lsd prints", {
  expect_that(print(x), prints_text())
  expect_that(print(deb_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA), c(2, 4, 6))),
              prints_text())
  expect_equal(vctrs::vec_ptype_abbr(x), "tetra[20s:12d:4f]")
  expect_equal(vctrs::vec_ptype_abbr(deb_tetra(1, 2, 3, 4, bases = c(60, 16, 8))),
               "tetra[60s:16d:8f]")
})
