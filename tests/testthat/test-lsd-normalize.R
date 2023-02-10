## Test normalization ##

test_that("is_negative works", {
  # lsd
  expect_true(is_negative(deb_lsd(-1, -2, -3)))
  expect_false(is_negative(deb_lsd(1, 2, 3)))
  expect_false(is_negative(deb_lsd(1, -2, -3)))
  expect_equal(is_negative(deb_lsd(c(1, -1), c(2, -2), c(3, -3))),
               c(FALSE, TRUE))
  # tetra
  expect_true(is_negative(deb_tetra(-1, -2, -3, -3)))
  expect_false(is_negative(deb_tetra(1, 2, 3, 3)))
  expect_false(is_negative(deb_tetra(1, -2, -3, -3)))
  expect_equal(is_negative(deb_tetra(c(1, -1), c(2, -2), c(3, -3), c(3, -3))),
               c(FALSE, TRUE))
})

test_that("decimal_check works", {
  # lsd
  expect_length(decimal_check(deb_lsd(c(1, -1), c(2, -2), c(3, -3))), 2)
  expect_equal(decimal_check(deb_lsd(1, 2, 3)), deb_lsd(1, 2, 3))
  expect_equal(decimal_check(deb_lsd(5.875, 84.325, 62.0999)),
               deb_lsd(5, 101, 71.9999))
  expect_equal(decimal_check(deb_lsd(-5.875, -84.325, -62.0999)),
               deb_lsd(-5, -101, -71.9999))
  expect_equal(decimal_check(deb_lsd(5.875, 84.325, 62.0999, c(50, 16))),
               deb_lsd(5, 128, 63.2999, c(50, 16)))
  # tetra
  expect_equal(decimal_check(deb_tetra(1, 2, 3, 3)), deb_tetra(1, 2, 3, 3))
  expect_equal(decimal_check(deb_tetra(5.11875, 84.325, 62.25, 25.3999)),
               deb_tetra(5, 86, 70, 27.9999))
  expect_equal(decimal_check(deb_tetra(-5.11875, -84.325, -62.25, -25.3999)),
               deb_tetra(-5, -86, -70, -27.9999))
  expect_equal(decimal_check(deb_tetra(5.11875, 84.325, 62.25, 25.3999,
                                       c(50, 16, 8))),
               deb_tetra(5, 90, 66, 28.9999, c(50, 16, 8)))

  # Floating point problems
  expect_equal(vctrs::field(decimal_check(deb_lsd(9.45, 0, 0)), "d"), 12)
})

test_that("lsd_normalize and lsd_normalize_neg work", {
  expect_equal(lsd_normalize(deb_lsd(5, 82, 56)), deb_lsd(9, 6, 8))
  expect_equal(lsd_normalize(deb_lsd(5, 82, 56, c(50, 16))),
               deb_lsd(6, 35, 8, c(50, 16)))
  expect_equal(lsd_normalize_neg(deb_lsd(-5, -82, -56)),
               deb_lsd(-9, -6, -8))
  expect_equal(lsd_normalize_neg(deb_lsd(-5, -82, -56, c(50, 16))),
               deb_lsd(-6, -35, -8, c(50, 16)))
})

test_that("tetra_normalize_pos and tetra_normalize_neg work", {
  expect_equal(tetra_normalize_pos(deb_tetra(5, 82, 51, 23)),
               deb_tetra(9, 6, 8, 3))
  expect_equal(tetra_normalize_pos(deb_tetra(5, 82, 51, 23, c(50, 16, 8))),
               deb_tetra(6, 35, 5, 7, c(50, 16, 8)))
  expect_equal(tetra_normalize_neg(deb_tetra(-5, -82, -51, -23)),
               deb_tetra(-9, -6, -8, -3))
})

test_that("it comes together with deb_normalize with deb_lsd", {
  # data
  lsd <- deb_lsd(c(5, -5, 5.875), c(82, -82, 84.325), c(56, -56, 62.0999))
  lsd_alt <- deb_lsd(c(5, -5, 5.875), c(82, -82, 84.325), c(56, -56, 62.0999),
                     c(50, 16))

  expect_equal(deb_normalize(deb_lsd(5, 82, 56)), deb_lsd(9, 6, 8))
  expect_equal(deb_normalize(deb_lsd(9.45, 0, 0)), deb_lsd(9, 9, 0))
  expect_equal(deb_normalize(deb_lsd(9, -6, 8)), deb_lsd(8, 14, 8))
  expect_equal(deb_normalize(lsd),
               deb_lsd(c(9, -9, 10), c(6, -6, 6), c(8, -8, 11.9999)))
  expect_equal(deb_normalize(lsd_alt),
               deb_lsd(c(6, -6, 7), c(35, -35, 31), c(8, -8, 15.2999),
                       c(50, 16)))
})

test_that("it comes together with deb_normalize with deb_tetra", {
  # Data
  tetra <- deb_tetra(c(5, -5, 5.11875), c(82, -82, 84.325),
                     c(51, -51, 62.25), c(23, -23, 25.3999))
  normalized <- deb_tetra(c(9, -9, 9), c(6, -6, 12),
                          c(8, -8, 4), c(3, -3, 3.9999))

  expect_equal(deb_normalize(deb_tetra(5, 82, 51, 23)), deb_tetra(9, 6, 8, 3))
  expect_equal(deb_normalize(deb_tetra(9, -6, 8, -2)), deb_tetra(8, 14, 7, 2))
  expect_equal(deb_normalize(tetra), normalized)
  expect_equal(deb_normalize(deb_tetra(5, 82, 51, 23, c(50, 16, 8))),
               deb_tetra(6, 35, 5, 7, c(50, 16, 8)))
})

test_that("deb_normalize works with numeric vector", {
  # It works for lsd
  expect_s3_class(deb_normalize(c(5, 82, 56)), "deb_lsd")
  expect_equal(deb_normalize(c(5, 82, 56)), deb_lsd(9, 6, 8))
  expect_equal(deb_normalize(c(-5, -82, -56)), deb_lsd(-9, -6, -8))
  expect_equal(deb_normalize(c(5, 82, 56), bases = c(50, 16)),
               deb_lsd(6, 35, 8, c(50, 16)))
  # It works for tetra
  expect_s3_class(deb_normalize(c(5, 82, 55, 7), bases = c(20, 12, 4)),
                  "deb_tetra")
  expect_equal(deb_normalize(c(5, 82, 55, 7), bases = c(20, 12, 4)),
               deb_tetra(9, 6, 8, 3))
  # Errors
  expect_snapshot_error(deb_normalize(TRUE))
  expect_snapshot_error(deb_normalize(4))
  expect_snapshot_error(deb_normalize(1:5))
  expect_snapshot_error(deb_normalize(1:3, bases = c(20, 12, 4)))
  expect_snapshot_error(deb_normalize(1:3, bases = c("hello", "hi")))
  expect_snapshot_error(deb_normalize(1:4))
})
