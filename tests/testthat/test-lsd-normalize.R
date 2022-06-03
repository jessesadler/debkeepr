## Test normalization ##

x <- deb_lsd(5, 82, 56)
neg <- deb_lsd(-5, -82, -56)
decimal <- deb_lsd(5.875, 84.325, 62.0999)
floating <- deb_lsd(9.45, 0, 0)

bases <- c(50, 16)
x_bases <- deb_lsd(5, 82, 56, bases)
neg_bases <- deb_lsd(-5, -82, -56, bases)
decimal_bases <- deb_lsd(5.875, 84.325, 62.0999, bases)

multi <- c(x, neg, decimal)
multi_bases <- c(x_bases, neg_bases, decimal_bases)

tetra <- deb_tetra(5, 82, 51, 23)
tetra_neg <- deb_tetra(-5, -82, -51, -23)
tetra_decimal <- deb_tetra(5.11875, 84.325, 62.25, 25.3999)
tetra_bases <- c(20, 12, 4)
tetra_alt_bases <- c(50, 16, 8)
tetra_alt <- deb_tetra(5, 82, 51, 23, tetra_alt_bases)
tetra_alt_decimal <- deb_tetra(5.11875, 84.325, 62.25, 25.3999,
                               tetra_alt_bases)
tetra_multi <- c(tetra, tetra_neg, tetra_decimal)



test_that("is_negative works", {
  # lsd
  expect_true(is_negative(neg))
  expect_false(is_negative(x))
  expect_false(is_negative(deb_lsd(5, -58, -11)))
  expect_equal(is_negative(multi), c(FALSE, TRUE, FALSE))
  # tetra
  expect_true(is_negative(tetra_neg))
  expect_false(is_negative(tetra))
  expect_false(is_negative(deb_tetra(5, -58, -11, -3)))
  expect_equal(is_negative(multi), c(FALSE, TRUE, FALSE))
})

test_that("decimal_check works", {
  # lsd
  expect_equal(length(decimal_check(multi)), 3)
  expect_equal(decimal_check(x), x)
  expect_equal(decimal_check(decimal), deb_lsd(5, 101, 71.9999))
  expect_equal(decimal_check(deb_lsd(-5.875, -84.325, -62.0999)),
               deb_lsd(-5, -101, -71.9999))
  expect_equal(decimal_check(decimal_bases), deb_lsd(5, 128, 63.2999, bases))
  # tetra
  expect_equal(decimal_check(tetra), tetra)
  expect_equal(decimal_check(tetra_decimal), deb_tetra(5, 86, 70, 27.9999))
  expect_equal(decimal_check(deb_tetra(-5.11875, -84.325, -62.25, -25.3999)),
                             deb_tetra(-5, -86, -70, -27.9999))
  expect_equal(decimal_check(tetra_alt_decimal),
               deb_tetra(5, 90, 66, 28.9999, tetra_alt_bases))

  # Floating point problems
  expect_equal(vctrs::field(decimal_check(floating), "d"), 12)
})

test_that("lsd_normalize and lsd_normalize_neg work", {
  expect_equal(lsd_normalize(x), deb_lsd(9, 6, 8))
  expect_equal(lsd_normalize(x_bases), deb_lsd(6, 35, 8, bases))
  expect_equal(lsd_normalize_neg(neg), deb_lsd(-9, -6, -8))
  expect_equal(lsd_normalize_neg(neg_bases), deb_lsd(-6, -35, -8, bases))
})

test_that("tetra_normalize_pos and tetra_normalize_neg work", {
  expect_equal(tetra_normalize_pos(tetra), deb_tetra(9, 6, 8, 3))
  expect_equal(tetra_normalize_pos(tetra_alt),
               deb_tetra(6, 35, 5, 7, tetra_alt_bases))
  expect_equal(tetra_normalize_neg(tetra_neg), deb_tetra(-9, -6, -8, -3))
})

test_that("it comes together with deb_normalize with deb_lsd", {
  expect_equal(deb_normalize(x), deb_lsd(9, 6, 8))
  expect_equal(deb_normalize(floating), deb_lsd(9, 9, 0))
  expect_equal(deb_normalize(deb_lsd(9, -6, 8)), deb_lsd(8, 14, 8))
  expect_equal(deb_normalize(multi),
               deb_lsd(c(9, -9, 10), c(6, -6, 6), c(8, -8, 11.9999)))
  expect_equal(deb_normalize(multi_bases),
               deb_lsd(c(6, -6, 7), c(35, -35, 31), c(8, -8, 15.2999), bases))
})

test_that("it comes together with deb_normalize with deb_tetra", {
  expect_equal(deb_normalize(tetra), deb_tetra(9, 6, 8, 3))
  expect_equal(deb_normalize(deb_tetra(9, -6, 8, -2)), deb_tetra(8, 14, 7, 2))
  expect_equal(deb_normalize(tetra_multi),
               deb_tetra(c(9, -9, 9), c(6, -6, 12),
                         c(8, -8, 4), c(3, -3, 3.9999)))
  expect_equal(deb_normalize(tetra_alt),
               deb_tetra(6, 35, 5, 7, tetra_alt_bases))
})

test_that("deb_normalize works with numeric vector", {
  # Errors
  expect_snapshot_error(deb_normalize(TRUE))
  expect_snapshot_error(deb_normalize(4))
  expect_snapshot_error(deb_normalize(1:5))
  expect_snapshot_error(deb_normalize(1:3, bases = c(20, 12, 4)))
  expect_snapshot_error(deb_normalize(1:3, bases = c("hello", "hi")))
  expect_snapshot_error(deb_normalize(1:4))

  # It works for lsd
  expect_s3_class(deb_normalize(c(5, 82, 56)), "deb_lsd")
  expect_equal(deb_normalize(c(5, 82, 56)), deb_lsd(9, 6, 8))
  expect_equal(deb_normalize(c(-5, -82, -56)), deb_lsd(-9, -6, -8))
  expect_equal(deb_normalize(c(5, 82, 56), bases), deb_lsd(6, 35, 8, bases))
  # It works for tetra
  expect_s3_class(deb_normalize(c(5, 82, 55, 7), bases = tetra_bases),
                  "deb_tetra")
  expect_equal(deb_normalize(c(5, 82, 55, 7), bases = tetra_bases),
               deb_tetra(9, 6, 8, 3))
})
