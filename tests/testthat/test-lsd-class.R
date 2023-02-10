## Test deb_lsd class ##

test_that("new_lsd works", {
  expect_length(new_lsd(), 0)
  expect_s3_class(new_lsd(), c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))
  expect_length(new_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)), 3)
})

test_that("deb_lsd works", {
  x <- deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))

  # Prototype
  expect_length(deb_lsd(), 0)
  expect_s3_class(deb_lsd(), c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))
  # Basics
  expect_s3_class(deb_lsd(1, 2, 3), c("deb_lsd", "vctrs_rcrd", "vctrs_vctr"))
  expect_true(deb_is_lsd(x))
  expect_false(deb_is_lsd(3))
  # NA
  expect_true(is.na(deb_lsd(3, 4, NA)))
  expect_equal(is.na(deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA))),
               c(FALSE, FALSE, TRUE))
  # Data is correct
  expect_equal(vctrs::field(x, "l"), c(1, 2, 3))
  expect_equal(vctrs::field(x, "s"), c(4, 5, 6))
  expect_equal(vctrs::field(x, "d"), c(7, 8, 9))
  expect_equal(deb_bases(x), c(s = 20L, d = 12L))
  expect_equal(deb_bases(deb_lsd(1, 2, 3, bases = c(60, 16))),
               c(s = 60L, d = 16L))
  # Errors
  expect_snapshot(deb_lsd(1), error = TRUE)
  expect_snapshot(deb_lsd(1, d = 2), error = TRUE)
  expect_snapshot(deb_lsd("hello", 3, 4), error = TRUE)
  expect_snapshot(deb_lsd(3, "hello", 4), error = TRUE)
  expect_snapshot(deb_lsd(1:3, 1:2, 0), error = TRUE)
  expect_snapshot(deb_lsd(1, 2, 3, bases = c(20, 12, 4)), error = TRUE)
})

test_that("deb_lsd prints", {
  x <- deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))

  expect_equal(vctrs::vec_ptype_abbr(x), "lsd[20s:12d]")
  expect_equal(vctrs::vec_ptype_abbr(deb_lsd(1, 2, 3, bases = c(60, 16))),
               "lsd[60s:16d]")
  expect_snapshot_output(print(x))
  expect_snapshot_output(print(deb_lsd(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA))))
})
