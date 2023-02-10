## Test deb_tetra class ##

test_that("new_tetra works", {
  expect_length(new_tetra(), 0)
  expect_s3_class(new_tetra(), c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))
  expect_length(new_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(2, 4, 6)), 3)
})

test_that("deb_tetra works", {
  x <- deb_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(2, 4, 6))

  # Prototype
  expect_length(deb_tetra(), 0)
  expect_s3_class(deb_tetra(), c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))
  # Basics
  expect_s3_class(deb_tetra(1, 2, 3, 4),
                  c("deb_tetra", "vctrs_rcrd", "vctrs_vctr"))
  expect_true(deb_is_tetra(x))
  expect_false(deb_is_tetra(3))
  # NA
  expect_true(is.na(deb_tetra(3, 4, NA, 1)))
  expect_equal(is.na(deb_tetra(c(1, 2, 3), c(4, 5, 6),
                               c(7, 8, 9), c(2, 4, NA))),
               c(FALSE, FALSE, TRUE))
  # Data is correct
  expect_equal(vctrs::field(x, "l"), c(1, 2, 3))
  expect_equal(vctrs::field(x, "s"), c(4, 5, 6))
  expect_equal(vctrs::field(x, "d"), c(7, 8, 9))
  expect_equal(vctrs::field(x, "f"), c(2, 4, 6))
  expect_equal(deb_bases(x), c(s = 20L, d = 12L, f = 4L))
  expect_equal(deb_bases(deb_tetra(1, 2, 3, 4, bases = c(60, 16, 8))),
               c(s = 60L, d = 16L, f = 8L))
  # Errors
  expect_snapshot(deb_tetra(1), error = TRUE)
  expect_snapshot(deb_tetra(1, 3, f = 2), error = TRUE)
  expect_snapshot(deb_tetra("hello", 3, 4, 2), error = TRUE)
  expect_snapshot(deb_tetra(3, "hello", 4, 2), error = TRUE)
  expect_snapshot(deb_tetra(1:3, 1:2, 0, 1:5), error = TRUE)
  expect_snapshot(deb_tetra(1, 2, 3, 4, bases = c(20, 12)), error = TRUE)
})

test_that("deb_tetra prints", {
  x <- deb_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(2, 4, 6))

  expect_equal(vctrs::vec_ptype_abbr(x), "tetra[20s:12d:4f]")
  expect_equal(vctrs::vec_ptype_abbr(
    deb_tetra(1, 2, 3, 4, bases = c(60, 16, 8))), "tetra[60s:16d:8f]")

  expect_snapshot_output(print(x))
  expect_snapshot_output(
    print(deb_tetra(c(1, 2, 3), c(4, 5, 6), c(7, 8, NA), c(2, 4, 6))))
})
