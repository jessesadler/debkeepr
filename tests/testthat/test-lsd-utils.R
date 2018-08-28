context("test-lsd-utils")

with_null <- deb_as_lsd(list(c(5, 3, 4), NULL, c(4, 12, 4), NULL))
with_zero <- deb_as_lsd(list(c(5, 3, 4), c(0, 0, 0), c(4, 12, 4), c(0, 0, 0)))
with_na <- deb_as_lsd(list(c(5, 3, 4),
                           as.numeric(c(NA, NA, NA)),
                           c(4, 12, 4),
                           as.numeric(c(NA, NA, NA))))
lsd <- deb_as_lsd(list(c(5, 3, 4), c(7, 12, 3), c(4, 12, 8)))

test_that("replace checks works", {
  expect_error(deb_replace_null(with_null, replace = list(c(0, 0, 0))),
               "replace must be a numeric vector")
  expect_error(deb_replace_null(with_null, replace = "hello"),
               "replace must be a numeric vector")
  expect_error(deb_replace_null(with_null, replace = c(1, 4)),
               "replace must be a numeric vector of length 3")
})

test_that("replace_null works", {
  expect_identical(deb_replace_null(lsd), lsd)
  expect_identical(deb_replace_null(with_null), with_zero)
  expect_identical(deb_replace_null(with_null, c(0, 0, 0)), with_zero)
  expect_identical(deb_replace_null(replace = c(0, 0, 0), lsd = with_null), with_zero)
  expect_identical(deb_replace_null(with_null, replace = c(NA, NA, NA)), with_na)
})
