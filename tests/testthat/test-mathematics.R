## Test mathematical functions ##

# vec_math ----------------------------------------------------------------
test_that("vec_math has error message for unimplemented functions", {
  expect_snapshot_error(prod(deb_lsd(1, 16, 9), deb_lsd(1, 16, 9)))
  expect_snapshot_error(sin(deb_lsd(1, 16, 9)))
  expect_snapshot_error(prod(deb_tetra(1, 16, 9, 3), deb_tetra(1, 16, 9, 3)))
})


# Math group --------------------------------------------------------------
test_that("sum and mean with deb_lsd work", {
  expect_equal(sum(deb_lsd(1, 16, 9), deb_lsd(5, 6, 8)), deb_lsd(7, 3, 5))
  expect_equal(mean(deb_lsd(c(1, 5), c(42, 30), c(13, 15), c(50, 16))),
               deb_lsd(3, 36, 14, c(50, 16)))
  # NA
  expect_equal(sum(deb_lsd(c(1, NA), c(2, NA), c(3, NA))),
               deb_lsd(NA, NA, NA))
  expect_equal(sum(deb_lsd(c(1, NA), c(2, NA), c(3, NA)), na.rm = TRUE),
               deb_lsd(1, 2, 3))
  expect_equal(mean(deb_lsd(c(1, NA), c(2, NA), c(3, NA)), na.rm = TRUE),
               deb_lsd(1, 2, 3))

  # Mean only takes first object
  expect_equal(mean(deb_lsd(1, 16, 9), deb_lsd(5, 6, 8)), deb_lsd(1, 16, 9))
  # Error with different bases
  expect_snapshot(error = TRUE,
                  sum(deb_lsd(1, 16, 9), deb_lsd(1, 16, 9, c(50, 16))))
})

test_that("sum and mean work with deb_decimal", {
  # Data
  x <- deb_decimal(c(36.8125, 112.3125, 72.375, 48.5625),
                   unit = "s", bases = c(20, 12, 4))

  expect_equal(sum(deb_decimal(1.8375), deb_decimal(1.5)), deb_decimal(3.3375))
  expect_equal(sum(deb_decimal(36.75, unit = "s"), deb_decimal(20, "s")),
               deb_decimal(56.75, "s"))
  expect_equal(sum(deb_decimal(c(1.8375, NA, 5.225, 3.2875, 1.1125)),
                   na.rm = TRUE),
               deb_decimal(11.4625))
  expect_equal(mean(deb_decimal(c(1.8375, NA, 5.225, 3.2875, 1.1125)),
                    na.rm = TRUE),
               deb_decimal(2.865625))
  # Different units work
  expect_equal(sum(deb_decimal(1.8375), deb_decimal(36.75, unit = "s")),
               deb_decimal(3.675))
  # Tetra bases
  expect_equal(sum(x), deb_decimal(270.0625, "s", c(20, 12, 4)))
  expect_equal(sum(deb_decimal(1.840625, bases = c(20, 12, 4)),
                   deb_decimal(1767, unit = "f", bases = c(20, 12, 4))),
               deb_decimal(3.68125, bases = c(20, 12, 4)))
  # Mixed bases
  expect_equal(sum(x, deb_decimal(1.8375)), deb_decimal(15.340625))
  # Errors
  expect_snapshot(error = TRUE,
    sum(deb_decimal(1.8375), deb_decimal(1.8375, bases = c(50, 16))))
  expect_snapshot(error = TRUE,
    sum(deb_decimal(1.8375), deb_decimal(1.8375, bases = c(50, 16, 8))))

})

test_that("sum and mean with deb_tetra work", {
  # Data
  x <- deb_tetra(c(1, 5), c(10, 36), c(3, 8), c(4, 6),
                 bases = c(50, 16, 8))

  expect_equal(sum(deb_tetra(1, 16, 9, 3), deb_tetra(5, 12, 3, 3)),
               deb_tetra(7, 9, 1, 2))
  expect_equal(sum(x), deb_tetra(6, 46, 12, 2, c(50, 16, 8)))
  expect_equal(mean(x), deb_tetra(3, 23, 6, 1, c(50, 16, 8)))
  # NA
  expect_equal(sum(deb_tetra(c(1, NA), c(16, NA), c(9, NA), c(3, NA))),
               deb_tetra(NA, NA, NA, NA))
  expect_equal(sum(deb_tetra(c(1, NA), c(16, NA), c(9, NA), c(3, NA)),
                   na.rm = TRUE),
               deb_tetra(1, 16, 9, 3))
  expect_equal(mean(deb_tetra(c(1, NA), c(16, NA), c(9, NA), c(3, NA)),
                   na.rm = TRUE),
               deb_tetra(1, 16, 9, 3))
  # Error with different bases
  expect_snapshot(sum(deb_tetra(1, 16, 9, 3), x), error = TRUE)
})

test_that("sum works with deb-style vectors and numeric", {
  expect_equal(sum(deb_lsd(5, 6, 8), 1.8375), deb_lsd(7, 3, 5))
  expect_equal(sum(deb_decimal(1.8375), 3.5), deb_decimal(5.3375))
  expect_equal(sum(deb_tetra(1, 16, 9, 3), 4.5), deb_tetra(6, 6, 9, 3))
})

test_that("sum works between deb-style vectors", {
  # Bases and data
  lsd <- deb_lsd(1, 16, 9)
  tetra <- deb_tetra(1, 16, 9, 3)

  # lsd and lsd decimal -> lsd
  expect_equal(sum(lsd, deb_decimal(1.8375)), deb_lsd(3, 13, 6))
  # lsd and tetra decimal -> lsd
  expect_equal(sum(lsd, deb_decimal(36.8125, "s", c(20, 12, 4))),
               deb_lsd(3, 13, 6.75))
  # lsd and tetra -> lsd
  expect_equal(sum(lsd, tetra), deb_lsd(3, 13, 6.75))
  # tetra and tetra decimal -> tetra
  expect_equal(sum(tetra, deb_decimal(36.8125, "s", c(20, 12, 4))),
               deb_tetra(3, 13, 7, 2))
  # tetra and lsd decimal -> tetra
  expect_equal(sum(tetra, deb_decimal(36.75, unit = "s")),
               deb_tetra(3, 13, 6, 3))
  # All three -> lsd
  expect_equal(sum(lsd, deb_decimal(36.75, unit = "s"), tetra),
               deb_lsd(5, 10, 3.75))
  # Errors
  expect_snapshot(error = TRUE,
                  sum(lsd, deb_decimal(1.8375, bases = c(50, 16))))
  expect_snapshot(error = TRUE,
                  sum(lsd, deb_tetra(1, 16, 9, 3, c(50, 16, 8))))
})

test_that("cumulative functions work", {
  lsd <- deb_lsd(c(2, 3, 1), c(2, 3, 1), c(2, 3, 1))
  dec <- deb_decimal(c(2, 3, 1))
  tetra <- deb_tetra(c(2, 3, 1), c(2, 3, 1), c(2, 3, 1), c(2, 3, 1))

  # cumsum
  expect_equal(cumsum(deb_lsd(rep(1, 5), rep(1, 5), rep(1, 5))),
               deb_lsd(1:5, 1:5, 1:5))
  expect_equal(cumsum(deb_decimal(c(1, 2, 3))), deb_decimal(c(1, 3, 6)))
  expect_equal(cumsum(deb_tetra(rep(1, 3), rep(1, 3), rep(1, 3), rep(1, 3))),
               deb_tetra(1:3, 1:3, 1:3, 1:3))
  # cummin
  expect_equal(cummin(lsd), c(lsd[[1]], lsd[[1]], lsd[[3]]))
  expect_equal(cummin(dec), c(dec[[1]], dec[[1]], dec[[3]]))
  expect_equal(cummin(tetra), c(tetra[[1]], tetra[[1]], tetra[[3]]))
  # cummax
  expect_equal(cummax(lsd), c(lsd[[1]], lsd[[2]], lsd[[2]]))
  expect_equal(cummax(dec), c(dec[[1]], dec[[2]], dec[[2]]))
  expect_equal(cummax(tetra), c(tetra[[1]], tetra[[2]], tetra[[2]]))
})

test_that("finite, infinite and NaN checks work with deb_lsd and deb_tetra", {
  # Data
  lsd <- deb_lsd(c(NA, 1), c(NA, 1), c(NA, 1))
  tetra <- deb_tetra(c(NA, 1), c(NA, 1), c(NA, 1), c(NA, 1))

  # lsd
  expect_equal(is.finite(lsd), c(FALSE, TRUE))
  expect_equal(is.infinite(lsd), c(FALSE, FALSE))
  expect_equal(is.nan(lsd), c(FALSE, FALSE))
  # tetra
  expect_equal(is.finite(tetra), c(FALSE, TRUE))
  expect_equal(is.infinite(tetra), c(FALSE, FALSE))
  expect_equal(is.nan(tetra), c(FALSE, FALSE))
})

# Round family with deb_lsd -----------------------------------------------

test_that("round family works with deb_lsd and deb_tetra", {
  # Data
  lsd <- deb_lsd(5, 19, 11.8755)
  tetra <- deb_tetra(5, 19, 11, 3.8755)

  # round
  expect_equal(round(lsd), deb_lsd(6, 0, 0))
  expect_equal(round(-lsd), deb_lsd(-6, 0, 0))
  expect_equal(round(deb_lsd(5, 49, 15.6, c(50, 16))),
               deb_lsd(6, 0, 0, c(50, 16)))
  expect_equal(round(lsd, 3), deb_lsd(5, 19, 11.876))
  expect_equal(round(deb_lsd(2, 3.3, 2.2)),
               round(deb_normalize(deb_lsd(2, 3.3, 2.2))))
  expect_equal(round(tetra), deb_tetra(6, 0, 0, 0))
  expect_equal(round(tetra, 2), deb_tetra(5, 19, 11, 3.88))
  # signif
  expect_equal(signif(lsd, 3), deb_lsd(5, 19, 11.9))
  expect_equal(signif(tetra, 3), deb_tetra(5, 19, 11, 3.88))
  # ceiling
  expect_equal(ceiling(lsd), deb_lsd(6, 0, 0))
  expect_equal(ceiling(-lsd), deb_lsd(-5, -19, -11))
  expect_equal(ceiling(tetra), deb_tetra(6, 0, 0, 0))
  expect_equal(ceiling(-tetra), deb_tetra(-5, -19, -11, -3))
  # floor
  expect_equal(floor(lsd), deb_lsd(5, 19, 11))
  expect_equal(floor(-lsd), deb_lsd(-6, 0, 0))
  expect_equal(floor(tetra), deb_tetra(5, 19, 11, 3))
  expect_equal(floor(-tetra), deb_tetra(-6, 0, 0, 0))
  # trunc
  expect_equal(trunc(lsd), deb_lsd(5, 19, 11))
  expect_equal(trunc(deb_lsd(2, 3.3, 2.2)),
               trunc(deb_normalize(deb_lsd(2, 3.3, 2.2))))
  expect_equal(trunc(tetra), deb_tetra(5, 19, 11, 3))
})

test_that("abs works with deb_lsd vectors", {
  expect_equal(abs(deb_lsd(1, 0, 0)), deb_lsd(1, 0, 0))
  expect_equal(abs(deb_lsd(-1, 0, 0)), deb_lsd(1, 0, 0))
  expect_equal(abs(deb_tetra(1, 0, 0, 0)), deb_tetra(1, 0, 0, 0))
  expect_equal(abs(deb_tetra(-1, 0, 0, 0)), deb_tetra(1, 0, 0, 0))
})
