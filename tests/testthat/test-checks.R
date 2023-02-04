## Test checks ##

# lsd_check ---------------------------------------------------------------

test_that("lsd: non-numeric or Inf is an error", {
  expect_snapshot_error(lsd_check("hello", 3, 4))
  expect_snapshot_error(lsd_check(3, "hello", 4))
  expect_snapshot_error(lsd_check(3, 4, "hello"))
  expect_snapshot_error(lsd_check(3, 4, Inf))
})

test_that("lsd: NA scalar is not an error", {
  expect_invisible(lsd_check(NA, 3, 4))
  expect_invisible(lsd_check(3, NA, 4))
  expect_invisible(lsd_check(3, 4, NA))
})

test_that("lsd: Multiple NA is not an error", {
  expect_invisible(lsd_check(c(NA, NA), 1:2, 3:4))
  expect_invisible(lsd_check(1:2, c(NA, NA), 3:4))
  expect_invisible(lsd_check(1:2, 3:4, c(NA, NA)))
})

test_that("lsd: length of l, s, and d all have values or are all length 0", {
  expect_invisible(lsd_check(double(), double(), double()))
  expect_snapshot_error(lsd_check(2, double(), double()))
  expect_snapshot_error(lsd_check(2, 3, double()))
  expect_snapshot_error(lsd_check(1:10, 11:20, double()))
})

test_that("lsd: length of l, s, and d are same length, length 1, or length 0", {
  # Successful
  expect_invisible(lsd_check(l = 3, s = 4, d = 1))
  expect_invisible(lsd_check(l = 1:3, s = 3:5, d = 5:7))
  expect_invisible(lsd_check(l = 1:3, s = 3:5, d = 0))
  # Error
  expect_snapshot_error(lsd_check(1:3, 1:2, 0))
})

# tetra_check -------------------------------------------------------------

test_that("tetra: non-numeric or Inf is an error", {
  expect_snapshot_error(tetra_check("hello", 3, 4, 0))
  expect_snapshot_error(tetra_check(3, "hello", 4, 0))
  expect_snapshot_error(tetra_check(3, 4, "hello", 0))
  expect_snapshot_error(tetra_check(3, 4, 0, "hello"))
  expect_snapshot_error(tetra_check(3, 4, 0, Inf))
})

test_that("tetra: NA scalar is not an error", {
  expect_invisible(tetra_check(NA, 3, 4, 2))
  expect_invisible(tetra_check(3, NA, 4, 2))
  expect_invisible(tetra_check(3, 4, NA, 2))
  expect_invisible(tetra_check(3, 4, 2, NA))
})

test_that("tetra: Multiple NA is not an error", {
  expect_invisible(tetra_check(c(NA, NA), 1:2, 3:4, 5:6))
  expect_invisible(tetra_check(1:2, c(NA, NA), 3:4, 5:6))
  expect_invisible(tetra_check(1:2, 3:4, c(NA, NA), 5:6))
  expect_invisible(tetra_check(1:2, 3:4, 5:6, c(NA, NA)))
})

test_that("tetra: length of units all have values or are all length 0", {
  expect_invisible(tetra_check(double(), double(), double(), double()))

  expect_snapshot_error(tetra_check(2, double(), double(), double()))
  expect_snapshot_error(tetra_check(2, 2, double(), double()))
  expect_snapshot_error(tetra_check(2, 2, 2, double()))
  expect_snapshot_error(tetra_check(1:10, 11:20, 21:30, double()))
})

test_that("tetra: length of units are same length, length 1, or length 0", {
  # Successful
  expect_invisible(tetra_check(l = 3, s = 4, d = 1, f = 2))
  expect_invisible(tetra_check(l = 1:3, s = 3:5, d = 5:7, f = 1:3))
  expect_invisible(tetra_check(l = 1:3, s = 3:5, d = 5:7, f = 0))
  # Error
  expect_snapshot_error(tetra_check(1:3, 1:2, 0, 1:4))
})

# lsd_bases_check ---------------------------------------------------------
test_that("lsd: bases is numeric vector of length 2", {
  # Successful
  expect_invisible(lsd_bases_check(c(20, 12)))
  bases <- TRUE
  expect_snapshot_error(lsd_bases_check(bases))
  bases <- c("hello", "goodbye")
  expect_snapshot_error(lsd_bases_check(bases))
  bases <- 1
  expect_snapshot_error(lsd_bases_check(bases))
  bases <- c(1, 2, 3)
  expect_snapshot_error(lsd_bases_check(bases))
})

test_that("lsd: bases does not have any missing values", {
  bases <- c(NA, 3)
  expect_snapshot_error(lsd_bases_check(bases))
  bases <- c(3, NA)
  expect_snapshot_error(lsd_bases_check(bases))
})

test_that("lsd: bases are natural numbers", {
  bases <- c(-12, -3)
  expect_snapshot_error(lsd_bases_check(bases))
  bases <- c(20, 0)
  expect_snapshot_error(lsd_bases_check(bases))
  bases <- c(20.5, 8.23)
  expect_snapshot_error(lsd_bases_check(bases))
})

# tetra_bases_check -------------------------------------------------------
test_that("tetra: bases is numeric vector of length 2", {
  # Successful
  expect_invisible(tetra_bases_check(c(20, 12, 4)))
  bases <- TRUE
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- c("hello", "goodbye")
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- 1
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- c(20, 12)
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- c(20, 12, 4, 2)
  expect_snapshot_error(tetra_bases_check(bases))
})

test_that("tetra: bases does not have any missing values", {
  bases <- c(NA, 3, 4)
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- c(3, NA, 4)
  expect_snapshot_error(tetra_bases_check(bases))
})

test_that("tetra: bases are natural numbers", {
  bases <- c(-12, -3, -6)
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- c(20, 0, 4)
  expect_snapshot_error(tetra_bases_check(bases))
  bases <- c(20.5, 8.23, 8.75)
  expect_snapshot_error(tetra_bases_check(bases))
})

# dec_bases_check ---------------------------------------------------------

test_that("decimal bases checks", {
  expect_invisible(dec_bases_check(c(20, 12, 4)))
  expect_invisible(dec_bases_check(c(20, 12)))
  bases <- 1
  expect_snapshot_error(dec_bases_check(bases))
  bases <- c(3, NA)
  expect_snapshot_error(dec_bases_check(bases))
  bases <- c(-12, -3)
  expect_snapshot_error(dec_bases_check(bases))
})

# f_bases_check -----------------------------------------------------------

test_that("f unit bases check", {
  expect_invisible(f_bases_check(4))
  expect_snapshot_error(f_bases_check("hello"))
  expect_snapshot_error(f_bases_check(c(20, 12, 4)))
  expect_snapshot_error(f_bases_check(NA_integer_))
  expect_snapshot_error(f_bases_check(-12))
})

# Equivalency -------------------------------------------------------------

test_that("bases equal tests equivalency", {
  expect_invisible(bases_equal(deb_lsd(2, 3, 4), deb_lsd(3, 2, 1)))
  expect_invisible(bases_equal(deb_lsd(2, 3, 4, bases = c(60, 12)),
                               deb_lsd(3, 2, 1, bases = c(60, 12))))
  expect_invisible(bases_equal(deb_decimal(1.25), deb_decimal(1.25)))
  expect_invisible(bases_equal(deb_tetra(2, 3, 4, 2), deb_tetra(3, 2, 1, 3)))
  # Errors
  expect_snapshot_error(bases_equal(deb_lsd(2, 3, 4),
                                    deb_lsd(3, 2, 1, c(60, 12))))
  expect_snapshot_error(bases_equal(deb_lsd(2, 3, 4),
                                    deb_lsd(3, 2, 1, c(20, 16))))
  expect_snapshot_error(bases_equal(deb_decimal(1.25),
                                    deb_decimal(1.25, bases = c(60, 12))))
  expect_snapshot_error(bases_equal(deb_decimal(1.25),
                                    deb_decimal(1.25, bases = c(20, 16))))
  expect_snapshot_error(bases_equal(deb_tetra(2, 3, 4, 2),
                                    deb_tetra(3, 2, 1, 2, c(60, 12, 4))))
  expect_snapshot_error(bases_equal(deb_tetra(2, 3, 4, 2),
                                    deb_tetra(3, 2, 1, 2, c(20, 16, 4))))
  expect_snapshot_error(bases_equal(deb_tetra(2, 3, 4, 2),
                                    deb_tetra(3, 2, 1, 2, c(20, 12, 8))))
})

test_that("mixed bases equality", {
  expect_snapshot_error(mixed_bases_equal(deb_tetra(1, 2, 3, 4),
                                          deb_lsd(1, 2, 3, c(50, 16))))
  expect_snapshot_error(mixed_bases_equal(deb_decimal(1),
                                          deb_decimal(2, bases = c(50, 16, 4))))
})

# list checks -------------------------------------------------------------

test_that("lsd list check works", {
  x <- list(c(5, 12, 3),
            c(13, 8, 11))
  y <- list(c(5, 12, 3, 2),
            c(13, 8, 11, 4))

  expect_invisible(lsd_list_check(x))
  expect_invisible(lsd_list_check(c(x, list(NULL))))
  expect_snapshot_error(lsd_list_check(c(x, "hello")))
  expect_snapshot_error(lsd_list_check(y))
  expect_snapshot_error(lsd_list_check(c(x, 5)))
})

test_that("tetra list check works", {
  x <- list(c(5, 12, 3),
            c(13, 8, 11))
  y <- list(c(5, 12, 3, 2),
            c(13, 8, 11, 4))

  expect_invisible(tetra_list_check(y))
  expect_invisible(tetra_list_check(c(y, list(NULL))))
  expect_snapshot_error(tetra_list_check(c(x, "hello")))
  expect_snapshot_error(tetra_list_check(x))
})
