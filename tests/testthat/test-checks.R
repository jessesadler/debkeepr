## Test checks ##

# Error messages
bases_error1 <- paste0("`bases` attributes must be equal to combine ",
                       "<deb_lsd>, <deb_tetra>, or <deb_decimal> vectors.")
bases_error2 <- "`bases` must be a numeric vector of length 2."
bases_error3 <- "`bases` cannot be `NA`."
bases_error4 <- "`bases` must be natural numbers greater than zero."
tetra_bases_error <- "`bases` must be a numeric vector of length 3."
lsd_error1 <- paste0("`l`, `s`, and `d` must all have values. ",
                     "You may have forgotten a value or need to use 0.")
tetra_error1 <- paste0("`l`, `s`, `d`, and `f` must all have values. ",
                       "You may have forgotten a value or need to use 0.")
list_error <- "`x` must be a list of numeric vectors of length 3."

# lsd_check ---------------------------------------------------------------

test_that("lsd: non-numeric is an error", {
  expect_error(lsd_check("hello", 3, 4),
               "`l` must be a numeric vector.")
  expect_error(lsd_check(3, "hello", 4),
               "`s` must be a numeric vector.")
  expect_error(lsd_check(3, 4, "hello"),
               "`d` must be a numeric vector.")
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
  expect_error(lsd_check(2, double(), double()), lsd_error1)
  expect_error(lsd_check(2, 3, double()), lsd_error1)
  expect_error(lsd_check(1:10, 11:20, double()), lsd_error1)
})

test_that("lsd: length of l, s, and d are same length, length 1, or length 0", {
  # Successful
  expect_invisible(lsd_check(l = 3, s = 4, d = 1))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = c(1, 3, 2)))
  expect_invisible(lsd_check(l = c(3, 5, 3),
                             s = c(4, 9, 5),
                             d = 0))

  # Errors
  expect_error(lsd_check(l = c(3, 5, 3),
                         s = c(4, 9),
                         d = 0),
               "`l`, `s`, and `d` must be vectors of equal length or length 1")
})

# tetra_check -------------------------------------------------------------

test_that("tetra: non-numeric is an error", {
  expect_error(tetra_check("hello", 3, 4, 0),
               "`l` must be a numeric vector.")
  expect_error(tetra_check(3, "hello", 4, 0),
               "`s` must be a numeric vector.")
  expect_error(tetra_check(3, 4, "hello", 0),
               "`d` must be a numeric vector.")
  expect_error(tetra_check(3, 4, 0, "hello"),
               "`f` must be a numeric vector.")
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
  expect_error(tetra_check(2, double(), double(), double()), tetra_error1)
  expect_error(tetra_check(2, 2, double(), double()), tetra_error1)
  expect_error(tetra_check(2, 2, 2, double()), tetra_error1)
  expect_error(tetra_check(1:10, 11:20, 21:30, double()), tetra_error1)
})

test_that("tetra: length of units are same length, length 1, or length 0", {
  # Successful
  expect_invisible(tetra_check(l = 3, s = 4, d = 1, f = 2))
  expect_invisible(tetra_check(l = c(3, 5, 3),
                               s = c(4, 9, 5),
                               d = c(1, 3, 2),
                               f = c(2, 3, 1)))
  expect_invisible(tetra_check(l = c(3, 5, 3),
                               s = c(4, 9, 5),
                               d = c(8, 4, 2),
                               f = 0))
  expect_error(tetra_check(l = c(3, 5, 3),
                           s = c(4, 9),
                           d = 0,
                           f = c(2, 3, 1)),
               "`l`, `s`, `d`, and `f` must be vectors of equal length or length 1.")
})

# lsd_bases_check ---------------------------------------------------------
test_that("lsd: bases is numeric vector of length 2", {
  # Successful
  expect_invisible(lsd_bases_check(c(20, 12)))
  expect_error(lsd_bases_check(TRUE), bases_error2)
  expect_error(lsd_bases_check(c("hello", "goodbye")), bases_error2)
  expect_error(lsd_bases_check(1), bases_error2)
  expect_error(lsd_bases_check(c(1, 3, 4)), bases_error2)
})

test_that("lsd: bases does not have any missing values", {
  expect_error(lsd_bases_check(c(NA, 3)), bases_error3)
  expect_error(lsd_bases_check(c(3, NA)), bases_error3)
})

test_that("lsd: bases are natural numbers", {
  expect_error(lsd_bases_check(c(-12, -3)), bases_error4)
  expect_error(lsd_bases_check(c(20, 0)), bases_error4)
  expect_error(lsd_bases_check(c(20.5, 8.23)), bases_error4)
})


# tetra_bases_check -------------------------------------------------------
test_that("tetra: bases is numeric vector of length 2", {
  # Successful
  expect_invisible(tetra_bases_check(c(20, 12, 4)))
  expect_error(tetra_bases_check(TRUE), tetra_bases_error)
  expect_error(tetra_bases_check(c("hello", "goodbye")), tetra_bases_error)
  expect_error(tetra_bases_check(1), tetra_bases_error)
  expect_error(tetra_bases_check(c(20, 12)), tetra_bases_error)
})

test_that("tetra: bases does not have any missing values", {
  expect_error(tetra_bases_check(c(NA, 3, 4)), bases_error3)
  expect_error(tetra_bases_check(c(3, NA, 4)), bases_error3)
})

test_that("tetra: bases are natural numbers", {
  expect_error(tetra_bases_check(c(-12, -3, -6)), bases_error4)
  expect_error(tetra_bases_check(c(20, 0, 4)), bases_error4)
  expect_error(tetra_bases_check(c(20.5, 8.23, 8.75)), bases_error4)
})

# dec_bases_check ---------------------------------------------------------

test_that("decimal bases checks", {
  expect_invisible(dec_bases_check(c(20, 12, 4)))
  expect_invisible(dec_bases_check(c(20, 12)))
  expect_error(dec_bases_check(1),
               "`bases` must be a numeric vector of length 2 for lsd or 3 for tetra.")
  expect_error(dec_bases_check(c(3, NA)), bases_error3)
  expect_error(dec_bases_check(c(-12, -3)), bases_error4)
})


# Equivalency -------------------------------------------------------------

test_that("bases assert allows named vector", {
  expect_equal(bases_assert(c(f = 20L, g = 12L)),
               c(s = 20L, d = 12L))
})

test_that("bases tests equivalency", {
  expect_invisible(bases_equal(deb_lsd(2, 3, 4), deb_lsd(3, 2, 1)))
  expect_invisible(bases_equal(deb_lsd(2, 3, 4, bases = c(60, 12)),
                               deb_lsd(3, 2, 1, bases = c(60, 12))))
  expect_invisible(bases_equal(deb_decimal(1.25), deb_decimal(1.25)))
  # Errors
  expect_error(bases_equal(deb_lsd(2, 3, 4),
                           deb_lsd(3, 2, 1, bases = c(60, 12))),
               bases_error1)
  expect_error(bases_equal(deb_lsd(2, 3, 4),
                           deb_lsd(3, 2, 1, bases = c(20, 16))),
               bases_error1)
  expect_error(bases_equal(deb_decimal(1.25),
                           deb_decimal(1.25, bases = c(60, 12))),
               bases_error1)
  expect_error(bases_equal(deb_decimal(1.25),
                           deb_decimal(1.25, bases = c(20, 16))),
               bases_error1)
})

test_that("mixed bases equality", {
  expect_error(mixed_bases_equal(deb_tetra(1, 2, 3, 4),
                                 deb_lsd(1, 2, 3, c(50, 16))),
               paste0("s and d units of the `bases` attributes must be equal to ",
                      "combine <deb_lsd> and <deb_tetra> vectors."))
})


# list checks -------------------------------------------------------------

x <- list(c(5, 12, 3),
          c(13, 8, 11),
          c(7, 16, 0),
          c(1, 2, 3))

y <- list(c(5, 12, 3, 2),
          c(13, 8, 11, 4))

test_that("lsd list check works", {
  expect_invisible(lsd_list_check(x))
  expect_invisible(lsd_list_check(c(x, list(NULL))))
  expect_error(lsd_list_check(c(x, "hello")),
               "`x` must be a list of numeric vectors.")
  expect_error(lsd_list_check(y), list_error)
  expect_error(lsd_list_check(c(x, 5)), list_error)
})

test_that("tetra list check works", {
  expect_invisible(tetra_list_check(y))
  expect_invisible(tetra_list_check(c(y, list(NULL))))
  expect_error(tetra_list_check(c(x, "hello")),
               "`x` must be a list of numeric vectors.")
  expect_error(tetra_list_check(x),
               "`x` must be a list of numeric vectors of length 4.")

})
