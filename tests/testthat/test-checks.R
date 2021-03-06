## Test checks ##

# Error messages
bases_error1 <- paste0("`bases` attributes must be equal to combine ",
                      "<deb_lsd> or <deb_decimal> vectors.")
bases_error2 <- "`bases` must be a numeric vector of length 2."
bases_error3 <- "`bases` cannot be `NA`."
bases_error4 <- "`bases` must be natural numbers greater than zero."
lsd_error1 <- paste0("`l`, `s`, and `d` must all have values. ",
                     "You may have forgotten a value or need to use 0.")
list_error <- "`x` must be a list of numeric vectors of length 3."

# lsd_check ---------------------------------------------------------------

test_that("non-numeric is an error", {
  expect_error(lsd_check("hello", 3, 4),
               "`l` must be a numeric vector.")
  expect_error(lsd_check(3, "hello", 4),
               "`s` must be a numeric vector.")
  expect_error(lsd_check(3, 4, "hello"),
               "`d` must be a numeric vector.")
})

test_that("NA scalar is not an error", {
  expect_invisible(lsd_check(NA, 3, 4))
  expect_invisible(lsd_check(3, NA, 4))
  expect_invisible(lsd_check(3, 4, NA))
})

test_that("Multiple NA is not an error", {
  expect_invisible(lsd_check(c(NA, NA), 1:2, 3:4))
  expect_invisible(lsd_check(1:2, c(NA, NA), 3:4))
  expect_invisible(lsd_check(1:2, 3:4, c(NA, NA)))
})

test_that("length of l, s, and d all have values or are all length 0", {
  expect_invisible(lsd_check(double(), double(), double()))
  expect_error(lsd_check(2, double(), double()), lsd_error1)
  expect_error(lsd_check(2, 3, double()), lsd_error1)
})

test_that("length of l, s, and d are same length, length 1, or length 0", {
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


# bases_check -------------------------------------------------------------
test_that("bases is numeric vector of length 2", {
  # Successful
  expect_invisible(bases_check(c(20, 12)))
  expect_error(bases_check(TRUE), bases_error2)
  expect_error(bases_check(c("hello", "goodbye")), bases_error2)
  expect_error(bases_check(1), bases_error2)
  expect_error(bases_check(c(1, 3, 4)), bases_error2)
})

test_that("bases does not have any missing values", {
  expect_error(bases_check(c(NA, 3)), bases_error3)
  expect_error(bases_check(c(3, NA)), bases_error3)
})

test_that("bases are natural numbers", {
  expect_error(bases_check(c(-12, -3)), bases_error4)
  expect_error(bases_check(c(20, 0)), bases_error4)
  expect_error(bases_check(c(20.5, 8.23)), bases_error4)
})


# Equivalency -------------------------------------------------------------

test_that("bases assert allows named vector", {
  expect_equal(bases_assert(c(s = 20L, d = 12L)),
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


# list checks -------------------------------------------------------------

x <- list(c(5, 12, 3),
          c(13, 8, 11),
          c(7, 16, 0),
          c(1, 2, 3))

test_that("list check works", {
  expect_invisible(list_check(x))
  expect_invisible(list_check(c(x, list(NULL))))
  expect_error(list_check(c(x, "hello")),
               "`x` must be a list of numeric vectors.")
  expect_error(list_check(c(x, list(c(5, 6, 7, 7)))), list_error)
  expect_error(list_check(c(x, 5)), list_error)
})
