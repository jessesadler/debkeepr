context("test-lsd-class.R")

x <- c(5, 8, 6)
y <- c(12, 5, 3)
z <- c(7, 12, 11)
b1 <- c(20, 12)
b2 <- c(8, 16)
list1 <- list(x, y, z)
list2 <- list(y, z, x)
x_b1 <- new_lsd(x, b1)
y_b1 <- new_lsd(y, b1)
z_b2 <- new_lsd(z, b2)
lsd_list1 <- to_lsd(list1, b2)
lsd_list2 <- list(x_b1, y_b1)
lsd_list3 <- list(x_b1, y_b1, z)
lsd_list4 <- list(x_b1, y_b1, z_b2)

test_that("constructors work", {
  expect_s3_class(new_lsd(x, b1), "lsd")
  expect_equal(attributes(new_lsd(x, b1))$names, c("l", "s", "d"))
  expect_equal(attributes(new_lsd(x, b1))$bases, b1)

  expect_s3_class(new_lsd_list(list1, b1), "lsd_list")
  expect_equal(attributes(new_lsd_list(list1, b1))$bases, b1)
})

test_that("helper works", {
  expect_s3_class(to_lsd(x, b1), "lsd")
  expect_s3_class(to_lsd(list1, b1), "lsd_list")
  expect_s3_class(to_lsd(list1, b1)[[1]], "lsd")
})

test_that("validator works", {
  expect_equal(validate_bases(x, b2), b2)
  expect_equal(validate_bases(x_b1), b1)

  expect_equal(validate_bases(lsd_list1), b2)
  expect_equal(validate_bases(lsd_list2), b1)
  expect_equal(validate_bases(lsd_list3), b1)
  expect_error(validate_bases(lsd_list4),
               "All lsd vectors in a list must have the same bases")
})

test_that("validator2 works", {
  # Neither are lsd class
  expect_equal(validate_bases2(x, y, b2), b2)
  expect_equal(validate_bases2(list1, y, b1), b1)
  expect_equal(validate_bases2(list1, list2, b1), b1)

  # One is lsd class
  expect_equal(validate_bases2(x_b1, x, b2), b1)
  expect_equal(validate_bases2(lsd_list1, x, b1), b2)
  expect_equal(validate_bases2(x, z_b2, b1), b2)
  expect_equal(validate_bases2(lsd_list1, x, b1), b2)

  # list with lsd vectors
  expect_equal(validate_bases2(lsd_list2, x, b2), b1)
  expect_equal(validate_bases2(x, lsd_list2, b2), b1)
  expect_equal(validate_bases2(list(z_b2, x), list1, b1), b2)
  expect_error(validate_bases2(lsd_list4, x, b1),
               "All lsd vectors in a list must have the same bases")

  # Both are lsd class
  expect_equal(validate_bases2(x_b1, y_b1, b2), b1)
  expect_error(validate_bases2(x_b1, z_b2, b2),
               "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")
  expect_equal(validate_bases2(lsd_list1, z_b2, b1), b2)
  expect_error(validate_bases2(x_b1, z_b2, b2),
               "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")
})

test_that("validate p works", {
  expect_equal(validate_bases_p(list(x, y, z), b1), b1)
  expect_equal(validate_bases_p(list(x, y, x_b1, y_b1), b2), b1)
  expect_equal(validate_bases_p(list(x, lsd_list2), b2), b1)
  expect_equal(validate_bases_p(list(x, lsd_list1), b1), b2)
  expect_equal(validate_bases_p(list(x, y_b1, lsd_list2), b2), b1)

  # errors
  expect_error(validate_bases_p(list(x, lsd_list4), b2),
               "All lsd vectors in a list must have the same bases")
  expect_error(validate_bases_p(list(x_b1, lsd_list4), b2),
               "All lsd vectors in a list must have the same bases")
  expect_error(validate_bases_p(list(x_b1, y_b1, lsd_list1, x), b2),
               "All objects of class lsd must have the same bases")
})

test_that("coercion works", {
  expect_equal(deb_as_lsd(x_b1), x_b1)
  expect_equal(deb_as_lsd(lsd_list1), lsd_list1)
  expect_equal(deb_as_lsd(x), x_b1)
  expect_equal(deb_as_lsd(z, b2), z_b2)
  expect_equal(deb_as_lsd(list1, b2), lsd_list1)
  expect_error(deb_as_lsd(c("hello", "goodbye")),
               "Cannot coerce an object of class character into an lsd object.")
  expect_error(deb_as_lsd(x, bases = c("hello", "goodbye")),
               "bases must be a numeric vector")
  expect_error(deb_as_lsd(list1, bases = c("hello", "goodbye")),
               "bases must be a numeric vector")
})

test_that("testor works", {
  expect_equal(deb_is_lsd(x), FALSE)
  expect_equal(deb_is_lsd(x_b1), TRUE)
  expect_equal(deb_is_lsd(list1), FALSE)
  expect_equal(deb_is_lsd(lsd_list2), FALSE)
  expect_equal(deb_is_lsd(lsd_list1), TRUE)
})

test_that("subset lsd_list objects works", {
  expect_s3_class(lsd_list1[1], "lsd_list")
  expect_s3_class(lsd_list1[[1]], "lsd")
})

test_that("combine lsd_lst objects works", {
  expect_s3_class(c(lsd_list1, z_b2), "lsd_list")
  # If non-lsd_list object is first, will not use lsd_list method
  expect_equal(class(c(z_b2, lsd_list1)), "list")

  expect_length(c(lsd_list1, z_b2, x), 5)
  expect_equal(deb_bases(c(lsd_list1, z_b2, x)),
               c(s = 8, d = 16))
  expect_error(c(lsd_list1, lsd_list2),
               "All objects of class lsd must have the same bases")
})

test_that("find bases works", {
  expect_error(deb_bases(x), "lsd must be of class lsd or lsd_list")
  expect_equal(deb_bases(x_b1), c(s = 20, d = 12))
  expect_equal(deb_bases(lsd_list1), c(s = 8, d = 16))
})

test_that("print output works", {
  expect_that(print(x_b1), prints_text())
  expect_that(print(lsd_list1), prints_text())
})
