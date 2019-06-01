## Test lsd class ##

x <- c(5, 8, 6)
y <- c(12, 5, 3)
z <- c(7, 12, 11)
b1 <- c(20, 12)
b2 <- c(8, 16)
list1 <- list(x, y, z)
list2 <- list(y, z, x)
x_b1 <- to_lsd(x, b1)
y_b1 <- to_lsd(y, b1)
y_b2 <- to_lsd(y, b2)
list1_b1 <- to_lsd(list1, b1)
list2_b2 <- to_lsd(list2, b2)

test_that("constructors work", {
  expect_s3_class(new_lsd(list1, b1), "lsd")
  expect_equal(attributes(new_lsd(list1, b1))$bases, b1)
})

test_that("helper works", {
  expect_s3_class(to_lsd(x, b1), "lsd")
  expect_s3_class(to_lsd(list1, b1), "lsd")
  expect_equal(to_lsd(list1, b1)[[1]], x)
})

test_that("validator works", {
  expect_equal(validate_bases(x, b2), b2)
  expect_equal(validate_bases(list1, b2), b2)

  expect_equal(validate_bases(x_b1), b1)
  expect_equal(validate_bases(list1_b1), b1)
})

test_that("validator2 works", {
  # Neither are lsd class
  expect_equal(validate_bases2(x, y, b2), b2)
  expect_equal(validate_bases2(list1, y, b1), b1)
  expect_equal(validate_bases2(list1, list2, b1), b1)

  # One is lsd class
  expect_equal(validate_bases2(x_b1, x, b2), b1)
  expect_equal(validate_bases2(list2_b2, x, b1), b2)
  expect_equal(validate_bases2(x, y_b2, b1), b2)
  expect_equal(validate_bases2(list2_b2, x, b1), b2)

  # Both are lsd class
  expect_equal(validate_bases2(x_b1, y_b1, b2), b1)
  expect_error(validate_bases2(x_b1, y_b2, b2),
               "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")
  expect_equal(validate_bases2(list1_b1, x_b1, b2), b1)
  expect_error(validate_bases2(x_b1, y_b2, b2),
               "bases for lsd1 and lsd2 must be equivalent if both are of class lsd")
})

test_that("validate p works", {
  expect_equal(validate_bases_p(list(x, y, z), b1), b1)
  expect_equal(validate_bases_p(list(x, y, x_b1, y_b1), b2), b1)
  expect_equal(validate_bases_p(list(x, list2_b2, y_b2), b1), b2)

  expect_error(validate_bases_p(list(x_b1, y_b2, list1_b1, x), b2),
               "All objects of class lsd must have the same bases")
})

test_that("construction works", {
  expect_equal(deb_lsd(5, 8, 6, b1), x_b1)
  expect_equal(deb_lsd(c(12, 7, 5), c(5, 12, 8), c(3, 11, 6), b2), list2_b2)

  expect_error(deb_lsd("g", 4, 5), "l, s, and d must be numeric")
  expect_error(deb_lsd(c(3, 2), 4, 5), "l, s, and d must be vectors of equal length")
})

test_that("coercion works", {
  expect_equal(deb_as_lsd(x_b1), x_b1)
  expect_equal(deb_as_lsd(list2_b2), list2_b2)

  expect_equal(deb_as_lsd(x), x_b1)
  expect_equal(deb_as_lsd(list2, b2), list2_b2)

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
  expect_equal(deb_is_lsd(list1_b1), TRUE)
})

test_that("subset lsd_list objects works", {
  expect_s3_class(list1_b1[1], "lsd")
  expect_true(is.numeric(list1_b1[[1]]))
})

test_that("combine lsd_lst objects works", {
  expect_s3_class(c(list1_b1, x_b1), "lsd")
  # If non-lsd_list object is first, will not use lsd_list method
  expect_equal(class(c(list1, list2_b2)), "list")

  expect_length(c(list1_b1, y_b1, x), 5)
  expect_equal(deb_bases(c(list2_b2, y, x)),
               c(s = 8, d = 16))
  expect_error(c(list1_b1, list2_b2),
               "All objects of class lsd must have the same bases")
})

test_that("find bases works", {
  expect_error(deb_bases(x), "Objects must be of class lsd")
  expect_equal(deb_bases(x_b1), c(s = 20, d = 12))
  expect_equal(deb_bases(list2_b2), c(s = 8, d = 16))
  expect_equal(deb_bases(x_b1, list2_b2),
               list(c(s = 20, d = 12), c(s = 8, d = 16)))
})

# Able to print with NA or NULL
with_null <- list1_b1
with_null[2] <- list(NULL)
with_na <- list1_b1
with_na[2] <- NA


test_that("print output works", {
  expect_that(print(x_b1), prints_text())
  expect_that(print(list1_b1), prints_text())
  expect_that(print(with_null), prints_text())
  expect_that(print(with_na), prints_text())
})
