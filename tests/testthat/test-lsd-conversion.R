context("test-lsd-conversion.R")

x <- c(10, 3, 2)
y <- c(20, 5, 8)
b1 <- c(20, 12)
b2 <- c(8, 16)
b3 <- c(20, 16)
x_b2 <- to_lsd(x, b2)
y_b2 <- to_lsd(y, b2)

list1 <- list(x, y)
list1_b1 <- to_lsd(list1, b1)
list1_b2 <- to_lsd(list1, b2)

test_that("ratio check works", {
  expect_error(deb_convert_bases(lsd = c(204, 3, 3),
                                 bases1 = b1,
                                 bases2 = b3,
                                 ratio = "hello"),
               "ratio must be a numeric vector")
  expect_error(deb_convert_bases(lsd = c(204, 3, 3),
                                 bases1 = b1,
                                 bases2 = b3,
                                 ratio = c(1, 2)),
               "ratio must be a numeric vector of length 1")
})

test_that("base conversion takes base attribute", {
  expect_identical(deb_convert_bases(x_b2, bases2 = b1),
                   deb_convert_bases(x, bases1 = b2, bases2 = b1))
  expect_identical(deb_convert_bases(list1_b1, bases2 = b2),
                   deb_convert_bases(list1, bases1 = b1, bases2 = b2))
  expect_identical(deb_convert_bases(list1_b2, bases2 = b1, round = 0),
                   deb_convert_bases(list1, bases1 = b2, bases2 = b1, round = 0))
  expect_error(deb_convert_bases(lsd = c(5, 6, 8),
                                 bases2 = c(20, 24)),
               "lsd must have a bases attribute or a value must be provided for bases1")
})

test_that("base conversion works", {
  expect_equal(deb_convert_bases(lsd = c(5, 6, 8),
                                 bases1 = b1,
                                 bases2 = c(20, 24)),
               to_lsd(c(5, 6, 16), c(20, 24)))
  expect_equal(deb_convert_bases(lsd = c(5, 6, 8),
                                 bases1 = b1,
                                 bases2 = c(40, 12)),
               to_lsd(c(5, 13, 4), c(40, 12)))
  expect_equal(deb_convert_bases(lsd = c(24, 10, 8),
                                 bases1 = b3,
                                 bases2 = b1,
                                 ratio = 1 / 6),
               to_lsd(c(4, 1, 9), b1))
  expect_equal(deb_convert_bases(lsd = c(4, 1, 9),
                                 bases1 = b1,
                                 bases2 = b3,
                                 ratio = 6),
               to_lsd(c(24, 10, 8), b3))
  expect_equal(deb_convert_bases(lsd = c(4, 1, 9.333),
                                 bases1 = b1,
                                 bases2 = b3,
                                 ratio = 6,
                                 round = 0),
               to_lsd(c(24, 10, 11), b3))
})

lsd_list <- list(c(5, 6, 8), c(24, 10, 8), c(20, 13, 4))
gulden_list <- to_lsd(list(c(1224, 19, 8),
                      c(101, 5, 13),
                      c(225, 13, 15)), b3)
flemish_list <- to_lsd(list(c(204, 3, 3),
                     c(16, 17, 7.625),
                     c(37, 12, 3.875)), b1)
round_list <- to_lsd(list(c(204, 3, 3),
                   c(16, 17, 8),
                   c(37, 12, 4)), b1)

test_that("base conversion is vectorized", {
  expect_equal(deb_convert_bases(lsd = lsd_list,
                                 bases1 = b1,
                                 bases2 = c(40, 24)),
               to_lsd(list(c(5, 13, 8),
                    c(24, 21, 8),
                    c(20, 26, 16)), c(40, 24)))
  expect_equal(deb_convert_bases(lsd = gulden_list,
                                 bases2 = b1,
                                 ratio = 1 / 6),
               flemish_list)
  expect_equal(deb_convert_bases(lsd = gulden_list,
                                 bases1 = b3,
                                 bases2 = b1,
                                 ratio = 1 / 6,
                                 round = 0),
               round_list)
  expect_equal(deb_convert_bases(lsd = flemish_list,
                                 bases1 = b1,
                                 bases2 = b3,
                                 ratio = 6),
               gulden_list)
})

lsd_df <- data.frame(l = c(5, 24, 20), s = c(6, 10, 13), d = c(8, 8, 4))
lsd_df2 <- data.frame(pounds = c(5, 24, 20), shillings = c(6, 10, 13), pence = c(8, 8, 4))
lsd_answer <- data.frame(l.1 = c(5, 24, 20), s.1 = c(13, 21, 26), d.1 = c(8, 8, 16))
lsd_answer2 <- data.frame(pounds.1 = c(5, 24, 20), shillings.1 = c(13, 21, 26), pence.1 = c(8, 8, 16))

gulden_df <- data.frame(l = c(1224, 101, 225), s = c(19, 5, 13), d = c(8, 13, 15))
flemish_answer <- data.frame(l.1 = c(204, 16, 37), s.1 = c(3, 17, 12), d.1 = c(3, 7.625, 3.875))
round_answer <- data.frame(l.1 = c(204, 16, 37), s.1 = c(3, 17, 12), d.1 = c(3, 8, 4))

flemish_df <- data.frame(l = c(204, 16, 37), s = c(3, 17, 12), d = c(3, 7.625, 3.875))
gulden_answer <- data.frame(l.1 = c(1224, 101, 225), s.1 = c(19, 5, 13), d.1 = c(8, 13, 15))

test_that("base conversion mutate works", {
  expect_equal(deb_convert_bases_mutate(lsd_df, bases1 = b1, bases2 = c(40, 24))[ , 4:6],
               lsd_answer)
  expect_equal(deb_convert_bases_mutate(lsd_df2, l = pounds, s = shillings, d = pence,
                                 bases1 = b1, bases2 = c(40, 24))[ , 4:6],
               lsd_answer2)
  expect_equal(deb_convert_bases_mutate(gulden_df,
                                        bases1 = b3, bases2 = b1,
                                        ratio = 1 / 6)[ , 4:6],
               flemish_answer)
  expect_equal(deb_convert_bases_mutate(gulden_df,
                                        bases1 = b3, bases2 = b1,
                                        ratio = 1 / 6,
                                        round = 0)[ , 4:6],
               round_answer)
  expect_equal(deb_convert_bases_mutate(flemish_df,
                                        bases1 = b1, bases2 = b3,
                                        ratio = 6)[ , 4:6],
               gulden_answer)
})
