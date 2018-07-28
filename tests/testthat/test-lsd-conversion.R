context("test-lsd-conversion.R")

test_that("ratio check works", {
  expect_error(deb_convert_bases(lsd = c(204, 3, 3),
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(20, 16),
                                 ratio = "hello"),
               "ratio must be a numeric vector")
  expect_error(deb_convert_bases(lsd = c(204, 3, 3),
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(20, 16),
                                 ratio = c(1, 2)),
               "ratio must be a numeric vector of length 1")
})

test_that("base conversion works", {
  expect_equal(deb_convert_bases(lsd = c(5, 6, 8),
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(20, 24)),
               c(l = 5, s = 6, d = 16))
  expect_equal(deb_convert_bases(lsd = c(5, 6, 8),
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(40, 12)),
               c(l = 5, s = 13, d = 4))
  expect_equal(deb_convert_bases(lsd = c(24, 10, 8),
                                 lsd_bases1 = c(20, 16),
                                 lsd_bases2 = c(20, 12),
                                 ratio = 1 / 6),
               c(l = 4, s = 1, d = 9))
  expect_equal(deb_convert_bases(lsd = c(4, 1, 9),
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(20, 16),
                                 ratio = 6),
               c(l = 24, s = 10, d = 8))
})

lsd_list <- list(c(5, 6, 8), c(24, 10, 8), c(20, 13, 4))
gulden_list <- list(c(l = 1224, s = 19, d = 8),
                      c(l = 101, s = 5, d = 13),
                      c(l = 225, s = 13, d = 15))
flemish_list <- list(c(l = 204, s = 3, d = 3),
                     c(l = 16, s = 17, d = 7.625),
                     c(l = 37, s = 12, d = 3.875))

test_that("base conversion is vectorized", {
  expect_equal(deb_convert_bases(lsd = lsd_list,
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(40, 24)),
               list(c(l = 5, s = 13, d = 8),
                    c(l = 24, s = 21, d = 8),
                    c(l = 20, s = 26, d = 16)))
  expect_equal(deb_convert_bases(lsd = gulden_list,
                                 lsd_bases1 = c(20, 16),
                                 lsd_bases2 = c(20, 12),
                                 ratio = 1 / 6),
               flemish_list)
  expect_equal(deb_convert_bases(lsd = flemish_list,
                                 lsd_bases1 = c(20, 12),
                                 lsd_bases2 = c(20, 16),
                                 ratio = 6),
               gulden_list)
})

lsd_df <- data.frame(l = c(5, 24, 20), s = c(6, 10, 13), d = c(8, 8, 4))
lsd_df2 <- data.frame(pounds = c(5, 24, 20), shillings = c(6, 10, 13), pence = c(8, 8, 4))
lsd_answer <- data.frame(l.1 = c(5, 24, 20), s.1 = c(13, 21, 26), d.1 = c(8, 8, 16))
lsd_answer2 <- data.frame(pounds.1 = c(5, 24, 20), shillings.1 = c(13, 21, 26), pence.1 = c(8, 8, 16))

gulden_df <- data.frame(l = c(1224, 101, 225), s = c(19, 5, 13), d = c(8, 13, 15))
flemish_answer <- data.frame(l.1 = c(204, 16, 37), s.1 = c(3, 17, 12), d.1 = c(3, 7.625, 3.875))

flemish_df <- data.frame(l = c(204, 16, 37), s = c(3, 17, 12), d = c(3, 7.625, 3.875))
gulden_answer <- data.frame(l.1 = c(1224, 101, 225), s.1 = c(19, 5, 13), d.1 = c(8, 13, 15))

test_that("base conversion mutate works", {
  expect_equal(deb_convert_bases_mutate(lsd_df, lsd_bases1 = c(20, 12), lsd_bases2 = c(40, 24))[ , 4:6],
               lsd_answer)
  expect_equal(deb_convert_bases_mutate(lsd_df2, l = pounds, s = shillings, d = pence,
                                 lsd_bases1 = c(20, 12), lsd_bases2 = c(40, 24))[ , 4:6],
               lsd_answer2)
  expect_equal(deb_convert_bases_mutate(gulden_df,
                                        lsd_bases1 = c(20, 16), lsd_bases2 = c(20, 12),
                                        ratio = 1 / 6)[ , 4:6],
               flemish_answer)
  expect_equal(deb_convert_bases_mutate(flemish_df,
                                        lsd_bases1 = c(20, 12), lsd_bases2 = c(20, 16),
                                        ratio = 6)[ , 4:6],
               gulden_answer)
})
