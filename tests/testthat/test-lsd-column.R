## Test deb_lsd column helper functions ##

test_that("deb_gather_lsd works", {
  # Data
  libra <- c(3, 5, 6, 2)
  solidus <- c(10, 18, 11, 16)
  denarius <- c(9, 11, 10, 5)
  accounts <- 1:4

  x <- data.frame(accounts = accounts,
                  l = libra,
                  s = solidus,
                  d = denarius)
  y <- data.frame(accounts = accounts,
                  lsd = deb_lsd(libra, solidus, denarius))

  x2 <- data.frame(accounts = accounts,
                   libra = libra,
                   solidus = solidus,
                   denarius = denarius)
  y2 <- data.frame(accounts = accounts,
                   data = deb_lsd(libra, solidus, denarius))

  # defaults
  expect_equal(ncol(deb_gather_lsd(x)), 5)
  expect_equal(deb_gather_lsd(x)[[5]], y[[2]])
  expect_identical(deb_gather_lsd(x, replace = TRUE), y)
  # With tibble
  expect_identical(deb_gather_lsd(tibble::as_tibble(x), replace = TRUE),
                   tibble::as_tibble(y))

  # non-default lsd column name
  expect_identical(deb_gather_lsd(x, lsd_col = data, replace = TRUE), y2)
  # non-default l, s, and d names
  expect_identical(deb_gather_lsd(x2, libra, solidus, denarius,
                                  replace = TRUE), y)
  # non-default bases
  res <- deb_gather_lsd(x, bases = c(8, 16))[[5]]
  expect_equal(deb_bases(res), c(s = 8, d = 16))
})

test_that("deb_spread_lsd works", {
  # Data
  libra <- c(3, 5, 6, 2)
  solidus <- c(10, 18, 11, 16)
  denarius <- c(9, 11, 10, 5)
  accounts <- 1:4

  x <- data.frame(accounts = accounts,
                  l = libra,
                  s = solidus,
                  d = denarius)
  y <- data.frame(accounts = accounts,
                  lsd = deb_lsd(libra, solidus, denarius))

  x2 <- data.frame(accounts = accounts,
                   libra = libra,
                   solidus = solidus,
                   denarius = denarius)
  y2 <- data.frame(accounts = accounts,
                   data = deb_lsd(libra, solidus, denarius))

  # defaults
  expect_equal(ncol(deb_spread_lsd(y)), 5)
  expect_equal(deb_spread_lsd(y)[[3]], libra)
  expect_equal(deb_spread_lsd(y)[[4]], solidus)
  expect_equal(deb_spread_lsd(y)[[5]], denarius)
  expect_identical(deb_spread_lsd(y, replace = TRUE), x)
  # non-default lsd column name
  expect_identical(deb_spread_lsd(y, l_col = libra,
                                  s_col = solidus, d_col = denarius,
                                  replace = TRUE), x2)
  # non-default l, s, and d names
  expect_identical(deb_spread_lsd(y2, lsd = data,
                                  replace = TRUE), x)
  # Error
  expect_snapshot(deb_spread_lsd(x, lsd = l), error = TRUE)
})
