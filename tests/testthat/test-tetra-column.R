## Test deb_lsd column helper functions ##

test_that("deb_gather_tetra works", {
  # Data
  libra <- c(3, 5, 6, 2)
  solidus <- c(10, 18, 11, 16)
  denarius <- c(9, 11, 10, 5)
  farthing <- c(2, 3, 1, 0)
  accounts <- 1:4

  x <- data.frame(accounts = accounts,
                  l = libra,
                  s = solidus,
                  d = denarius,
                  f = farthing)
  y <- data.frame(accounts = accounts,
                  tetra = deb_tetra(libra, solidus, denarius, farthing))

  x2 <- data.frame(accounts = accounts,
                   libra = libra,
                   solidus = solidus,
                   denarius = denarius,
                   farthing = farthing)
  y2 <- data.frame(accounts = accounts,
                   data = deb_tetra(libra, solidus, denarius, farthing))

  # defaults
  expect_equal(ncol(deb_gather_tetra(x)), 6)
  expect_equal(deb_gather_tetra(x)[[6]], y[[2]])
  expect_identical(deb_gather_tetra(x, replace = TRUE), y)
  # With tibble
  expect_identical(deb_gather_tetra(tibble::as_tibble(x), replace = TRUE),
                   tibble::as_tibble(y))

  # non-default lsd column name
  expect_identical(deb_gather_tetra(x, tetra_col = data, replace = TRUE), y2)
  # non-default l, s, d, and f names
  expect_identical(deb_gather_tetra(x2, libra, solidus, denarius, farthing,
                                  replace = TRUE), y)
  # non-default bases
  res <- deb_gather_tetra(x, bases = c(60, 16, 8))[[6]]
  expect_equal(deb_bases(res), c(s = 60, d = 16, f = 8))
})

test_that("deb_spread_tetra works", {
  # Data
  libra <- c(3, 5, 6, 2)
  solidus <- c(10, 18, 11, 16)
  denarius <- c(9, 11, 10, 5)
  farthing <- c(2, 3, 1, 0)
  accounts <- 1:4

  x <- data.frame(accounts = accounts,
                  l = libra,
                  s = solidus,
                  d = denarius,
                  f = farthing)
  y <- data.frame(accounts = accounts,
                  tetra = deb_tetra(libra, solidus, denarius, farthing))

  x2 <- data.frame(accounts = accounts,
                   libra = libra,
                   solidus = solidus,
                   denarius = denarius,
                   farthing = farthing)
  y2 <- data.frame(accounts = accounts,
                   data = deb_tetra(libra, solidus, denarius, farthing))

  # defaults
  expect_equal(ncol(deb_spread_tetra(y)), 6)
  expect_equal(deb_spread_tetra(y)[[3]], libra)
  expect_equal(deb_spread_tetra(y)[[4]], solidus)
  expect_equal(deb_spread_tetra(y)[[5]], denarius)
  expect_equal(deb_spread_tetra(y)[[6]], farthing)
  expect_identical(deb_spread_tetra(y, replace = TRUE), x)
  # non-default tetra column name
  expect_identical(deb_spread_tetra(y, l_col = libra, s_col = solidus,
                                    d_col = denarius, f_col = farthing,
                                    replace = TRUE), x2)
  # non-default l, s, d, and f names
  expect_identical(deb_spread_tetra(y2, tetra = data,
                                    replace = TRUE), x)
  # Error
  expect_snapshot(deb_spread_tetra(x, tetra = l), error = TRUE)
})
