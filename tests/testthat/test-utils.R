# Test utility functions: These are also widely tested elsewhere

test_that("all_present works", {
  x <- tibble::tibble(id = rep(1:5, 5))
  y <- tibble::tibble(id = rep(3:7, 4))

  expect_false(all_present(x, y))

  x <- tibble::tibble(id = rep(1:5, 5))
  y <- tibble::tibble(id = rep(1:5, 4))

  expect_true(all_present(x, y))

  x <- tibble::tibble(id = rep(letters[1:9], 5))
  y <- tibble::tibble(id = rep(letters[10:20], 4))

  expect_false(all_present(x, y))
})

test_that("create_debtype works", {

  # Basic functionality
  expect_equal(create_debtype(deb_lsd()), vec_init(deb_lsd()))
  expect_equal(create_debtype(deb_decimal()), vec_init(deb_decimal()))
  expect_equal(create_debtype(deb_tetra()), vec_init(deb_tetra()))

  # Different value
  expect_equal(create_debtype(deb_lsd(), val = 0), deb_lsd(0, 0, 0))
  expect_equal(create_debtype(deb_decimal(), val = 0), deb_decimal(0))
  expect_equal(create_debtype(deb_tetra(), val = 0), deb_tetra(0, 0, 0, 0))

  # Different size
  expect_equal(create_debtype(deb_lsd(), n = 3), vec_init(deb_lsd(), 3))
  expect_equal(create_debtype(deb_decimal(), n = 3),vec_init(deb_decimal(), 3))
  expect_equal(create_debtype(deb_tetra(), n = 3), vec_init(deb_tetra(), 3))

  # Different bases
  expect_equal(create_debtype(deb_lsd(bases = c(50, 16)), val = 1.5),
               deb_lsd(1, 25, 0, bases = c(50, 16)))
  expect_equal(create_debtype(deb_decimal(bases = c(50, 16)), val = 1.5),
               deb_decimal(1.5, bases = c(50, 16)))
  expect_equal(create_debtype(deb_decimal(bases = c(20, 12, 4)), val = 1.5),
               deb_decimal(1.5, bases = c(20, 12, 4)))
  expect_equal(create_debtype(deb_tetra(bases = c(50, 16, 8)), val = 1.5),
               deb_tetra(1, 25, 0, 0, bases = c(50, 16, 8)))

  # Length of input does not matter
  expect_length(create_debtype(deb_as_lsd(1:10)), 1)
  expect_length(create_debtype(deb_decimal(1:10)), 1)
  expect_length(create_debtype(deb_as_tetra(1:10)), 1)
})

test_that("deb_summarise works", {
  # This is also thoroughly tested in test-transactions.R
  # Data
  l <- c(10, 10, 7, 9, 15, 12, NA)
  s <- c(15, 15, 11, 2, 0, 10, NA)
  d <- c(6, 6, 8, 11, 9, 4.5, NA)

  tbl <- tibble::tibble(
    credit = c("a", "b", "a", "c", "d", "c", "a"),
    debit = c("b", "a", "c", "a", "a", "e", "b"),
    from = credit,
    to = debit,
    lsd = deb_lsd(l, s, d),
    tetra = deb_as_tetra(lsd, f = 4)
  )

  # Function works
  expect_equal(dim(deb_summarise(tbl, credit, lsd, na.rm = TRUE)), c(4, 2))
  expect_equal(dim(deb_summarise(tbl, debit, lsd, na.rm = TRUE)), c(4, 2))
  # Correct column names
  expect_equal(names(deb_summarise(tbl, credit, lsd, na.rm = TRUE)),
               c("account_id", "lsd"))
  expect_equal(names(deb_summarise(tbl, credit, tetra, na.rm = TRUE)),
               c("account_id", "tetra"))
  # Only contains accounts for that relation
  expect_equal(deb_summarise(tbl, credit, lsd, na.rm = TRUE)[["account_id"]],
               c("a", "b", "c", "d"))
  expect_equal(deb_summarise(tbl, debit, lsd, na.rm = TRUE)[["account_id"]],
               c("a", "b", "c", "e"))
  # With different relation column names
  expect_equal(deb_summarise(tbl, relation = credit, lsd, na.rm = TRUE),
               deb_summarise(tbl, relation = from, lsd, na.rm = TRUE))
  expect_equal(deb_summarise(tbl, relation = debit, lsd, na.rm = TRUE),
               deb_summarise(tbl, relation = to, lsd, na.rm = TRUE))
  # NA
  expect_true(is.na(deb_summarise(tbl, credit, lsd, na.rm = FALSE)[[1, 2]]))
  expect_true(is.na(deb_summarise(tbl, debit, lsd, na.rm = FALSE)[[2, 2]]))
})

test_that("deb_fill_missing works", {
  # This is also thoroughly tested in test-transactions.R
  # Data is produced by deb_summarise() above
  # Data
  credit <- tibble::tibble(
    account_id = c("a", "b", "c", "d"),
    credit = deb_lsd(c(NA, 10, 21, 15), c(NA, 15, 13, 0), c(NA, 6, 3.5, 9))
  )
  debit <- tibble::tibble(
    account_id = c("a", "b", "c", "e"),
    debit = deb_lsd(c(34, NA, 7, 12), c(19, NA, 11, 10), c(2, NA, 8, 4.5))
  )

  # Function works
  expect_equal(dim(deb_fill_missing(credit, debit)), c(5, 2))
  expect_equal(dim(deb_fill_missing(debit, credit)), c(5, 2))
  # Gets column names right
  expect_equal(names(deb_fill_missing(credit, debit)),
               c("account_id", "credit"))
  expect_equal(names(deb_fill_missing(debit, credit)),
               c("account_id", "debit"))
  # Adds correct missing account
  expect_equal(deb_fill_missing(credit, debit)[[5, 1]], "e")
  expect_equal(deb_fill_missing(debit, credit)[[5, 1]], "d")
  # Adds correct value of 0
  expect_equal(deb_fill_missing(credit, debit)[[5, 2]], deb_lsd(0, 0, 0))
  expect_equal(deb_fill_missing(debit, credit)[[5, 2]], deb_lsd(0, 0, 0))
})
