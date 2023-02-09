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
