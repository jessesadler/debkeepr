# Test deb_text #

lsd <- deb_lsd(c(10000, NA, 0, -10000),
               c(8, NA, 0, -8),
               c(5.8252, NA, 0, -5.8252))
lsd2 <- deb_lsd(c(10000.8252, 0),
                c(10000.8252, 0),
                c(10000.8252, 0))
dec <- deb_decimal(c(10000.8252, NA, 0, -10000.8252))
dec2 <- deb_decimal(c(8.123456789, 0))
tetra <- deb_tetra(c(10000, NA, 0, -10000),
                   c(8, NA, 0, -8),
                   c(5, NA, 0, -5),
                   c(2.8252, NA, 0, -2.8252))

test_that("deb_text works", {
  expect_error(deb_text(c(2, 3)),
    "`x` must be a <deb_lsd>, <deb_decimal>, or <deb_tetra> vector.")
  expect_type(deb_text(lsd), "character")
  expect_length(deb_text(lsd), 4)
  expect_type(deb_text(dec), "character")
  expect_length(deb_text(dec), 4)
})

test_that("deb_text properly formats deb_lsd", {
  expect_equal(deb_text(lsd), c("£10,000 8s. 6d.", NA,
                                "£0 0s. 0d.", "-£10,000 8s. 6d."))
  expect_equal(deb_text(lsd, sep = ".", s.mark = "", d.mark = ""),
               c("£10,000.8.6", NA, "£0.0.0", "-£10,000.8.6"))
  expect_equal(deb_text(lsd, currency = "$", s.mark = "", d.mark = "",
                        sep = ":", digits = 3, big.mark = ".",
                        decimal.mark = ",", suffix = " Flemish"),
               c("$10.000:8:5,825 Flemish", NA, "$0:0:0 Flemish",
                 "-$10.000:8:5,825 Flemish"))
  expect_equal(deb_text(lsd2),
               c("£10,001 10,001s. 10,001d.", "£0 0s. 0d."))
  expect_equal(deb_text(lsd2, digits = 3),
               c("£10,000.825 10,000.825s. 10,000.825d.", "£0 0s. 0d."))
})

test_that("deb_text properly formats deb_decimal", {
  expect_equal(deb_text(dec), c("£10,001", NA, "£0", "-£10,001"))
  expect_equal(deb_text(dec, currency = "/", digits = 3, big.mark = ".",
                        decimal.mark = ",", suffix = " sols"),
               c("/10.000,825 sols", NA, "/0 sols", "-/10.000,825 sols"))
  expect_equal(deb_text(dec2, digits = 10), c("£8.123456789", "£0"))
  # Able to pass on arguments
  expect_equal(deb_text(dec2, digits = 10, small.mark = ","),
               c("£8.12345,6789", "£0.00000,"))
})

test_that("deb_text properly formats deb_tetra", {
  expect_equal(deb_text(tetra), c("£10,000 8s. 5d. 3f.", NA,
                                  "£0 0s. 0d. 0f.", "-£10,000 8s. 5d. 3f."))
  expect_equal(deb_text(tetra, sep = ".", s.mark = "",
                        d.mark = "", f.mark = ""),
               c("£10,000.8.5.3", NA, "£0.0.0.0", "-£10,000.8.5.3"))
  expect_equal(deb_text(tetra, currency = "$",
                        s.mark = "", d.mark = "", f.mark = "",
                        sep = ":", digits = 3, big.mark = ".",
                        decimal.mark = ",", suffix = " Flemish"),
               c("$10.000:8:5:2,825 Flemish", NA, "$0:0:0:0 Flemish",
                 "-$10.000:8:5:2,825 Flemish"))
  expect_equal(deb_text(tetra, digits = 3),
               c("£10,000 8s. 5d. 2.825f.", NA,
                 "£0 0s. 0d. 0f.", "-£10,000 8s. 5d. 2.825f."))
})
