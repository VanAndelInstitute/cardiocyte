context("Signal parameters")

test_that("Peak detection", {
  data(ca_flux)
  signal <- ca_flux$Mean1
  peaks <- find_peaks(signal)
  expect_equal(length(peaks), 23)
})

