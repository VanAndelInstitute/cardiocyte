context("Baseline Correction")

test_that("Baseline correction works", {
  data(ca_flux)
  corrected <- correct_baseline(ca_flux$Mean1)
  expect_equal(corrected[1], 0.998)
})

