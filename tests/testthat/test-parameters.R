context("Signal parameters")

test_that("Peak detection", {
  data(ca_flux)
  signal <- ca_flux$Mean1
  peaks <- find_peaks(signal)
  expect_equal(length(peaks), 23)
})

test_that("Esemble pulses", {
  data(ca_flux)
  signal <- ca_flux$Mean1
  dat <- ensemble(signal, norm = FALSE)
  expect_equal(nrow(dat), 22)
  expect_equal(ncol(dat), 9)
  expect_equal(round(sum(dat), 1), 17540.6)

  dat <- ensemble(signal, norm = TRUE)
  expect_equal(max(dat), 1)
  expect_equal(min(dat), 0)
})

test_that("Pulse width can be calculated", {
  data(ca_flux)
  dat <- ensemble(ca_flux$Mean1)
  widths <- pulse_widths(dat)
  expect_equal(widths[1], 10.5)
})

