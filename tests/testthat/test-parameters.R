context("Signal parameters")

test_that("Peak detection", {
  data(ca_flux)
  signal <- ca_flux$Mean1
  peaks <- find_peaks(signal, drop = 1)
  expect_equal(length(peaks), 23)
  sp <- sum(find_peaks(ca_flux$Mean1))
  expect_equal(sp, 2358) # issue #1
})

test_that("Esemble pulses", {
  data(ca_flux)
  signal <- ca_flux$Mean1
  dat <- ensemble(signal, norm = FALSE)
  expect_equal(nrow(dat), 22)
  expect_equal(ncol(dat), 9)
  expect_equal(round(sum(dat), 1), 17535.3)

  dat <- ensemble(signal, norm = TRUE)
  expect_equal(max(dat), 1)
  expect_equal(min(dat), 0)
})

test_that("Pulse width can be calculated", {
  data(ca_flux)
  dat <- ensemble(ca_flux$Mean1)
  widths <- pulse_widths(dat, .1)
  expect_equal(widths[1], 8)
})

test_that("Max velocities can be calculated", {
  data(ca_flux)
  vels <- max_velocities(ca_flux$Mean1)
  expect_equal(round(vels$velocity.up[1],2), 0.06)
})

test_that("Peak height can be calculated", {
  data(ca_flux)
  height <- peak_height(ca_flux$Mean1)
  expect_equal(round(height[1], 1), 25.5)
})

test_that("Peak can be calculated", {
  data(ca_flux)
  percent <- percent_peak(ca_flux$Mean1)
  expect_equal(round(percent[1], 4), 126.8591)
})

test_that("%Peak height can be calculated", {
  data(ca_flux)
  percent <- percent_peak_height(ca_flux$Mean1)
  expect_equal(round(percent[1], 1), 28.4)
})

test_that("Time to Peak", {
  data(ca_flux)
  time <- time_to_peak(ca_flux$Mean1)
  expect_equal(time[1], 1)
})

test_that("Max Rates per Baseline can be calculated", {
  data(ca_flux)
  rates <- max_rates_bl(ca_flux$Mean1)
  expect_equal(round(rates[1], 4), 7e-04)
})

test_that("Max Rates per peak height can be calculated", {
  data(ca_flux)
  rates <- max_rates_ph(ca_flux$Mean1)
  expect_equal(round(rates[1], 3), 0.002)
  rates <- max_rates_ph(ca_flux$Mean4)
  expect_equal(round(rates[1], 3), 0.022)
})

test_that("transient integral can be calculated", {
  data(ca_flux)
  integral <- trans_integral(ca_flux$Mean1)
  expect_equal(round(integral, 2), 1051.02)
})

test_that("ctf can be calculated", {
  data(ca_flux)
  diff <- ctf(ca_flux$Mean1, ca_flux$Mean4)
  expect_equal(diff[1], -0.589)
})


test_that("peak integrals can be calculated", {
  data(ca_flux)
  aucs <- peak_integrals(ca_flux$Mean1)
  expect_equal(round(aucs[1],1), 56.1)
})

test_that("FFT can be calculated", {
  data(ca_flux)
  fftrans <- FFT(ca_flux$Mean1, 20)
  expect_equal(round(fftrans[1],1), 95.5)
})

test_that("BPM can be calculated", {
  data(ca_flux)
  BPM <- bpm(ca_flux$Mean1, 10)
  expect_equal(round(BPM,1), 73.8)
})
