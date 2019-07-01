#' find_peaks
#'
#' Find peaks in signal using hilbert transform. Note that this approach
#' works better if the signal is not baseline corrected.
#'
#' @param x a vector of data to find peaks in
#' @param f signal frequency. Default is 5Hz.
#'
#' @return vector of indices of x where peaks are
#' @importFrom seewave hilbert bwfilter
#' @export
#'
#' @examples
#' data(ca_flux)
#' a <- find_peaks(ca_flux$Mean1)
find_peaks <- function(x, f = 5) {
  hil <- Im(hilbert(x, f))
  hil <- correct_baseline(hil)

  # peaks are where hilbert transform crosses zero.
  peaks <- which(diff(sign(hil)) > 0)
  peaks <- .check_peaks(x, peaks)

  # first peak is likely noise
  return(peaks[-1])
}

# internal function to clean up peak misses from hilbert
.check_peaks <- function(x, p) {
  done = FALSE
  while (!done) {
    corrected = FALSE
    for (i in 1:length(p)) {
      ix <- p[i]
      if (ix > 1) {
        if (x[ix - 1] > x[ix]) {
          p[i] <- ix - 1
          corrected <- TRUE
        }
      }
      if (ix < length(x)) {
        if (x[ix + 1] > x[ix]) {
          p[i] <- ix + 1
          corrected <- TRUE
        }
      }
    }
    if (!corrected)
      done <- TRUE
  }
  p
}
