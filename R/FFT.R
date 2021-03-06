#' FFT
#'
#' Applies Fast Fourier Transform to a vector of time series data quantifying a transient
#'
#' @param x vector of trace data
#' @param y the number of FFT components to keep in the signal
#' @param norm if you want to normalize the data as a percentage of 1.
#' Specify "baseline" for the correct_baseline method of normalization,
#' Specify "min" to just subtract the minimum value (for relatively flat transients)
#'
#' @return a vector of the transformed data
#' @export
#' @importFrom stats fft
#' @examples
#' data(ca_flux)
#' dat <- FFT(ca_flux$Mean1, 20, norm="baseline")
FFT <- function(x, y, norm = FALSE, p=0) {
dat_fft <- fft(x)
dat_fft[y:(length(dat_fft)-y)] <- 0 + 0i
dat_sm <- fft(dat_fft, inverse = TRUE)/length(dat_fft)
dat_sm <- Re(dat_sm)

norm <- norm
if (norm == FALSE) {
  return(dat_sm)
}
if(norm == "baseline") {
 normalized <- correct_baseline(dat_sm)
 max <- max(normalized[find_peaks(normalized, p=p)])
 return(normalized/max)
}
if(norm == "min") {
  normalized <- dat_sm-min(dat_sm)
  normalized <- normalized/max(normalized)
  return(normalized)
}

}
