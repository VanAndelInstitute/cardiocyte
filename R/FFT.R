#' FFT
#'
#' Applies Fast Fourier Transform to a vector of time series data quantifying a transient
#'
#' @param x vector of trace data
#' @param y the number of FFT components to keep in the signal
#'
#' @return a vector of the transformed data
#' @export
#' @importFrom stats fft
#' @examples
#' data(ca_flux)
#' dat <- FFT(ca_flux$Mean1, 20)
FFT <- function(x, y) {
dat_fft <- fft(x)
dat_fft[y:(length(dat_fft)-y)] <- 0 + 0i
dat_sm <- fft(dat_fft, inverse = TRUE)/length(dat_fft)
dat_sm <- Re(dat_sm)
return(dat_sm)
}
