#' max_velocities
#'
#' Find maximum velocity for each transient in a trace
#'
#' @param x a vector of data
#' @param offset Move the sliding window offset time points  if desired
#' @param smooth should smoothing be performed (see ?find_peaks)
#' @param correct should baseline correction be performed (see ?find_peaks)
#' @param smoothness a number in [0,1] indicating how much smoothing to perform.
#'                   default is 0 (minimal smoothing)
#' @param p degree to which to suppress small wavelets (see ?find_peaks)
#' @param ... additional parameters to send to find_peaks
#'
#' @return A dataframe with x (time index of max velocities) and velocity
#' (the maximum velocity for each peak)
#'
#' @importFrom stats smooth.spline predict
#' @export
#'
max_velocities <- function(x,
                           offset = 0,
                           smooth = TRUE,
                           correct = TRUE,
                           smoothness = 0,
                           p = 0,
                           ...) {
  x <- smooth.spline(x, spar=smoothness, nknots = round((1-smoothness)*length(x)))
  x.prime <- predict(x, deriv = 1)
  peaks.u <- find_peaks(x.prime$y, p = p, drop = 0, smooth = smooth, correct = correct, ...)
  peaks.d <- find_peaks(x.prime$y, p = p, drop = 0, smooth = smooth, correct = correct, ...)
  list(x.up = peaks.u,
             x.down = peaks.d,
             velocity.up = x.prime$y[peaks.u],
             velocity.down = x.prime$y[peaks.d])
}
