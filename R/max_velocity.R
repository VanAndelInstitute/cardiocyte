#' max_velocities
#'
#' Find maximum velocity for each transient in a trace
#'
#' @param x a vector of data
#' @param offset Move the sliding window offset time points  if desired
#'
#' @return A dataframe with x (time index of max velocities) and velocity
#' (the maximum velocity for each peak)
#'
#' @export
#'
max_velocities <- function(x, offset = 0) {
  sm <- smooth.spline(x)
  sm.prime <- predict(sm, deriv = 1)
  peaks.u <- find_peaks(correct_baseline(sm.prime$y), drop = 0)
  peaks.d <- find_peaks(-correct_baseline(sm.prime$y), drop = 0)
  list(x.up = peaks.u,
             x.down = peaks.d,
             velocity.up = sm.prime$y[peaks.u],
             velocity.down = sm.prime$y[peaks.d])
}
