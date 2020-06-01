#' peak_vels
#'
#' Find the maximum velocity of each peak in a vector of data quantifiying a transient.
#'
#' @param x a vector of data
#' @param downward.vel specifies if you want upward or downward velocities. Specify "TRUE" for downward.
#' @param p a percentage of the highest peak with which to limit the results (in the case of lots of noisy peaks). Default is 0.
#' @param offset Move the sliding window offset time points  if desired
#' @param norm Should each pulse be normalized to [0,1]?
#' @param ... additional parameters to pass to find_peaks
#'
#'
#' @importFrom stats smooth.spline predict
#' @export
#'
peak_vels <- function(x, downward.vel = FALSE, p=0, offset = 0, norm = FALSE, ...) {
  storage <- numeric(nrow(ensemble(x, p=p, offset = offset, norm = norm, ...)))
  for (i in 1:nrow(ensemble(x, p=p, offset = offset, norm = norm, ...))) {
    storage[i] <- peak_vel(x, peak.number=i, downward.vel = downward.vel, p=p, offset = offset, norm = norm, ...)
  }
  return(storage)
}


# internal function to find the velocity of each peak
peak_vel <- function(x, peak.number = 1, downward.vel = FALSE, p=0, offset = 0, norm = FALSE, ...){
  ens <- ensemble(x, p=p, offset=offset, norm=norm, ...)
  peak <- ens[peak.number,]
  smooth.peak <- smooth.spline(peak)
  derivs <- predict(smooth.peak, deriv=1)
  down <- downward.vel
  if(down == TRUE) {
    return(min(derivs$y))
  } else {
    return(max(derivs$y))
  }
}
