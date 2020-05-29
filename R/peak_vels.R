#' peak_vels
#'
#' Find the maximum velocity of each peak in a vector of data quantifiying a transient.
#'
#' @param x a vector of data
#' @param downward.vel specifies if you want upward or downward velocities. Specify "TRUE" for downward.
#'
#' @importFrom stats smooth.spline predict
#' @export
#'
peak_vels <- function(x, downward.vel = FALSE) {
  storage <- numeric(nrow(ensemble(x)))
  for (i in 1:nrow(ensemble(x))) {
    storage[i] <- peak_vel(x, peak.number=i, downward.vel = downward.vel)
  }
  return(storage)
}


# internal function to find the velocity of each peak
peak_vel <- function(x, peak.number = 1, downward.vel = FALSE){
  ens <- ensemble(x)
  peak <- ens[peak.number,]
  smooth.peak <- smooth.spline(peak)
  derivs <- predict(smooth.peak, deriv=1)
  down <- downward.vel
  if(down == TRUE) {
    print(min(derivs$y))
  } else {
    print(max(derivs$y))
  }
}
