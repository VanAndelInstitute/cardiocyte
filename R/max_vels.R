#' peak_vels
#'
#' Find the maximum velocity of each peak in a vector of data quantifiying a transient.
#'
#' @param x a vector of data
#' @param down specifies if you want upward or downward velocities. Specify "TRUE" for downward.
#' @param p a percentage of the highest peak with which to limit the results (in the case of lots of noisy peaks). Default is 0.
#' @param ... additional parameters to pass to find_peaks
#'
#'
#' @importFrom stats smooth.spline predict
#' @export
#'
max_vels <- function(x, p = 0, down = FALSE, ...) {
  smooth <- smooth.spline(x, ...)
  derivs <- predict(smooth, deriv = 1)
  downward.vel <- down

  if(downward.vel == FALSE) {
  max.xs <- find_peaks(derivs$y)
  max.vs <- derivs$y
  max.vs <- max.vs[max.xs]
  return(list(x.up = max.xs, velocity.up = max.vs))
  }
  else {
    min.xs <- find_peaks(-1*derivs$y)
    min.vs <- -1*derivs$y
    min.vs <- min.vs[min.xs]
    return(list(x.down = min.xs, velocity.down = -1*min.vs ))

  }
}
