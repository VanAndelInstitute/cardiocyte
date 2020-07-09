#' max_rates
#'
#' Find the maximum velocity of each peak in a vector of data quantifying a transient.
#'
#' @param x a vector of data
#' @param p a percentage of the highest peak with which to limit the results (in the case of lots of noisy peaks). Default is 0.
#'
#'
#' @importFrom stats smooth.spline predict
#' @export
#'
max_rates <- function(x, p = 0, ...) {
  smooth <- smooth.spline(x)
  derivs <- predict(smooth, deriv = 1)


    max.xs <- find_peaks(derivs$y, p=p, ...)
    max.vs <- derivs$y
    max.vs <- max.vs[max.xs]

    min.xs <- find_peaks(-1*derivs$y, p=p, ...)
    min.vs <- -1*derivs$y
    min.vs <- min.vs[min.xs]

  return(list(x.up = max.xs, x.down = min.xs, velocity.up = max.vs, velocity.down = -1*min.vs))
}
