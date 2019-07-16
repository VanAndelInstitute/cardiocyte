#' pulse_widths
#'
#' Calculate widths of pulses in ensemble matrix
#'
#' @param x data matrix, such as produced by ensemble function
#' @param p percentile of max at which to calculate width
#'
#' @return vector containing widths of each pulse
#' @export
#' @import foreach
#' @examples
#' data(ca_flux)
#' dat <- ensemble(ca_flux$Mean1)
#' pw <- pulse_widths(dat, .9)
pulse_widths <- function(x, p) {
  # for each row of x...(use foreach)
  i <- NULL # keep R CMD check happy
  res <- foreach(i = 1:nrow(x)) %do% {
    # find max for that pulse ... x[i,]
    m <- max(x[i,])

    # find the threshold value for that pulse (p * m)
    thr <- (m * p)

    # find the minimum and maximum index for values greater than threshold
    min <- min(which(x[i,] > thr))
    max <- max(which(x[i,] > thr))

    # the range is maximum index - minimum index
    range <- max - min
    return(range)
  }
  return(unlist(res))
}
