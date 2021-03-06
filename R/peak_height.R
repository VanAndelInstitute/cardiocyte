#' peak_height
#'
#' Calculate the heights of peaks in ensemble matrix
#'
#' @param x vector of trace data
#' @param p percent of max to limit noisy peaks
#' @param ... additional parameters to send to find_peaks
#'
#' @return vector containing the peak heights of the ensemble matrix
#' @export
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- ensemble(ca_flux$Mean1)
#' ph <- peak_height(dat)
peak_height <- function(x, p = 0, ...) {
  # find the location of the peaks
    peaks <- find_peaks(x, ...)
      # find the baseline for each peak
    bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)
      # subtract the baseline from the peak value
    heights <- x[peaks] - bl@baseline[peaks]
    heights <- heights[which(heights > p*max(heights))]
    return(heights)
}
