#' peak_height
#'
#' Calculate the heights of peaks in ensemble matrix
#'
#' @param x vector of trace data
#'
#' @return vector containing the peak heights of the ensemble matrix
#' @export
#' @import foreach
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- ensemble(ca_flux$Mean1)
#' ph <- peak_height(dat)
peak_height <- function(x) {
  # find the location of the peaks
    peaks <- find_peaks(x)
      # find the baseline for each peak
    bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)
      # subtract the baseline from the peak value
    heights <- x[peaks] - bl@baseline[peaks]
    return(heights)
}
