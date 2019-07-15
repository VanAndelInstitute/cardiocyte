#' percent_peak
#'
#' Calculate the % of the peaks compared to the baseline
#'
#' @param x vector of trace data
#'
#' @return vector containing the peak %
#' @export
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' ph <- percent_peak(ca_flux$Mean1)
percent_peak <- function(x) {
  # find the location of the peaks
    peaks <- find_peaks(x)
      # find the baseline for each peak
    bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)
      # divide the baseline from the peak value and multiply by 100
    percent <- (x[peaks] / bl@baseline[peaks]) * 100
    return(percent)
}
