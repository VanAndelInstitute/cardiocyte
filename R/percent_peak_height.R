#' percent_peak_height
#'
#' Calculate the % of the peak height compared to the baseline
#'
#' @param x vector of trace data
#'
#' @return vector containing the peak height %
#' @export
#' @import foreach
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- ensemble(ca_flux$Mean1)
#' ph <- percent_peak_height(dat)
percent_peak_height <- function(x) {
  # find the height of the peaks
    peaks <- peak_height(x)
      # find the baseline for each peak
    bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)
      # divide the peak heights by the baseline and * by 100
    percent <- (peaks / bl@baseline[peaks]) * 100
    return(percent)
}
