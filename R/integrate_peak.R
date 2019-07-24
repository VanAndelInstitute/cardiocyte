#' integrate_peak
#'
#' Calculate the area under a specific peak of the transient relative to the baseline
#'
#' @param x vector of trace data
#' @param y the peak #
#'
#' @return the area under the curve
#' @export
#' @importFrom baseline baseline
#' @importFrom DescTools AUC
#' @examples
#' data(ca_flux)
#' dat <- integrate_peak(ca_flux$Mean1, 3)
integrate_peak <- function(x, y) {
  # extract peaks from baseline corrected data
 ens <- ensemble(correct_baseline(x))
  # find the area under a specific oeak
 Auc <- AUC(x = 1:ncol(ens), y = ens[y,], method = "trapezoid")
 return(Auc)
}


