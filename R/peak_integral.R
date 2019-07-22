#' peak_integral
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
#' dat <- peak_integral(ca_flux$Mean1, 3)
peak_integral <- function(x, y) {
  # extract peaks from baseline corrected data
 ens <- ensemble(correct_baseline(ca_flux$Mean1))
  # find the area under a specific oeak
 Auc <- AUC(x = 1:ncol(ens), y = ens[y,], method = "trapezoid")
 return(Auc)
}


