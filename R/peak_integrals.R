#' peak_integrals
#'
#' Calculate the area under each of the peaks of the transient relative to the baseline
#'
#' @param x vector of trace data
#'
#' @return the area under the curves
#' @export
#' @importFrom baseline baseline
#' @importFrom DescTools AUC
#' @examples
#' data(ca_flux)
#' dat <- peak_integrals(ca_flux$Mean1)
peak_integrals <- function(x) {
  ens <- ensemble(correct_baseline(x))
  areas <- apply(ens, 1, integrateauc)
  areas <- as.numeric(areas)
  return(areas)
}

integrateauc <- function(z) {
  area <- AUC(x = 1:length(z), y = z, method = "trapezoid")
  return(area)
}

