#' trans_integral
#'
#' Calculate the area under the transient relative to the baseline
#'
#' @param x vector of trace data
#'
#' @return the area under the transient
#' @export
#' @importFrom baseline baseline
#' @importFrom DescTools AUC
#' @examples
#' data(ca_flux)
#' dat <- trans_integral(ca_flux$Mean1)
trans_integral <- function(x) {
  #correct the baseline of the transient
trans <- correct_baseline(x)
  #set any negative values to 0
trans[trans < 0] <- 0
  #set the x axis from which to calculate the area
x <- c(1:length(trans))
  #calculate the area using the AUC function from package "DescTools"
area <- AUC(x = x, y = trans, method = "trapezoid", na.rm = TRUE)
return(area)
}
