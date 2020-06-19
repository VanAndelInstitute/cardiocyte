#' max_rates_bl
#'
#' Calculate the maximum velocities / baseline
#'
#' @param x vector of trace data
#'
#' @return vector containing the peak %
#' @export
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' mr <- max_rates_bl(ca_flux$Mean1)
max_rates_bl <- function(x) {
  mvs <- max_vels(x)
  bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)
  rates <- mvs$velocity.up / bl@baseline[mvs$x.up]
  return(rates)
}
