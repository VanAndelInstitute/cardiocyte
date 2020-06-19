#' max_rates_ph
#'
#' Calculate the maximum velocities / peak height
#'
#' @param x vector of trace data
#'
#' @return vector containing the peak %
#' @export
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- max_rates_ph(ca_flux$Mean1)
max_rates_ph <- function(x) {
  mvs <- max_vels(x)
  ph <- peak_height(x)
  rates <- mvs$velocity.up / ph
  return(rates)
}
