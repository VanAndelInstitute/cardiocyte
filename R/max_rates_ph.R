#' max_rates_ph
#'
#' Calculate the maximum velocities / peak height
#'
#' @param x vector of trace data
#'
#' @return vector containing the peak %
#' @export
#' @import foreach
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- ensemble(ca_flux$Mean1)
#' ph <- max_rates_ph(dat)
max_rates_ph <- function(x) {
  mvs <- max_velocities(x)
  ph <- peak_height(x)
  rates <- mvs$velocity.up / ph
  return(rates)
}
