#' max_rates_baseline
#'
#' Calculate the maximum velocities / baseline
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
#' ph <- max_rates_baseline(dat)
max_rates_baseline <- function(x) {
  mvs <- max_velocities(x)
  bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)
  rates <- mvs$velocity.up / bl@baseline[mvs$x.up]
  return(rates)
}
