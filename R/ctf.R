#' ctf
#'
#' Calculate the mean intensity - backround intensity
#'
#' @param x a vector of trace data
#' @param y a vector of backround data
#'
#' @return vector containing the peak %
#' @export
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- ctf(ca_flux$Mean1, ca_flux$Mean4)
ctf <- function(x, y) {
diff <- x - y
return(diff)
}
