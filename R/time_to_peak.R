#' time_to_peak
#'
#' Calculate the time at which peak occurs relative to the transient time.
#'
#' @param x vector of trace data
#'
#' @return vector containing the time
#' @export
#' @import foreach
#' @importFrom baseline baseline
#' @examples
#' data(ca_flux)
#' dat <- ensemble(ca_flux$Mean1)
#' ph <- time_to_peak(dat)
time_to_peak <- function(x) {
 bl <- baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)@baseline
 pp <- find_peaks(x)
 ph <- peak_height(x)
# for each peak,
 res <- foreach(i = 1:length(pp)) %do% {
  # set the threshold (10%)
    thr <- 0.1 * ph[i] + bl[pp[i]]
    start <- 0
    continue <- TRUE
    ii <- pp[i] - 1
    while(continue) {
      if(x[ii] < thr) {
        start <- ii
        continue <- FALSE

      } else {
        ii <- ii - 1
      }
    }
    pp[i] - start
 }
 return(unlist(res))
}
