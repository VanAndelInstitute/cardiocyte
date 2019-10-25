#' find_peaks
#'
#' Find peaks in signal. The first peak
#' is often spurious (the result of catching a transient in the middle),
#' so the drop parameter defaults to 1.
#'
#' @param x a vector of data to find peaks in
#' @param drop number of intial observations to drop. Default is 1.
#' @param p a percentage of the highest peak with which to limit the results (in the case of lots of noisy peaks). Default is 0
#'
#' @return vector of indices of x where peaks are
#' @importFrom seewave hilbert bwfilter
#' @importFrom pracma findpeaks
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' data(ca_flux)
#' a <- find_peaks(ca_flux$Mean1)
find_peaks <- function(x, drop=0, p = 0, smooth=TRUE, correct=TRUE) {
  d <- x
  if(correct)
    d <- correct_baseline(d)
  if(smooth)
    d <- smooth.spline(d)$y
  peaks <- findpeaks(d)[,2]
  peaks <- .check_peaks(x, peaks)
  if(drop > 0) {
    peaks <- peaks[-c(1:drop)]
  }
  heights <- data.frame(V1 = peaks, V2 = x[peaks])
  heights <- heights[which(heights$V2  > p*max(heights$V2)), ]
  heights$V1
}

# internal function to clean up peak misses from hilbert
.check_peaks <- function(x, p) {
  done = FALSE
  while (!done) {
    corrected = FALSE
    for (i in 1:length(p)) {
      ix <- p[i]
      if (ix > 1) {
        if (x[ix - 1] > x[ix]) {
          p[i] <- ix - 1
          corrected <- TRUE
        }
      }
      if (ix < length(x)) {
        if (x[ix + 1] > x[ix]) {
          p[i] <- ix + 1
          corrected <- TRUE
        }
      }
    }
    if (!corrected)
      done <- TRUE
  }
  p
}
