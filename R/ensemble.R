#' ensemble
#'
#' Extract all curves from a trace
#'
#' @param x a vector of data
#' @param offset Move the sliding window offset time points  if desired
#' @param norm Should each pulse be normalized to [0,1]?
#' @param ... additional parameters to pass to find_peaks
#'
#' @return A (melted) data frame with the curves
#' @export
#'
ensemble <- function(x, offset = 0, norm=FALSE, ...) {
  peaks <- find_peaks(x, ...)
  peaks <- peaks + offset
  cor <- correct_baseline(x)
  bw <- length(x) / length(peaks) / 2

  # eliminate peaks too close to edges
  ix <- which(peaks > bw & peaks < (length(x) - bw))
  peaks <- peaks[ix]

  dat <- data.frame(x = 1:(2 * bw + 1),
                    pulse = rep(1, 2 * bw + 1),
                    y = x[((peaks[1] - bw)+1):((peaks[1] + bw)+1)])
  for (ix in peaks[-1]) {
    res <- data.frame(x = 1:(2 * bw + 1),
                      pulse = rep(ix, 2 * bw + 1),
                      y = x[(ix - bw):(ix + bw)])
    dat <- rbind(dat, res)
  }
  dat <- do.call(rbind, tapply(dat$y, dat$pulse, function(x) { x }))
  if(norm) {
    mins <- apply(dat, 1, min)
    dat <- sweep(dat, 1, mins)
    maxs <- apply(dat, 1, max)
    dat <- sweep(dat, 1, maxs, "/")
  }
  dat
}


