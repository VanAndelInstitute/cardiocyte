#' correct_baseline
#'
#' Correct baseline drift using the 'baseline' package.
#'
#' @param x a vector of data to correct
#' @param wm window size for averaging. Minimum is 2.
#' @param ws window size for smoothing. Minimum is 2.
#'
#' @return vector containing baseline corrected values
#' @importFrom baseline baseline
#' @export
#'
#' @examples
#' a <- correct_baseline(rnorm(100))
correct_baseline <- function(x, wm = 5, ws = 5) {
  if(wm < 2)
    wm <- 2
  if(ws < 2)
    ws <- 2
  baseline(as.matrix(t(x)), method = 'rolling', wm = wm, ws = ws)@corrected[1,]
}
