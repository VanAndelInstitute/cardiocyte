#' correct_baseline
#'
#' Correct baseline drift using the 'baseline' package.
#'
#' @param x a vector of data to correct
#' @param wm window size for averaging
#' @param ws window size for smoothing
#'
#' @return vector containing baseline corrected values
#' @importFrom baseline baseline
#' @export
#'
#' @examples
#' a <- correct_baseline(rnorm(100))
correct_baseline <- function(x, wm = 5, ws = 5) {
  baseline(as.matrix(t(x)), method = 'rolling', wm = 5, ws = 5)@corrected[1,]
}
