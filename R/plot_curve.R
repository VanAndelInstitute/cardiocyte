#' plot_curve
#'
#' Plot curves with annotation
#'
#' @param x a vector of data
#'
#' @return none. Called for side effect of generating plot
#' @import ggplot2
#' @export
#'
plot_curve <- function(x) {
  peaks <- find_peaks(x)
  cor <- correct_baseline(x)
  peak_size <- rep(0, length(x))
  peak_size[peaks] <- 1
  gd <- data.frame(y = cor,
                   x = 1:length(cor),
                   size = peak_size)
  ggplot(gd, aes(x = x, y = y)) +
    geom_line() +
    geom_point(aes(y = 1.02 * cor, alpha = size,
                   size = size),
               show.legend = FALSE,
               pch = 18, color = "#EB6221") +
    xlab("Slice") +
    ylab("Normalized signal") +
    theme_bw() +
    scale_radius(range = c(0,2)) +
    theme(axis.title = element_text(face = "bold"))
}




