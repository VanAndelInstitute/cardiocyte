globalVariables(c("y", "size", "pulse"))

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
  peaks <- find_peaks(x, drop =0)
  cor <- correct_baseline(x)
  cor <- x
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

geom_vel_up <- function(x, digits = 3, ...) {
  dat <- data.frame(x = x)
  dat$vel.up <- NA
  dat$y <- NA
  vel <- max_velocities(dat$x)

  dat$vel.up[vel$x.up] <- round(vel$velocity.up, digits)
  dat$y[vel$x.up] <- x[vel$x.up]

  geom_text(aes(y = dat$y, label= paste(dat$vel.up, "\u2192")),
              size = 2,
              col = "darkgreen",
              srt = 90,
              na.rm = TRUE,
              nudge_x = -1.7,
              ...)
}

geom_vel_down <- function(x, digits = 3, ...) {
  dat <- data.frame(x = x)
  dat$vel.down <- NA
  dat$y <- NA
  vel <- max_velocities(dat$x)
  dat$vel.down[vel$x.down] <- round(vel$velocity.down, digits)
  dat$y[vel$x.down] <- x[vel$x.down]
  geom_text(aes(y = dat$y, label= paste(dat$vel.down, "\u2192")),
            size = 2,
            col = "darkred",
            srt = 270,
            na.rm = TRUE,
            nudge_x = 1.7,
            ...)
}


#' plot_ensemble
#'
#' Extract all curves and overlay
#'
#' @param x a vector of data
#' @param offset Move the sliding window offset time points  if desired
#' @param norm Should each pulse be normalized to [0,1]?
#'
#' @return none. Called for side effect of generating plot
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
#'
plot_ensemble <- function(x, offset = 0, norm = TRUE) {
  dat <- ensemble(x, offset, norm)
  dat <- melt(dat)
  colnames(dat) <- c("pulse", "x", "y")
  if (norm) {
    lab <- "Normalized signal"
  } else {
    lab <- "Signal"
  }
  ggplot(dat, aes(x = x, y = y,
                  color = factor(pulse))) +
    geom_path(show.legend = FALSE) +
    ylab(lab) +
    xlab("Slice") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"))
}


