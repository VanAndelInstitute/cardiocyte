globalVariables(c("y", "size", "pulse"))

#' plot_curve
#'
#' Plot transient curves, optionally with annotation layers
#'
#' @param x a vector of data
#' @rdname plots
#' @return An object of class ggplot
#' @import ggplot2
#' @export
#'
plot_curve <- function(x) {
  gd <- data.frame(y = x,
                   x = 1:length(x))
  ggplot(gd, aes(x = x, y = y)) +
    geom_line() +
    xlab("Time") +
    ylab("Signal") +
    theme_bw() +
    scale_y_continuous(expand = c(.3, 0)) +
    scale_radius(range = c(0,2)) +
    theme(axis.title = element_text(face = "bold"))
}


#' geom_vel
#'
#' @param direction Default (\code{up}) is to annotate maximum upward velocity.
#' Alternative is \code{down} to annotate maximum downward velocity.
#' @param digits Number of digits to round/pad do in plot. Default is 3.
#' @param cex Size for annotation text.
#' @param smoothness How much smoothing to peform (see ?max_velocites)
#' @param p Dampening factor to remove small wavelets (see ?find_peaks)
#' @param color Color for annotations.
#' @param ... Additional arguments passed to gpplot layer.
#'
#' @rdname plots
#' @return none. Called for side effect of generating plot layer
#' @importFrom ggplot2 geom_text
#' @export
#'
geom_vel <- function(direction = "up",
                     digits = 3,
                     cex = 2.5,
                     smoothness = 0,
                     p = 0,
                     color = NA,
                     ...) {
  if(direction == "up") {
    if(is.na(color))
      color <- "darkgreen"
    srt <- 90
  } else {
    if(is.na(color))
      color <- "darkred"
    srt <- 270
  }

  a <- layer(
    stat = StatVel, data = NULL, geom = "text",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(na.rm = TRUE,
                  digits = digits,
                  direction = direction,
                  color = color,
                  smoothness = smoothness,
                  p = p,
                  cex = cex,
                  srt = srt,
                  ...)
  )
  b <- layer(
    stat = StatVel, data = NULL, geom = "vline",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(na.rm = TRUE,
                  digits = digits,
                  direction = direction,
                  color = color,
                  p = p,
                  smoothness = smoothness,
                  alpha=0.1,
                  ...)
  )
  list(a,b)
}

#' geom_peaks
#'
#'
#' @rdname plots
#' @return none. Called for side effect of generating plot layer
#' @importFrom ggplot2 geom_text
#' @export
#'
geom_peaks <- function(color = "#EB6221", p = 0, ...) {
  layer(
    stat = StatPeak, data = NULL, geom = "point",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(na.rm = TRUE,
                  color = color,
                  p = p,
                  ...)
  )
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
  ggplot(dat, aes(x = x, y = y)) +
    geom_path(aes(group = factor(pulse)),
              show.legend = FALSE,
              alpha=0.2,
              lty=3) +
    geom_point(aes(group = factor(pulse)),
               show.legend = FALSE,
               shape = 21,
               color="#00000000",
               fill="#00000033") +
    geom_smooth(method = "gam",
                formula = y ~ s(x, k = 5),
                size = 1,
                alpha = 0.1,
                fill = "#EB6221",
                color = "#EB6221") +
    ylab(lab) +
    xlab("Time") +
    theme_bw() +
    theme(axis.title = element_text(face = "bold"))
}

# internal Stat function for velocity annotation layers
StatVel <- ggproto("StatVel",
                   Stat,
                   compute_layer = function (self, data, params, layout) {
                     y <- rep(NA, nrow(data))
                     v <- rep(NA, nrow(data))
                     xi <- rep(NA, nrow(data))
                     vel <- max_velocities(data$y,
                                           p = params$p,
                                           smooth = params$smooth)
                     if(params$direction == "up") {
                       v[vel$x.up] <- format(round(vel$velocity.up, params$digits),
                                             nsmall = params$digits)
                       y[vel$x.up] <- min(data$y) - 0.1 * diff(range(data$y))
                       xi[vel$x.up] <- vel$x.up
                     } else {
                       v[vel$x.down] <- format(round(vel$velocity.down, params$digits),
                                               nsmall = params$digits)
                       y[vel$x.down] <- max(data$y) + 0.1 * diff(range(data$y))
                       xi[vel$x.down] <- vel$x.down
                     }
                     label= paste(v, "\u2192")
                     data$y <- y
                     data$xintercept <- xi
                     data$label <- label
                     data
                   },
                   compute_group = function(self,
                                            data,
                                            scales,
                                            na.rm,
                                            digits,
                                            smoothness,
                                            p,
                                            direction) {
                   },
                   required_aes = c("x", "y")
)

# internal Stat function for peak annotation layer
StatPeak <- ggproto("StatPeak",
                    Stat,
                    compute_layer = function (self, data, params, layout) {
                      peaks <- find_peaks(data$y, drop = 0, p = params$p)
                      data <- data[peaks, ]
                      data$y <- data$y + 0.02 * diff(range(data$y))
                      data
                    },
                    compute_group = function(self, data, scales, na.rm, p){
                    },
                    required_aes = c("x", "y")
)


