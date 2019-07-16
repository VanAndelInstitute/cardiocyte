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
    #geom_point(aes(y = 1.02 * cor, alpha = size,
    #               size = size),
    #           show.legend = FALSE,
    #           pch = 18, color = "#EB6221") +
    xlab("Slice") +
    ylab("Normalized signal") +
    theme_bw() +
    scale_y_continuous(expand = c(.3, 0)) +
    scale_radius(range = c(0,2)) +
    theme(axis.title = element_text(face = "bold"))
}


#' geom_vel
#'
#' @details Here are a bunch of details
#'
#' @param direction Default (\code{up}) is to annotate maximum upward velocity.
#' Alternative is \code{down} to annotate maximum downward velocity.
#' @param digits Number of digits to round/pad do in plot. Default is 3.
#' @param cex Size for annotation text.
#' @param color Color for annotation text. Default is dark green for upward
#' velocity and dark red for downward velocity.
#' @param ... Additioan arguments passed to gpplot layer.
#'
#' @rdname plots
#' @return none. Called for side effect of generating plot layer
#' @importFrom ggplot2 geom_text
#' @export
#'
geom_vel <- function(direction = "up",
                     digits = 3,
                     cex = 2.5,
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

  layer(
    stat = StatVel, data = NULL, geom = "text",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = TRUE,
    params = list(na.rm = TRUE,
                  digits = digits,
                  direction = direction,
                  color = color,
                  cex = cex,
                  srt = srt,
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
                     vel <- max_velocities(data$y)
                     if(params$direction == "up") {
                       v[vel$x.up] <- format(round(vel$velocity.up, params$digits),
                                             nsmall = params$digits)
                       y[vel$x.up] <- min(data$y) * 0.9
                     } else {
                       v[vel$x.down] <- format(round(vel$velocity.down, params$digits),
                                               nsmall = params$digits)
                       y[vel$x.down] <- max(data$y) * 1.1
                     }
                     label= paste(v, "\u2192")
                     data$y <- y
                     data$label <- label
                     data
                   },
                   compute_group = function(self, data, scales, na.rm, digits, direction) {
                   },
                   required_aes = c("x", "y")
)

# internal Stat function for annotation layers
StatPeak <- ggproto("StatPeak",
                    Stat,
                    compute_layer = function (self, data, params, layout) {
                      y <- rep(NA, nrow(data))
                      v <- rep(NA, nrow(data))
                      vel <- max_velocities(data$y)
                      if(params$direction == "up") {
                        v[vel$x.up] <- format(round(vel$velocity.up, params$digits),
                                              nsmall = params$digits)
                        y[vel$x.up] <- min(data$y) * 0.9
                      } else {
                        v[vel$x.down] <- format(round(vel$velocity.down, params$digits),
                                                nsmall = params$digits)
                        y[vel$x.down] <- max(data$y) * 1.1
                      }
                      label= paste(v, "\u2192")
                      data$y <- y
                      data$label <- label
                      data
                    },
                    compute_group = function(self, data, scales, na.rm, digits, direction) {
                    },
                    required_aes = c("x", "y")
)


