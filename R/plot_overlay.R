#' plot_overlay
#'
#' plot the peaks of the transient over top of each other, in order to find an "average" peak.
#'
#' @param x vector of trace data

#' @return a plot of the peaks overlaid on top of each other
#' @importFrom reshape2 melt
#'
#' @export

overlay <- function(x) {
  peaks <- vector("list", 1)
  upsidown <- x *-1 + max(x)
  valleys <- find_peaks(upsidown)
  for (i in 1:(length(valleys)-1)) {
    peaks[[i]] <- x[valleys[i]:valleys[i+1]]
  }
  peaks
  peaks.NA <- lapply(peaks, `length<-`, max(lengths(peaks)))
  return(as.data.frame(do.call(rbind, peaks.NA)))

}

#' @export

plot_overlay <- function(x, p=.5) {

options(warn = -1)

  bean <- overlay(x)
  bean2 <- t(bean)
  bean2 <- as.data.frame(bean2)
  bean.clean <- bean2[,!sapply(bean2, isna)>(1-p)]

  bean3 <- melt(bean.clean)
  bean3$rowid <- 1:ncol(bean)

  bean.mean <- mutate(bean.clean, Mean = rowMeans(bean.clean, na.rm = TRUE))
  bean.mean <- as.numeric(bean.mean$Mean)
  length(bean.mean) <- length(bean3$value)


  return(ggplot(bean3, aes(x=rowid, y=value, group=variable)) +
    geom_point(alpha=.2) +
    geom_line(aes(y=bean.mean), color="#7997FF", size=2) +
    labs(x = "Time", y="Signal"))

options(warn=0)
}
#' @export

isna <- function(x){
  prop <- mean(is.na(x))
  return(prop)
}

