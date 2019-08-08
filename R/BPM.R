#' bpm
#'
#' Calculate the BPM of the transient (beats per minute)
#'
#' @param x vector of trace data
#' @param framerate the framerate
#'
#' @return The BPM of the transient
#' @export
#' @importFrom baseline baseline
#' @importFrom DescTools AUC
#' @examples
#' data(ca_flux)
#' dat <- bpm(ca_flux$Mean1)
bpm <- function(x, framerate) {
BPF <- length(find_peaks(x))/length(x)
BPM <- BPF * framerate *60
return(BPM)
}
