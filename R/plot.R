#' Plots the data of a single data set.
#'
#' Plots the data of a single data set.
#'
#' For details ...
#'
#' @param x object to plot
#' @return A ggplot2 graph object.
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @seealso \code{\link{ggplot}}
#' @keywords hplot
#' @examples
#'
#' rnorm(100)
#'
#' @export plot.eNase
#'
plot.eNase <- function (x) {
  dat <-x[["data_long"]]
  baseline <- as.data.frame(x[["baseline.data"]])
  #baseline.date <- ymd_hms(strsplit(baseline[1,1], split="|", fixed=TRUE)[[1]][2])

  plot <- ggplot(dat, aes_string(x="timestamp", y="value")) + geom_line() +  facet_wrap(~type,  ncol=8, scales = "free")
  plot <- plot + geom_vline(xintercept = x[["start"]])
  plot <- plot + geom_vline(xintercept = x[["stop"]])
  plot <- plot + geom_vline(xintercept = x[["start.probe"]])
  plot <- plot + geom_vline(xintercept = x[["stop.probe"]])
  plot <- plot + geom_hline(data = data.frame(type = unique(dat$type),
                                              baseline_value = t(baseline[,-1])),
                            aes_string(yintercept = "baseline_value"), color = "red", linetype = "dashed")
  plot <- plot + ggtitle(x[["name"]])
  return(plot)
}
