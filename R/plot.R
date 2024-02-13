#' Plots the data of a single data set.
#'
#' Plots the data of a single data set.
#'
#' Plots the data over the time. For each sensor type (base and features) as well as for temperature and humidity there will be a facet showing the corresponding data.
#' The events of the measurement will be shown as vertical lines.
#'
#' @param x object to plot
#' @param show_baseline logical whether to show the baseline or not
#' @return A ggplot2 graph object.
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @seealso \code{\link{ggplot}}
#' @keywords hplot
#' @examples
#'
#' rnorm(100)
#'
#' @export plot.odor
#'
plot.odor <- function (x, show_baseline=FALSE) {
  if (!is.null(x[["corrected_data_long"]])) {
    dat <- x[["corrected_data_long"]]
  } else {
    dat <-x[["data_long"]]
  }
  baseline <- as.data.frame(x[["baseline_data"]])

  plot <- ggplot(dat, aes_string(x="timestamp", y="value")) + geom_point(size=0) +  facet_wrap(~feature,  ncol=4, scales = "free")
  plot <- plot + geom_vline(xintercept = x[["start"]])
  plot <- plot + geom_vline(xintercept = x[["stop"]])
  plot <- plot + geom_vline(xintercept = x[["start_exposure"]])
  plot <- plot + geom_vline(xintercept = x[["stop_exposure"]])
  if (show_baseline) {
    plot <- plot + geom_hline(data = data.frame(channel = unique(dat$channel),
                                                baseline_value = t(baseline[,-1])),
                              aes_string(yintercept = "baseline_value"), color = "red", linetype = "dashed")
  }
  plot <- plot + ggtitle(x[["name"]])
  return(plot)
}
