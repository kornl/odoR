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
#' @export plot.odor
#'
plot.odor <- function (x, show_baseline=FALSE) {
  if (!is.null(x[["corrected_data_long"]])) {
    dat <- x[["corrected_data_long"]]
  } else {
    dat <-x[["data_long"]]
  }
  baseline <- as.data.frame(x[["baseline_data"]])
  #baseline.date <- ymd_hms(strsplit(baseline[1,1], split="|", fixed=TRUE)[[1]][2])

  plot <- ggplot(dat, aes_string(x="timestamp", y="value")) + geom_point(size=0) +  facet_wrap(~feature,  ncol=4, scales = "free")
  plot <- plot + geom_vline(xintercept = x[["start"]])
  plot <- plot + geom_vline(xintercept = x[["stop"]])
  plot <- plot + geom_vline(xintercept = x[["start_probe"]])
  plot <- plot + geom_vline(xintercept = x[["stop_probe"]])
  if (show_baseline) {
    plot <- plot + geom_hline(data = data.frame(channel = unique(dat$channel),
                                                baseline_value = t(baseline[,-1])),
                              aes_string(yintercept = "baseline_value"), color = "red", linetype = "dashed")
  }
  plot <- plot + ggtitle(x[["name"]])
  return(plot)
}

spider.plot.odor <- function(x) {
  plot <- ggplot(x$response, aes(x = channel, y = value)) +
    geom_polygon(fill = "blue") +
    geom_point(size = 4, color = "white") +
    coord_polar() +
    #theme_void() +
    ggtitle("Spiderplot of Features") +
    theme(legend.position = "bottom")
}
