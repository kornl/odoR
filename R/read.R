utils::globalVariables(c("type", "value"))

#' Reads the data of a single data set.
#'
#' Reads the data of a single data set.
#'
#' For details ...
#'
#' @param filename The name of the file which the data are to be read from.
#' @return A ggplot2 graph object.
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @seealso \code{\link{ggplot}}
#' @keywords IO
#' @examples
#'
#' rnorm(100)
#'
#' @export read.odor
#'
read.odor <- function (filename) {
  meta_data <- read_csv(filename, skip = 1, n_max=1, col_names = FALSE, show_col_types = FALSE)
  baseline_data <- read_csv(filename, skip = 3, n_max=1, col_names = FALSE, show_col_types = FALSE)
  json_meta_data <- fromJSON(as.data.frame(meta_data)[1, 2])
  name <- pluck(json_meta_data, "data", "name")
  start <- ymd_hms(pluck(json_meta_data, "data", "started_at"))
  stop <- ymd_hms(pluck(json_meta_data, "data", "stopped_at"))
  events <- pluck(json_meta_data, "data", "measurement_events")
  start_probe <- tryCatch(ymd_hms(events[1,2]), error=function(err) {cat("No events in file: ", filename); NA})
  stop_probe <- tryCatch(ymd_hms(events[2,2]), error=function(err) {cat("No events in file: ", filename); NA} )
  data <- read_csv(filename, skip = 4, show_col_types = FALSE)
  data$timestamp <- ymd_hms(data[[1]])
  data_wide <- data[,-1]

  data_long <- gather(data_wide, key=type, value=value,-timestamp)

  data_long$type <- ordered(data_long$type, levels=unique(data_long$type))

  return(odorMeasurement$new(data_wide=data_wide, data_long=data_long, baseline_data=baseline_data,
              name=name, start=start, stop=stop,
              start_probe=start_probe, stop_probe=stop_probe, meta_data=json_meta_data))
}
