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

  data_long$feature <- c("ch1"="base", "ch2"="base",
                         "ch3"="f11", "ch4"="f11", "ch5"="f11",
                         "ch6"="f3", "ch7"="f3", "ch8"="f3",
                         "ch9"="f2", "ch10"="f2", "ch11"="f2",
                         "ch12"="f1", "ch13"="f1", "ch14"="f1",
                         "ch15"="base", "ch16"="base",
                         "ch17"="base", "ch18"="base",
                         "ch19"="base", "ch20"="base", "ch21"="base",
                         "ch22"="f9", "ch23"="f9", "ch24"="f9",
                         "ch25"="f6", "ch26"="f6", "ch27"="f6",
                         "ch28"="f5", "ch29"="f5", "ch30"="f5",
                         "ch31"="base", "ch32"="base",
                         "ch33"="base", "ch34"="base",
                         "ch35"="f14", "ch36"="f14", "ch37"="f14",
                         "ch38"="f10", "ch39"="f10", "ch40"="f10",
                         "ch41"="f7", "ch42"="f7", "ch43"="f7",
                         "ch44"="f4", "ch45"="f4", "ch46"="f4",
                         "ch47"="base", "ch48"="base",
                         "ch49"="base", "ch50"="base",
                         "ch51"="f15", "ch52"="f15", "ch53"="f15",
                         "ch54"="f13", "ch55"="f13", "ch56"="f13",
                         "ch57"="f12", "ch58"="f12", "ch59"="f12",
                         "ch60"="f8", "ch61"="f8", "ch62"="f8",
                         "ch63"="base", "ch64"="base",
                         )[data_long$type]

  data_long$feature <- ordered(data_long$feature, levels=sort(unique(data_long$feature)))
  data_long$type <- ordered(data_long$type, levels=unique(data_long$type))


  return(odorMeasurement$new(data_wide=data_wide, data_long=data_long, baseline_data=baseline_data,
              name=name, start=start, stop=stop,
              start_probe=start_probe, stop_probe=stop_probe, meta_data=json_meta_data))
}
