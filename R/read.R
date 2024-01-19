channel2feature <- function(x) {
  return(c("ch1"="base", "ch2"="base",
    "ch3"="f11", "ch4"="f11", "ch5"="f11",
    "ch6"="f03", "ch7"="f03", "ch8"="f03",
    "ch9"="f02", "ch10"="f02", "ch11"="f02",
    "ch12"="f01", "ch13"="f01", "ch14"="f01",
    "ch15"="base", "ch16"="base",
    "ch17"="base", "ch18"="base",
    "ch19"="base", "ch20"="base", "ch21"="base",
    "ch22"="f09", "ch23"="f09", "ch24"="f09",
    "ch25"="f06", "ch26"="f06", "ch27"="f06",
    "ch28"="f05", "ch29"="f05", "ch30"="f05",
    "ch31"="base", "ch32"="base",
    "ch33"="base", "ch34"="base",
    "ch35"="f14", "ch36"="f14", "ch37"="f14",
    "ch38"="f10", "ch39"="f10", "ch40"="f10",
    "ch41"="f07", "ch42"="f07", "ch43"="f07",
    "ch44"="f04", "ch45"="f04", "ch46"="f04",
    "ch47"="base", "ch48"="base",
    "ch49"="base", "ch50"="base",
    "ch51"="f15", "ch52"="f15", "ch53"="f15",
    "ch54"="f13", "ch55"="f13", "ch56"="f13",
    "ch57"="f12", "ch58"="f12", "ch59"="f12",
    "ch60"="f08", "ch61"="f08", "ch62"="f08",
    "ch63"="base", "ch64"="base",
    "humidity"="humidity", "temperature"="temperature"
  )[x])
}

base_channels <- c(1, 2, 15, 16, 17, 18, 19, 20, 21, 31, 32, 33, 34, 47, 48, 49, 50, 63, 64)

utils::globalVariables(c("channel", "value"))

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
  start_probe <- tryCatch(ymd_hms(events[1,2]), error=function(err) {warning(paste("No start event in file:", filename, sep=" ")); NA})
  stop_probe <- tryCatch(ymd_hms(events[2,2]), error=function(err) {warning(paste("No end event in file:", filename, sep=" ")); NA} )
  data <- read_csv(filename, skip = 4, show_col_types = FALSE)
  data$timestamp <- ymd_hms(data[[1]])
  data_wide <- data[,-1]

  data_long <- gather(data_wide, key=channel, value=value,-timestamp)

  data_long$feature <- channel2feature(data_long$channel)

  data_long$feature <- ordered(data_long$feature, levels=sort(unique(data_long$feature)))
  data_long$channel <- ordered(data_long$channel, levels=unique(data_long$channel))

  firmware_version <- json_meta_data$data$device_info$device_firmware_info$version
  if (is.character(firmware_version) && compareVersion(firmware_version, "2.1.1")>0)
    warning("Firmware version is old - newer firmware can give better results!")

  return(odorMeasurement$new(data_wide=data_wide, data_long=data_long, baseline_data=baseline_data,
              name=name, start=start, stop=stop,
              start_probe=start_probe, stop_probe=stop_probe, meta_data=json_meta_data))
}
