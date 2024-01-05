# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
utils::globalVariables(c("type", "value"))

read.eNase <- function (filename) {
  meta.data <- read_csv(filename, skip = 1, n_max=1, col_names = FALSE, show_col_types = FALSE)
  baseline.data <- read_csv(filename, skip = 3, n_max=1, col_names = FALSE, show_col_types = FALSE)
  json.meta.data <- fromJSON(as.data.frame(meta.data)[1, 2])
  name <- pluck(json.meta.data, "data", "name")
  start <- ymd_hms(pluck(json.meta.data, "data", "started_at"))
  stop <- ymd_hms(pluck(json.meta.data, "data", "stopped_at"))
  events <- pluck(json.meta.data, "data", "measurement_events")
  start.probe <- tryCatch(ymd_hms(events[1,2]), error=function(err) {cat("No events in file: ", filename); NA})
  stop.probe <- tryCatch(ymd_hms(events[2,2]), error=function(err) {cat("No events in file: ", filename); NA} )
  data <- read_csv(filename, skip = 4, show_col_types = FALSE)
  data$timestamp <- ymd_hms(data[[1]])
  data_wide <- data[,-1]

  data_long <- gather(data_wide, key=type, value=value,-timestamp)

  data_long$type <- ordered(data_long$type, levels=unique(data_long$type))

  change <- data_long %>%
    group_by(type) %>%
    summarize(mean = mean(value, na.rm = TRUE))

  change$baseline <- t(baseline.data[, -1])[,1]
  change$change <- change$baseline - change$mean

  return(list(data_wide=data_wide, data_long=data_long, baseline.data=baseline.data, change=change,
              name=name, start=start, stop=stop,
              start.probe=start.probe, stop.probe=stop.probe, meta.data=json.meta.data))
}
