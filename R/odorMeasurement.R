#' R6 Class representing a measurement
#'
#' R6 Class representing a measurement
#'
#' For details ...
#'
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @examples
#'
#' rnorm(100)
#'
#' @export odorMeasurement
odorMeasurement <-
  R6Class("odorMeasurement",
          list(
            #' @field meta_data meta data
            meta_data = NULL,
            #' @field data_wide data in wide format
            data_wide=NULL,
            #' @field data_long data in long format
            data_long=NULL,
            #' @field baseline_data baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.
            baseline_data=NULL,
            #' @field change ...
            change=NULL,
            #' @field name ...
            name=NULL,
            #' @field start ...
            start=NULL,
            #' @field stop ...
            stop=NULL,
            #' @field start_probe ...
            start_probe=NULL,
            #' @field stop_probe ...
            stop_probe=NULL,
            #' @description
            #' Create a eNoseMeasurement object.
            #' @param meta_data meta data
            #' @param data_wide data in wide format
            #' @param data_long data in long format
            #' @param baseline_data baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.
            #' @param change ...
            #' @param name ...
            #' @param start ...
            #' @param stop ...
            #' @param start_probe ...
            #' @param stop_probe ...
            #' @return A new `eNoseMeasurement` object.
            initialize = function(meta_data = NULL,
                                  data_wide=NULL,
                                  data_long=NULL,
                                  baseline_data=NULL,
                                  name=NULL,
                                  start=NULL,
                                  stop=NULL,
                                  start_probe=NULL,
                                  stop_probe=NULL) {
              self$meta_data <- meta_data
              self$data_wide <- data_wide
              self$data_long <- data_long
              self$baseline_data <- baseline_data
              self$name <- name
              self$start <- start
              self$stop <- stop
              self$start_probe <- start_probe
              self$stop_probe <- stop_probe
            },
            model_baseline = function() {
              return(invisible(self))
            },
            calculate_response = function() {
              change <- data_long %>%
                group_by(type) %>%
                summarize(mean = mean(value, na.rm = TRUE))

              change$baseline <- t(baseline_data[, -1])[,1]
              change$change <- change$baseline - change$mean
              self$change <- change
              return(invisible(self))
            }
          )
  )



