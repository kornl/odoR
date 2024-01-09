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
            #' @field name Name of the measurement. If read from a csv file the value that was in the included JSON meta data in the field name.
            name=NULL,
            #' @field start ...
            start=NULL,
            #' @field stop ...
            stop=NULL,
            #' @field start_probe ...
            start_probe=NULL,
            #' @field stop_probe ...
            stop_probe=NULL,
            #' @field base_line_models ...
            base_line_models=NULL,
            #' @field corrected_data_long ...
            corrected_data_long=NULL,
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
            #' @description
            #' Model the baseline (default: as a polynomial of degree 2)
            #' @param polynomial_degree Degree of polynomial to fit. Default is 2.
            #' @param use_data_after_measurement_for_baseline Should the data after the exposure to the odor also be used?
            #' Default is TRUE to reduce extrapolation errors. If there are still major effects from the odor after the measurement, it could make sense to set this to FALSE.
            model_baseline = function(polynomial_degree=2, use_data_after_measurement_for_baseline=TRUE) {
              base_line_models <- list()
              data_long <- self$data_long
              for(i in 1:64) {
                subdata <- data_long[data_long$type==paste("ch",i,sep="")
                                     & (data_long$timestamp < self$start_probe | (data_long$timestamp > self$stop_probe & use_data_after_measurement_for_baseline)),]
                mod <- lm(value ~ poly(timestamp, 2), data=subdata)
                base_line_models[[i]] <- mod
              }
              self$base_line_models <- base_line_models
              return(invisible(self))            },
            #' @description
            #' Correct the data by subtracting the baseline.
            correct_for_baseline = function() {
              corrected_data_long <- self$data_long
              for(i in 1:64) {
                mod <- self$base_line_models[[i]]
                index <- corrected_data_long$type==paste("ch",i,sep="")
                corrected_data_long[index, "value"] <- corrected_data_long[index, "value"] - predict(mod, corrected_data_long[index,])
              }
              self$corrected_data_long <- corrected_data_long
            },
            #' @description
            #' ...
            calculate_response = function() {
              change <- data_long %>%
                group_by(type) %>%
                summarize(mean = mean(value, na.rm = TRUE))

              change$baseline <- t(baseline_data[, -1])[,1]
              change$change <- change$baseline - change$mean
              self$change <- change
              return(invisible(self))
            },
            #' @description
            #' ...
            calculate_features = function() {

            }
          )
  )



