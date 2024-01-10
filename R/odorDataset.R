#' R6 Class representing a data set of measurements
#'
#' R6 Class representing a data set of measurements
#'
#' For details ...
#'
#' @author Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @examples
#'
#' rnorm(100)
#'
#' @export odorDataset
odorDataset <-
  R6Class("odorDataset",
          list(
            #' @field measurements list of measurements
            measurements = list(),
            #' @description
            #' Create a odorDataset object.
            #' @return A new `eNoseMeasurement` object.
            initialize = function() {
            },
            #' @description
            #' Model the baseline (default: as a polynomial of degree 2)
            #' @param measurement Measurement of type odorMeasurement to add.
            #' @param label Optional label of sample that was measured.
            #' @param concentration Optional numerical value of sample concentration that was measured.
            add_measurement = function(measurement, label=NA, concentration=NA) {
              self$measurements[[length(dataset$measurements)+1]] <- list(measurement, label, concentration)
            },
            #' @description
            #' ...
            spider_plot = function() {
              if (is.null(self$features)) self$calculate_features()
              self$features$max <- max(self$features$mean_value)
              self$features$min <- min(self$features$mean_value)
              df <- as.data.frame(t(dat$features[,c("max", "min", "mean_value")]))
              colnames(df) <- dat$features$feature
              radarchart(df)
            }
          )
  )



