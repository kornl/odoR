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
  R6Class("eNoseMeasurement",
          list(
            #' @field meta.data meta data
            meta.data = NULL,
            #' @field data_wide data in wide format
            data_wide=NULL,
            #' @field data_long data in long format
            data_long=NULL,
            #' @field baseline.data baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.
            baseline.data=NULL,
            #' @field change ...
            change=NULL,
            #' @field name ...
            name=NULL,
            #' @field start ...
            start=NULL,
            #' @field stop ...
            stop=NULL,
            #' @field start.probe ...
            start.probe=NULL,
            #' @field stop.probe ...
            stop.probe=NULL,
            #' @description
            #' Create a eNoseMeasurement object.
            #' @param meta.data meta data
            #' @param data_wide data in wide format
            #' @param data_long data in long format
            #' @param baseline.data baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.
            #' @param change ...
            #' @param name ...
            #' @param start ...
            #' @param stop ...
            #' @param start.probe ...
            #' @param stop.probe ...
            #' @return A new `eNoseMeasurement` object.
            initialize = function(meta.data = NULL,
                                  data_wide=NULL,
                                  data_long=NULL,
                                  baseline.data=NULL,
                                  change=NULL,
                                  name=NULL,
                                  start=NULL,
                                  stop=NULL,
                                  start.probe=NULL,
                                  stop.probe=NULL) {
              self$meta.data <- meta.data
              self$data_wide <- data_wide
              self$data_long <- data_long
              self$baseline.data <- baseline.data
              self$change <- change
              self$name <- name
              self$start <- start
              self$stop <- stop
              self$start.probe <- start.probe
              self$stop.probe <- stop.probe
            }
          )
  )
