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
#' @export eNoseMeasurement
eNoseMeasurement <-
  R6Class("eNoseMeasurement",
          list(
            #' @field meta.data meta data
            meta.data = NULL,
            #' @field data_wide data in wide format
            data_wide=NULL,
            data_long=NULL,
            baseline.data=NULL,
            change=NULL,
            name=NULL,
            start=NULL,
            stop=NULL,
            start.probe=NULL,
            stop.probe=NULL,
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
