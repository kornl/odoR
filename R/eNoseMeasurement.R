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
            #' @field baseline.data baseline data
            baseline.data = NULL,
            json.meta.data = NULL,
            name = NULL,
            start = NULL,
            stop = NULL,
            events = NULL,
            start.probe = NULL,
            stop.probe = NULL,
            data_wide = NULL,
            data_long = NULL
          )
  )
