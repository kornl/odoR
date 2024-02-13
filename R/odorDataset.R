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
              self$measurements[[length(dataset$measurements)+1]] <- list(measurement=measurement, label=label, concentration=concentration)
            },
            #' @description
            #' Draws combined spiderplot of all features in this data set.
            spider_plot = function() {
              df <- c()
              labels <- c()
              for (m in measurements) {
                if (is.null(df)) {
                  df <- m$measurement$get_features()
                  df$max <- NULL
                  df$min <- NULL
                } else {
                  df <- full_join(df, m$measurement$get_features()[c("feature", "mean_value")], by = "feature")
                }
                labels <- c(labels, m$label)
              }
              df$max <- max(df[,-1])
              df$min <- min(df[,-1])
              df2 <- as.data.frame(df[c("max", "min", setdiff(names(df), c("max", "min", "feature")))])
              colnames(df2) <- c("max", "min", labels)
              rownames(df2) <- df$feature
              radarchart(as.data.frame(t(df2)), maxmin=TRUE)
            }
          )
  )



