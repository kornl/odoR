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
            #' @field response data.frame of the calculated responses from this measurement for each sensor
            response=NULL,
            #' @field name Name of the measurement. If read from a csv file the value that was in the included JSON meta data in the field name.
            name=NULL,
            #' @field start POSIXct object representing the start time of the measurement
            start=NULL,
            #' @field stop POSIXct object representing the stop/end time of the measurement
            stop=NULL,
            #' @field start_exposure POSIXct object representing the start time of exposure to a test substance
            start_exposure=NULL,
            #' @field stop_exposure POSIXct object representing the end time of exposure to a test substance
            stop_exposure=NULL,
            #' @field base_line_models list of models to fit the base line
            base_line_models=NULL,
            #' @field corrected_data_long baseline corrected or residualized data
            corrected_data_long=NULL,
            #' @field features Summarized responses by features
            features=NULL,
            #' @description
            #' Create a odorMeasurement object.
            #' @param meta_data meta data
            #' @param data_wide data in wide format
            #' @param data_long data in long format
            #' @param baseline_data baseline data as given in the header. Perhaps not that useful, see the discussion about falling response values.
            #' @param name Name of the measurement as character string
            #' @param start POSIXct object representing the start time of the measurement
            #' @param stop POSIXct object representing the stop/end time of the measurement
            #' @param start_exposure POSIXct object representing the start time of exposure to a test substance
            #' @param stop_exposure POSIXct object representing the end time of exposure to a test substance
            #' @return A new `odorMeasurement` object.
            initialize = function(meta_data = NULL,
                                  data_wide=NULL,
                                  data_long=NULL,
                                  baseline_data=NULL,
                                  name=NULL,
                                  start=NULL,
                                  stop=NULL,
                                  start_exposure=NULL,
                                  stop_exposure=NULL) {
              self$meta_data <- meta_data
              self$data_wide <- data_wide
              self$data_long <- data_long
              self$baseline_data <- baseline_data
              self$name <- name
              self$start <- start
              self$stop <- stop
              self$start_exposure <- start_exposure
              self$stop_exposure <- stop_exposure
            },
            #' @description
            #' Model the baseline (default: as a polynomial of degree 2)
            #' @param use_data_after_measurement_for_baseline Should the data after the exposure to the odor also be used?
            #' Default is TRUE to reduce extrapolation errors. If there are still major effects from the odor after the measurement, it could make sense to set this to FALSE.
            #' @param method Either exponential, polynomial or power-law.
            #' @param polynomial_degree Only used if method="polynomial". Degree of polynomial to fit. Default is 2.
            model_baseline = function(use_data_after_measurement_for_baseline=TRUE, method="polynomial", polynomial_degree=2) {
              base_line_models <- list()
              data_long <- self$data_long
              for(i in 1:64) {
                subdata <- data_long[data_long$channel==paste("ch",i,sep="")
                                     & (data_long$timestamp < self$start_exposure | (data_long$timestamp > self$stop_exposure & use_data_after_measurement_for_baseline)),]
                subdata$time <- as.numeric(difftime(subdata$timestamp, data_long$timestamp[1], units = "secs"))
                if (!(method %in% c("exponential", "polynomial", "power-law"))) stop("Method must be either exponential, polynomial or power-law.")
                if (method == "polynomial") {
                  mod <- lm(value ~ poly(timestamp, polynomial_degree), data=subdata)
                } else if (method == "exponential") {
                  c_est = min(subdata$value)-1
                  a_est = max(subdata$value)-c_est
                  mod1 <- lm(log(value-c_est) ~ time, data=subdata)
                  b_est = -coef(mod1)[2]
                  mod <- nls(value ~ a * exp(-b * time) + c, start=list(a=a_est, b=b_est, c=c_est), data=subdata)
                } else if (method == "power-law") {
                  c_est = min(subdata$value)-1
                  mod1 <- lm(log(value-c_est) ~ log(time+1), data=subdata)
                  b_est = -coef(mod1)[2]
                  a_est = exp(coef(mod1)[1])
                  mod <- nls(value ~ a * time^(-b) + c, start=list(a=a_est, b=b_est, c=c_est), data=subdata)
                }
                base_line_models[[i]] <- mod
              }
              self$base_line_models <- base_line_models
              return(invisible(self))
            },
            #' @description
            #' Residualize.
            residualize = function() {
              data_wide <- self$data_wide
              for (i in (1:64)[-base_channels]) {
                y <- paste("ch", i, sep="")
                f <- as.formula(
                  paste(y,
                        paste(c(paste("ch", base_channels, sep=""), "humidity", "temperature"), collapse = " + "),
                        sep = " ~ "))
                mod <- lm(f, data = data_wide)
                data_wide[[y]] <- residuals(mod)
                attr(data_wide[[y]], "names") <- NULL
              }
              corrected_data_long <- gather(data_wide, key=channel, value=value,-timestamp)
              corrected_data_long$feature <- channel2feature(corrected_data_long$channel)
              self$corrected_data_long <- corrected_data_long
            },
            #' @description
            #' Correct the data by subtracting the baseline.
            correct_for_baseline = function() {
              corrected_data_long <- self$data_long
              for(i in 1:64) {
                mod <- self$base_line_models[[i]]
                index <- corrected_data_long$channel==paste("ch",i,sep="")
                corrected_data_long[index, "value"] <- corrected_data_long[index, "value"] - predict(mod, corrected_data_long[index,])
              }
              self$corrected_data_long <- corrected_data_long
            },
            #' @description
            #' Calculate the responses for each sensor as the mean of the values of the corrected data (if existing - otherwise the raw data)
            #' between the start_exposure and stop_exposure events.
            calculate_response = function() {
              if (!is.null(self$corrected_data_long)) {
                dat <- self$corrected_data_long
              } else {
                dat <-self$data_long
              }
              response <- c()
              for(i in 1:64) {
                subdata <- dat[dat$channel==paste("ch",i,sep="")
                                     & dat$timestamp > self$start_exposure & dat$timestamp < self$stop_exposure, ]
                response <- rbind(response, data.frame(channel=paste("ch",i,sep=""), value=mean(subdata$value)))
              }
              response$feature <- channel2feature(response$channel)
              response <- response[response$feature!="base",]
              self$response <- response
              return(invisible(self))
            },
            #' @description
            #' Calculate features by summarizing the responses by feature.
            calculate_features = function() {
              if (is.null(self$response)) self$calculate_response()
              self$features <- self$response %>%
                group_by(feature) %>%
                summarize(mean_value = mean(value, na.rm = TRUE))
            },
            #' @description
            #' Getter method for the features. Calculates the features if they are not yet existing.
            get_features = function() {
              if (is.null(self$features)) self$calculate_features()
              return(self$features)
            },
            #' @description
            #' Draws a radar chart of the features.
            spider_plot = function() {
              if (is.null(self$features)) self$calculate_features()
              self$features$max <- max(self$features$mean_value)
              self$features$min <- min(self$features$mean_value)
              df <- as.data.frame(t(dat$features[,c("max", "min", "mean_value")]))
              colnames(df) <- dat$features$feature
              radarchart(df, title=self$name)
            }
          )
  )



