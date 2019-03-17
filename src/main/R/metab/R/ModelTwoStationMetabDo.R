# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom infmod Model
#' @importFrom R6 R6Class
NULL

# Class ModelTwoStationMetabDo (R6) ####

#' @export
#' 
#' @title
#'    Class ModelTwoStationMetabDo (R6)
#'
#' @description
#'    A model for predicting changes in dissolved oxygen concentrations 
#'    along a stream reach using the two station method.
#' 
#' @usage 
#'    ModelTwoStationMetabDo$new(...)
#' @param dailyGPP 
#'    daily average GPP influence on oxygen 
#'    in grams per cubic meter per day 
#'    (numerical vector with single value)
#' @param dailyER 
#'    daily average ER influence on oxygen
#'    in grams per cubic meter per day 
#'    (numerical vector with single value)
#' @param k600 
#'    gas exchange rate per day for air-water interface
#'    (numerical vector with single value)
#' @param residenceTime
#'    average residence time of water between the upstream
#'    and downstream stations
#'    (numerical vector with single value)
#' @param airPressure 
#'    air pressure in mm of Hg
#'    (numerical vector with single value)
#' @param upstreamDO 
#'    upstream DO concentration in grams per cubic meter
#'    (numerical vector, only first value will be used)
#' @param time 
#'    upstream and downstream times for DO data (and possibly temperature)
#'    (A list of two POSIXct vectors or character vectors in a 
#'    standard text format to be forced to POSIXct "YYYY-MM-DD HH:mm:ss".
#'    The list elements should be named "upstream" and "downstream" for 
#'    the respective times for data at the upstream and downstream end
#'    of the reach.)
#' @param temp 
#'    upstream and downstream water temperature data in degrees Celsius
#'    (A list of numerical vectors named "upstream" and "downstream" for the
#'    temperature data from respective ends of the reach)
#' @param timeTemp
#'    optional upstream and downstream times for temperature data. If values
#'    are not provided, the times for temperature will be assumed to be the 
#'    same times as the DO data.
#'    (A list of two POSIXct vectors or character vectors in a 
#'    standard text format to be forced to POSIXct "YYYY-MM-DD HH:mm:ss".
#'    The list elements should be named "upstream" and "downstream" for 
#'    the respective times for data at the upstream and downstream end
#'    of the reach.)
#' @param par 
#'    photosynthetically active radiation data. Units are arbitrary,
#'    but must be consistent with PAR total if it is specified. Data are
#'    assumed to represent the average PAR over the reach during the
#'    transport of a parcel of water passing the upstream end of the reach.
#'    (numerical vector)
#' @param timePar
#'    times for the PAR data. PAR data will be interpolated to match the upstream
#'    DO data if times do not match.
#'    (POSIXct vector, defaults to be same as time argument)
#' @param timeStepCount 
#'    total number of time steps to which upstream data should be
#'    interpolated. Can be used to reduce the number of time steps calculated for
#'    excessively high resolution data
#'    (numerical vector with single value, defaults to NA)
#' @param parTotal 
#'    optional value of total PAR. Defaults to NA which forces calculation 
#'    of total PAR from the PAR data provided.
#'    (numerical vector with single value)
#' @param timeRange 
#'    vector with 2 values for the start and end time for
#'    the interpolated time steps. Defaults to the start and end time
#'    of the time argument.
#'    (numerical vector with two values)
#' @param doSatFunc
#'    Option to change the function used for calculating saturated DO
#'    concentration from a temperature and air pressure. 
#'    Defaults to \code{\link{doSat}}.
#' @param densityWaterFunc
#'    Option to change the function used for calculating the density of 
#'    water from temperature. Defaults to \code{\link{densityWater}}
#' @param kSchmidtFunc
#'    Option to change the function used for calculating the gas exchange
#'    rate from the gas exchange parameter normalized to a Schmidt number.
#'    Defaults to \code{\link{kSchmidt}}
#' @return 
#'    The object of class \code{ModelTwoStationMetabDo} 
#'    instantiated by the constructor
ModelTwoStationMetabDo <- R6Class(
   classname = "ModelTwoStationMetabDo",
   inherit = Model,
   public = list(
      output = NULL,
      dailyGPP = NULL, 
      dailyER = NULL, 
      k600 = NULL, 
      residenceTime = NULL,
      airPressure = NULL, 
      upstreamDO = NULL, 
      time = NULL, 
      temp = NULL, 
      par = NULL, 
      parTotal = NULL, 
      doSatFunc = NULL,
      densityWaterFunc = NULL,
      kSchmidtFunc = NULL,
      initialize = function(
         dailyGPP, 
         dailyER, 
         k600,
         residenceTime,
         airPressure, 
         upstreamDO, 
         time, 
         temp,
         timeTemp = NA,
         par, 
         timePar,
         timeStepCount = NA, 
         parTotal = NA, 
         timeRange = c(
            time$upstream[1], 
            time$upstream[length(time$upstream)]
            ),
         doSatFunc = doSat,
         densityWaterFunc = densityWater,
         kSchmidtFunc = kSchmidt
         ) {
            #Populate attributes
            self$dailyGPP <- dailyGPP; 
            self$dailyER <- dailyER;
            self$k600 <- k600; 
            self$residenceTime <- residenceTime;
            self$airPressure <- airPressure;
            self$parTotal <- parTotal; 
            self$doSatFunc <- doSatFunc;
            self$densityWaterFunc <- densityWaterFunc;
            self$kSchmidtFunc <- kSchmidtFunc;
            
            # Set up interpolation of data to align with desired
            # upstream times and reach residence times
            upstreamTimeIn <- 
               as.numeric(as.POSIXct(time$upstream)) / 86400;
            downstreamTimeIn <- 
               as.numeric(as.POSIXct(time$downstream)) / 86400;
            downstreamTimeTempIn <- 
               as.numeric(as.POSIXct(time$downstream)) / 86400;
            if (!is.na(timeStepCount)) {
               # A timeStepcount is provided, so upstream DO and temp 
               # data need to be interpolated to the desired time sequence
               timeRangeIn <- 
                  as.numeric(as.POSIXct(timeRange)) / 86400;
               self$time <- list(
                  upstream = seq(
                     from = timeRangeIn[1],
                     to = timeRangeIn[2],
                     length.out = timeStepCount + 1
                  ),
                  downstream = numeric(length = timeStepCount + 1)
               );
               
               self$upstreamDO <- approx(
                  x = upstreamTimeIn, 
                  y = upstreamDO, 
                  xout = self$time$upstream,
                  rule = 2
               )$y;
               
               # Proper interpolation of temperature depends on if a 
               # separate time series was provided for temperature
               if (is.na(timeTemp)) {
                  # If the time series of temperature matches that for
                  # DO
                  self$temp <- list(
                     upstream = approx(
                        x = upstreamTimeIn, 
                        y = temp$upstream, 
                        xout = self$time$upstream,
                        rule = 2
                     )$y,
                     downstream = numeric(length = timeStepCount + 1)
                  );
               } else {
                  # If the time series for temperature was provided
                  upstreamTimeTempIn <- 
                     as.numeric(as.POSIXct(timeTemp$upstream)) / 86400;
                  downstreamTimeTempIn <- 
                     as.numeric(as.POSIXct(timeTemp$downstream)) / 86400;
                  self$temp <- list(
                     upstream = self$temp$upstream <- approx(
                        x = upstreamTimeTempIn, 
                        y = temp$upstream, 
                        xout = self$time$upstream,
                        rule = 2
                        )$y,
                     downstream = numeric(length = timeStepCount + 1)
                  );
               }
            } else {
               # A timeStepCount was not provided, so using the upstream
               # times and DO data provided as the time base
               self$time <- list(
                  upstream = upstreamTimeIn,
                  downstream = numeric(length = length(time$upstream))
               );
               
               self$upstreamDO <- upstreamDO;
               
               if (is.na(timeTemp)) {
                  # If the time series of temperature matches that for
                  # DO
                  self$temp <- list(
                     upstream = temp$upstream,
                     downstream = numeric(length = length(time$upstream))
                  );
               } else {
                  # If the time series for temperature was provided
                  upstreamTimeTempIn <- 
                     as.numeric(as.POSIXct(timeTemp$upstream)) / 86400;
                  downstreamTimeTempIn <- 
                     as.numeric(as.POSIXct(timeTemp$downstream)) / 86400;
                  self$temp <- list(
                     upstream = self$temp$upstream <- approx(
                        x = upstreamTimeTempIn, 
                        y = temp$upstream, 
                        xout = self$time$upstream,
                        rule = 2
                     )$y,
                     downstream = numeric(length = length(time$upstream))
                  );
               }
            }
            
            # Create the downstream arrival times based on 
            # upstream times (potentially adjusted) and the residence time 
            # of the reach between the stations
            self$time$downstream <- self$time$upstream + self$residenceTime;
            
            # Interpolate the downstream temperatures from provided downstream data
            # based on the calculated downstream arrival times
            self$temp$downstream <- approx(
               x = downstreamTimeTempIn, 
               y = temp$downstream, 
               xout = self$time$downstream,
               rule = 2
            )$y;
            
            # Interpolate PAR to align with upstream times
            timeParIn <- as.numeric(as.POSIXct(timePar)) / 86400;
            self$par <- approx(
               x = timeParIn, 
               y = par, 
               xout = self$time$upstream,
               rule = 2
            )$y

            # If total PAR is not provided as an argument, total PAR is calculated
            # over the vector using trapezoidal integration
            # Calculates the time step between data points
            if (is.na(self$parTotal)) {
               dtUpstream <- c(
                  self$time$upstream[2:length(self$time$upstream)] - 
                     self$time$upstream[1:length(self$time$upstream) - 1], 
                  0
               );
               parAverage <- c(
                  0.5 * (
                     self$par[1:length(self$time$upstream) - 1] + 
                        self$par[2:length(self$time$upstream)]
                  ),
                  0
               );
               self$parTotal <- sum(parAverage * dtUpstream);
            }
         },
      run = function() {
            # Calculates the saturated oxygen concentration for the 
            # provided temperatures and air pressure
            upstreamDOSat <- self$doSatFunc(
               self$temp$upstream, 
               self$airPressure, 
               self$densityWaterFunc
            );
            upstreamDODeficit <- upstreamDOSat - self$upstreamDO;
            
            downstreamDOSat <- self$doSatFunc(
               self$temp$downstream, 
               self$airPressure, 
               self$densityWaterFunc
            );
            
            # Calculate the temperature adjusted gas exchange rate from 
            # the k600 rate at a Schmidt number of 600
            k <- self$kSchmidtFunc(
               (self$temp$upstream + self$temp$downstream) / 2, 
               self$k600
            );
           
            # Calculate the DO production and consumption for
            # each time step
            doProduction <- self$dailyGPP * 
               ((self$par * self$residenceTime) / self$parTotal); 
            doConsumption <- rep(
               x = self$residenceTime * self$dailyER,
               times = length(self$time$upstream)
            );

            # Calculate the downstream DO concentration 
            downstreamDO <- 
               (
                  self$upstreamDO +
                     doProduction +
                     doConsumption +
                     self$residenceTime * 
                        k * 
                        ((upstreamDODeficit + downstreamDOSat) / 2)
               ) / 
               (
                  1 + (
                     (self$residenceTime * k) / 2
                  )
               );
            
            self$output <- data.frame(
               time = as.POSIXct(
                  self$time$downstream * 86400, 
                  origin = as.POSIXct("1970-01-01", tz = "GMT")
               ),
               do = downstreamDO,
               doSat = downstreamDOSat,
               doProduction = doProduction,
               doConsumption = doConsumption,
               k = k,
               temp = self$temp$downstream,
               upstreamTime = as.POSIXct(
                  self$time$upstream * 86400, 
                  origin = as.POSIXct("1970-01-01", tz = "GMT")
               ),
               upstreamDO = self$upstreamDO,
               upstreamDOSat = upstreamDOSat,
               upstreamTemp = self$temp$upstream
            );
            return(self$output);
         }
   )
);
