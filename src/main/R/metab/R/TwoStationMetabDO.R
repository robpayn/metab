# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#' @importFrom inferno Model
NULL

#' @export
#' 
TwoStationMetabDO <- R6Class(
   classname = "TwoStationMetabDo",
   inherit = Model,
   public = list(
      output = NULL,
      dailyGPP = NULL, 
      dailyER = NULL, 
      k600 = NULL, 
      airPressure = NULL,
      par = NULL, 
      parTotal = NULL, 
      upstreamTime = NULL,
      upstreamTemp = NULL,
      upstreamDO = NULL, 
      upstreamDOSat = NULL,
      downstreamTime = NULL, 
      downstreamTemp = NULL,
      downstreamDOSat = NULL,
      doSatCalculator = NULL,
      kSchmidtFunc = NULL,
      initialize = function
         (
            dailyGPP, 
            dailyER, 
            k600,
            airPressure, 
            par,
            upstreamTime, 
            upstreamTemp,
            upstreamDO,
            downstreamTime,
            downstreamTemp,
            parTotal = NA,
            doSatUnitConv = 1,
            stdAirPressure = 760,
            doSatCalculator = DoSatCalculator$new(
               unitConvFactor = doSatUnitConv,
               stdAirPressure = stdAirPressure
               ),
            kSchmidtFunc = kSchmidt
         ) 
         {
            #Populate attributes
            self$dailyGPP <- dailyGPP; 
            self$dailyER <- dailyER;
            self$k600 <- k600; 
            self$airPressure <- airPressure;
            self$par <- par;
            self$parTotal <- parTotal; 
            
            self$upstreamTime <- 
               as.numeric(as.POSIXct(upstreamTime)) / 86400;
            self$upstreamTemp <- upstreamTemp;
            self$upstreamDO <- upstreamDO;
            
            self$downstreamTime <- 
               as.numeric(as.POSIXct(downstreamTime)) / 86400;
            self$downstreamTemp <- downstreamTemp;
            self$doSatCalculator <- doSatCalculator;
            self$kSchmidtFunc <- kSchmidtFunc;

            self$upstreamDOSat <- self$doSatCalculator$calculate(
               self$upstreamTemp, 
               self$airPressure 
            );
            self$downstreamDOSat <- self$doSatCalculator$calculate(
               self$downstreamTemp, 
               self$airPressure
            );
            
            # If total PAR is not provided as an argument, total PAR is calculated
            # over the vector using trapezoidal integration
            # Calculates the time step between data points
            if (is.na(self$parTotal)) {
               dtUpstream <- c(
                  self$upstreamTime[2:length(self$upstreamTime)] - 
                     self$upstreamTime[1:length(self$upstreamTime) - 1], 
                  0
               );
               parAverage <- c(
                  0.5 * (
                     self$par[1:length(self$upstreamTime) - 1] + 
                        self$par[2:length(self$upstreamTime)]
                     ),
                  0
               );
               self$parTotal <- sum(parAverage * dtUpstream);
            }
         }
      )
);

# Method ModelTwoStationMetabDo$run ####

TwoStationMetabDO$set(
   which = "public",
   name = "run",
   value = function() 
      {
         upstreamDODeficit <- self$upstreamDOSat - self$upstreamDO;
         
         # Calculate the temperature adjusted gas exchange rate from 
         # the k600 rate at a Schmidt number of 600
         k <- self$kSchmidtFunc(
            0.5 * (self$upstreamTemp + self$downstreamTemp), 
            self$k600
         );
         
         # Calculate the residence time of each parcel of water
         residenceTime <- self$downstreamTime - self$upstreamTime;
         
         # Calculate the DO production and consumption for
         # each time step
         doProduction <- self$dailyGPP * 
            ((self$par * residenceTime) / self$parTotal); 
         doConsumption <- residenceTime * self$dailyER;
         doEquilibration <- residenceTime * k * 
            (0.5 * (upstreamDODeficit + self$downstreamDOSat));
         
         # Calculate the downstream DO concentration 
         downstreamDO <- 
            (
               self$upstreamDO +
               doProduction +
               doConsumption +
               doEquilibration
            ) / 
            (
               1 + (0.5 * (residenceTime * k))
            );
         
         self$output <- data.frame(
            time = as.POSIXct(
               self$downstreamTime * 86400, 
               origin = as.POSIXct("1970-01-01", tz = "GMT")
            ),
            residenceTime = residenceTime,
            do = downstreamDO,
            doProduction = doProduction,
            doConsumption = doConsumption,
            doEquilibration = doEquilibration,
            k = k
         );
         return(self$output);
      }
);

