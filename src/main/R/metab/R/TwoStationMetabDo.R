# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#
#' @importFrom R6 R6Class
NULL

# Class TwoStationMetabDo (R6) ####

#' @export
#' 
#' @title
#'    Class TwoStationMetabDo (R6)
#'
#' @description
#'    A model for predicting changes in dissolved oxygen concentrations
#'    over a reach of a stream using the two-station method.
#'    Parameters are for the R6 constructor method ($new).
#' 
#' @param dailyGPP 
#'    Daily average influence of gross primary production on oxygen concentration.
#'    Units depend on the unit conversion factor for DO saturation.
#'    Default value of the unit conversion factor results in
#'    units of micomolarity per day.
#' @param dailyER 
#'    Daily average influence of ecosystem respiration on oxygen.
#'    Units depend on the unit conversion factor for DO saturation.
#'    Default value of the unit conversion factor results in
#'    units of micomolarity per day.
#' @param k600 
#'    Influence of atmospheric gas exchange on oxygen concentration as a first-order rate
#'    depending on saturation deficit. Units are per day.
#' @param airPressure 
#'    Barometric pressure.
#'    Units must be consistent with the units of the optional standard air pressure
#'    argument.
#'    Default value for standard air pressure results in units of mm Hg.
#' @param par 
#'    Photosynthetically active radiation. 
#'    Units are arbitrary, but must be consistent with total PAR total if the
#'    optional argument for total PAR is specified.
#' @param upstreamTime
#'    Time vector for data at upstream location.
#'    (POSIXct vector or character vector in a standard text format
#'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
#' @param upstreamTemp
#'    Vector of temperatures in degrees C at upstream location.
#' @param upstreamDO
#'    Vector of DO concentrations at upstream location.
#' @param downstreamTime
#'    Time vector for data at downstream location.
#'    In relation to upstream times, the downstream times should reflect
#'    the travel times of water over the reach.
#'    (POSIXct vector or character vector in a standard text format
#'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
#' @param downstreamTemp
#'    Vector of temperatures in degrees C at the downstream location.
#' @param parTotal 
#'    Total PAR over the evaluation period. Units must be the same as the PAR
#'    data argument.
#'    Default is the integration of the PAR data provided over the evaluation period.
#' @param doSatUnitConv
#'    The unit conversion used to convert the saturated DO concentration in
#'    micromoles per liter to the units of concentration desired. 
#'    Default value is 1.
#' @param stdAirPressure
#'    The standard air pressure in the desired units. 
#'    Defaults to 760 mm Hg.
#' @param doSatCalculator
#'    The DO saturation calculator used to estimate DO saturation concentrations
#'    at a given temperature and air pressure. 
#'    Defaults to a new instance of \code{\link{DoSatCalculator}} based on
#'    the standard air pressure and saturation concentration unit conversion
#'    arguments.
#' @param kSchmidtFunc
#'    Option to change the function used for calculating the gas exchange
#'    rate from the gas exchange parameter normalized to a Schmidt number.
#'    Defaults to \code{\link{kSchmidt}}
#'
#' @return 
#'    Reference to a new TwoStationMetabDo object configured with the provided arguments.
#'    
#' @section Extends \code{\link{Model}}:
#'   \code{$run}
#'   \itemize{
#'     \item see \code{\link{Model_run}}
#'     \item see \code{\link{TwoStationMetabDo_run}}
#'   }
#'   
TwoStationMetabDo <- R6Class(
   classname = "TwoStationMetabDo",
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
            # Populate attributes
            self$dailyGPP <- dailyGPP; 
            self$dailyER <- dailyER;
            self$k600 <- k600; 
            self$airPressure <- airPressure;
            self$par <- par;
            self$parTotal <- parTotal; 
            
            # Time attribute should be POSIXct
            self$upstreamTime <- 
               as.numeric(as.POSIXct(upstreamTime)) / 86400;
            self$upstreamTemp <- upstreamTemp;
            self$upstreamDO <- upstreamDO;
            
            # Time attribute should be POSIXct
            self$downstreamTime <- 
               as.numeric(as.POSIXct(downstreamTime)) / 86400;
            self$downstreamTemp <- downstreamTemp;
            
            self$doSatCalculator <- doSatCalculator;
            self$kSchmidtFunc <- kSchmidtFunc;

            # Use the default or provided saturation calculator to
            # calculate the saturation DO concentration attribute.
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

# Method TwoStationMetabDo$run ####

#' @name TwoStationMetabDo_run
#'
#' @title
#'   Runs a two-station metabolism model
#'
#' @return 
#'    Data frame with incremental and final results of the simulation,
#'    with columns:
#'    \itemize{
#'      \item time: POSIXct simulation time
#'      \item residenceTime: length of time between upstream and downstream times
#'      \item do: dissolved oxygen concentration
#'      \item doProduction: increase in DO concentration in travel over the reach
#'      \item doConsumption: decrease in DO concentration in travel over the reach
#'      \item doEquilibration: two-station term for gas exchange in travel over the reach
#'      \item k: DO gas exchange applied in travel over the reach
#'    }
#'    
#' @section Method of class:
#'   \code{\link{TwoStationMetabDo}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Model_run}} - See abstract method for documentation
#'   
TwoStationMetabDo$set(
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

