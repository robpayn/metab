# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#
#' @importFrom R6 R6Class
#' @importFrom inferno Model
NULL

# Class OneStationMetabDo (R6) ####

#' @export
#' 
#' @title
#'    Class OneStationMetabDo (R6)
#'
#' @description
#'    A model for predicting dissolved oxygen concentrations in
#'    a stream using the one-station method.
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
#' @param initialDO 
#'    Initial DO concentration at the beginning of the evaluation period.
#'    Units depend on the unit conversion factor for DO saturation.
#'    Default value of the unit conversion factor results in
#'    units of micomolarity per day.
#' @param time 
#'    Time vector for data
#'    (POSIXct vector or character vector in a standard text format
#'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
#' @param temp 
#'    Water temperature vector in degrees Celsius
#' @param par 
#'    Photosynthetically active radiation vector. 
#'    Units are arbitrary, but must be consistent with total PAR total if the
#'    optional argument for total PAR is specified.
#' @param timeStepCount 
#'    total number of time steps to which data should be
#'    interpolated, used to reduce the number of time steps calculated for
#'    excessively high resolution data
#'    (numerical vector with single value, defaults to NA)
#' @param timePar 
#'    times for the PAR data, see note about data typein time
#'    argument description
#'    (POSIXct vector, defaults to be same as time argument)
#' @param parTotal 
#'    total PAR, see units note in par argument description
#'    (numerical vector with single value, defaults to NA which forces
#'    calculation of total PAR from PAR data vector)
#' @param timeRange 
#'    vector with 2 values for the start and end time for
#'    the interpolated time steps
#'    (numerical vector with two values, defaults to start and end of 
#'    time argument)
#' @param stdAirPressure
#'    The standard air pressure in the desired units. Defaults to 760 mm Hg.
#' @param doSatUnitConv
#'    The unit conversion to convert the saturated DO concentration from
#'    micromoles per liter. Defaults to 1.
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
#'    Reference to a new OneStationMetabDo object configured with the provided arguments.
#'    
#' @section Extends \code{\link{Model}}:
#'   \code{$run}
#'   \itemize{
#'     \item see \code{\link{Model_run}}
#'     \item see \code{\link{OneStationMetabDo_run}}
#'   }
#'   
OneStationMetabDo <- R6Class(
   classname = "OneStationMetabDo",
   inherit = Model,
   public = list(
      output = NULL,
      dailyGPP = NULL, 
      dailyER = NULL, 
      k600 = NULL, 
      airPressure = NULL,
      initialDO = NULL, 
      time = NULL, 
      temp = NULL, 
      par = NULL, 
      timeStepCount  = NULL, 
      timePar = NULL,
      parTotal = NULL, 
      timeRange = NULL,
      dt = NULL,
      doSatCalculator = NULL,
      kSchmidtFunc = NULL,
      initialize = function
         (
            dailyGPP, 
            dailyER, 
            k600, 
            airPressure, 
            initialDO, 
            time, 
            temp, 
            par, 
            timeStepCount = NA, 
            timePar = time,
            parTotal = NA, 
            timeRange = c(time[1], time[length(time)]),
            stdAirPressure = 760,
            doSatUnitConv = 1,
            doSatCalculator = DoSatCalculator$new(
               unitConvFactor = doSatUnitConv,
               stdAirPressure = stdAirPressure
               ),
            kSchmidtFunc = kSchmidt
         ) 
         {
            self$dailyGPP <- dailyGPP; 
            self$dailyER <- dailyER;
            self$k600 <- k600; 
            self$airPressure <- airPressure;
            self$initialDO <- initialDO;
            self$temp <- temp;
            self$par <- par;
            self$timeStepCount  <- timeStepCount; 
            self$timePar <- timePar;
            self$parTotal <- parTotal; 
            self$timeRange <- timeRange;
            self$doSatCalculator <- doSatCalculator;
            self$kSchmidtFunc <- kSchmidtFunc;
            
            # Interpolates temperature and PAR data to new time step 
            # if the time step is adjusted
            if (!is.na(self$timeStepCount)) {
               timeIn <- as.numeric(as.POSIXct(time)) / 86400;
               self$timeRange <- as.numeric(as.POSIXct(self$timeRange)) / 86400;
               timeParNum <- as.numeric(as.POSIXct(self$timePar)) / 86400;
               self$time <- seq(
                  from = self$timeRange[1], 
                  to = self$timeRange[2], 
                  length.out = self$timeStepCount + 1
               );
               self$temp <- approx(
                  x = timeIn, 
                  y = self$temp, 
                  xout = self$time,
                  rule =2
               )$y;
               self$par <- approx(
                  x = timeParNum, 
                  y = self$par, 
                  xout = self$time,
                  rule = 2
               )$y;
            } else {
               self$time <- as.numeric(as.POSIXct(time)) / 86400;
            }
            
            # Calculates the time step between data points
            self$dt <- c(
               self$time[2:length(self$time)] - self$time[1:length(self$time) - 1], 
               0
            );
            
            # If total PAR is not provided as an argument, total PAR is calculated
            # over the vector using trapezoidal integration
            if (is.na(self$parTotal)) {
               parAverage <- c(
                  0.5 * (self$par[1:length(self$time) - 1] + self$par[2:length(self$time)]),
                  0
               );
               self$parTotal <- sum(parAverage * self$dt);
            }
         }
      )
);

# Method OneStationMetabDo$run ####

#' @name OneStationMetabDo_run
#'
#' @title
#'   Runs a one-station metabolism model
#'
#' @return 
#'    Data frame with incremental and final results of the simulation,
#'    with columns:
#'    \itemize{
#'      \item time: POSIXct simulation time
#'      \item do: dissolved oxygen concentration
#'      \item doSat: Saturated dissolved oxygen concentration
#'      \item doProduction: increase in DO concentration during the time step
#'      \item doConsumption: decrease in DO concentration during the time step
#'      \item k: DO gas exchange rate
#'      \item temp: water temperature in degrees Celsius
#'      \item dt: length of time step
#'    }
#' @section Method of class:
#'   \code{\link{OneStationMetabDo}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Model_run}} - See abstract method for documentation
#'   
OneStationMetabDo$set(
   which = "public",
   name = "run",
   value = function
      () 
      {
         # Calculates the saturated oxygen concentration for the 
         # provided temperatures and air pressure
         doSat <- self$doSatCalculator$calculate(
            temp = self$temp,
            airPressure = self$airPressure
         );
         
         # Calculate the temperature adjusted gas exchange rate from 
         # the k600 rate at a Schmidt number of 600
         k <- self$kSchmidtFunc(self$temp, self$k600);
         
         # specifies length to use for vectors and the data frame based on the length 
         # of time vector
         doPredLength <- length(self$time);
         
         # Creates a data frame to store values calculated in oneStationMetabDO function
         
         self$output <- data.frame(
            time = as.POSIXct(
               self$time * 86400, 
               origin = as.POSIXct("1970-01-01", tz = "GMT")
            ),
            do = numeric(doPredLength),
            doSat = doSat,
            doProduction = numeric(doPredLength),
            doConsumption = numeric(doPredLength),
            k = k,
            temp = self$temp,
            dt = self$dt
         );
         
         # Creates first value for the vector doPred$do  
         self$output$do[1] <- self$initialDO[1];
         
         # Calculate the DO production and consumption for
         # each time step
         self$output$doProduction <- self$dailyGPP * 
            ((self$par * self$dt) / self$parTotal); 
         self$output$doConsumption <- self$dt * self$dailyER;
         
         # Iterates over the time steps to model the change in dissolved
         # oxygen over time (one station method)
         for (i in 2:doPredLength) {
            self$output$do[i] <- self$output$do[i - 1] +
               self$output$doProduction[i - 1] +
               self$output$doConsumption[i - 1] + 
               self$dt[i - 1] * k[i - 1] * (doSat[i - 1] - self$output$do[i - 1]);
         }
         
         return(self$output);
      }
);
