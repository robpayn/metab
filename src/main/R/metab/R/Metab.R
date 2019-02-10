# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom infmod Model
#' @importFrom R6 R6Class
NULL

# Utility functions ####

#' @export
#' 
#' @title
#'    Calculates the density of water
#' 
#' @description 
#'    Density of water is calculated based on an empirical 
#'    relationship with the temperature of water
#'    
#' @param temp 
#'    Numerical vector of temperatures (deg C) 
#' @return 
#'    Numerical vector of densities of water in kilograms per liter
densityWater <- function(temp) 
{
   return (
      0.999842 +
      6.7940e-5 * temp -
      9.0953e-6 * temp^2 +
      1.0017e-7 * temp^3 -
      1.1201e-9 * temp^4 +
      6.5363e-12 * temp^5
   );
}

#' @export
#' 
#' @title
#'    Calculates saturated oxygen concentration 
#' 
#' @description 
#'    Saturated oxygen concentration is calculated based 
#'    on an empirical relationship with water temperature
#'    
#' @param temp 
#'    Numerical vector of water temperatures (deg C)
#' @param airPressure 
#'    Numerical vector of air pressures (mm Hg)
#' @return 
#'    Numerical vector of saturation concentrations for oxygen 
#'    in grams per cubic meter
#'    
#' @details 
#'    Vectors for temperature and air pressure are assumed 
#'    to be of the same length, or values will be reused
doSat <- function(temp, airPressure, densityWaterFunc) 
{
   normTemp <- log((298.15 - temp) / (273.15 + temp));
   doSat <- 0.032 * densityWaterFunc(temp) *
      exp(
         5.80871 +
         3.20291 * normTemp +
         4.17887 * normTemp^2 +
         5.10006 * normTemp^3 -
         0.0986643 * normTemp^4 +
         3.80369 * normTemp^5
      );
   doSat <- doSat * (airPressure / 760);
   return(doSat);
}

#' @export
#' 
#' @title
#'    Calculates temperature adjusted gas exchange
#' 
#' @description 
#'    Adjusts a gas exchange velocity or rate based on temperature
#'    and the provided gas exchange velocity or rate at a Schmidt number of 600
#'    
#' @param temp 
#'    Water temperature in degrees Celsius
#'    (numerical vector)
#' @param k600 
#'    Gas exchange rate (per time) or velocity (length per time)
#'    at a Schmidt number of 600
#'    (numerical vector)
#' @return 
#'    Numerical vector of temperature corrected gas exchange rates 
#'    in same units as k600 argument
kSchmidt <- function(temp, k600)
{
   schmidt <- 1800.6 -
      120.1 * temp +
      3.7818 * temp^2 -
      0.047608 * temp^3;
   return (k600 * (schmidt / 600)^-0.5);
}

# Class ParameterProcessorMetab (R6) ####

#' @export
#' 
#' @title 
#'    Class ParameterProcessorMetab
#'
#' @description 
#'    A parameter processor allowing GPP, ER, and k600 to be used
#'    as proposed parameters for an objective function using a
#'    stream metabolism model.
#' 
#' @usage 
#'    ParameterProcessorMetab$new()
#' @return 
#'    The object of class \code{ParameterProcessorMetab} 
#'    instantiated by the constructor
ParameterProcessorMetab <- R6Class(
   classname = "ParameterProcessorMetab",
   inherit = ParameterProcessor,
   public = list(
      process = function(params) 
         {
            self$model$dailyGPP <- params[1];
            self$model$dailyER <- params[2];
            self$model$k600 <- params[3];
         }
   )
);

# Class PredictionProcessorMetabDo (R6) ####

#' @export
#' 
#' @title 
#'    Class PredictionProcessorMetabDo (R6)
#'
#' @description 
#'    A prediction processor allowing dissolved oxygen outputs
#'    from a stream metabolism model to be used as predictions
#'    in an objective function.
#' 
#' @usage 
#'    PredictionProcessorMetabDo$new()
#' @return 
#'    The object of class \code{PredictionProcessorMetabDo} 
#'    instantiated by the constructor
PredictionProcessorMetabDo <- R6Class(
   classname = "PredictionProcessorMetabDoDic",
   inherit = PredictionProcessor,
   public = list(
      process = function() 
         {
            return(data.frame(
               do = self$model$output$do
            ));
         }
   )
);

# Class PredictionProcessorMetabDoDic (R6) ####

#' @export
#' 
#' @title 
#'    Class PredictionProcessorMetabDoDic (R6)
#'
#' @description 
#'    A prediction processor allowing dissolved oxygen and carbo
#'    dioxide outputs from a stream metabolism model to be used as 
#'    predictions in an objective function.
#' 
#' @usage 
#'    PredictionProcessorMetabDoDic$new()
#' @return 
#'    The object of class \code{PredictionProcessorMetabDoDic} 
#'    instantiated by the constructor
PredictionProcessorMetabDoDic <- R6Class(
   classname = "PredictionProcessorMetabDoDic",
   inherit = PredictionProcessor,
   public = list(
      process = function() 
         {
            return(data.frame(
               do = self$model$output$do,
               pCO2 = self$model$output$pCO2
            ));
         }
   )
);

# Class ModelOneStationMetabDo (R6) ####

#' @export
#' 
#' @title
#'    Class ModelOneStationMetabDo (R6)
#'
#' @description
#'    A model for predicting dissolved oxygen concentrations in
#'    a stream using the one station method.
#' 
#' @usage 
#'    ModelOneStationMetabDo$new(...)
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
#' @param airPressure 
#'    air pressure in mm of Hg
#'    (numerical vector with single value)
#' @param initialDO 
#'    initial DO concentration in grams per cubic meter
#'    (numerical vector, only first value will be used)
#' @param time 
#'    times for temperature and par data
#'    (POSIXct vector or character vector in a standard text format
#'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
#' @param temp 
#'    water temperature data in degrees Celsius
#'    (numerical vector)
#' @param par 
#'    photosynthetically active radiation data. Units are arbitrary,
#'    but must be consistent with PAR total if it is specified
#'    (numerical vector)
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
#'    The object of class \code{ModelOneStationMetabDo} 
#'    instantiated by the constructor
ModelOneStationMetabDo <- R6Class(
   classname = "ModelOneStationMetabDo",
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
      doSatFunc = NULL,
      densityWaterFunc = NULL,
      kSchmidtFunc = NULL,
      initialize = function(
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
         doSatFunc = doSat,
         densityWaterFunc = densityWater,
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
            self$doSatFunc <- doSatFunc;
            self$densityWaterFunc <- densityWaterFunc;
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
         },
      run = function() 
         {
            # Calculates the saturated oxygen concentration for the 
            # provided temperatures and air pressure
            doSat <- self$doSatFunc(self$temp, self$airPressure, self$densityWaterFunc) ;
            
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
   )
)

#' @name ModelOneStationMetabDo_run
#' 
#' @title
#'    Runs the model (R6 method)
#' 
#' @description 
#'    Runs the model predicting the change in concentration of oxygen
#'    at a location in a stream (one-station approach)
#' 
#' @return 
#'    Data frame with incremental and final results of the simulation,
#'    with columns \cr
#'    time: POSIXct simulation time \cr
#'    do: dissolved oxygen concentration in grams per cubic meter \cr
#'    doSat: Saturated dissolved oxygen concentration in grams per cubic meter \cr
#'    doProduction: increase in DO concentration during the time step in 
#'       grams per cubic meter \cr
#'    doConsumption: decrease in DO concentration during the time step in
#'       grams per cubic meter \cr
#'    k: gas exchange rate per day for oxygen \cr
#'    temp: water temperature in degrees Celsius \cr
#'    dt: length of time step
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

#' @name ModelOneStationMetabDo_run
#' 
#' @title
#'    Runs the model (R6 method)
#' 
#' @description 
#'    Runs a model that predicts the downstream oxygenn concentration
#'    from upstream concentrations and metabolic parameters (two-station approach)
#' 
#' @return 
#'    Data frame with incremental and final results of the simulation,
#'    with columns \cr
#'    time: POSIXct simulation time \cr
#'    do: dissolved oxygen concentration in grams per cubic meter \cr
#'    doSat: Saturated dissolved oxygen concentration in grams per cubic meter \cr
#'    doProduction: increase in DO concentration during the time step in 
#'       grams per cubic meter \cr
#'    doConsumption: decrease in DO concentration during the time step in
#'       grams per cubic meter \cr
#'    k: gas exchange rate per day for oxygen \cr
#'    temp: water temperature in degrees Celsius \cr
#'    dt: length of time step
NULL

# Class ModelOneStationMetabDoDic (R6) ####

#' @export
#' 
#' @title
#'    Class ModelOneStationMetabDoDic (R6)
#' 
#' @description
#'    A model for predicting dissolved oxygen and carbon dioxide concentrations in
#'    a stream using the one station method.
#' 
#' @usage 
#'    ModelOneStationMetabDoDic$new(...)
#' @param ... 
#'    Arguments passed to constructor \code{ModelOneStationMetabDoDic$new(...)} will be 
#'    passed generically to the constructor for the superclass \code{ModelOneStationMetabDo}. 
#'    See documentation for the class \code{\link{ModelOneStationMetabDo}} for a description
#'    of these arguments.
#' @param initialDIC 
#'    initial DIC concentration in micromoles per liter
#'    (numerical vector, only first value will be used)
#' @param pCO2air 
#'    partial pressure of CO2 in the air in microatmospheres
#'    (numerical vector)
#' @param alkalinity 
#'    alkalinity of stream water
#'    (numerical vector)
#' @param RQ 
#'    Respiratory quotient
#'    (single value numerical vector, default value is 0.85)
#' @param PQ 
#'    Photosynthetic quotient
#'    (single value numerical vector, default value is 1.22)
#' @return 
#'    The object of class \code{ModelOneStationMetabDoDic} 
#'    instantiated by the constructor
ModelOneStationMetabDoDic <- R6Class(
   classname = "ModelOneStationMetabDoDIC",
   inherit = ModelOneStationMetabDo,
   public = list(
      initialDIC = NULL, 
      pCO2air = NULL, 
      alkalinity = NULL,
      RQ = NULL, 
      PQ = NULL,
      carbonateEq = NULL,
      kHenryCO2fromTempFunc = NULL,
      initialize = function(
         ..., 
         initialDIC, 
         pCO2air, 
         alkalinity,
         RQ = 0.85, 
         PQ = 1.22,
         kHenryCO2fromTempFunc = kHenryCO2fromTemp
         ) 
         {
            super$initialize(...);
            self$carbonateEq <- CarbonateEq$new(tempC = self$temp[1]);
            self$initialDIC <- initialDIC;
            self$pCO2air <- pCO2air;
            self$alkalinity <- alkalinity;
            self$RQ <- RQ;
            self$PQ <- PQ;
            self$kHenryCO2fromTempFunc <- kHenryCO2fromTempFunc;
         },
      run = function()
         {
            # Run the superclass one station metabolism model for DO
            super$run();
           
            kH <- self$kHenryCO2fromTempFunc(
               tempK = self$temp, 
               convertC = TRUE
               );
         
            # Set up the data frame that will be returned
            dicPredLength <- length(self$time);
            self$output <- data.frame(
               self$output, 
               co2Production = -self$output$doConsumption * 31.25 * self$RQ,
               co2Consumption = -self$output$doProduction * 31.25 / self$PQ,
               kH = kH,
               co2Sat = self$pCO2air * kH,
               fGas = numeric(length = dicPredLength),
               pH = numeric(length = dicPredLength),
               pCO2 = numeric(length = dicPredLength),
               dic = numeric(length = dicPredLength)
               );
         
            # Create the carbonate equilibrium object with the initial 
            # temperature
            self$carbonateEq$resetFromTemp(tempC = self$temp[1]);
            
            # Set the initial DIC concentration, pCO2, and fgas
            self$output$dic[1] <- self$initialDIC[1];
            self$output$pCO2[1] <- self$carbonateEq$optimizepCO2(
               concDIC = self$initialDIC * 1e-6, 
               totalAlk = self$alkalinity * 1e-6
               )$fCO2;
            self$output$fGas[1] <- self$output$dt[1] * self$output$k[1] * 0.915 * 
               self$output$kH[1] * (self$pCO2air - self$output$pCO2[1]);
           
            # Iterate through time to predict the change in DIC and 
            # the consequent changes in pH, pCO2, and gas exchange
            for (i in 2:dicPredLength) {
               # Calculate current DIC based on previous state
               self$output$dic[i] <- self$output$dic[i - 1] +
                  self$output$co2Production[i - 1] + 
                  self$output$co2Consumption[i - 1] + 
                  self$output$fGas[i - 1];
               
               # Calculate pCO2 and fGas based on new DIC
               self$carbonateEq$resetFromTemp(tempC = self$temp[i]);
               optim <- self$carbonateEq$optimizepCO2(
                  concDIC = self$output$dic[i] * 1e-6,
                  totalAlk = self$alkalinity * 1e-6
                  );
               self$output$pH[i] <- optim$pH;
               self$output$pCO2[i] <- optim$fCO2;
               self$output$fGas[i] <- self$output$dt[i] * self$output$k[i] * 0.915 * 
                  self$output$kH[i] * (self$pCO2air - self$output$pCO2[i]);
            }
            
            return(self$output);
         }
      )
   );

#' @name ModelOneStationMetabDoDic_run
#' 
#' @title 
#'    Runs the model (R6 method)
#' 
#' @description 
#'    Runs the model predicting the change in DO and DIC concentrations over
#'    time at a location along a stream.
#' 
#' @return 
#'    Data frame with incremental and final results of the simulation,
#'    with columns \cr
#'    time: POSIXct simulation time \cr
#'    do: dissolved oxygen concentration in grams per cubic meter \cr
#'    doSat: Saturated dissolved oxygen concentration in grams per cubic meter \cr
#'    doProduction: increase in DO concentration during the time step in 
#'       grams per cubic meter \cr
#'    doConsumption: decrease in DO concentration during the time step in
#'       grams per cubic meter \cr
#'    k: gas exchange rate per day for oxygen \cr
#'    temp: water temperature in degrees Celsius \cr
#'    dt: length of time step \cr
#'    co2Production: increase in co2 concentration during the time step in
#'       micromoles per liter \cr
#'    co2Consumption: decrease in co2 concentration during the time step in
#'       micromoles per liter \cr
#'    kH: Henry's constant during the time step in micromoles per liter per 
#'       microatmospheres \cr
#'    co2Sat: saturation concentration for co2 in micromoles per liter \cr
#'    fGas: change in co2 concentration due to air-water exchange in
#'       micromoles per liter \cr
#'    pCO2: partial pressure of co2 in water in microatmosphere \cr
#'    dic: DIC concentration in water in moles per liter 
NULL
