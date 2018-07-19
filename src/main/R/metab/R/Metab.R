library(R6)

#' Calculates the density of water
#' 
#' @export
#' @description Density of water is calculated based on an empirical 
#'    relationship with the temperature of water
#' @param temp Numerical vector of temperatures (deg C) 
#' @return Numerical vector of densities of water in kilograms per liter
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

#' Calculates saturated oxygen concentration 
#' 
#' @export
#' @description Saturated oxygen concentration is calculated based 
#'    on an empirical relationship with water temperature
#' @param temp Numerical vector of water temperatures (deg C)
#' @param airPressure Numerical vector of air pressures (mm Hg)
#' @return Numerical vector of saturation concentrations for oxygen 
#'    in grams per cubic meter
#' @details Vectors for temperature and air pressure are assumed 
#'    to be of the same length, or values will be reused
doSat <- function(temp, airPressure) 
{
   normTemp <- log((298.15 - temp) / (273.15 + temp));
   doSat <- 0.032 * densityWater(temp) *
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

#' Calculates temperature adjusted gas exchange
#' 
#' @description Adjusts a gas exchange velocity or rate based on temperature
#'    and the provided gas exchange velocity or rate at a Schmidt number of 600
#' @param temp Water temperature in degrees Celsius
#'    (numerical vector)
#' @param k600 Gas exchange rate (per time) or velocity (length per time)
#'    at a Schmidt number of 600
#'    (numerical vector)
#' @return Numerical vector of temperature corrected gas exchange rates 
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
ParameterProcessorMetab <- R6Class(
   classname = "ParameterProcessorMetab",
   inherit = ParameterProcessor,
   public = list(
      process = function(model, params)
         {
            model$dailyGPP <- params[1];
            model$dailyER <- params[2];
            model$k600 <- params[3];
         }
      )
   );

# Class PredictionProcessorMetabDo (R6) ####
PredictionProcessorMetabDo <- R6Class(
   classname = "PredictionProcessorMetabDoDic",
   inherit = PredictionProcessor,
   public = list(
      process = function(model)
         {
            return(data.frame(
               do = model$output$do
               ));
         }
      )
   );

# Class PredictionProcessorMetabDoDic (R6) ####
PredictionProcessorMetabDoDic <- R6Class(
   classname = "PredictionProcessorMetabDoDic",
   inherit = PredictionProcessor,
   public = list(
      process = function(model)
         {
            return(data.frame(
               do = model$output$do,
               pCO2 = model$output$pCO2
               ));
         }
      )
   );

# Class ModelOneStationMetabDo (R6) ####

#' Class ModelOneStationMetabDo
#'
#' A model for predicting dissolved oxygen concentrations in
#' a stream using the one station method.
#' 
#' @export
#' @usage \code{ModelOneStationMetabDo$new(...)}
#' @param dailyGPP daily average GPP influence on oxygen 
#'    in grams per cubic meter per day 
#'    (numerical vector with single value)
#' @param dailyER daily average ER influence on oxygen
#'    in grams per cubic meter per day 
#'    (numerical vector with single value)
#' @param k600 gas exchange rate per day for air-water interface
#'    (numerical vector with single value)
#' @param airPressure air pressure in mm of Hg
#'    (numerical vector with single value)
#' @param initialDO initial DO concentration in grams per cubic meter
#'    (numerical vector, only first value will be used)
#' @param time times for temperature and par data
#'    (POSIXct vector or character vector in a standard text format
#'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
#' @param temp water temperature data in degrees Celsius
#'    (numerical vector)
#' @param par photosynthetically active radiation data. Units are arbitrary,
#'    but must be consistent with PAR total if it is specified
#'    (numerical vector)
#' @param timeStepCount total number of time steps to which data should be
#'    interpolated, used to reduce the number of time steps calculated for
#'    excessively high resolution data
#'    (numerical vector with single value, defaults to NA)
#' @param timePar times for the PAR data, see note about data typein time
#'    argument description
#'    (POSIXct vector, defaults to be same as time argument)
#' @param parTotal total PAR, see units note in par argument description
#'    (numerical vector with single value, defaults to NA which forces
#'    calculation of total PAR from PAR data vector)
#' @param timeRange vector with 2 values for the start and end time for
#'    the interpolated time steps
#'    (numerical vector with two values, defaults to start and end of 
#'    time argument)
#' @return The object of class \code{ModelOneStationMetabDo} 
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
         timeRange = c(time[1], time[length(time)])
         ) 
         {
            self$dailyGPP <- dailyGPP; 
            self$dailyER <- dailyER;
            self$k600 <- k600; 
            self$airPressure <- airPressure;
            self$initialDO <- initialDO;
            self$time <- time;
            self$temp <- temp;
            self$par <- par;
            self$timeStepCount  <- timeStepCount; 
            self$timePar <- timePar;
            self$parTotal <- parTotal; 
            self$timeRange <- timeRange;
            
            # Interpolates temperature and PAR data to new time step 
            # if the time step is adjusted
            if (!is.na(self$timeStepCount)) {
               timeIn <- as.numeric(as.POSIXct(self$time)) / 86400;
               self$timeRange <- as.numeric(as.POSIXct(self$timeRange)) / 86400;
               timeParNum <- as.numeric(as.POSIXct(self$timePar)) / 86400;
               self$time <- seq(
                  from = self$timeRange[1], 
                  to = self$timeRange[2], 
                  length.out = self$timeStepCount + 1
                  );
               self$temp <- approx(x = timeIn, y = self$temp, xout = self$time)$y;
               self$par <- approx(x = timeParNum, y = self$par, xout = self$time)$y;
            } else {
               self$time <- as.numeric(as.POSIXct(self$time)) / 86400;
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
            doSat <- doSat(self$temp, self$airPressure) ;
           
            # Calculate the temperature adjusted gas exchange rate from 
            # the k600 rate at a Schmidt number of 600
            k <- kSchmidt(self$temp, self$k600);
           
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

#' Runs the model
#' 
#' Runs the model based on the provided parameters for primary production,
#' respriation, and air-water gas exchange rate
#' 
#' @name ModelOneStationMetabDo_run
#' @param params A vector of three values for the parameters dailyGPP,
#'    dailyER, and k600
#' @return Data frame with incremental and final results of the simulation,
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

#' Class ModelOneStationMetabDoDic
#'
#' A model for predicting dissolved oxygen and carbon dioxide concentrations in
#' a stream using the one station method.
#' 
#' @export
#' @usage \code{ModelOneStationMetabDoDic$new(...)}
#' @param ... Arguments passed to constructor \code{ModelOneStationMetabDoDic$new(...)} will be 
#'    passed generically to the constructor for the superclass \code{ModelOneStationMetabDo}. 
#'    See documentation for the class \code{\link{ModelOneStationMetabDo}} for a description
#'    of these arguments.
#' @param initialDIC initial DIC concentration in micromoles per liter
#'    (numerical vector, only first value will be used)
#' @param pCO2air partial pressure of CO2 in the air in microatmospheres
#'    (numerical vector)
#' @param alkalinity alkalinity of stream water
#'    (numerical vector)
#' @param RQ Respiratory quotient
#'    (single value numerical vector, default value is 0.85)
#' @param PQ Photosynthetic quotient
#'    (single value numerical vector, default value is 1.22)
#' @return The object of class \code{ModelOneStationMetabDoDic} 
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
      initialize = function(
         ..., 
         initialDIC, 
         pCO2air, 
         alkalinity,
         RQ = 0.85, 
         PQ = 1.22
         ) 
         {
            super$initialize(...);
            self$initialDIC = initialDIC;
            self$pCO2air = pCO2air;
            self$alkalinity = alkalinity;
            self$RQ = RQ;
            self$PQ = PQ;
         },
      run = function()
         {
            # Run the superclass one station metabolism model for DO
            super$run();
           
            kH <- kHenryCO2fromTemp(
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
            carbonateEq <- CarbonateEq$new(tempC = self$temp[1]);
            
            # Set the initial DIC concentration, pCO2, and fgas
            self$output$dic[1] <- self$initialDIC[1];
            self$output$pCO2[1] <- carbonateEq$optimizepCO2(
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
               carbonateEq$resetFromTemp(self$temp[i]);
               optim <- carbonateEq$optimizepCO2(
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

#' Runs the model
#' 
#' Runs the model based on the provided parameters for primary production,
#' respriation, and air-water gas exchange rate
#' 
#' @name ModelOneStationMetabDoDic_run
#' @param params A vector of three values for the parameters dailyGPP,
#'    dailyER, and k600
#' @return Data frame with incremental and final results of the simulation,
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
