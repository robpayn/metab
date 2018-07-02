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

#' Simulates DO variation in streams
#'
#' @export
#' @description Uses the one-station model to simulate DO variation in 
#'    stream water due to metabolic processes and air-water gas exchange
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
oneStationMetabDo <- function(
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
   # Interpolates temperature and PAR data to new time step 
   # if the time step is adjusted
   if (!is.na(timeStepCount)) {
      timeIn <- as.numeric(as.POSIXct(time)) / 86400;
      timeRange <- as.numeric(as.POSIXct(timeRange)) / 86400;
      timeParNum <- as.numeric(as.POSIXct(timePar)) / 86400;
      time <- seq(
         from = timeRange[1], 
         to = timeRange[2], 
         length.out = timeStepCount + 1
         );
      temp <- approx(x = timeIn, y = temp, xout = time)$y;
      par <- approx(x = timeParNum, y = par, xout = time)$y;
   } else {
      time <- as.numeric(as.POSIXct(time)) / 86400;
   }

   # Calculates the time step between data points
   dt <- c(
      time[2:length(time)] - time[1:length(time) - 1], 
      0
      );
  
   # If total PAR is not provided as an argument, total PAR is calculated
   # over the vector using trapezoidal integration
   if (is.na(parTotal)) {
      parAverage <- c(
         0.5 * (par[1:length(time) - 1] + par[2:length(time)]),
         0
         );
      parTotal <- sum(parAverage * dt);
   }
  
   # Calculates the saturated oxygen concentration for the 
   # provided temperatures and air pressure
   doSat <- doSat(temp, airPressure) ;
  
   # Calculate the temperature adjusted gas exchange rate from 
   # the k600 rate at a Schmidt number of 600
   k <- kSchmidt(temp, k600);
  
   # specifies length to use for vectors and the data frame based on the length 
   # of time vector
   doPredLength <- length(time);
  
   # Creates a data frame to store values calculated in oneStationMetabDO function
   
   doPred <- data.frame(
      time = as.POSIXct(
         time * 86400, 
         origin = as.POSIXct("1970-01-01", tz = "GMT")
         ),
      do = numeric(doPredLength),
      doSat = doSat,
      doProduction = numeric(doPredLength),
      doConsumption = numeric(doPredLength),
      k = k,
      temp = temp,
      dt = dt
   );
   
   # Creates first value for the vector doPred$do  
   doPred$do[1] <- initialDO[1];

   # Calculate the DO production and consumption for
   # each time step
   doPred$doProduction <- dailyGPP * ((par * dt) / parTotal); 
   doPred$doConsumption <- dt * dailyER;
   
   # Iterates over the time steps to model the change in dissolved
   # oxygen over time (one station method)
   for (i in 2:doPredLength) {
      doPred$do[i] <- doPred$do[i - 1] +
      doPred$doProduction[i - 1] +
      doPred$doConsumption[i - 1] + 
      dt[i - 1] * k[i - 1] * (doSat[i - 1] - doPred$do[i - 1]);
   }
   
   return(doPred);
};

#' Simulates DO and DIC variation in streams
#'
#' @export
#' @description Uses the one-station model to simulate DO variation in 
#'    stream water due to metabolic processes and air-water gas exchange
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
#' @param initialDIC initial DIC concentration in micromoles per liter
#'    (numerical vector, only first value will be used)
#' @param pCO2air partial pressure of CO2 in the air in microatmospheres
#'    (numerical vector)
#' @param alkalinity alkalinity of stream water
#'    (numerical vector)
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
oneStationMetabDoDic <- function(
   dailyGPP, 
   dailyER, 
   k600, 
   airPressure, 
   initialDO, 
   time, 
   temp, 
   par, 
   initialDIC, 
   pCO2air, 
   alkalinity,
   timeStepCount = NA, 
   timePar = time,
   parTotal = NA, 
   timeRange = c(time[1], time[length(time)]),
   RQ = 0.85, 
   PQ = 1.22
   )
{
   # Run the one station metabolism model for DO
   doPred <- oneStationMetabDo(
      dailyGPP = dailyGPP, 
      dailyER = dailyER, 
      k600 = k600, 
      airPressure = airPressure,
      initialDO = initialDO, 
      time = time, 
      temp = temp, 
      par = par, 
      timeStepCount = timeStepCount,
      timePar = timePar, 
      parTotal = parTotal, 
      timeRange = timeRange
      );
  
   time <- doPred$time;
   temp <- doPred$temp;
  
   kH <- kHenryCO2fromTemp(tempK = temp, convertC = TRUE);

   # Set up the data frame that will be returned
   dicPredLength <- length(time);
   dicPred <- data.frame(
      doPred, 
      co2Production = -doPred$doConsumption * 31.25 * RQ,
      co2Consumption = -doPred$doProduction * 31.25 / PQ,
      kH = kH,
      co2Sat = pCO2air * kH,
      fGas = numeric(length = dicPredLength),
      pCO2 = numeric(length = dicPredLength),
      dic = numeric(length = dicPredLength)
      );

   # Set the initial DIC concentration, pCO2, and fgas
   dicPred$dic[1] <- initialDIC[1];
   dicPred$pCO2[1] <- optimizepCO2(
      carbonateEq = CarbonateEq(tempC = temp[1]), 
      concDIC = initialDIC * 1e-6, 
      totalAlk = alkalinity * 1e-6
      )$fCO2;
   dicPred$fGas[1] <- doPred$dt[1] * doPred$k[1] * 0.915 * 
      dicPred$kH[1] * (pCO2air - dicPred$pCO2[1]);
  
   # Iterate through time to predict the change in DIC and 
   # the consequent changes in pCO2 and gas exchange
   for (i in 2:dicPredLength) {
      # Calculate current DIC based on previous state
      dicPred$dic[i] <- dicPred$dic[i - 1] +
         dicPred$co2Production[i - 1] + 
         dicPred$co2Consumption[i - 1] + 
         dicPred$fGas[i - 1];
      
      # Calculate pCO2 and fGas based on new DIC
      dicPred$pCO2[i] <- optimizepCO2(
         carbonateEq = CarbonateEq(tempC = temp[i]),
         concDIC = dicPred$dic[i] * 1e-6,
         totalAlk = alkalinity * 1e-6
         )$fCO2;
      dicPred$fGas[i] <- doPred$dt[i] * doPred$k[i] * 0.915 * 
         dicPred$kH[i] * (pCO2air - dicPred$pCO2[i]);
   }
   
   return(dicPred);
};

#' Calculates an objective function value
#'
#' @export
#' @description Uses the provided model function and objective function
#'    to calculate an objective function value for a comparison of modeled
#'    and observed values. This function has proposed parameter set for the
#'    model as the first argument, and is thus designed to be compatible for 
#'    use with the "nlm" nonlinear minimization function. Use with nlm 
#'    requires that lower values of the objective function value indicates
#'    better agreement between the modeled and observed values.
#' @param params Vector of proposed parameter values for the provided 
#'    model function
#' @param modelFunc Model function applied to the parameters to produce a
#'    model prediction
#' @param objectiveFunc Objective function used to calculate a single value
#'    that characterizes the agreement between the modeled and observed values
#' @param obs Data frame of the observations
#' @param ... Additional parameters that are required by the objective function
#' @return The objective function value that characterizes the agreement between
#'    the provided observations and the predictions from the model executed with 
#'    the proposed parameters
#' @details This function depends on specific data structures produced by the model
#'    model function and the provided observations. The model function must return a
#'    a list with an element named "predict". This element must be a data frame where
#'    one or more columns are the vectors that represent the model predictions. The
#'    observations must also be a data frame, where the order of columns represent
#'    the observed variables that correspond to the variables in the predictions.
nlmObjectiveValue <- function(
   params, 
   modelFunc, 
   objectiveFunc, 
   obs, 
   ...
   )
{
   predict <- modelFunc(params)$predict;
   return(objectiveFunc(
      predict = predict, 
      obs = obs, 
      params = params, 
      ...
      ));
}

#' Minus log likelihood objective function
#' 
#' @export
#' @description Calculates the negative of the log likelihood for a model 
#'    prediction based on a multivariate comparison with observations.
#' @param predict A data frame where each column contains a vector of values
#'    for a given variable used in the multivariate comparison
#' @param obs A data frame where each column represents of vector of observations
#'    for a variable corresponding to the same column in the predict argument
#' @param params The proposed parameters (not currently used, will provide
#'    future functionality of allowing estimation of the standard deviation
#'    for each variable)
#' @param sd A vector of standard deviations expected for each variable in the
#'    multivariate likelihood (one value per column in the predict argument)
objLogLike <- function(predict, obs, params, sd, ...) 
{
   logLike <- 0;
   for (colNum in 1:length(predict)) {
      logLike <- logLike + sum(log(dnorm(
         x = obs[[colNum]], 
         mean = predict[[colNum]], 
         sd = sd[colNum]
      )))
   }
   return(-logLike);
}