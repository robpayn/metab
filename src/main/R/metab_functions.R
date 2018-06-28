# Function densityWater()
# Calculates the density of water (in kg L-1) for the provided temperatures
#
# Arguments
#   temp - Numerical vector with temperature data (deg C)
#
# Value 
#   densitywater - Numerical vector of water densities based on water temperature (kg L-1)

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

# Function doSat()
# Calculates the saturated oxygen concentration (in g m-3 or mg L-1) for the 
# provided temperatures and air pressure
#  
# Arguments
#   temp - Numerical vector with temperature data (deg C)
#   airPressure - Air pressure (mm Hg), if a vector of dynamic air pressures are used 
#                 it must be the same length and times as temp vector
#  
# Value
#   doSat - Numerical vector of dissolved oxygen saturation concentration based
#           on water temperature and air pressure at a given time (g m-3)
# 
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

# Function kSchmidt()
# Calculates the temperature adjusted gas exchange rate 
# (or velocity, same dimensions as k600) from the k600 velocity at a Schmidt number of 600
#
# Arguments
#   k600 - Gas exchange rate at a Schmidt number of 600 
#   temp - Numerical vector with temperature data
#
# Value
#   k - gas exchange rate in same units as the provided k600

kSchmidt <- function(temp, k600)
  
{
  schmidt <- 1800.6 -
    120.1 * temp +
    3.7818 * temp^2 -
    0.047608 * temp^3;
  return (k600 * (schmidt / 600)^-0.5);
}


# Function plotDO()
# Shows fit of predicted DO curve on the observed DO curve
#
# Arguments 
#   time - Numerical vector used in oneStationMetab calculations
#   predictedDO - Numerical vector of dissolved oxygen predicted from oneStationMetab function
#   obsDO - Numerical vector of observed dissolved oxygen 
#   par - Numerical vector of par data used in oneStationMetab calculations
#
# Creates plots of predicted DO, observed DO, predicted DO saturation concentration vs time
# Also plots par vs time on new axis
# The desired device for the plot should be opened before calling this function.

plotDO <- function(
  time, 
  predictedDO, 
  obsDO, 
  par, 
  ylim = c(
    min(predictedDO$do, obsDO, predictedDO$doSat), 
    max(predictedDO$do, obsDO, predictedDO$doSat)
    )
  )
{

  par(mar = c(4,4,4,4))
  plot(
    predictedDO$time,predictedDO$do, 
    type="l", 
    xlab = "time", 
    ylab = "Dissolved Oxygen", 
    col = "blue", 
    ylim = ylim
    );
  points(time, obsDO, col = "blue");
  points(predictedDO$time, predictedDO$doSat);
  par(new = TRUE);
  plot(
    time, 
    par, 
    yaxt = "n", 
    ylab = "", 
    xaxt = "n", 
    xlab = "",
    col = "red"
    );
  axis(
    side = 4
  )
  mtext(text = "PAR",
        side = 4,
        line = 2);
}

# Function oneStationMetabDO()
# Calculates a dissolved oxygen time series based on a 
# one station metabolism model 
#
# Arguments (provided data must be over the same time step)
#   dailyGPP - Total GPP over analysis period (g m-3)
#   dailyER - Daily ER flux over analysis period (g m-3 day-1)
#   k600 - Gas exchange rate (day-1)
#   airPressure - Numerical vector of air pressure (mm Hg)
#   time - Numerical vector with time of measurements (DO, temp, and PAR vectors must be same length)
#          (time must be able to be coerced into POSIXct type)
#   initialDO - Numerical vector with time and DO concentration data (g m-3) 
#   temp - Numerical vector with temperature data (deg C)
#   par - Numerical vector with PAR data
#   parTotal (= NA) - Total par (calculated from PAR by default, must be same units as par)
#   timePar - A Numerical vector of time correlated to PAR data, set to time by default
#   timeStepCount - optional, Numerical vector (length = 1) of data points to be calculated 
# 
#
# Value
#   Data frame with "time" and "do" columns.  First time and DO will be the same as 
#   provided DO data as initial conditions.  Remaining numerical vector is the one station modeled DO
#   curve.

oneStationMetabDO <- function(
  dailyGPP, dailyER, k600, airPressure, initialDO, timeStepCount = NA, time,
  temp, timePar = time, par, parTotal = NA, optimize = FALSE,
  timeRange = c(time[1], time[length(time)]))

{
  
  if (!is.na(timeStepCount)){
    timeIn <-  as.numeric(as.POSIXct(time)) / 86400;
    timeRange <- as.numeric(as.POSIXct(timeRange)) / 86400;
    timeParNum <- as.numeric(as.POSIXct(timePar)) / 86400;
    time <- seq(from = timeRange[1], to = timeRange[2], length.out = timeStepCount + 1);
    temp <- approx(x = timeIn, y = temp, xout = time)$y;
    par <- approx(x = timeParNum, y = par, xout = time)$y
  } else {
    time <-  as.numeric(as.POSIXct(time)) / 86400;
  }
  
  
  # Calculates the time step (dt) between data points
  
  dt <- c(
      time[2:length(time)] - time[1:length(time) - 1], 
      0
      );
  
  # Calculates parTotal over the length of the par vector using trapezoidal integration
  #   If parTotal = NA, parTotal will be calculated based provided vector of par
  #   If parTotal is specified, this function will be negated

  if (is.na(parTotal)) {
    parAverage <- c(
        0.5 * (par[1:length(time) - 1] + par[2:length(time)]),
        0
        );
    parTotal <- sum(parAverage * dt);
  }
  
  # Calls doSat function and calculates the saturated oxygen concentration for the 
  # provided temperatures and air pressure

  doSat <- doSat(temp[1:length(time)], airPressure) ;
  
  # Calls kSchmidt function to calculate the temperature adjusted gas exchange rate from 
  # the k600 rate at a Schmidt number of 600
  
  k <- kSchmidt(temp[1:length(time)], k600);
  
  # specifies length to use for vectors and the data frame based on the length 
  # of time vector
  
  doPredLength <- length(time);
  
  # Creates a data frame to store values calculated in oneStationMetabDO function

  doPred <- data.frame(
    time = as.POSIXct(time * 86400, origin = as.POSIXct("1970-01-01", tz = "GMT")),
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
  
  # Creates a loop used in the oneStationMetabDO calculation and fills values in 
  # doPred data frame
  #
  # The first value has been specified
  #
  for (i in 2:doPredLength) {
    
    doPred$doProduction[i - 1] <- dailyGPP * ((par[i - 1] * dt[i - 1]) / parTotal) 
    doPred$doConsumption[i - 1] <-  dt[i - 1] * dailyER 
    
    
    doPred$do[i] <- doPred$do[i - 1] +
      doPred$doProduction[i - 1] +
      doPred$doConsumption[i - 1] + 
      dt[i - 1] * k[i - 1] * (doSat[i - 1] - doPred$do[i - 1]);
    
  }
  if (optimize) {
    return(doPred$do)
    
  } else {
    
    return(doPred);
    
  } 
};

# Function Kh()
# Calculates Henry's constant for CO2 (mol L-1 atm-1)
# 
# Arguments
#   temp - Numerical vector of temperature (degrees Celsius)
# 
# 
# Value
#   returns a numerical vector of Henry's constants based on supplied temperature data
#   units of mol L-1 atm-1

Kh <- function(temp)

{
  degreeCelcius <- temp + 273.15;
  Kh <- exp(-58.0931 + 
              90.5069 * (100 / degreeCelcius) + 
              22.2940 * log(degreeCelcius / 100));
  
  return(Kh)
};

# Function oneStationMetabMulti()
# Calculates a dissolved inorganic carbon and pCO2 time series based on a
# one station metabolism model and DO model 
#
# Arguments (provided data must be over the same time step)
#   dailyGPP - Total GPP over analysis period (g m-3)
#   dailyER - Daily ER flux over analysis period (g m-3 day-1)
#   k600 - Gas exchange rate (day-1)
#   airPressure - Numerical vector of air pressure (mm Hg)
#   time - Numerical vector with time of measurements (DO, temp, and PAR vectors must be same length)
#     (time must be able to be coerced into POSIXct type)
#   initialDO - Numerical vector with initial DO concentration (g m-3) 
#   temp - Numerical vector with temperature data (deg C)
#   par - Numerical vector with PAR data
#   parTotal (= NA) - Total par (calculated from PAR by default, must be same units as par)
#   optimize (= TRUE) - Returns only numerical vector of predicted DIC, (= FALSE) - returns 
#   initialDIC - Numerical vector for initial DIC data in calculations (umol L-1)
#   pCO2air - Numerical vector of partial pressure of CO2 in atmosphere (uatm)
#   RQ - Numerical vector of resiratory quotient, CO2 produced / O2 consumed, commonly .85
#   PQ - Numerical vector of Photosythentic quotient, O2 produced / CO2 consumed, commonly 1.22
#   pH - Numerical vector of pH of river water (used to calculate pCO2 equilibrium concentration)
#   alkalinity - Numerical vector of alkalinity assumed constant for a given day
#                (used to calculate pCO2 equilibrium concentration)
#
# Value
#   Data frame with Numerical vectors of time, DO, DO saturation concentration, DO production, 
#   DO consumption, k (m day-1) - gas exchange velocity of O2,
#   CO2 production, CO2 consumption, kH (mol L-1 atm-1) -  Henry's law contant for CO2 (mol L-1 atm-1), 
#   CO2 saturation concentration, fGas (mol L-1),
#   pCO2 (uatm), dic (mol L-1)
#
#   First element of the DO and DIC columns will be the same as provided DO data as initial conditions. 
#   Remaining vectors are the one station modeled DO and DIC time series

oneStationMetabMulti<- function(
  dailyGPP, dailyER, k600, airPressure, initialDO, timeStepCount = NA, time, temp, timePar = time,
  par, parTotal = NA, optimize = NA,  timeRange = c(time[1], time[length(time)]),
  initialDIC, pCO2air, RQ = 0.85, PQ = 1.22, alkalinity
)
{
  # calls oneStationMetab() using arguments provided to oneStationMetabMulti()
  doPred <- oneStationMetabDO(
    dailyGPP = dailyGPP, dailyER = dailyER, k600 = k600, airPressure = airPressure,
    initialDO = initialDO, timeStepCount = timeStepCount,
    time = time, temp = temp, timePar = timePar, par = par, parTotal = parTotal, 
    optimize = FALSE, timeRange = timeRange
  );
  
  time <- doPred$time;
  
  temp <- doPred$temp;
  
  dicPredLength <- length(time);
  
  kH <- Kh(temp);
  
  carbonateEq <- CarbonateEq(tempC = temp);
  
  
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
  
  for (i in 2:dicPredLength) {
    
    # Calculate current DIC based on previous state
    dicPred$dic[i] <- 
      dicPred$dic[i - 1] +
      dicPred$co2Production[i - 1] + 
      dicPred$co2Consumption[i - 1] + 
      dicPred$fGas[i - 1]
    
    # Calculate pCO2 and fGas based on new DIC
    dicPred$pCO2[i] <- optimizepCO2(
      carbonateEq = CarbonateEq(tempC = temp[i]),
      concDIC = dicPred$dic[i] * 1e-6,
      totalAlk = alkalinity * 1e-6
      )$fCO2;
    
    dicPred$fGas[i] <- doPred$dt[i] * doPred$k[i] * 0.915 * 
      dicPred$kH[i] * (pCO2air - dicPred$pCO2[i]);
      
  }
  if (optimize == "pCO2") {
    return(dicPred$pCO2);
  } else if (optimize == "MULTI") {
    return(c(dicPred$do, dicPred$pCO2));
  } else {
    return(dicPred);
  }
};

# Function oneStationMonteCarlo()
# Runs Monte Carlo analysis on oneStationMetab function
# Used to asses uncertainty in GPP, ER, and k600 inferences
# using nonlinear regression based on assumed error in observed data

# Arguments
#   totalRealizations - number of iterations for function to run
#   knownSdError - random standard error to add to DO data points (creates synthetic idealized dataset)
#   knownDailyGPP - Daily GPP used to create synthetic dataset
#   knownDailyER - Daily ER used to create synthetic dataset
#   knownK600 - Daily k600 used to create synthetic dataset
#   airPressure - Numerical vector of atmospheric pressure (mm Hg)
#   time - Numerical vector of time used in oneSationMetab
#   initialDO - provides starting DO value for oneStationMetab
#   temp - Numerical vector of temperature (degrees C) used in oneStationMetab
#   par - Numerical vector of par data used in oneStationMetab
#
# Value
#   Data frame of estimated Daily GPP, daily ER, and daily k600 values
#   Same length as totalRealizations vector

oneStationMonteCarlo <- function(
  totalRealizations, knownSdError, knownDailyGPP, knownDailyER,
  knownK600, airPressure, time, initialDO, temp, par, timeStepCount = NA,
  warnOnly = FALSE
)
  
{
  knownDO <- oneStationMetabDO(
    dailyGPP = knownDailyGPP, 
    dailyER = knownDailyER, 
    k600 = knownK600, 
    airPressure = airPressure, 
    time = time,
    initialDO = initialDO, 
    temp = temp,
    par = par,
    optimize = TRUE,
    timeStepCount = timeStepCount
  );
  
  monteCarloEnsemble <- data.frame(
    dailyGPP = numeric(length = totalRealizations), 
    dailyER = numeric(length = totalRealizations),
    k600 = numeric(length = totalRealizations) 
  );
  
  formula <- as.formula(
    DO_error ~ oneStationMetabDO(
      dailyGPP = P, 
      dailyER = R, 
      k600 = k, 
      airPressure = airPressure, 
      time = time,
      initialDO = initialDO, 
      temp = temp,
      par = par, 
      optimize = TRUE,
      timeStepCount = timeStepCount
    )
  );
  
  for(i in 1:totalRealizations) {
    
    DO_error <- knownDO + rnorm(
      n = length(knownDO), 
      sd = knownSdError
    );
    
    nlsr <- nls(
      formula = formula, 
      start = list(
        P = knownDailyGPP, 
        R = knownDailyER, 
        k = knownK600
      ),
      control = nls.control(warnOnly = warnOnly)
    );
    
    summNlsr <- summary(nlsr);
    
    monteCarloEnsemble$dailyGPP[i] <- 
      summNlsr$coefficients["P", "Estimate"];
    monteCarloEnsemble$dailyER[i] <- 
      summNlsr$coefficients["R", "Estimate"];
    monteCarloEnsemble$k600[i] <- 
      summNlsr$coefficients["k", "Estimate"]
    
  }
  
  return(monteCarloEnsemble)
  
}
#
# Function oneStationMonteCarloMulti()
# Runs Monte Carlo analysis on oneStationMetabMulti function
# Used to compared effects on model predictions from changing
# known daily GPP, ER, k600, and error in data
#
# Arguments
#   totalRealizations - number of iterations for function to run
#   knownSdError - random standard error to add to DO data points (creates synthetic idealized dataset)
#   knownDailyGPP - Daily GPP used to create synthetic dataset
#   knownDailyER - Daily ER used to create synthetic dataset
#   knownK600 - Daily k600 used to create synthetic dataset
#   airPressure - Numerical vector of atmospheric pressure (mm Hg)
#   time - Numerical vector of time used in oneSationMetab
#   initialDO - provides sarting DO value for oneStationMetab
#   temp - Numerical vector of temperature (degrees C) used in oneStationMetab
#   par - Numerical vector of par data used in oneStationMetab
#   initialDIC - Value for initial DIC data in calculations (umol L-1)
#   pCO2air - Numerical vector of partial pressure of CO2 in atmosphere (uatm)
#   RQ - resiratory quotient, CO2 produced / O2 consumed, commonly .85
#   PQ - Photosythentic quotient, O2 produced / CO2 consumed, commonly 1.22
#   alkalinity - Numerical vector of alkalinity, assumed constant for a given day
#                (used to calculate pCO2 equilibrium concentration)
# Value
#   Data frame containing numerical vectors of estimated Daily GPP, daily ER, and daily k600 values
#   Number of rows in data frame will be the same as the total number of iterations

oneStationMonteCarloMulti <- function(
   totalRealizations, knownSdError, knownDailyGPP, knownDailyER,
   knownK600, airPressure, time, initialDO, temp, par, parTotal = NA,
   initialDIC, pCO2air, RQ = .85, PQ = 1.22, alkalinity, timeStepCount = NA,
   warnOnly = FALSE
   )

{
# create synthetic idealized pCO2 dataset based on known GPP, ER, and k600 
   knownPCO2 <- oneStationMetabMulti(
      dailyGPP = knownDailyGPP,
      dailyER = knownDailyER,
      k600 = knownK600,
      airPressure = airPressure,
      time = time,
      initialDO = initialDO,
      temp = temp,
      par = par,
      parTotal = parTotal,
      optimize = "pCO2",
      initialDIC = initialDIC,
      pCO2air = pCO2air,
      RQ = RQ,
      PQ = PQ,
      alkalinity = alkalinity,
      timeStepCount = timeStepCount
      );
   
# create data frame for ensemable of parameter estimates 
   monteCarloEnsemble <- data.frame(
      dailyGPP = numeric(length = totalRealizations),
      dailyER = numeric(length = totalRealizations),
      k600 = numeric(length = totalRealizations)
      );
# create formula telling nls() to match oneStationMetabMulti() pCO2 predictions 
# to known synthetic pCO2 with added error by changing parameters GPP, ER, and k600
    
   formula <- as.formula(pCO2_error ~ oneStationMetabMulti(
      dailyGPP = P,
      dailyER = R,
      k600 = k,
      airPressure = airPressure,
      time = time,
      initialDO = initialDO,
      temp = temp,
      par = par,
      optimize = "pCO2",
      initialDIC = initialDIC,
      pCO2air = pCO2air,
      RQ = RQ,
      PQ = PQ,
      alkalinity = alkalinity,
      timeStepCount = timeStepCount
      ));
# create loop to iteratively add random error to synthetic dataset and fit model to that dataset
# using nls()   
   for(i in 1:totalRealizations) {
# add random error with standard error = knownSdError to syntehtic idealzied dataset       
     pCO2_error <- knownPCO2 + 
         rnorm(n = length(knownPCO2), sd = knownSdError)
# run nls with provided formula and starting values that are equal to the parameters used to create 
# the synthetic data
      nlsr <- nls(
         formula = formula,
         start = list(
            P = knownDailyGPP,
            R = knownDailyER, 
            k = knownK600
            ),
         # argument for nls to send warning message if a single iteration does not converge
         control = nls.control(warnOnly = warnOnly)
         );
      
      summNlsr <- summary(nlsr);
# fill data frame with parameter estimates from each iteration of the Monte carlo analysis
      monteCarloEnsemble$dailyGPP[i] <- summNlsr$coefficients["P", "Estimate"];
      monteCarloEnsemble$dailyER[i] <- summNlsr$coefficients["R", "Estimate"];
      monteCarloEnsemble$k600[i] <- summNlsr$coefficients["k", "Estimate"]
   }

  return(monteCarloEnsemble)

}


#' Plotting function to be used with one station metab Monte Carlo functions
#' Plots density distributions of estimated GPP, ER, and k600 from two Monte Carlo analysis
#' for comparison
#'
#' @param monteCarloEnsemble Ensemble of GPP, ER, k600 constructed during one station Monte Carlo
#' function calls
#' @param monteCarloEnseble2  Ensemble of GPP, ER, k600 constructed during second one station Monte Carlo
#' function calls
#' 
#' @return Density distribution plots of GPP, ER, and k600 from two Monte Carlo analyses

monteCarloDistributionDensity <- function(monteCarloEnsemble, monteCarloEnsemble2, color1 = "black",
                                          color2 = "blue")
  {
    windows(width= 8, height = 15)
    par(mfrow = c(3,1), mar = c(4.6, 4.1, 0.5, 0.9))
    
    xlim <- c(min(monteCarloEnsemble$dailyGPP, monteCarloEnsemble2$dailyGPP),
              max(monteCarloEnsemble$dailyGPP, monteCarloEnsemble2$dailyGPP));
    density1 <- density(monteCarloEnsemble$dailyGPP)
    density2 <- density(monteCarloEnsemble2$dailyGPP)
    ylim <- c(min(density1$y, density2$y), max(density1$y, density2$y))
    
    plot(density1, xlim = xlim, ylim = ylim, xlab = expression(paste(
      "GPP ( ",
      mg~L^-1,
      ")"
    )), main = "", col = color1)
    lines(density2, xlim = xlim, ylim = ylim, col = color2)
    
    xlim <- c(min(monteCarloEnsemble$dailyER, monteCarloEnsemble2$dailyER),
              max(monteCarloEnsemble$dailyER, monteCarloEnsemble2$dailyER));
    density1 <- density(monteCarloEnsemble$dailyER)
    density2 <- density(monteCarloEnsemble2$dailyER)
    ylim <- c(min(density1$y, density2$y), max(density1$y, density2$y))
    
    plot(density1, xlim = xlim, ylim = ylim, xlab = expression(paste(
      "ER ( ",
      mg~L^-1,
      ")"
    )), main = "", col = color1)
    lines(density2, xlim = xlim, ylim = ylim, col = color2)
    
    xlim <- c(min(monteCarloEnsemble$k600, monteCarloEnsemble2$k600),
              max(monteCarloEnsemble$k600, monteCarloEnsemble2$k600));
    density1 <- density(monteCarloEnsemble$k600)
    density2 <- density(monteCarloEnsemble2$k600)
    ylim <- c(min(density1$y, density2$y), max(density1$y, density2$y))
    
    plot(density1, xlim = xlim, ylim = ylim,
      xlab = expression(paste(
     k[600],
        " (",
     days^-1,
      ")"
      )),
         main ="", col = color1)
    lines(density2, xlim = xlim, ylim = ylim, col = color2)

  }
