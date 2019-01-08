# This script runs a monte carlo sensitivity analysis for DO and pCO2 gases at high and low GPP scenarios
#
# it also plots an example of 1 iteration of the pCO2 monte carlo analysis

# Clears global environment
rm(list = ls());

# Loads stream metabolism functions
source(file = "../CarbonateEq.R");
source(file = "../metab_functions.R");

# Imports data frame
#
# Must include:
#   PAR
#   Initial DO data point
#   Initial DIC data point
#   time
#   Temperature (degrees C)
#   pH
#   air pressure (mm Hg) is optional

doData <- read.table(
  file = "./8-4-17_big_sky_doData.csv",
  header = FALSE,
  sep = ",",
  skip = 2,
  row.names = NULL,
  stringsAsFactors = FALSE
);

#removes incomplete data segments

doData <- doData[complete.cases(doData),];

#rename columns in doData

attributes(doData)$names[1] <- "time";
attributes(doData)$names[2] <- "temp";
attributes(doData)$names[3] <- "dissolvedOxygen";
attributes(doData)$names[4] <- "dissolvedOxygenSat";
attributes(doData)$names[9] <- "LUX";
attributes(doData)$names[7] <- "GMT"

# Arguments for metabolism functions

par <- doData$LUX * 0.0185;
airPressure <- 609

#### 'Known'parameters ####

knownDailyGPP <- 10
knownDailyER <- -5
knownK600 <- 12
# Arguments for Monte Carlo analysis
totalRealizations <- 2
knownSdError <- .05

#### Monte Carlo DO ####
# monte carlo analysis for DO at GPP = 10
monteCarloEnsemble <- oneStationMonteCarlo(
  totalRealizations = totalRealizations,
  knownSdError = knownSdError,
  knownDailyGPP = knownDailyGPP, 
  knownDailyER = knownDailyER,
  knownK600 = knownK600, airPressure = airPressure,
  time = doData$time, initialDO = doData$dissolvedOxygen,
  temp = doData$temp, par = par, timeStepCount = NA,
  warnOnly = TRUE
  );

# monte carlo analysis for DO at GPP = 2
knownDailyGPP <- 2

monteCarloEnsemble2 <- oneStationMonteCarlo(
  totalRealizations = totalRealizations, knownSdError = knownSdError,
  knownDailyGPP = knownDailyGPP, knownDailyER = knownDailyER,
  knownK600 = knownK600, airPressure = airPressure,
  time = doData$time, initialDO = doData$dissolvedOxygen,
  temp = doData$temp, par = par, timeStepCount = NA,
  warnOnly = TRUE
  );


# plot distributions of GPP, ER, and k600 at high and low GPP scenarios
monteCarloDistributionDensity(monteCarloEnsemble = monteCarloEnsemble,
                              monteCarloEnsemble2 = monteCarloEnsemble2)

 


#### Monte Carlo pCO2 ####
  knownDailyGPP <- 10
 knownSdError <- 8.75

# monte carlo analysis for pCO2 at GPP = 10
 
  monteCarloEnsembleMulti <- oneStationMonteCarloMulti(totalRealizations = totalRealizations,
                                                     knownSdError = knownSdError,
                                                     knownDailyGPP = knownDailyGPP,
                                                     knownDailyER = knownDailyER,
                                                     knownK600 = knownK600,
                                                     airPressure = airPressure,
                                                     time = doData$time,
                                                     initialDO = doData$dissolvedOxygen,
                                                     temp = doData$temp,
                                                     par = par,
                                                     initialDIC = 2404.389,
                                                     pCO2air = 400, 
                                                     alkalinity = 2410,
                                                     timeStepCount = NA,
                                                     warnOnly = TRUE)
# monte carlo analysis for pCO2 at GPP = 2
  knownDailyGPP <- 2
  
  monteCarloEnsembleMulti2 <- oneStationMonteCarloMulti(totalRealizations = totalRealizations,
                                                       knownSdError = knownSdError,
                                                       knownDailyGPP = knownDailyGPP,
                                                       knownDailyER = knownDailyER,
                                                       knownK600 = knownK600,
                                                       airPressure = airPressure,
                                                       time = doData$time,
                                                       initialDO = doData$dissolvedOxygen,
                                                       temp = doData$temp,
                                                       par = par,
                                                       initialDIC = 2404.389,
                                                       pCO2air = 400, 
                                                       alkalinity = 2410,
                                                       timeStepCount = NA,
                                                       warnOnly = TRUE);
  
# plot distributions of GPP, ER, and k600 at high and low GPP scenarios
  monteCarloDistributionDensity(monteCarloEnsemble = monteCarloEnsembleMulti,
                              monteCarloEnsemble2 = monteCarloEnsembleMulti2)

 
### pCO2 Monte Carlos test plot ####
# create dataframe of pCO2 with known parameters
  dicPred <- oneStationMetabMulti(
  dailyGPP = 10,
  dailyER = knownDailyER,
  k600 = knownK600,
  airPressure = airPressure,
  time = doData$time,
  initialDO = doData$dissolvedOxygen,
  temp = doData$temp,
  par = par,
  parTotal = NA,
  optimize = FALSE,
  initialDIC = 2404.389,
  pCO2air = 400,
  alkalinity = 2410
);

# add synthetic error
DIC_error <- dicPred$pCO2 + rnorm(n = length(doData$dissolvedOxygen), sd = knownSdError)

# create example plot of single iteration of pCO2 monte carlo analysis
# 
# It plots the synthetic pCO2 data, synthetic pCO2 data with error, and pCO2 data created with 
# parameter estimates from a monte carlo run
windows(width = 10, height = 10);
par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(
  dicPred$time,
  dicPred$pCO2,
  # dicPred$do / dicPred$doSat,
  ylab = "DIC (mol/L)",
  col = "blue",
  type = "l"

);

points(dicPred$time,
       DIC_error,
       col = "green")


dicPredMonteCarlo <- oneStationMetabMulti(
  dailyGPP = monteCarloEnsembleMulti$dailyGPP,
  dailyER = monteCarloEnsembleMulti$dailyER,
  k600 = monteCarloEnsembleMulti$k600,
  airPressure = airPressure,
  time = doData$time,
  initialDO = doData$dissolvedOxygen,
  temp = doData$temp,
  par = par,
  parTotal = NA,
  optimize = FALSE,
  initialDIC = 2404.389,
  pCO2air = 400,
  alkalinity = 2410
);
lines(dicPred$time,
      dicPredMonteCarlo$pCO2,
      col = "green")

