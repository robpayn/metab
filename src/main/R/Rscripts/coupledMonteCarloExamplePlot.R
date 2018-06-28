# This script graphically displays a single iteration of a Monte Carlo Sensitivity analysis
# of DO and pCO2
#
# It creates and graphs synthetic data and graphs the model fit of that synthetic data

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
knownDailyGPP <- 5.2
knownDailyER <- -5.1
knownK600 <- 12.5

# Arguments for Monte Carlo analysis
# totalRealizations must equal 1
# Known standard errors must be scaled relative to eachother

totalRealizations <- 1
knownSdErrorDO <- .05
knownSdErrorDIC <- 8.75

#run single iteration of oneStaionMonteCarloDO
monteCarloEnsemble <- oneStationMonteCarlo(totalRealizations = totalRealizations, 
                                           knownSdError = knownSdErrorDO,
                                           knownDailyGPP = knownDailyGPP,
                                           knownDailyER = knownDailyER,
                                           knownK600 = knownK600,
                                           airPressure = airPressure,
                                           time = doData$time,
                                           initialDO = doData$dissolvedOxygen,
                                           temp = doData$temp, 
                                           par = par, 
                                           timeStepCount = NA);

#run single iteration of oneStationMonteCarloMulti
monteCarloEnsembleMulti <- oneStationMonteCarloMulti(totalRealizations = totalRealizations,
                                                     knownSdError = knownSdErrorDIC,
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
                                                     timeStepCount = NA);

# create synthetic DO data with known metabolic rates and rate of reaeration 
knownDO <- oneStationMetabDO(
                            dailyGPP = knownDailyGPP,
                            dailyER = knownDailyER,
                            k600 = knownK600,
                            airPressure = airPressure,
                            time = doData$time,
                            initialDO  = doData$dissolvedOxygen,
                            temp = doData$temp,
                            par = par,
                            optimize = FALSE,
                            timeStepCount = NA
);
# Add random error to knownDO dataset with the previously defined standard error
DO_error <- knownDO$do + rnorm(
  n = length(knownDO$do), 
  sd = knownSdErrorDO
);

#create DO dataset with paramters estimated during the single iteration of oneStationMonteCarlo 
doPred <- oneStationMetabDO(
                            dailyGPP = monteCarloEnsemble$dailyGPP,
                            dailyER = monteCarloEnsemble$dailyER,
                            k600 = monteCarloEnsemble$k600,
                            airPressure = airPressure,
                            time = doData$time,
                            initialDO  = doData$dissolvedOxygen,
                            temp = doData$temp,
                            par = par,
                            optimize = FALSE,
                            timeStepCount = NA
);

# create synthetic pCO2 data with known metabolic rates and rate of reaeration 
knownDIC <- oneStationMetabMulti(
                                dailyGPP = knownDailyGPP,
                                dailyER = knownDailyER,
                                k600 = knownK600,
                                airPressure = airPressure,
                                time = doData$time, 
                                initialDO = doData$dissolvedOxygen,
                                temp = doData$temp,
                                par = par, 
                                parTotal = NA, 
                                optimize = "na", 
                                initialDIC = 2404.389,
                                pCO2air = 400, 
                                alkalinity = 2410,
                                timeStepCount = NA
);

# Add random error to knownDIC dataset with the previously defined standard error
DIC_error <- knownDIC$pCO2 + rnorm(
  n = length(knownDIC$pCO2), 
  sd = knownSdErrorDIC
);

#create DO dataset with paramters estimated during the single iteration of oneStationMonteCarloMulti
dicPred <- oneStationMetabMulti(
                            dailyGPP = monteCarloEnsembleMulti$dailyGPP,
                            dailyER = monteCarloEnsembleMulti$dailyER,
                            k600 = monteCarloEnsembleMulti$k600,
                            airPressure = airPressure,
                            time = doData$time, 
                            initialDO = doData$dissolvedOxygen,
                            temp = doData$temp,
                            par = par, 
                            parTotal = NA, 
                            optimize = "na", 
                            initialDIC = 2404.389,
                            pCO2air = 400, 
                            alkalinity = 2410,
                            timeStepCount = NA
);

#plot knownDO with random error and predicted DO from single iteration of oneStationMonteCarlo()

windows(width = 10, height = 10);
par(mar = c(5.1, 4.5, 4.1, 4.1))
plot(
  doPred$time, 
  DO_error,
  ylab = expression(paste(
    "DO concentration ( ",
    mg~L^-1,
    ")"
  )),
  xlab = "Time",
  xaxt = "n",
  col = "blue"
  
);
legend(x = c("topright"), legend=c("Synthetic DO", text = expression(paste(
  "Synthetic pCO",
  phantom()[2]
)), "Predicted DO", text = expression(paste(
  "Predicted pCO",
  phantom()[2]
))),
       col=c("blue", "red"), pch = c(1, 1, NA, NA), lty = c(NA, NA, 1, 1), bty = "n" )

axis.POSIXct(side = 1,
             x = knownDO$time,
             format = "%e %b %H:%M"
);
lines(
  doPred$time,
  doPred$do,
  type = "l",
  col = "navy blue",
  lwd = 2
);
par(new = TRUE);

#plot knownDIC with random error and predicted pCO2 from single iteration of oneStationMonteCarloMulti()
plot(
  knownDIC$time, 
  DIC_error,
  xaxt = "n",
  xlab = "",
  yaxt = "n",
  ylab = "",
  col = "red",
  ylim = c(220, 680)
);
axis(
  side = 4
);
mtext(
  text = expression(paste(
    "pCO",
    phantom()[2],
    " (",
    mu,
    "atm)"
  )),
  side = 4,
  line = 2.5
);
lines(
  dicPred$time,
  dicPred$pCO2,
  lwd = 2,
  col = "dark red"
);




