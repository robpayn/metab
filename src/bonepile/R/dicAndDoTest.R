# This scrpit plots predicted DO and predicted pCO2 to show the relationship between the two 
# gases over one day
#
# It also creates a second set of predicted pCO2 data that can be used to compared dicPred model 
# predictions with varrying rates of GPP, ER and k600

# Clears global environment

# rm(list = ls());

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

totalRealizations <- 1
knownSdError <- 1

tempIncrease <- 3
# plots do vs time and co2 vs time
dicPred <- oneStationMetabMulti(
  dailyGPP = knownDailyGPP * 2.7^(tempIncrease/10),
  dailyER = knownDailyER * 3.5^(tempIncrease/10),
  k600 = knownK600,
  airPressure = airPressure,
  time = doData$time, 
  initialDO = doData$dissolvedOxygen,
  temp = doData$temp + tempIncrease,
  par = par, 
  parTotal = NA, 
  optimize = "na", 
  initialDIC = 2404.389,
  pCO2air = 400, 
  alkalinity = 2410,
  timeStepCount = NA
  
  );

windows(width = 7, height = 7);
par(mar = c(5.1, 4.5, 4.1, 4.1))
plot(
  dicPred$time, 
  dicPred$do,
  type = "l",
  lwd = 3,
  ylab = expression(paste(
    "DO concentration ( ",
    mg~L^-1,
    ")"
    )),
  xaxt = "n",
  col = "blue",
  xlab = " ",
  main = "Modeled Dissolved Oxygen and Carbon Dioxide"
  
);
legend("topright", legend = c("Model DO", text = expression(paste(
  "Model pCO",
  phantom()[2]
))), col = c("blue", "red"), lty = 1, lwd = 3, bty = "n");

axis.POSIXct(side = 1,
          x = dicPred$time,
          format = "%e %b %H:%M"
          )
par(new = TRUE);
plot(
  dicPred$time, 
  dicPred$pCO2,
  type = "l",
  lwd = 3,
  xaxt = "n",
  xlab = "Time",
  yaxt = "n",
  ylab = "",
  col = "red",
  ylim = c(220, 640)
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

# create second set of predicted DO and pCO2 with different metabolic rates and/or
# reaeration rates

dicPred <- oneStationMetabMulti(
  dailyGPP = 5,
  dailyER = -5, 
  k600 = 12.5,
  airPressure = airPressure,
  time = doData$time,
  initialDO = doData$dissolvedOxygen,
  temp = doData$temp ,
  par = par,
  parTotal = NA,
  optimize = "na",
  initialDIC = 2404.389,
  pCO2air = 400,
  alkalinity = 2410
);
points(
  dicPred$time,
  dicPred$pCO2,
col = "dark orange"
)