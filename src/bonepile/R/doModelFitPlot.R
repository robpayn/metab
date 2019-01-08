# This script uses nls() to fit the onStationMetabDO() model to observed DO data
# 
# It also creates a plot of observed DO data, predicted DO saturation, and the predicted DO from the model
# data from the model fit

# clears global environment
rm(list = ls());

#loads functions used in this script
source(file = "../CarbonateEq.R");
source(file = "../metab_functions.R");

#import data frame
# setwd("../8-4-17_big_sky_doData.csv")

doData <- read.table("./8-4-17_big_sky_doData.csv",
                     header = FALSE,
                     sep = ",",
                     skip = 2,
                     row.names = NULL,
                     stringsAsFactors = FALSE
);

#removes incomplete data segments

doData <- doData[complete.cases(doData),];

#rename columns

attributes(doData)$names[1] <- "time";
attributes(doData)$names[2] <- "temp";
attributes(doData)$names[3] <- "dissolvedOxygen";
attributes(doData)$names[4] <- "dissolvedOxygenSat";
attributes(doData)$names[9] <- "LUX";
attributes(doData)$names[7] <- "GMT"

# change time items from characters to numeric
timeLT <- strptime(doData$time, format = "%Y-%m-%d %T");
timeCT <- as.POSIXct(timeLT);


#convert lux to PAR and define arguments for functions

par <- doData$LUX * 0.0185;
airPressure <- 609
knownDailyGPP <- 10
knownDailyER <- -7
knownK600 <- 21.6
totalRealizations <- 10
knownSdError <- .05


# creates the formula for nls to use during the model fit
formula <- as.formula(doData$dissolvedOxygen ~ oneStationMetabDO(dailyGPP = P,
                                                                 dailyER = R,
                                                                 k600 = k,
                                                                 airPressure = airPressure,
                                                                 initialDO = doData$dissolvedOxygen,
                                                                 timeStepCount = NA,
                                                                 time = time,
                                                                 temp = temp,
                                                                 timePar = time,
                                                                 par = par,
                                                                 parTotal = NA,
                                                                 optimize = TRUE,
                                                 timeRange = c(time[1], time[length(time)])));
# uses nls to fit model to observed data
#
# allows user to set starting parameter values for nls and provie the data to be used
nlsr <- nls(
  formula = formula,
  start = list(
    P = 7,
    R = -7,
    k = 21
  ),
  data = doData
);

# creates summary of parameters and statistical information from nls
summNlsr <- summary(nlsr);

# creates data frame using parameters predicted from nls fitting model to observed data 
doPred <- oneStationMetabDO(
    dailyGPP = summNlsr$parameters[1,1],
    dailyER = summNlsr$parameters[2,1],
    k600 = summNlsr$parameters[3,1],
    airPressure = airPressure,
    time = doData$time,
    initialDO  = doData$dissolvedOxygen,
    temp = doData$temp,
    par = par,
    optimize = FALSE,
    timeStepCount = NA);

# plot observed DO data, predicted DO data, and predicted DO saturation
windows(width = 7, height = 7);
par(mar = c(5.1, 5.2, 4.1, 4.1))
plot(
  doPred$time, 
  doData$dissolvedOxygen,
  ylab = expression(paste(
    "DO concentration ( ",
    mg~L^-1,
    ")"
  )),
  xaxt = "n",
  col = "blue",
  xlab = "Time",
  title(main = 'Diel Dissolved Oxygen Curve')
  
);
axis.POSIXct(side = 1,
             x = doPred$time,
             format = "%e %b %H:%M"
);
legend(x = c("topright"), legend=c("Observed Dissolved Oxygen", "Dissolved Oxygen Saturation"),
       col=c("blue", "cyan2"), pch = c(1, NA), lty = c(NA, 1), lwd = c(1,3) , cex=1, bty = "n")
lines(doPred$time, doPred$do, col = "sky blue", lwd = 3);
lines(doPred$time, doPred$doSat, col = "cyan2", lwd = 3)
