rm(list = ls());
library(inferno);
library(metab);

# Read the data file that is providing sample PAR
# and temperature data
doData <- read.table(
   file = "./8-4-17_big_sky_doData.csv",
   header = FALSE,
   sep = ",",
   skip = 2,
   row.names = NULL,
   stringsAsFactors = FALSE
);
doData <- doData[complete.cases(doData),];
attributes(doData)$names[1] <- "time";
attributes(doData)$names[2] <- "temp";
attributes(doData)$names[3] <- "dissolvedOxygen";
attributes(doData)$names[4] <- "dissolvedOxygenSat";
attributes(doData)$names[9] <- "LUX";
attributes(doData)$names[7] <- "GMT";
par <- doData$LUX;

# Set values for simluated stream environment
knownGPP <- 2;
knownER <- -5;
knownk600 <- 12;
airPressure <- 609;
initialpCO2 <- 450;
pCO2air <- 400; 
alkalinity <- 2410;

# Create the model object
model <- OneStationMetabDoDic$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = airPressure,
   time = doData$time,
   initialDO = doData$dissolvedOxygen,
   temp = doData$temp,
   par = par,
   doSatUnitConv = 0.032,
   initialpCO2 = initialpCO2,
   pCO2air = pCO2air,
   alkalinity = alkalinity
);

model$run();

windows(width = 8, height = 10);
par(
   mfrow = c(2, 1), 
   mar = c(2.5, 4.5, 1, 2),
   oma = c(2, 0, 0, 0)
);
plot(
   x = model$output$time, 
   y = model$output$do, 
   ylab = expression(paste(
      "[DO] (g ", m^-3, ")"
   ))
);
plot(
   x = model$output$time, 
   y = model$output$pCO2, 
   ylab = expression(paste(
      pCO[2], " (", mu, "atm)"
   ))
);
mtext(
   text = "Time",
   side = 1,
   outer = TRUE
)
