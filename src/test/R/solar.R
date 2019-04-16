rm(list = ls());
library(infmod);
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

solarRadiation <- SolarRadiation$new(
   latitude = 45 + 15/60 + 59.55/3600,
   longitude = -(111 + 18/60 + 13.14/3600),
   differenceFromGMT = -7,
   adjustDST = 1
);
insolation <- solarRadiation$getExtraterrestrialInsolation(
   time = doData$time
);

windows(width = 10, height = 8);
par()
time <- as.POSIXct(doData$time)
plot(time, doData$LUX);
par(new = TRUE);
plot(time, insolation, col = "red", xaxt = "n", xlab = "", yaxt = "n", ylab = "");
axis(side = 4);
