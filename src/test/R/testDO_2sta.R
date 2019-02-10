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
par <- doData$LUX * 0.0185;

# Set known simluated stream environment
knownGPP <- 2;
knownER <- -5;
knownk600 <- 12;

knownsdDO <- 0.05;
knownsdpCO2 <- 8.75;

airPressure <- 609;
initialDIC <- 2404.389;
pCO2air <- 400; 
alkalinity <- 2410;

residenceTime <- 1/24;

# Define the model object to be optimized.
model <- ModelTwoStationMetabDo$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   residenceTime = residenceTime,
   airPressure = airPressure,
   upstreamDO = doData$dissolvedOxygen,
   time = list(
      upstream = doData$time,
      downstream = as.POSIXct(doData$time) + residenceTime * 86400
      ),
   temp = list(
      upstream = doData$temp,
      downstream = doData$temp
      ),
   par = par,
   timePar = doData$time,
   timeStepCount = 150
   );

# Define the objective function to use in the optimization
objFunc <- LogLikelihood$new(
   model = model,
   parameterProcessor = ParameterProcessorMetab$new(),
   predictionProcessor = PredictionProcessorMetabDo$new(),
   synthErrorProcessor = SynthErrorNormal$new(mean = list(do = 0), sd = list(do =knownsdDO)),
   sd = knownsdDO,
   negate = TRUE
   );

# Infer metabolic parameter values by minimizing the value returned 
# by the objective function.
optimr <- optim(
   par = c(
      knownGPP,
      knownER,
      knownk600
      ),
   fn = objFunc$propose
   );

# Run the model with the best fit parameters
objFunc$propose(optimr$par);

windows(width = 10, height = 10);
par(
   mfrow = c(1, 1), 
   mar = c(2.5, 4.5, 1, 2),
   oma = c(2, 0, 0, 0)
);
plot(
   x = model$output$time, 
   y = objFunc$synthPrediction$do, 
   type = "l",
   ylab = expression(paste(
      "[DO] (g ", m^-3, ")"
   ))
);
points(
   x = model$output$time, 
   y = objFunc$observation$do
   );
lines(
   x = model$output$upstreamTime,
   y = model$output$upstreamDO,
   col = "black",
   lty = "dashed"
);
lines(
   x = model$output$time,
   y = (model$output$doSat + model$output$upstreamDOSat) / 2,
   col = "darkorange",
   lty = "dashed"
);
lines(
   x = model$output$time,
   y = model$output$do,
   col = "red",
   lty = "dashed"
);
mtext(
   text = "Time",
   side = 1,
   outer = TRUE
);
