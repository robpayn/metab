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
par <- doData$LUX * 0.0185;

# Set known simluated stream environment
knownGPP <- 300;
knownER <- -300;
knownk600 <- 12;

knownsdDO <- 2;
knownsdpCO2 <- 8.75;

# Define the model object to be optimized.
model <- TwoStationMetabDoDic$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = 609,
   par = par,
   upstreamTime = doData$time,
   upstreamTemp = doData$temp,
   upstreamDO = doData$dissolvedOxygen * 31.25,
   downstreamTime = as.POSIXct(doData$time) + 3600,
   downstreamTemp = doData$temp,
   upstreamDIC = rep(2400, length(doData$time)),
   pCO2air = 400,
   alkalinity = 2410
);

# Define the objective function to use in the optimization
objFunc <- LogLikelihood$new(
   simulator = Simulator$new(
      model = model,
      parameterTranslator = ParameterTranslatorMetab$new(model),
      predictionExtractor = PredictionExtractorMetabDo$new(model)
   ),
   observationGenerator = ObservationGeneratorNormalErr$new(
      mean = list(do = 0), 
      sd = list(do =knownsdDO)
   ),
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

# Plot the model fit
windows(width = 10, height = 10);
par(
   mar = c(2.5, 4.5, 1, 1),
   oma = c(2, 0, 0, 0)
);
objFunc$plotFit(
   params = optimr$par,
   x = model$output$time,
   lineArgs = list(
      col = "red",
      lty = "dashed"
   )
);

lines(
   x = as.POSIXct(doData$time),
   y = doData$dissolvedOxygen * 31.25,
   col = "black",
   lty = "dashed"
);
lines(
   x = model$output$time,
   y = (model$downstreamDOSat + model$upstreamDOSat) / 2,
   col = "darkorange",
   lty = "dashed"
)
