rm(list = ls());
library(disco);
library(inferno);
library(metab);

# Read the data file that is providing sample PAR
# and temperature data

signals <- readRDS(
   file = "./data/twostation/dates/2014-09-12/signal.RData"
);

# Set known simluated stream environment
knownGPP <- 300;
knownER <- 300;
knownk600 <- 12;

knownsdDO <- 2;

# Define the model object to be optimized.
model <- TwoStationMetabDo$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = 609,
   par = 0.5 * (
      signals$signalIn$getVariable("par") +
      signals$signalOut$getVariable("par")
   ),
   upstreamTime = signals$signalIn$time,
   upstreamTemp = signals$signalIn$getVariable("temp"),
   upstreamDO = signals$signalIn$getVariable("do"),
   downstreamTime = signals$signalOut$time,
   downstreamTemp = signals$signalOut$getVariable("temp")
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
windows(width = 15, height = 8);
model$plot(obs.DO = objFunc$observation$do)
# 
# windows(width = 10, height = 10);
# par(
#    mar = c(2.5, 4.5, 1, 1),
#    oma = c(2, 0, 0, 0)
# );
# objFunc$plotFit(
#    params = optimr$par,
#    x = model$output$time,
#    lineArgs = list(
#       col = "red",
#       lty = "dashed"
#    )
# );
# 
# lines(
#    x = as.POSIXct(doData$time),
#    y = doData$dissolvedOxygen * 31.25,
#    col = "black",
#    lty = "dashed"
# );
# lines(
#    x = model$output$time,
#    y = (model$downstreamDOSat + model$upstreamDOSat) / 2,
#    col = "darkorange",
#    lty = "dashed"
# )
