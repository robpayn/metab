rm(list = ls());
library(disco);
library(inferno);
library(metab);

# Read the data file that is providing sample PAR
# and temperature data

signal <- readRDS(
   file = "./data/onestation/downstream/dates/2014-09-12/signal.RData"
);

# Set known simluated stream environment

knownGPP <- 150;
knownER <- 150;
knownk600 <- 12;

knownsdDO <- 2;

# Define the model object to be optimized.

model <- OneStationMetabDo$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = 0.84,
   time = signal$time,
   initialDO = signal$getVariable("do"),
   temp = signal$getVariable("temp"),
   par = signal$getVariable("par"),
   stdAirPressure = 1
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

# Run the model with the best-fit parameter values

objFunc$propose(params = optimr$par);

results <- list(objFunc = objFunc, optimr = optimr);
saveRDS(
   results, 
   file = "./results.RData"
);

# Plot the results

plotter <- OneStationMetabPlotter$new(
   signal = NULL,
   # signal = signal,
   # outputPath = NULL,
   outputPath = ".",
   fileName = "test_onestation_DO_results.pdf",
   timeTicks = 14,
   mfrow = c(2,2)
);

plotter$open(path = ".");
plotter$summarize(
   label = "Test results",
   timeBounds = as.POSIXct(c(
      "2014-09-12 23:00:00",
      "2014-09-14 01:00:00"
   ))
);
plotter$close()
