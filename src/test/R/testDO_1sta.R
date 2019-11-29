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

knownGPP <- 150;
knownER <- -150;
knownk600 <- 12;

knownsdDO <- 2;

# Define the model object to be optimized.

model <- OneStationMetabDo$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = 609,
   time = doData$time,
   initialDO = 225,
   temp = doData$temp,
   par = par
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

# Plot the results

windows(width = 10, height = 8);
model$plot(
   obsDO = objFunc$observation$do
)
