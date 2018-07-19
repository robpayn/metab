rm(list = ls());

source(file = "./metab/debug.R");
loadObjective(path = "./metab");

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

# Define the model object to be optimized.
model <- ModelOneStationMetabDo$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = airPressure,
   time = doData$time,
   initialDO = doData$dissolvedOxygen,
   temp = doData$temp,
   par = par
   );

# Run the model forward to provide the basis for a
# synthetic data set
doPred <- model$run();

# create a dataframe of synthetic observations with normally
# distributed random error.
obsSynth <- data.frame(
   do = doPred$do + 
      rnorm(n = length(doPred$do), sd = knownsdDO)
   );

# Define the objective function to use in the optimization
objFunc <- ObjFuncLogLikelihood$new(
   model = model,
   parameterProcessor = ParameterProcessorMetab$new(),
   predictionProcessor = PredictionProcessorMetabDo$new(),
   observation = obsSynth,
   sd = knownsdDO
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

windows(width = 10, height = 10);
par(
   mfrow = c(1, 1), 
   mar = c(2.5, 4.5, 1, 2),
   oma = c(2, 0, 0, 0)
   );
plot(
   x = doPred$time, 
   y = doPred$do, 
   type = "l",
   ylab = expression(paste(
      "[DO] (g ", m^-3, ")"
      ))
   );
points(
   x = doPred$time, 
   y = obsSynth$do 
   );
mtext(
   text = "Time",
   side = 1,
   outer = TRUE
   );
