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
knownGPP <- 5;
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

# Define the objective function to use in the optimization
objFunc <- BayesLogLikelihood$new(
   paramDists = list(
      GPP = RVUniform$new(min = 0, max = 1000),
      ER = RVUniform$new(min = -1000, max = 0),
      k600 = RVUniform$new(min = 0, max = 100)
      ),
   baseObjFunc = ObjFuncLogLikelihood$new(
      model = model,
      parameterProcessor = ParameterProcessorMetab$new(),
      predictionProcessor = PredictionProcessorMetabDo$new(),
      synthErrorProcessor = SynthErrorNormal$new(
         mean = list(do = 0), 
         sd = list(do = knownsdDO)
         ),
      sd = knownsdDO
      )
   );

objFunc$realize();

offsetFactor <- 0.8;
burninSDAdjust <- 75;
sampler <- BayesAMMCMCSampler$new(
   bayesObjFunc = objFunc,
   initialParams = c(
      GPP = knownGPP * offsetFactor, 
      ER = knownER * offsetFactor, 
      k600 = knownk600 * offsetFactor
      ),
   burninStepSD = c(
      GPP = knownGPP / burninSDAdjust,
      ER = -knownER / burninSDAdjust,
      k600 = knownk600 / burninSDAdjust
      ),
   burninRealizations = 200,
   staticRealizations = 200,
   adaptiveRealizations = 3000,
   adaptiveCovarianceFactor = 0.5
   );
sampler$optimize();

# Plot the full traces of the parameter samples
windows(width = 8, height = 10);
par(mfrow = c(3, 1), mar = c(4, 5, 1, 1));
plot(sampler$paramSamples[,"GPP"]);
plot(sampler$paramSamples[,"ER"]);
plot(sampler$paramSamples[,"k600"]);

# plot the ensemble of parameter samples (eliminating burnin)
paramEnsemble <- sampler$paramSamples[400:nrow(sampler$paramSamples),];
windows(width = 8, height = 10);
par(mfrow = c(3, 1), mar = c(4, 5, 1, 1));
plot(paramEnsemble[,"GPP"]);
plot(paramEnsemble[,"ER"]);
plot(paramEnsemble[,"k600"]);

# Plot the posterior probability densities for parameter estimates
windows(width = 8, height = 10);
par(mfrow = c(3, 1), mar = c(4, 5, 2, 1));
plot(density(paramEnsemble[,"GPP"]));
plot(density(paramEnsemble[,"ER"]));
plot(density(paramEnsemble[,"k600"]));

# Plot the fit with the highest likelihood on the data
maxIndex <- which.max(sampler$likeSamples$posterior);
objFunc$propose(sampler$paramSamples[maxIndex,]);
windows(width = 8, height = 8);
plot(
   x = model$output$time, 
   y = objFunc$baseObjFunc$synthPrediction$do, 
   type = "l",
   ylab = expression(paste(
      "[DO] (g ", m^-3, ")"
   ))
);
points(
   x = model$output$time, 
   y = objFunc$baseObjFunc$observation$do
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
