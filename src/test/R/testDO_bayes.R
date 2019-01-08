rm(list = ls());

loadpath <- "../../../../infmod/src/main/R/infmod/R";
source(file = paste(loadpath, "RandomVariable.R", sep = "/"));
source(file = paste(loadpath, "ObjectiveFunction.R", sep = "/"));
source(file = paste(loadpath, "Likelihood.R", sep = "/"));
source(file = paste(loadpath, "MCMCSampler.R", sep = "/"));

loadpath <- "../../main/R/metab/R";
source(file = paste(loadpath, "CarbonateEq.R", sep = "/"));
source(file = paste(loadpath, "Metab.R", sep = "/"));


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

# DEBUG
DebugPredProc <- R6Class(
   classname = "",
   inherit = PredictionProcessorMetabDo,
   public = list(
      counter = 0,
      process = function() 
         {
            if(self$counter == 10) {
               self$counter = 0;
               return(NULL);
            } else {
               self$counter = self$counter + 1;
               return(super$process());
            }
         }
      )
   );

# Define the objective function to use in the optimization
# Include a synthetic error processor to allow the objective
#    function to generate synthetic realizations from a 
#    known model.
objFunc <- BayesLogLikelihood$new(
   paramDists = list(
      GPP = RVUniform$new(min = 0, max = 1000),
      ER = RVUniform$new(min = -1000, max = 0),
      k600 = RVUniform$new(min = 0, max = 100)
      ),
   baseObjFunc = LogLikelihood$new(
      model = model,
      parameterProcessor = ParameterProcessorMetab$new(),
      predictionProcessor = DebugPredProc$new(),
      # predictionProcessor = PredictionProcessorMetabDo$new(),
      synthErrorProcessor = SynthErrorNormal$new(
         mean = list(do = 0), 
         sd = list(do = knownsdDO)
         ),
      sd = knownsdDO
      )
   );

# Create a Bayesian Adaptive Metropolis Markov Chain
#    Monte Carlo sampler, and exectue an optimization.
offsetFactor <- 0.8;
burninSDAdjust <- 75;
sampler <- AdaptiveMCMCSampler$new(
   objFunc = objFunc,
   initialParams = c(
      GPP = knownGPP * offsetFactor,
      ER = knownER * offsetFactor,
      k600 = knownk600 * offsetFactor
      ),
   burninCovariance = diag((c(
      GPP = knownGPP,
      ER = -knownER,
      k600 = knownk600
      ) / burninSDAdjust)^2),
   burninRealizations = 200,
   staticRealizations = 200,
   adaptiveRealizations = 2000,
   adaptiveCovarianceFactor = 0.5,
   statsLogger = StatsLoggerBayes$new()
   );
sampler$optimize();

sampler$plotSummary(device = "pdf", file = "./output/summary.pdf");
