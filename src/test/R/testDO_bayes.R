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
   par = par,
   doSatUnitConv = 0.032
   );

# DEBUG
# library(R6);
# DebugPredProc <- R6Class(
#    classname = "",
#    inherit = PredictionProcessorMetabDo,
#    public = list(
#       counter = 0,
#       process = function()
#          {
#             if(self$counter == 10) {
#                self$counter = 0;
#                return(NULL);
#             } else {
#                self$counter = self$counter + 1;
#                return(super$process());
#             }
#          }
#       )
#    );

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
      parameterTranslator = ParameterTranslatorMetab$new(model),
      predictionExtractor = PredictionExtractorMetabDo$new(model),
      observationGenerator = ObservationGeneratorNormalErr$new(
         mean = list(do = 0), 
         sd = list(do =knownsdDO)
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
   burninProposalDist = RVMultivariateNormal$new(
      covariance = diag((c(
         GPP = knownGPP,
         ER = -knownER,
         k600 = knownk600
         ) / burninSDAdjust)^2),
      adjustCovarianceFactor = 0.5
      ),
   burninRealizations = 200,
   staticRealizations = 200,
   adaptiveRealizations = 2000,
   statsLoggers = list(bayes = StatsLoggerBayes$new())
   );
sampler$optimize();

sampler$plotSummary(device = "windows", file = "./output/summary.pdf");
