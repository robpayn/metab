rm(list = ls());

source(file = "./metab/R/CarbonateEq.R");
source(file = "./metab/R/metab_functions.R");

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

# Run the model forward to provide the basis for a
# synthetic data set
doPred <- oneStationMetabDoDic(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = airPressure,
   time = doData$time,
   initialDO = doData$dissolvedOxygen,
   temp = doData$temp,
   par = par,
   initialDIC = initialDIC,
   pCO2air = pCO2air, 
   alkalinity = alkalinity
   );

# create a dataframe of synthetic observations with normally
# distributed random error.
obsSynth <- data.frame(
   do = doPred$do + 
      rnorm(n = length(doPred$do), sd = knownsdDO),
   pCO2 = doPred$pCO2 + 
      rnorm(n = length(doPred$pCO2), sd = knownsdpCO2)
   );

# Define the model function to be optimized. The model function 
# must have a simple parameter vector argument to be compatible 
# for use with the nlmObjectiveValue function
model <- function(params)
{
   dicPred <- oneStationMetabDoDic(
      dailyGPP = params[1],
      dailyER = params[2],
      k600 = params[3],
      airPressure = airPressure,
      time = doData$time, 
      initialDO = doData$dissolvedOxygen,
      temp = doData$temp,
      par = par, 
      initialDIC = initialDIC,
      pCO2air = pCO2air, 
      alkalinity = alkalinity
      );
   return(list(
      output = dicPred,
      predict = data.frame(
         do = dicPred$do,
         pCO2 = dicPred$pCO2
         )
      ));
}

# Infer metabolic parameter values by minimizing the value returned 
# by the nlmObjectiveValue function. The model and objective function
# used for the minimization are defined abstractly by the model function
# passed to the modelFunc argument and the object function passed to the 
# objectiveFunc argument.
nlmr <- nlm(
   f = nlmObjectiveValue,
   p = c(
      knownGPP,
      knownER,
      knownk600
      ),
   modelFunc = model,
   objectiveFunc = objLogLike,
   obs = obsSynth,
   sd = c(
      knownsdDO,
      knownsdpCO2
      )
   );
