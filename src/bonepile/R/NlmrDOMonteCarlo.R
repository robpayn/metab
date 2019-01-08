# this script is used to complete a Monte Carlo Sensitivity Analysis of a DO river metab model
# over any number of realizations. It uses nlm and can be changed to minimize either the sum of squared residuals
# or the -log likelihood  

# Clears global environment

rm(list = ls());

# Loads stream metabolism functions
source(file = "../CarbonateEq.R");
source(file = "../metab_functions.R");

# the base of the function to be minimized with nlm, this function can uses another
# objective function such as objSSR or objLogLike
#
# Arguments
#   params - vector of parameter values (GPP,ER,k600)
#   modelFunc - the predictive model to be matched to observed data as a function of params
#   objectiveFunc - objective function to be minimized
#   obs - observed data
# value
#   predict - dataframe of DO and/or pCO2 data from modelFunc
#   obs - observed data
#   params - vector of parameters
nlmFunction <- function(params, modelFunc, objectiveFunc, obs, ...)
{
  predict <- modelFunc(params)$predict;
  return(objectiveFunc(
    predict = predict, 
    obs = obs, 
    params = params, 
    ...
  ));
}

# Sum of Sqaured residuals objective function to be used in the nlmFunction 
# Arguments
#   predict - dataframe with predicted DO and/or pCO2 data
#   obs - observed data
#   params - vector of parameters to be used by the modelFunc
# Value
#   returns the sum of sqaured residuals

objSSR <- function(predict, obs, params, ...)
{
  ssr <- 0;
  for (colNum in 1:length(predict)) {
    ssr <- ssr + sum((predict[[colNum]] - obs[[colNum]])^2)
  }
  return(ssr);
}

# Log likelihood objective function to be used in the nlmFunction 
# Arguments
#   predict - dataframe with predicted DO and/or pCO2 data
#   obs - observed data
#   params - vector of parameters to be used by the modelFunc
#   sd - known standard error of observed data 
# Value
#   returns the - log likelihood
objLogLike <- function(predict, obs, params, sd, ...) 
{
  logLike <- 0;
  for (colNum in 1:length(predict)) {
    logLike <- logLike + sum(log(dnorm(
      x = obs[[colNum]], 
      mean = predict[[colNum]], 
      sd = sd[colNum]
    )))
  }
  return(-logLike);
}

# Imports data frame
#
# Must include:
#   PAR
#   Initial DO data point
#   Initial DIC data point
#   time
#   Temperature (degrees C)
#   pH
#   air pressure (mm Hg) is optional

doData <- read.table(
  file = "./8-4-17_big_sky_doData.csv",
  header = FALSE,
  sep = ",",
  skip = 2,
  row.names = NULL,
  stringsAsFactors = FALSE
);

#removes incomplete data segments

doData <- doData[complete.cases(doData),];

#rename columns in doData

attributes(doData)$names[1] <- "time";
attributes(doData)$names[2] <- "temp";
attributes(doData)$names[3] <- "dissolvedOxygen";
attributes(doData)$names[4] <- "dissolvedOxygenSat";
attributes(doData)$names[9] <- "LUX";
attributes(doData)$names[7] <- "GMT"

# Arguments for metabolism functions

par <- doData$LUX * 0.0185;
airPressure <- 609
knownDailyGPP <- 10
knownDailyER <- -5
knownK600 <- 12

# Arguments for Monte Carlo analysis

totalRealizations <- 100


# NLM test DO ####
# Set known standard error for DO 

knownSdErrorDO <- 0.05;

#create function that allows us to easily change parameters in oneStationMetabDO model
#
# argument
#   params - a vector of parameter values
#           - c(GPP,ER,k600)
# value
#   Returns a list of dicPred dataframe and predict dataframe containing DO and pCO2
modelFunc <- function(params)
{
  doPred <- oneStationMetabDO(
    dailyGPP = params[1],
    dailyER = params[2],
    k600 = params[3],
    airPressure = airPressure,
    time = doData$time, 
    initialDO = doData$dissolvedOxygen,
    temp = doData$temp,
    par = par, 
    parTotal = NA
    );
  return(list(
    output = doPred,
    predict = data.frame(
      do = doPred$do
    )
  ));
}
# create synthetic data with known parameters
sim <- modelFunc(c(
  knownDailyGPP,
  knownDailyER,
  knownK600
));
# create dataframe of synthetich data with added error
obsSynth <- data.frame(
  do = sim$predict$do + 
    rnorm(n = length(sim$predict$do), sd = knownSdErrorDO)
);
# test nlmFunction to make sure it is working corectly
test <- nlmFunction(
  c(
    knownDailyGPP,
    knownDailyER,
    knownK600
  ),
  modelFunc = modelFunc,
  objectiveFunc = objLogLike,
  obs = obsSynth,
  sd = c(
    knownSdErrorDO
  )
);
# construct dataframe for nlm results 

monteCarloEnsembleNlmr <- data.frame(dailyGPP = rep(x = NaN, times = totalRealizations),
                                     dailyER = rep(x = NaN, times = totalRealizations),
                                     k600 = rep(x = NaN, times = totalRealizations))

# # create csv file in working directory and name columns dailyGPP, dailyER, and k600
write(x = c("dailyGPP", "dailyER", "k600"), file = "blah.csv",
      ncolumns = 3, sep = ",", append = TRUE)

# create for loop to iteratively run nlm on synthetic data
monteCarloNlm <- for (i in 1:totalRealizations){
 # add random error to simulated data
   obsSynth <- data.frame(
    do = sim$predict$do + 
      rnorm(n = length(sim$predict$do), sd = knownSdErrorDO)
  );
# loglikelyhood nlm 
# use nlm to minimize -logLikelyhood using obsSynth and the modelFunc
   nlmr <- nlm(
    f = nlmFunction,
    p = c(
      knownDailyGPP,
      knownDailyER,
      knownK600
    ),
    modelFunc = modelFunc,
    objectiveFunc = objLogLike,
    obs = obsSynth,
    sd = c(
      knownSdErrorDO
    )
  );
  # fill data frame
  monteCarloEnsembleNlmr$dailyGPP[i] <- nlmr$estimate[1];
  monteCarloEnsembleNlmr$dailyER[i] <- nlmr$estimate[2];
  monteCarloEnsembleNlmr$k600[i] <- nlmr$estimate[3];

# fill columns of previously created csv file with parameter estimations
   write(x = nlmr$estimate, file = "blah.csv",
        ncolumns = 3, append = TRUE, sep = ",")

  }

# plot example of one iteration of DO monte carlo
windows(width = 10, height = 10);

model <- modelFunc(c(
  nlmr$estimate[1],
  nlmr$estimate[2],
  nlmr$estimate[3]
));
plot(
  x = sim$output$time,
  y = obsSynth$do
);
lines(
  x = sim$output$time,
  y = model$predict$do
);
