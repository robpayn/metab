# This is a test script that uses nlm() to minimize the objSSR objective funtion or the
# objLogLike objective funtion. It is capable of completeing a Monte Carlo Analysis of a 
# single or multigas stream metabolism model where the multigas model has two separate 
# standard errors associated with the creation of synthetic data.

# Clears global environment

rm(list = ls());

# Loads stream metabolism functions
source(file = "../CarbonateEq.R");
source(file = "../metab_functions.R");

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

objSSR <- function(predict, obs, params, ...)
{
   ssr <- 0;
   for (colNum in 1:length(predict)) {
      ssr <- ssr + sum((predict[[colNum]] - obs[[colNum]])^2)
   }
   return(ssr);
}

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
knownDailyGPP <- 5.2
knownDailyER <- -5.1
knownK600 <- 12.5

# Arguments for Monte Carlo analysis

totalRealizations <- 2
knownSdError <- 1

# plots do/dosat vs time and co2/co2sat vs time
dicPred <- oneStationMetabMulti(
  dailyGPP = knownDailyGPP,
  dailyER = knownDailyER,
  k600 = knownK600,
  airPressure = airPressure,
  time = doData$time, 
  initialDO = doData$dissolvedOxygen,
  temp = doData$temp,
  par = par, 
  parTotal = NA, 
  optimize = "na", 
  initialDIC = 2404.389,
  pCO2air = 400, 
  alkalinity = 2410,
  timeStepCount = 150
  
  );

windows(width = 10, height = 10);
par(mar = c(5.1, 4.5, 4.1, 4.1))
plot(
  dicPred$time, 
  dicPred$do,
  # dicPred$do / dicPred$doSat,
  ylab = expression(paste(
    "DO concentration ( ",
    mg~L^-1,
    ")"
    )),
  xaxt = "n",
  col = "blue"
  
);
axis.POSIXct(side = 1,
          x = dicPred$time,
          format = "%e %b %H:%M"
          )
par(new = TRUE);
plot(
  dicPred$time, 
  dicPred$pCO2,
  # (dicPred$pCO2 * dicPred$kH) / dicPred$co2Sat ,
  xaxt = "n",
  xlab = "",
  yaxt = "n",
  ylab = "",
  col = "red"
  );
axis(
  side = 4
);
mtext(
  text = expression(paste(
    "fCO",
    phantom()[2],
    " (",
    mu,
    "atm)"
  )),
  side = 4,
  line = 2.5
);

dicPred <- oneStationMetabMulti(
  dailyGPP = 5,
  dailyER = -5, 
  k600 = 12.5,
  airPressure = airPressure,
  time = doData$time,
  initialDO = doData$dissolvedOxygen,
  temp = doData$temp ,
  par = par,
  parTotal = NA,
  optimize = "na",
  initialDIC = 2404.389,
  pCO2air = 400,
  alkalinity = 2410
);
points(
  dicPred$time,
  dicPred$pCO2,
# (dicPred$pCO2 * dicPred$kH) / dicPred$co2Sat ,
col = "dark orange"
);

# NLM test ####

par <- doData$LUX * 0.0185;
airPressure <- 609

knownDailyGPP <- 10
knownDailyER <- -5
knownK600 <- 12

knownSdError <- 10

modelFunc <- function(params)
{
   # plots do/dosat vs time and co2/co2sat vs time
   dicPred <- oneStationMetabMulti(
      dailyGPP = params[1],
      dailyER = params[2],
      k600 = params[3],
      airPressure = airPressure,
      time = doData$time, 
      initialDO = doData$dissolvedOxygen,
      temp = doData$temp,
      par = par, 
      parTotal = NA, 
      optimize = "na", 
      initialDIC = 2404.389,
      pCO2air = 400, 
      alkalinity = 2410,
      timeStepCount = 150
      
   );
   return(list(
      output = dicPred,
      predict = data.frame(pCO2 = dicPred$pCO2)
      ));
}

sim <- modelFunc(c(
   knownDailyGPP,
   knownDailyER,
   knownK600
   ));

obsSynth <- data.frame(
   pCO2 = sim$predict$pCO2 + 
      rnorm(n = length(sim$predict$pCO2), sd = knownSdError)
   );

test <- nlmFunction(
   c(
      knownDailyGPP,
      knownDailyER,
      knownK600
      ),
   modelFunc = modelFunc,
   objectiveFunc = objLogLike,
   obs = obsSynth,
   sd = knownSdError
   );

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
   sd = knownSdError
   );

windows(width = 10, height = 10);

plot(
   x = sim$output$time,
   y = obsSynth$pCO2
   );
lines(
   x = sim$output$time,
   y = modelFunc(c(
      nlmr$estimate[1],
      nlmr$estimate[2],
      nlmr$estimate[3]
      ))$predict$pCO2
   );

# NLM test multigas ####

knownSdErrorDO <- 0.1;
knownSdErrorpCO2 <- 10;

modelFunc <- function(params)
{
   dicPred <- oneStationMetabMulti(
      dailyGPP = params[1],
      dailyER = params[2],
      k600 = params[3],
      airPressure = airPressure,
      time = doData$time, 
      initialDO = doData$dissolvedOxygen,
      temp = doData$temp,
      par = par, 
      parTotal = NA, 
      optimize = "na", 
      initialDIC = 2404.389,
      pCO2air = 400, 
      alkalinity = 2410,
      timeStepCount = 150
   );
   return(list(
      output = dicPred,
      predict = data.frame(
         do = dicPred$do,
         pCO2 = dicPred$pCO2
         )
      ));
}

sim <- modelFunc(c(
   knownDailyGPP,
   knownDailyER,
   knownK600
   ));

obsSynth <- data.frame(
   do = sim$predict$do + 
      rnorm(n = length(sim$predict$do), sd = knownSdErrorDO),
   pCO2 = sim$predict$pCO2 + 
      rnorm(n = length(sim$predict$pCO2), sd = knownSdErrorpCO2)
   );

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
      knownSdErrorDO,
      knownSdErrorpCO2
      )
   );


monteCarloEnsembleNlmr <- data.frame(dailyGPP = numeric(length = totalRealizations),
                                     dailyER = numeric(length = totalRealizations),
                                     k600 = numeric(length = totalRealizations))
monteCarloNlm <- for (i in 1:totalRealizations){
  obsSynth <- data.frame(
    do = sim$predict$do + 
      rnorm(n = length(sim$predict$do), sd = knownSdErrorDO),
    pCO2 = sim$predict$pCO2 + 
      rnorm(n = length(sim$predict$pCO2), sd = knownSdErrorpCO2)
  );
  
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
      knownSdErrorDO,
      knownSdErrorpCO2
      )
   );
  monteCarloEnsembleNlmr$dailyGPP[i] <- nlmr$estimate[1];
  monteCarloEnsembleNlmr$dailyER[i] <- nlmr$estimate[2];
  monteCarloEnsembleNlmr$k600[i] <- nlmr$estimate[3];
}


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

par(new = TRUE);

plot(
   x = sim$output$time,
   y = obsSynth$pCO2,
   col = "red"
   );
lines(
   x = sim$output$time,
   y = model$predict$pCO2,
   col = "red"
   );
