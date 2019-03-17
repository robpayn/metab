rm(list = ls());
library(infmod);
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
model <- ModelOneStationMetabDoDic$new(
   dailyGPP = knownGPP,
   dailyER = knownER,
   k600 = knownk600,
   airPressure = airPressure,
   time = doData$time,
   initialDO = doData$dissolvedOxygen,
   temp = doData$temp,
   par = par,
   doSatUnitConv = 0.032,
   initialDIC = initialDIC,
   pCO2air = pCO2air,
   alkalinity = alkalinity
   );

# Define the objective function to use in the optimization
objFunc <- LogLikelihood$new(
   model = model,
   parameterProcessor = ParameterProcessorMetab$new(),
   predictionProcessor = PredictionProcessorMetabDoDic$new(),
   synthErrorProcessor = SynthErrorNormal$new(
      mean = list(do = 0, pCO2 = 0), 
      sd = list(do = knownsdDO, pCO2 = knownsdpCO2)
      ),
   sd = c(knownsdDO, knownsdpCO2),
   negate = TRUE
   );

objFunc$realize();

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

# # Define the objective function to use in the optimization
# objFunc <- ObjFuncLogLikelihood$new(
#    model = model,
#    parameterProcessor = ParameterProcessorMetab$new(),
#    predictionProcessor = PredictionProcessorMetabDoDic$new(),
#    observation = obsSynth,
#    sd = c(NaN, NaN),
#    invert = TRUE
#    );
# 
# # Infer metabolic parameter values by minimizing the value returned
# # by the objective function.
# optimr <- optim(
#    par = c(
#       knownGPP,
#       knownER,
#       knownk600,
#       knownsdDO,
#       knownsdpCO2
#       ),
#    fn = objFunc$propose
#    );

objFunc$propose(optimr$par);

windows(width = 8, height = 10);
par(
   mfrow = c(2, 1), 
   mar = c(2.5, 4.5, 1, 2),
   oma = c(2, 0, 0, 0)
   );
plot(
   x = model$output$time, 
   y = objFunc$observation$do, 
   ylab = expression(paste(
      "[DO] (g ", m^-3, ")"
      ))
   );
lines(
   x = model$output$time, 
   y = objFunc$synthPrediction$do
   );
lines(
   x = model$output$time,
   y = model$output$do,
   lty = "dashed",
   col = "red"
   );
plot(
   x = model$output$time, 
   y = objFunc$observation$pCO2, 
   ylab = expression(paste(
      pCO[2], " (", mu, "atm)"
      ))
   );
lines(
   x = model$output$time, 
   y = objFunc$synthPrediction$pCO2
   );
lines(
   x = model$output$time,
   y = model$output$pCO2,
   lty = "dashed",
   col = "red"
   );
mtext(
   text = "Time",
   side = 1,
   outer = TRUE
   );

windows(width = 10, height = 10);
plot(model$output$time, model$output$pCO2);
par(new = TRUE);
plot(model$output$time, model$output$dic);

points(model$output$dic, model$output$dic);

dTemp <- (model$output$temp[2:193] - model$output$temp[1:192]) / 
   model$output$temp[1:192];
dpCO2 <- (model$output$pCO2[2:193] - model$output$pCO2[1:192]) / 
   model$output$pCO2[1:192];
dDIC <- (model$output$dic[2:193] - model$output$dic[1:192]) / 
   model$output$dic[1:192];
revelle <- dpCO2 / dDIC;

# windows(width = 10, height = 10);
# plot(model$output$time, model$output$temp);
# par(new = TRUE);
# plot(model$output$time[1:192], revelle, ylim = c(-10,50));
