# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#' @importFrom disco SignalDerivation
#' @importFrom inferno LogLikelihood
#' @importFrom inferno Simulator
NULL

#' @export
#' 
#' @title 
#'   Basic MLE inference for One Station stream metabolism
#'   
OneStationMetabMLE <- R6Class(
   classname = "OneStationMetabMLE",
   inherit = SignalDerivation,
   public = list(
      initParams = NULL,
      staticAirPressure = NULL,
      sdLikelihood = NULL,
      useDO = NULL,
      usepCO2 = NULL,
      outputFile = NULL,
      staticCO2Air = NULL,
      staticAlkalinity = NULL,
      initialize = function
         (
            ...,
            initParams,
            sdLikelihood,
            useDO = TRUE,
            usepCO2 = FALSE,
            staticAirPressure = NULL,
            outputFile = "results",
            staticCO2Air = NULL,
            staticAlkalinity = NULL
         )
         {
            super$initialize(...);
            self$initParams <- initParams;
            self$sdLikelihood <- sdLikelihood;
            self$useDO <- useDO;
            self$usepCO2 <- usepCO2;
            self$staticAirPressure <- staticAirPressure;
            self$outputFile <- outputFile;
            self$staticCO2Air = staticCO2Air;
            self$staticAlkalinity = staticAlkalinity;
         }
   )
);

OneStationMetabMLE$set(
   which = "public",
   name = "derive",
   value = function
      (
         signal = NULL, 
         prevResults = NULL, 
         path
      ) 
      {
         if(!is.null(signal)) {
            self$signal <- signal;
         } else {
            if(is.null(self$signal)) {
               stop(paste(
                  "Signal not provided to SignalDerivation$derive."
               ));
            }
         }
         
         if (!is.null(self$staticAirPressure)) {
            airPressure <- self$staticAirPressure;
         } else {
            airPressure <- self$signal$getVariable("airPressure");
         }
         
         if(self$usepCO2) {
            if (!is.null(self$staticCO2Air)) {
               co2Air <- self$staticCO2Air;
            } else {
               co2Air <- self$signal$getVariable("co2Air");
            }
            if (!is.null(self$staticAlkalinity)) {
               alkalinity <- self$staticAlkalinity;
            } else {
               alkalinity <- self$signal$getVariable("alkalinity");
            }
         }
         
         if(!self$usepCO2) {
            model <- ModelOneStationMetabDo$new(
               dailyGPP = self$initParams[1],
               dailyER = self$initParams[2],
               k600 = self$initParams[3],
               airPressure = airPressure,
               time = self$signal$time,
               initialDO = self$signal$getVariable("do"),
               temp = self$signal$getVariable("temp"),
               par = self$signal$getVariable("par"),
               stdAirPressure = 1
            );
         } else {
            pCO2obs <- self$signal$getVariable("pCO2");
            pCO2obs <- pCO2obs[!is.nan(pCO2obs)];
            model <- ModelOneStationMetabDoDic$new(
               dailyGPP = self$initParams[1],
               dailyER = self$initParams[2],
               k600 = self$initParams[3],
               airPressure = airPressure,
               time = self$signal$time,
               initialDO = self$signal$getVariable("do"),
               temp = self$signal$getVariable("temp"),
               par = self$signal$getVariable("par"),
               stdAirPressure = 1,
               initialpCO2 = pCO2obs[1],
               pCO2air = co2Air,
               alkalinity = alkalinity
            );
         }
         
         if (self$useDO) {
            observation <- data.frame(do = self$signal$getVariable("do"));
            if (self$usepCO2) {
               observation$pCO2 <- self$signal$getVariable("pCO2");
               predictionExtractor <- PredictionExtractorMetabDoDic$new(model);
            } else {
               predictionExtractor <- PredictionExtractorMetabDo$new(model);
            }
            
         } else if (self$usepCO2) {
            observation <- data.frame(pCO2 = self$signal$getVariable("pCO2"));
            predictionExtractor <- PredictionExtractorMetabDic$new(model);
         } else {
            stop("Cannot perform MLE without at least one observation variable.");
         }
         objFunc <- LogLikelihood$new(
            simulator = Simulator$new(
               model = model,
               parameterTranslator = ParameterTranslatorMetab$new(model),
               predictionExtractor = predictionExtractor
            ),
            observation = observation,
            sd = self$sdLikelihood,
            negate = TRUE
         );
         if (is.null(prevResults)) {
            par <- c(
               self$initParams[1],
               self$initParams[2],
               self$initParams[3]
            );
         } else {
            par <- c(
               prevResults$optimr$par[1],
               prevResults$optimr$par[2],
               prevResults$optimr$par[3]
            );
         }
         optimr <- optim(
            par = par,
            fn = objFunc$propose
         );
         objFunc$propose(optimr$par);
         results <- list(objFunc = objFunc, optimr = optimr);
         save(
            results, 
            file = sprintf(
               fmt = "%s/%s.RData",
               path,
               self$outputFile
               )
         );
         return(results);
      }
);
