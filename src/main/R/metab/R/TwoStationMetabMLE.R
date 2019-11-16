# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
#' @title 
#'   Basic MLE inference for One Station stream metabolism
#'   
TwoStationMetabMLE <- R6Class(
   classname = "TwoStationMetabMLE",
   inherit = disco::TransferFunctionDerivation,
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

# Method TwoStationMetabMLE$derive ####

TwoStationMetabMLE$set(
   which = "public",
   name = "derive",
   value = function
      (
         signalIn = NULL,
         signalOut = NULL,
         prevResults = NULL, 
         path
      ) 
      {
         if(!is.null(signalIn)) {
            self$signalIn <- signalIn;
         } else {
            if(is.null(self$signalIn)) {
               stop(paste(
                  "Input signal not provided to SignalDerivation$derive."
               ));
            }
         }
         if(!is.null(signalOut)) {
            self$signalOut <- signalOut;
         } else {
            if(is.null(self$signalOut)) {
               stop(paste(
                  "Output signal not provided to SignalDerivation$derive."
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
         
         par <- 0.5 *
            (
               signalIn$getVariable("par") + 
               signalOut$getVariable("par")
            );
         
         if(!self$usepCO2) {
            model <- TwoStationMetabDo$new(
               dailyGPP = self$initParams[1],
               dailyER = self$initParams[2],
               k600 = self$initParams[3],
               airPressure = airPressure,
               par = par,
               upstreamTime = signalIn$time,
               upstreamTemp = signalIn$getVariable("temp"),
               upstreamDO = signalIn$getVariable("do"),
               downstreamTime = signalOut$time,
               downstreamTemp = signalOut$getVariable("temp"),
               stdAirPressure = 1
            );
         } else {
            upstreampCO2 <- self$signalIn$getVariable("pCO2");
            upstreampCO2 <- upstreampCO2[is.finite(upstreampCO2)];
            model <- TwoStationMetabDoDic$new(
               dailyGPP = self$initParams[1],
               dailyER = self$initParams[2],
               k600 = self$initParams[3],
               airPressure = airPressure,
               par = par,
               upstreamTime = signalIn$time,
               upstreamTemp = signalIn$getVariable("temp"),
               upstreamDO = signalIn$getVariable("do"),
               downstreamTime = signalOut$time,
               downstreamTemp = signalOut$getVariable("temp"),
               stdAirPressure = 1,
               upstreampCO2 = upstreampCO2,
               pCO2air = co2Air,
               alkalinity = alkalinity
            );
         }
         
         if (self$useDO) {
            observation <- data.frame(do = self$signalOut$getVariable("do"));
            if (self$usepCO2) {
               observation$pCO2 <- self$signalOut$getVariable("pCO2");
               predictionExtractor <- PredictionExtractorMetabDoDic$new(model);
            } else {
               predictionExtractor <- PredictionExtractorMetabDo$new(model);
            }
            
         } else if (self$usepCO2) {
            observation <- data.frame(pCO2 = self$signalOut$getVariable("pCO2"));
            predictionExtractor <- PredictionExtractorMetabDic$new(model);
         } else {
            stop("Cannot perform MLE without at least one observation variable.");
         }
         objFunc <- inferno::LogLikelihood$new(
            simulator = inferno::Simulator$new(
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
            fn = objFunc$propose,
            method = "L-BFGS-B",
            lower = c(0, -Inf, 0),
            upper = c(Inf, 0, Inf)
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
