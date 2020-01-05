# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
#' @title 
#'   R6 class defining an MLE inference using a two station metabolism model
#' 
#' @description 
#'   Basic MLE inference for two station stream metabolism
#'   
TwoStationMetabMLE <- R6Class(
   classname = "TwoStationMetabMLE",
   inherit = disco::TransferFunctionDerivation,
   public = list(
      
      #' @field initParams
      #'   The intial parameter values to use for the MLE algorithm
      initParams = NULL,
      
      #' @field staticAirPressure
      #'   The air pressure.
      staticAirPressure = NULL,
      
      #' @field sdLikelihood
      #'   The standard deviations used to calculate likelihood
      sdLikelihood = NULL,
      
      #' @field useDO
      #'   Set to TRUE to use DO in the inference.
      useDO = NULL,
      
      #' @field usepCO2
      #'   Set to TRUE to use pCO2 in the inference.
      usepCO2 = NULL,
      
      #' @field outputFile
      #'   The name of the output file.
      outputFile = NULL,
      
      #' @field staticCO2Air
      #'   The pCO2 in the air.
      staticCO2Air = NULL,
      
      #' @field staticAlkalinity
      #'   The alkalinity.
      staticAlkalinity = NULL,
      
      # Method TwoStationMetabMLE$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #'   
      #' @param ...
      #'   Arguments needed by the constructor of the super class
      #' @param initParams
      #'   The intial parameter values to use for the MLE algorithm
      #' @param sdLikelihood
      #'   The standard deviations used to calculate likelihood
      #' @param useDO
      #'   Set to TRUE to use DO in the inference.
      #'   Defaults to TRUE.
      #' @param usepCO2
      #'   Set to TRUE to use pCO2 in the inference.
      #'   Defaults to FALSE.
      #' @param staticAirPressure
      #'   The air pressure.
      #'   Defaults to NULL, which will cause use of air pressure
      #'   from the signal.
      #' @param outputFile
      #'   The name of the output file.
      #'   Defaults to "results".
      #' @param staticCO2Air
      #'   The pCO2 in the air.
      #'   Defaults to NULL, which will cause use of pCO2
      #'   from the signal.
      #' @param staticAlkalinity
      #'   The alkalinity.
      #'   Defaults to NULL, which will cause use of alkalinity
      #'   from the signal.
      #' 
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
      },
      
      # Method TwoStationMetabMLE$derive ####
      #
      #' @description 
      #'   Uses a two-station model to infer whole-stream metabolism and
      #'   gas exchange parameters from an upstream and downstream signal.
      #' 
      #' @param signalIn
      #'   The input (upstream) signal on which the MLE inference is based
      #' @param signalOut
      #'   The output (downstream) signal on which the MLE inference is based
      #' @param prevResults
      #'   The results from the previous inference
      #' @param path
      #'   The path to where the results should be written
      #'   
      #' @return
      #'   The two-element list containint the results of the MLE.
      #'   \itemize{
      #'     \item objFunc: The objective function used for the MLE
      #'     \item optimr: The results from optim performing the MLE
      #'   }
      #'   
      derive = function
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
         saveRDS(
            results, 
            file = sprintf(
               fmt = "%s/%s.RData",
               path,
               self$outputFile
            )
         );
         return(results);
      }
   )
)
