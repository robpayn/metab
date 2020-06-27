# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
#' @title 
#'   R6 class defining an MLE inference using a one station metabolism model
#'   
#' @description 
#'   Processes a signal of DO and pCO2 data 
#'   (with supporting PAR and temperature information)
#'   for inferring whole-stream metabolism using the one-station
#'   method.
#'   
OneStationMetabMLE <- R6Class(
   classname = "OneStationMetabMLE",
   inherit = disco::SignalDerivation,
   public = list(
      
      #' @field initParams
      #'   The intial parameter values to use for the MLE algorithm
      initParams = NULL,
      
      #' @field parameterTranslatorGenerator
      #'   The translator used to convert the parameters provided into
      #'   model input
      parameterTranslatorGenerator = NULL,
      
      #' @field isEstimated
      #'   A vector of logical values indicating which parameters
      #'   to include in the estimation.
      isEstimated = NULL,
      
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
      
      # Method OneStationMetabMLE$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #' 
      #' @param ...
      #'   Arguments needed by the constructor of the super class
      #' @param initParams
      #'   The intial parameter values to use for the MLE algorithm
      #' @param parameterTranslatorGenerator
      #'   The R6 class generator for the translator used to convert the parameters 
      #'   provided into model input.
      #'   Default value is the R6 class generator ParameterTranslatorMetab.
      #' @param isEstimated
      #'   A vector of logical values indicating which parameters
      #'   to include in the estimation.
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
         parameterTranslatorGenerator = ParameterTranslatorMetab,
         isEstimated = c(TRUE, TRUE, TRUE),
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
         self$parameterTranslatorGenerator <- parameterTranslatorGenerator;
         self$isEstimated <- isEstimated;
         self$sdLikelihood <- sdLikelihood;
         self$useDO <- useDO;
         self$usepCO2 <- usepCO2;
         self$staticAirPressure <- staticAirPressure;
         self$outputFile <- outputFile;
         self$staticCO2Air = staticCO2Air;
         self$staticAlkalinity = staticAlkalinity;
      },
      
      # Method OneStationMetabMLE$derive ####
      #
      #' @description 
      #'   Uses a one-station model to infer whole-stream metabolism and
      #'   gas exchange parameters from a single signal.
      #' 
      #' @param signal
      #'   The signal on which the MLE inference is based
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
            model <- OneStationMetabDo$new(
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
            pCO2obs <- pCO2obs[is.finite(pCO2obs)];
            model <- OneStationMetabDoDic$new(
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
         
         objFunc <- inferno::LogLikelihood$new(
            simulator = inferno::Simulator$new(
               model = model,
               parameterTranslator = self$parameterTranslatorGenerator$new(
                  model = model,
                  isEstimated = self$isEstimated
               ),
               predictionExtractor = predictionExtractor
            ),
            observation = observation,
            sd = self$sdLikelihood,
            negate = TRUE
         );
         
         if (is.null(prevResults)) {
            par <- self$initParams[self$isEstimated];
         } else {
            par <- prevResults$optimr$par;
         }
         
         optimr <- optim(
            par = par,
            fn = objFunc$propose
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
