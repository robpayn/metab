# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class ParameterTranslatorMetab (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a parameter translator for metabolism models
#'
#' @description 
#'    A parameter translator allowing GPP, ER, and k600 to be used
#'    as proposed parameters for an objective function using a
#'    stream metabolism model. 
#' 
ParameterTranslatorMetab <- R6Class(
   classname = "ParameterTranslatorMetab",
   inherit = inferno::ParameterTranslator,
   public = list(
      
      #' @field isEstimated
      #'   A vector of logical values indicating which parameters
      #'   to include in the estimation.
      isEstimated = NULL,
      
      # Method ParameterTranslatorMetab$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #' 
      #' @param ...
      #'   Arguments to pass to constructor of super class
      #' 
      #' @param isEstimated
      #'   A vector of logical values indicating which parameters
      #'   to include in the estimation.
      #'  
      initialize = function(..., isEstimated = c(TRUE, TRUE, TRUE))
      {
         super$initialize(...);
         self$isEstimated <- isEstimated;
      },
       
      # Method ParameterTranslatorMetab$translate ####
      #
      #' @description 
      #'   Used to translate 3 parameters in the order of
      #'   gross primary production, ecosystem respiration,
      #'   and gas exchange at a Schmidt number of 600
      #'   
      #' @param params
      #'   Vector of values to use for GPP, ER, and k600 parameters
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      translate = function
      (
         params
      ) 
      {
         if (self$isEstimated[1]) {
            self$model$dailyGPP = params[1];
         }
         if (self$isEstimated[2]) {
            self$model$dailyER = params[2];
         }
         if (self$isEstimated[3]) {
            self$model$k600 <- params[3];
         }
      }
   )
);


# Class PredictionExtractorMetabDo (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a prediction extractor for DO from metabolism models
#'
#' @description 
#'    A prediction extractor allowing dissolved oxygen outputs
#'    from a stream metabolism model to be used as predictions
#'    in an objective function.
#' 
PredictionExtractorMetabDo <- R6Class(
   classname = "PredictionExtractorMetabDo",
   inherit = inferno::PredictionExtractor,
   public = list(
      
      # Method PredictionExtractorMetabDo$extract ####
      #
      #' @description 
      #'   Used to extract dissolved oxygen predictions from a metabolism model
      #' 
      #' @return 
      #'   A data frame with a single column of DO data
      #'   
      extract = function()
      {
         return(data.frame(
            do = self$model$output$do
         ));
      }
   )
);


# Class PredictionExtractorMetabDoDic (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a prediction extractor for DO and pCO2 from metabolism models
#'
#' @description 
#'    A prediction extractor allowing dissolved oxygen and carbo
#'    dioxide outputs from a stream metabolism model to be used as 
#'    predictions in an objective function.
#' 
PredictionExtractorMetabDoDic <- R6Class(
   classname = "PredictionExtractorMetabDoDic",
   inherit = inferno::PredictionExtractor,
   public = list(
      # Method PredictionExtractorMetabDoDic$extract ####
      #
      #' @description 
      #'   Used to extract dissolved oxygen and dissolved carbon dioxide
      #'   predictions from a metabolism model
      #'   
      #' @return 
      #'   Data frame with two columns for DO and pCO2 data
      #'   
      extract = function() 
      {
         return(data.frame(
            do = self$model$output$do,
            pCO2 = self$model$output$pCO2
         ));
      }
   )
);


# Class PredictionExtractorMetabDic (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a prediction extractor for pCO2 from metabolism models
#'
#' @description 
#'    A prediction extractor allowing dissolved carbon dioxide concentrations
#'    from a stream metabolism model to be used as 
#'    predictions in an objective function.
#' 
PredictionExtractorMetabDic <- R6Class(
   classname = "PredictionExtractorMetabDic",
   inherit = inferno::PredictionExtractor,
   public = list(
      
      # Method PredictionExtractorMetabDic$extract
      #
      #' @description 
      #'   Used to extract dissolved carbon dioxide
      #'   predictions from a metabolism model
      #'   
      #' @return 
      #'   Data frame with one column for pCO2 data
      #'   
      extract = function() 
      {
         return(data.frame(
            pCO2 = self$model$output$pCO2
         ));
      }
   )
)
