# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#' @importFrom inferno ParameterTranslator
#' @importFrom inferno PredictionExtractor
NULL

# Class ParameterTranslatorMetab (R6) ####

#' @export
#' 
#' @title 
#'    Parameter tranlator for metabolism models
#'
#' @description 
#'    A parameter translator allowing GPP, ER, and k600 to be used
#'    as proposed parameters for an objective function using a
#'    stream metabolism model.
#' 
#' @usage 
#'    ParameterTranslatorMetab$new()
#' @return 
#'    The object of class \code{ParameterTranslatorMetab} 
#'    instantiated by the constructor
ParameterTranslatorMetab <- R6Class(
   classname = "ParameterTranslatorMetab",
   inherit = ParameterTranslator,
   public = list(
      translate = function(params) 
      {
         self$model$dailyGPP <- params[1];
         self$model$dailyER <- params[2];
         self$model$k600 <- params[3];
      }
   )
);

# Class PredictionExtractorMetabDo (R6) ####

#' @export
#' 
#' @title 
#'    A prediction extractor for DO output from a metabolism model (R6)
#'
#' @description 
#'    A prediction extractor allowing dissolved oxygen outputs
#'    from a stream metabolism model to be used as predictions
#'    in an objective function.
#' 
#' @usage 
#'    PredictionExtractorMetabDo$new()
#' @return 
#'    The object of class \code{PredictionExtractorMetabDo} 
#'    instantiated by the constructor
PredictionExtractorMetabDo <- R6Class(
   classname = "PredictionExtractorMetabDo",
   inherit = PredictionExtractor,
   public = list(
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
#'    A prediction extractor for DO and pCO2 output from a metabolism model (R6)
#'
#' @description 
#'    A prediction extractor allowing dissolved oxygen and carbo
#'    dioxide outputs from a stream metabolism model to be used as 
#'    predictions in an objective function.
#' 
#' @usage 
#'    PredictionExtractorMetabDoDic$new()
#' @return 
#'    The object of class \code{PredictionExtractorMetabDoDic} 
#'    instantiated by the constructor
PredictionExtractorMetabDoDic <- R6Class(
   classname = "PredictionExtractorMetabDoDic",
   inherit = PredictionExtractor,
   public = list(
      extract = function() 
      {
         return(data.frame(
            do = self$model$output$do,
            pCO2 = self$model$output$pCO2
         ));
      }
   )
);
