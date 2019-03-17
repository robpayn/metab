# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class ParameterProcessorMetab (R6) ####

#' @export
#' 
#' @title 
#'    Class ParameterProcessorMetab
#'
#' @description 
#'    A parameter processor allowing GPP, ER, and k600 to be used
#'    as proposed parameters for an objective function using a
#'    stream metabolism model.
#' 
#' @usage 
#'    ParameterProcessorMetab$new()
#' @return 
#'    The object of class \code{ParameterProcessorMetab} 
#'    instantiated by the constructor
ParameterProcessorMetab <- R6Class(
   classname = "ParameterProcessorMetab",
   inherit = ParameterProcessor,
   public = list(
      process = function(params) 
      {
         self$model$dailyGPP <- params[1];
         self$model$dailyER <- params[2];
         self$model$k600 <- params[3];
      }
   )
);

# Class PredictionProcessorMetabDo (R6) ####

#' @export
#' 
#' @title 
#'    Class PredictionProcessorMetabDo (R6)
#'
#' @description 
#'    A prediction processor allowing dissolved oxygen outputs
#'    from a stream metabolism model to be used as predictions
#'    in an objective function.
#' 
#' @usage 
#'    PredictionProcessorMetabDo$new()
#' @return 
#'    The object of class \code{PredictionProcessorMetabDo} 
#'    instantiated by the constructor
PredictionProcessorMetabDo <- R6Class(
   classname = "PredictionProcessorMetabDoDic",
   inherit = PredictionProcessor,
   public = list(
      process = function() 
      {
         return(data.frame(
            do = self$model$output$do
         ));
      }
   )
);

# Class PredictionProcessorMetabDoDic (R6) ####

#' @export
#' 
#' @title 
#'    Class PredictionProcessorMetabDoDic (R6)
#'
#' @description 
#'    A prediction processor allowing dissolved oxygen and carbo
#'    dioxide outputs from a stream metabolism model to be used as 
#'    predictions in an objective function.
#' 
#' @usage 
#'    PredictionProcessorMetabDoDic$new()
#' @return 
#'    The object of class \code{PredictionProcessorMetabDoDic} 
#'    instantiated by the constructor
PredictionProcessorMetabDoDic <- R6Class(
   classname = "PredictionProcessorMetabDoDic",
   inherit = PredictionProcessor,
   public = list(
      process = function() 
      {
         return(data.frame(
            do = self$model$output$do,
            pCO2 = self$model$output$pCO2
         ));
      }
   )
);
