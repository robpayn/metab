# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class ParameterTranslatorMetab (R6) ####

#' @export
#' 
#' @title 
#'    Parameter translator for metabolism models
#'
#' @description 
#'    A parameter translator allowing GPP, ER, and k600 to be used
#'    as proposed parameters for an objective function using a
#'    stream metabolism model. 
#' 
#' @return 
#'    The object of class \code{ParameterTranslatorMetab} 
#'    instantiated by the constructor
#'    
#' @section Extends \code{\link{ParameterTranslator}}:
#'   \code{$translate}
#'   \itemize{
#'     \item see \code{\link{ParameterTranslator_translate}}
#'     \item see \code{\link{ParameterTranslatorMetab_translate}}
#'   }
#'     
ParameterTranslatorMetab <- R6Class(
   classname = "ParameterTranslatorMetab",
   inherit = inferno::ParameterTranslator
);

#' @name ParameterTranslatorMetab_translate
#' 
#' @title 
#'   Translates parameters for a metabolism model
#'   
#' @description 
#'   Used to translate 3 parameters in the order of
#'   gross primary production, ecosystem respiration,
#'   and gas exchange at a Schmidt number of 600
#'   
#' @section Method of class:
#'   \code{\link{ParameterTranslatorMetab}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{ParameterTranslator_translate}}
#'   
ParameterTranslatorMetab$set(
   which = "public",
   name = "translate",
   value = function
      (
         params
      ) 
      {
         self$model$dailyGPP <- params[1];
         self$model$dailyER <- params[2];
         self$model$k600 <- params[3];
      }
)

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
#' @return 
#'    The object of class \code{PredictionExtractorMetabDo} 
#'    instantiated by the constructor
#'    
#' @section Extends \code{\link{PredictionExtractor}}:
#'   \code{$extract}
#'   \itemize{
#'     \item see \code{\link{PredictionExtractor_extract}}
#'     \item see \code{\link{PredictionExtractorMetabDo_extract}}
#'   }
#'   
PredictionExtractorMetabDo <- R6Class(
   classname = "PredictionExtractorMetabDo",
   inherit = inferno::PredictionExtractor
);

#' @name PredictionExtractorMetabDo_extract
#' 
#' @title 
#'   Extracts dissolved oxygen predictions from a metabolism model
#'   
#' @description 
#'   Used to extract dissolved oxygen predictions from a metabolism model
#'   
#' @section Method of class:
#'   \code{\link{PredictionExtractorMetabDo}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{PredictionExtractor_extract}}
#'   
PredictionExtractorMetabDo$set(
   which = "public",
   name = "extract",
   value = function
      ()
      {
         return(data.frame(
            do = self$model$output$do
         ));
      }
)

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
#' @return 
#'    The object of class \code{PredictionExtractorMetabDoDic} 
#'    instantiated by the constructor
#'    
#' @section Extends \code{\link{PredictionExtractor}}:
#'   \code{$extract}
#'   \itemize{
#'     \item see \code{\link{PredictionExtractor_extract}}
#'     \item see \code{\link{PredictionExtractorMetabDoDic_extract}}
#'   }
#'   
PredictionExtractorMetabDoDic <- R6Class(
   classname = "PredictionExtractorMetabDoDic",
   inherit = inferno::PredictionExtractor
);

#' @name PredictionExtractorMetabDoDic_extract
#' 
#' @title 
#'   Extracts dissolved oxygen and dissolved carbon dioxide
#'   predictions from a metabolism model.
#'   
#' @description 
#'   Used to extract dissolved oxygen and dissolved carbon dioxide
#'   predictions from a metabolism model
#'   
#' @section Method of class:
#'   \code{\link{PredictionExtractorMetabDoDic}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{PredictionExtractor_extract}}
#'   
PredictionExtractorMetabDoDic$set(
   which = "public",
   name = "extract",
   value = function
      () 
      {
         return(data.frame(
            do = self$model$output$do,
            pCO2 = self$model$output$pCO2
         ));
      }
)

# Class PredictionExtractorMetabDic (R6) ####

#' @export
#' 
#' @title 
#'    A prediction extractor for pCO2 output from a metabolism model (R6)
#'
#' @description 
#'    A prediction extractor allowing dissolved carbon dioxide concentrations
#'    from a stream metabolism model to be used as 
#'    predictions in an objective function.
#' 
#' @return 
#'    The object of class \code{PredictionExtractorMetabDic} 
#'    instantiated by the constructor
#'    
#' @section Extends \code{\link{PredictionExtractor}}:
#'   \code{$extract}
#'   \itemize{
#'     \item see \code{\link{PredictionExtractor_extract}}
#'     \item see \code{\link{PredictionExtractorMetabDic_extract}}
#'   }
#'   
PredictionExtractorMetabDic <- R6Class(
   classname = "PredictionExtractorMetabDic",
   inherit = inferno::PredictionExtractor
)

#' @name PredictionExtractorMetabDic_extract
#' 
#' @title 
#'   Extracts ddissolved carbon dioxide
#'   predictions from a metabolism model.
#'   
#' @description 
#'   Used to extract dissolved carbon dioxide
#'   predictions from a metabolism model
#'   
#' @section Method of class:
#'   \code{\link{PredictionExtractorMetabDic}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{PredictionExtractor_extract}}
#'   
PredictionExtractorMetabDic$set(
   which = "public",
   name = "extract",
   value = function
      () 
      {
         return(data.frame(
            pCO2 = self$model$output$pCO2
         ));
      }
)
