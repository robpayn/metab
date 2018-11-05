library(R6);

# Class Model (R6) ####

#' Abstract model class
#' 
#' A class representing a model used to make a prediction
#' that is compared to an observation for calculation of an
#' objective function.
#' 
#' This class is abstract and is not intended to be instantiated
#' directly. 
#' 
#' @export
Model <- R6Class(
   classname = "Model",
   public = list(
      run = function() 
         {
            stop("Abstract function 'run' has not been implemented.");
         }
      )
   );

# Class ParameterProcessor ####

ParameterProcessor <- R6Class(
   classname = "ParameterProcessor",
   public = list(
      process = function(model, params)
         {
            stop("Abstract function 'ParameterProcessor.process' 
                 has not been implemented");
         }
      )
   );

# Class PredictionProcessor ####

PredictionProcessor <- R6Class(
   classname = "PredictionProcessor",
   public = list(
      process = function(model)
         {
            stop("Abstract function 'PredictionProcessor.process' 
                 has not been implemented");
         }
      )
   );

# Class SynthErrorProcessor ####

SynthErrorProcessor <- R6Class(
   classname = "SynthErrorProcessor",
   public = list(
      process = function(objFunc)
         {
            stop("Abstract function 'SynthErrorProcessor.process' 
                    has not been implemented");
         }
      )
   );

# Class SynthErrorNormal ####

SynthErrorNormal <- R6Class(
   classname = "SynthErrorNormal",
   inherit = SynthErrorProcessor,
   public = list(
      mean = NULL,
      sd = NULL,
      initialize = function(mean, sd)
         {
            self$mean <- mean;
            self$sd <- sd;
         },
      process = function(objFunc)
         {
            objFunc$observation <- data.frame(mapply(
               FUN = function(pred, mean, sd) 
                  {
                     return(pred + rnorm(n = nrow(objFunc$synthPrediction), mean = mean, sd = sd));
                  }, 
               pred = objFunc$synthPrediction,
               mean = self$mean,
               sd = self$sd,
               SIMPLIFY = FALSE
               ));
         }
      )
   );

# Class ObjectiveFunction (R6) ####

#' Abstract objective function class
#' 
#' A class representing an objective function for calculating a 
#' value that represents to what degree a model prediction matches
#' an observation.
#' This class is abstract and is not intended to be instantiated
#' directly. The constructor is only intended to be called by
#' an extending subclass.
#' 
#' @export
#' @usage \code{ObjectiveFunction$new(...)}
#' @param model The model used to generate the predictions to be
#'    compared to the observations by the objective function
#' @param observation The observatins to compare to the predictions
#'    by the objective function
ObjectiveFunction <- R6Class(
   classname = "ObjectiveFunction",
   public = list(
      params = NULL,
      parameterProcessor = NULL,
      model = NULL,
      prediction = NULL,
      predictionProcessor = NULL,
      synthPrediction = NULL,
      observation = NULL,
      synthErrorProcessor = NULL,
      value = NULL,
      initialize = function(
         model,
         parameterProcessor,
         predictionProcessor,
         synthErrorProcessor = NULL,
         observation = NULL
         ) 
         {
            self$model <- model;
            self$parameterProcessor <- parameterProcessor;
            self$predictionProcessor <- predictionProcessor;
            self$observation <- observation;
            self$synthErrorProcessor <- synthErrorProcessor;
            if (!is.null(self$synthErrorProcessor)) {
               self$model$run();
               self$prediction <- self$predictionProcessor$process(self$model);
               self$synthPrediction <- self$prediction;
            }
         },
      propose = function(params)
         {
            self$params <- params;
            self$parameterProcessor$process(model = self$model, params = params);
            self$model$run();
            self$prediction <- self$predictionProcessor$process(model = self$model);
            self$value <- self$compare(params);
            return(self$value);
         },
      realize = function()
         {
            self$synthErrorProcessor$process(objFunc = self);
         },
      compare = function(params) 
         {
            stop("Abstract function 'compare' has not been implemented.");
         }
      )
   );

# Class ObjFuncLogLikelihood (R6) ####

#' Log likelihood objective function class
#' 
#' Provides the tools for calculating a log likelihood
#' value for the comparison of a model prediction and
#' observations.
#' 
#' @export
#' @usage \code{ObjFuncLogLikelihood$new(...)}
#' @param ... Arguments passed to constructor \code{ObjFuncLogLikelihood$new(...)} will be 
#'    passed generically to the constructor for the superclass \code{ObjectiveFunction}. 
#'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
#'    of these arguments.
#' @param sd A vector of standard deviations to be used for calculating the 
#'    likelihood. There should be the same number of standard deviations as
#'    the number of columns in the dataframes of predictons and observations
#'    to be compared by the objective function.
#' @return The object of class \code{ObjFuncLogLikelihood} created
#'    by the constructor
ObjFuncLogLikelihood <- R6Class(
   classname = "ObjFuncLogLikelihood",
   inherit = ObjectiveFunction,
   public = list(
      sd = NULL,
      invert = NULL,
      initialize = function(..., sd, invert = FALSE)
         {
            super$initialize(...);
            self$sd <- sd;
            self$invert <- invert;
         },
      compare = function(params)
         {
            sd <- self$sd;
            estimateSD <- is.nan(sd);
            if(any(estimateSD)) {
               replaceIndeces <- which(estimateSD);
               paramIndeces <- 
                  (length(params) - length(replaceIndeces) + 1):length(params);
               sd[replaceIndeces] <- params[paramIndeces];
            }
            logLike <- 0;
            for (colNum in 1:length(self$prediction)) {
               logLike <- logLike + sum(dnorm(
                  x = self$observation[[colNum]], 
                  mean = self$prediction[[colNum]], 
                  sd = sd[colNum],
                  log = TRUE
                  ))
            }
            if (self$invert) {
               return(-logLike);
            } else {
               return(logLike);
            }
         }         
      )
   );

#' Calculate log likelihood
#' 
#' Calculates the negative of the log likelihood for a model 
#'    prediction based on a multivariate comparison with observations.
#'    Observations from the ObjectiveFunction object are compared
#'    to predictions from the Model object associated with the 
#'    ObjectiveFunction object.
#' 
#' @name ObjFuncLogLikelihood_compare
#' @return The negative of the log likelihood from the comparison of
#'    model prediction with observations
NULL