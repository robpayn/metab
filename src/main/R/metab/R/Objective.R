library(R6);
library(MASS);

# Class RandomVariable (R6) ####

RandomVariable <- R6Class(
   classname = "RandomVariable",
   public = list(
      density = function(val, log)
         {
            stop("Abstract function 'density' has not been implemented");   
         }
      )
   );

# Class RVUniform (R6) ####

RVUniform <- R6Class(
   classname = "RVUniform",
   inherit = RandomVariable,
   public = list(
      min = NULL,
      max = NULL,
      initialize = function(min, max)
         {
            self$min <- min;
            self$max <- max;
         },
      density = function(val, log = FALSE)
         {
            return(dunif(
               x = val, 
               min = self$min,
               max = self$max,
               log = log
               ));
         }
      )
   );


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
      negate = NULL,
      initialize = function(..., sd, negate = FALSE)
         {
            super$initialize(...);
            self$sd <- sd;
            self$negate <- negate;
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
            if (self$negate) {
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

# Class BayesLogLikelihood (R6) ####

#' Bayes posterior likelihood objective function class
#' 
#' Provides the tools for calculating a Bayes posterior 
#' likelihood value for the comparison of a model prediction and
#' observations. The Bayes likelihood is calculated by adding
#' the summed log likelihood from prior distributions of
#' parameters to the log likelihood of a provided objective 
#' function.
#' 
#' @export
#' @usage \code{BayesLogLikelihood$new(...)}
#' @param paramDists A list of random variables representing the prior
#'    prior probabilites for the parameters being estimated
#' @param baseObjFunc The objective function that will calculate the 
#'    fit metric (most like a log likelihood) that will be added to the
#'    sum of the prior log likelihoods to generate the overall objective
#'    function value
#' @param negate Optional switch to negate the objective function value
#'    to adjust for algorthims that minimize or maximize objective funciton
#'    values
#' @return The object of class \code{BayesLogLikelihood} created
#'    by the constructor
BayesLogLikelihood <- R6Class(
   classname = "BayesLogLikelihood",
   inherit = ObjectiveFunction,
   public = list(
      paramDists = NULL,
      baseObjFunc = NULL,
      logPriors = NULL,
      negate = NULL,
      initialize = function(
         paramDists, 
         baseObjFunc, 
         negate = FALSE
         ) 
         {
            # Initialize the super class
            # Models and processors are not
            # required because they are handled
            # by the base objective function
            super$initialize(
               model = NULL,
               parameterProcessor = NULL,
               predictionProcessor = NULL,
               );
         
            # Set the values of attributes based on
            # arguments to the constructor
            self$paramDists <- paramDists;
            self$baseObjFunc <- baseObjFunc;
            self$negate <- negate;
         },
      propose = function(params)
         {
            # Override the implementation of propose to allow for
            # operation of the base objective function calculations
            self$params <- params;
            self$baseObjFunc$propose(params);
            self$value <- self$compare(params);
            return(self$value);
         },
      realize = function()
         {
            # Override the implementation of realize to allow for
            # operation of the base objective function calculations
            self$baseObjFunc$realize();
            self$observation = self$baseObjFunc$observation;
         },
      compare = function(params)
         {
            # Calculate the log of the prior likelihoods
            self$logPriors <- mapply(
               FUN = function(paramDist, param)
                  {
                     return(paramDist$density(param, log = TRUE));   
                  },
               paramDist = self$paramDists,
               param = params
               );
            
            # Sum the priors with the result from the base objective
            # function (depending on value of negate switch)
            if (self$negate) {
               return(self$baseObjFunc$value - sum(self$logPriors));
            } else {
               return(self$baseObjFunc$value + sum(self$logPriors));
            }
         }
      )
   );

# Class BayesAMMCMCSampler (R6) ####

#' A Bayesian Adaptive Metropolis Markov Chain Monte Carlo Sampler
#' 
#' Provides the tools for executing a Bayesian optimization using a
#' Markov Chain sampler with an adaptive covariance matrix to determine
#' the step size. The Metropolis algorith is used to determine whether
#' a proposed parameter set is accepted into the ensemble estimate of
#' the posterior distributions of parameter estimates.
#' 
#' @export
#' @usage \code{BayesAMMCMCSampler$new(...)}
#' @param baseObjFunc The objective function used to calculate the base
#'    likelihood.  The sum of the log prior likelihoods are added to this
#'    to generate the overall value of the Bayesian objective function
#' @param initialParams A vector with initial values for the parameter 
#'    being estimated
#'    (initial location in parameter space for the Markov Chain)
#' @param burninStepSD The standard deviations used to stochastically 
#'    constrain Markov Chain step sizes during the burnin phase
#' @param burninRealizations Number of realizations for the burnin phase
#' @param staticStepSD The standard deviations used to stochastically
#'    constrain Markov Chain step sizes during the static Metropolis
#'    phase.  By default this will be the same as burninStepSD.
#' @param staticRealizations Number of realizations for the static 
#'    Metropolis phase
#' @param adaptiveRealizations Number of realizations for the phase
#'    where the covariance used to constrain Markov Chain step size is
#'    adapted according to the covariance of previous accepted parameter
#'    sets in the ensemble
#' @param adaptiveCovarianceFactor An adaptive covariance factor (scalar) 
#'    which is multiplied by the raw covariance of the parameter ensemble.
#'    This can be used to adjust for the acceptance rate during the
#'    adaptive phase. By default this is set to 1.
#' @param tinyIdentFactor The value added the diagonal of the covariance
#'    matrix to preven zero values.  By default this is 1e-20.
#' @return The object of class \code{BayesAMMCMCSampler} created
#'    by the constructor
BayesAMMCMCSampler <- R6Class(
   classname = "AMMCMCSampler",
   public = list(
      bayesObjFunc = NULL,
      initialParams = NULL,
      burninRealizations = NULL,
      startCovarianceIndex = NULL,
      staticRealizations = NULL,
      totalStaticRealizations = NULL,
      adaptiveRealizations = NULL,
      totalRealizations = NULL,
      adaptiveCovarianceFactor = NULL,
      paramSamples = NULL,
      paramProposed = NULL,
      likeSamples = NULL,
      burninStepSD = NULL,
      staticStepSD = NULL,
      tinyIdentFactor = NULL,
      initialize = function(
         bayesObjFunc, 
         initialParams, 
         burninStepSD,
         burninRealizations,
         staticStepSD = burninStepSD,
         staticRealizations,
         adaptiveRealizations,
         adaptiveCovarianceFactor = 1,
         tinyIdentFactor = 1e-20
         )
         {
            # Assign attributes according to arguments
            self$bayesObjFunc <- bayesObjFunc;   
            self$initialParams <- initialParams;
            self$burninStepSD <- burninStepSD;
            self$burninRealizations <- burninRealizations;
            self$staticStepSD <- staticStepSD;
            self$staticRealizations <- staticRealizations;
            self$adaptiveRealizations <- adaptiveRealizations;
            self$adaptiveCovarianceFactor <- adaptiveCovarianceFactor;
            self$tinyIdentFactor <- tinyIdentFactor;
            
            # Derive attributes for indexing phases of AMMCMC algorithm
            self$startCovarianceIndex <- burninRealizations + 1;
            self$totalStaticRealizations <- 
               self$burninRealizations + self$staticRealizations;
            self$totalRealizations <-
               self$totalStaticRealizations + self$adaptiveRealizations;
            
            # Create the matrix for the parameter samples and
            # populate the first row
            self$paramSamples <- matrix(
               nrow = self$totalRealizations, 
               ncol = length(initialParams)
               );
            colnames(self$paramSamples) <- names(initialParams);
            self$paramSamples[1,] <- initialParams;

            # Create the matrix for the parameters proposed and
            # populate the first row
            self$paramProposed <- matrix(
               nrow = self$totalRealizations, 
               ncol = length(initialParams)
               );
            colnames(self$paramProposed) <- names(initialParams);
            self$paramProposed[1,] <- initialParams;

            # Create the data frame for the likelihood ensemble and
            # populate the first row
            self$likeSamples <- data.frame(
               posterior = numeric(length = self$totalRealizations),
               likelihood = numeric(length = self$totalRealizations),
               wasAccepted = logical(length = self$totalRealizations)
               );
            self$bayesObjFunc$propose(self$initialParams);
            self$likeSamples$posterior[1] <- self$bayesObjFunc$value;
            self$likeSamples$likelihood[1] <- self$bayesObjFunc$baseObjFunc$value;
            self$likeSamples$wasAccepted[1] <- TRUE;
         },
      optimize = function()
         {
            numParams = ncol(self$paramSamples);
            tinyIdent = 
               diag(numParams) * 
               self$tinyIdentFactor * 
               self$adaptiveCovarianceFactor;
            
            # Start the static convariance burnin phase
            loop <- 2:self$burninRealizations;
            for(realizationCount in loop) {
               # Take a Markov Chain step in parameter space based on a
               # static covariance and propose the parameter set to the
               # Bayesian criterion
               self$paramProposed[realizationCount,] <- 
                  self$paramSamples[(realizationCount - 1),] +
                  rnorm(
                     n = numParams,
                     mean = 0,
                     sd = self$burninStepSD
                  );
               self$propose(realizationCount);
            }
            
            # Start the static covariance Metropolis phase
            loop <- (self$burninRealizations + 1):self$totalStaticRealizations;
            for(realizationCount in loop) {
               # Take a Markov Chain step in parameter space based on a
               # static covariance and propose the parameter set to the
               # Bayesian criterion
               self$paramProposed[realizationCount,] <- 
                  self$paramSamples[(realizationCount - 1),] +
                  rnorm(
                     n = numParams,
                     mean = 0,
                     sd = self$staticStepSD
                     );
               self$propose(realizationCount);
            }
            
            # Start the adaptive covariance Metropolis phase
            loop <- (self$totalStaticRealizations + 1):self$totalRealizations;
            for(realizationCount in loop) {
               
               # Adapt the covariance matrix based on the current parameter
               # ensemble
               covarianceIndeces <- 
                  self$startCovarianceIndex:(realizationCount - 1);
               covarianceMatrix <- 
                  cov(self$paramSamples[covarianceIndeces,]) *
                  self$adaptiveCovarianceFactor +
                  tinyIdent;
               
               # Take a Markov Chain step in parameter space based on an
               # adapted covariance and propose the parameter set to the
               # Bayesian criterion
               self$paramProposed[realizationCount,] <- 
                  self$paramSamples[(realizationCount - 1),] +
                  mvrnorm(
                     n = 1,
                     mu = rep(0, numParams),
                     Sigma = covarianceMatrix
                  );
               self$propose(realizationCount);
            }
         },
      propose = function(index)
         {
            # Determine the difference in posterior likelihood
            # between proposed parameter set and the previous
            # Markov Chain step
            posteriorProposed <-
               self$bayesObjFunc$propose(self$paramProposed[index,]);
            if(is.infinite(posteriorProposed)) {
               deltaPosterior <- 0;
            } else {
               deltaPosterior <- exp(
                  posteriorProposed - 
                  self$likeSamples$posterior[index - 1]
                  );
            }
            
            # Record results of current realization depending on whether
            # proposed parameter set is accepted or rejected relative to
            # previous Markov Chain step
            if(runif(n = 1) < deltaPosterior) {
               # Accept the proposed parameters
               self$likeSamples$wasAccepted[index] <- TRUE;
               self$paramSamples[index,] <- self$paramProposed[index,];
               self$likeSamples$posterior[index] <- 
                  self$bayesObjFunc$value;
               self$likeSamples$likelihood[index] <- 
                  self$bayesObjFunc$baseObjFunc$value;
            } else {
               # Reject the proposed parameters
               self$likeSamples$wasAccepted[index] <- FALSE;
               self$paramSamples[index,] <- 
                  self$paramSamples[(index - 1),];
               self$likeSamples$posterior[index] <- 
                  self$likeSamples$posterior[index - 1];
               self$likeSamples$likelihood[index] <- 
                  self$likeSamples$likelihood[index - 1];
            }
         }
      )
   );
