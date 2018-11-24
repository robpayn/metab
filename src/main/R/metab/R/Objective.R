library(R6);
library(MASS);

# Class RandomVariable (R6) ####

#' Abstract random variable class (R6)
#' 
#' A class representing a random variable with a given distribution.
#' This class is abstract and is not intended to be instantiated
#' directly. 
#' 
#' @export
RandomVariable <- R6Class(
   classname = "RandomVariable",
   public = list(
      density = function(val, log)
         {
            stop("Abstract function 'density' has not been implemented");   
         }
      )
   );

#' Provides the probability density 
#' 
#' Provides the probability density for a given value in the 
#' distribution of the random variable.
#' 
#' @name RandomVariable_density
#' @param val The value for which the probability density is requested
#' @param log A boolean switch for requesting the log probability density
#' @return The probability density of the value requested 
NULL


# Class RVUniform (R6) ####

#' Unifrom random variable class (R6)
#' 
#' Provides tools for working with a random variable with
#' a uniform distribution defined by minimum and maximum
#' values
#' 
#' @export
#' @usage \code{RVUnifrom$new(min, max)}
#' @param min Mininum value for the uniform distribution
#' @param max Maximum value for the uniform distribution
#' @return The object of class \code{RVUniform} created
#'    by the constructor
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

#' Provides the probability density for a uniform distribution
#' 
#' Method to provide the probability density for a given value in the 
#' uniform distribution represented by the object
#' 
#' @name RVUniform_density
#' @param val The value for which the probability density is requested
#' @param log A boolean switch for requesting the log probability density
#' @return The probability density of the value requested 
NULL

# Class RVNormal (R6) ####

#' Normally distributed random variable class (R6)
#' 
#' Provides tools for working with a random variable with
#' a normal distribution defined by a mean and standard deviation
#' 
#' @export
#' @usage \code{RVNormal$new(mean, sd)}
#' @param mean Mean of the distribution
#' @param sd Standard deviation of the uniform distribution
#' @return The object of class \code{RVNormal} created
#'    by the constructor
RVNormal <- R6Class(
   classname = "RVNormal",
   inherit = RandomVariable,
   public = list(
      mean = NULL,
      sd = NULL,
      initialize = function(mean, sd)
         {
            self$mean <- mean;
            self$sd <- sd;
         },
      density = function(val, log = FALSE)
         {
            return(dnorm(
               x = val, 
               mean = self$mean,
               sd = self$sd,
               log = log
               ));
         }
      )
   );

#' Provides the probability density for a normal distribution
#' 
#' Method to provide the probability density for a given value in the 
#' normal distribution represented by the object
#' 
#' @name RVNormal_density
#' @param val The value for which the probability density is requested
#' @param log A boolean switch for requesting the log probability density
#' @return The probability density of the value requested 
NULL


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
      model = NULL,
      initialize = function(model = NULL)
         {
            self$model <- model;  
         },
      process = function(params)
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
      model = NULL,
      initialize = function(model = NULL)
         {
            self$model <- model;  
         },
      process = function()
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
      objFunc = NULL,
      process = function()
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
      process = function()
         {
            return(data.frame(mapply(
               FUN = function(pred, mean, sd) 
                  {
                     return(
                        pred + rnorm(
                           n = nrow(self$objFunc$synthPrediction), 
                           mean = mean, 
                           sd = sd
                           )
                        );
                  }, 
               pred = self$objFunc$synthPrediction,
               mean = self$mean,
               sd = self$sd,
               SIMPLIFY = FALSE
               )));
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
      multivariateValues = NULL,
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
            if(is.null(self$parameterProcessor$model)) {
               self$parameterProcessor$model <- self$model;
            }
            self$predictionProcessor <- predictionProcessor;
            if(is.null(self$predictionProcessor$model)) {
               self$predictionProcessor$model <- self$model;
            }
 
            self$synthErrorProcessor <- synthErrorProcessor;
            if (!is.null(self$synthErrorProcessor)) {
               self$synthErrorProcessor$objFunc <- self;
               self$model$run();
               self$prediction <- self$predictionProcessor$process();
               self$synthPrediction <- self$prediction;
               self$realize();
            } else {
               self$observation <- observation;
            }
         },
      propose = function(params)
         {
            self$params <- params;
            self$parameterProcessor$process(params = params);
            self$model$run();
            self$prediction <- self$predictionProcessor$process();
            if(is.null(self$prediction)) {
               self$multivariateValues <- NULL;
               self$value <- NULL;   
            } else {
               self$multivariateValues <- self$compare(params);
               self$value <- sum(self$multivariateValues);
            }
            return(self$value);
         },
      realize = function()
         {
            self$observation <- self$synthErrorProcessor$process();
         },
      compare = function(params) 
         {
            stop("Abstract function 'compare' has not been implemented.");
         }
      )
   );

# Class LogLikelihood (R6) ####

#' Log likelihood objective function class
#' 
#' Provides the tools for calculating a log likelihood
#' value for the comparison of a model prediction and
#' observations.
#' 
#' @export
#' @usage \code{LogLikelihood$new(...)}
#' @param ... Arguments passed to constructor \code{LogLikelihood$new(...)} will be 
#'    passed generically to the constructor for the superclass \code{ObjectiveFunction}. 
#'    See documentation for the class \code{\link{ObjectiveFunction}} for a description
#'    of these arguments.
#' @param sd A vector of standard deviations to be used for calculating the 
#'    likelihood. There should be the same number of standard deviations as
#'    the number of columns in the dataframes of predictons and observations
#'    to be compared by the objective function.
#' @param negate A boolean switch indicating if objective function value
#'    should be negated for switching between maximization or minimization
#'    algorithms
#' @return The object of class \code{LogLikelihood} created
#'    by the constructor
LogLikelihood <- R6Class(
   classname = "LogLikelihood",
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
               replaceIndices <- which(estimateSD);
               paramIndices <- 
                  (length(params) - length(replaceIndices) + 1):length(params);
               sd[replaceIndices] <- params[paramIndices];
            }
            logLike <- mapply(
               FUN = function(p, o, sd) 
                  {
                     sum(dnorm(
                        x = o,
                        mean = p,
                        sd = sd,
                        log = TRUE
                        ))
                  },
               p = self$prediction,
               o = self$observation,
               sd = as.list(sd)
               );
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
#' @name LogLikelihood_compare
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
            # Parameter and prediction processors are not 
            # necessary
            self$parameterProcessor <- NULL;
            self$predictionProcessor <- NULL;
            
            # Set the values of attributes based on
            # arguments to the constructor
            self$model <- baseObjFunc$model;
            self$negate <- negate;
            self$paramDists <- paramDists;
            self$baseObjFunc <- baseObjFunc;
            self$synthPrediction <- baseObjFunc$synthPrediction;
            self$observation <- baseObjFunc$observation;
         },
      propose = function(params)
         {
            # Override the implementation of propose to allow for
            # operation of the base objective function calculations
            self$params <- params;
            self$baseObjFunc$propose(params);
            if(is.null(self$baseObjFunc$value)) {
               self$value <- NULL;
            } else {
               self$value <- self$compare(params);
            }
            return(self$value);
         },
      realize = function()
         {
            # Override the implementation of realize to allow for
            # operation of the base objective function calculations
            self$baseObjFunc$realize();
            self$observation <- self$baseObjFunc$observation;
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

# Class Criterion (R6) ####

Criterion <- R6Class(
   classname = "Criterion",
   public = list(
      isAccepted = function(prob, probRef)
         {
            stop("Method 'isAccepted' has not been implemented");
         }
      )
   );

# Class CriterionLogLikelihood (R6) ####

CriterionLogLikelihood <- R6Class(
   classname = "CriterionLogLikelihood",
   inherit = Criterion,
   public = list(
      isAccepted = function(prob, probRef)
      {
         if(is.null(prob)) {
            deltaPosterior <- 0;
         } else {
            deltaPosterior <- exp(prob - probRef);
         }
         return(runif(n = 1) < deltaPosterior);
      }
   )
);

# Class StatsLogger (R6) ####

StatsLogger <- R6Class(
   classname = "StatsLogger",
   public = list(
      numRows = NULL,
      objFunc = NULL,
      stats = NULL,
      statsFile = NULL,
      filePath = NULL,
      initialize = function(
         filePath = NULL, 
         statsFile = "stats.csv"
         )
         {
            self$filePath <- filePath;
            if(is.null(self$filePath)) {
               self$statsFile <- statsFile;
            } else {
               self$statsFile <- paste(
                  self$filePath,
                  statsFile,
                  sep = "/"
                  );
            }
         },
      buildLog = function(numRows, objFunc, filePath = "./output")
         {
            self$numRows <- numRows;
            self$objFunc <- objFunc;
            self$stats <- data.frame(
               objective = numeric(length = self$numRows),
               propObjective = numeric(length = self$numRows),
               wasAccepted = logical(length = self$numRows)
               );
            if(is.null(self$filePath)) {
               self$filePath <- filePath;
               self$statsFile <- paste(
                  self$filePath,
                  self$statsFile,
                  sep = "/"
               );
            }
         },
      logProposed = function(index)
         {
            self$stats[index, 2] <- self$objFunc$value;
         },
      logAccepted = function(index)
         {
            self$stats[index, 1] <- self$objFunc$value;
            self$stats[index, 3] <- TRUE;
         },
      logRejected = function(index)
         {
            self$stats[index, 1] <- self$stats[index - 1, 1];
            self$stats[index, 3] <- FALSE;
         },
      writeFirstRow = function()
         {
            dir.create(
               path = self$filePath, 
               showWarnings = FALSE,
               recursive = TRUE
               );
            write.table(
               x = self$stats[1,], 
               file = self$statsFile, 
               append = FALSE,
               sep = ",",
               col.names = TRUE,
               row.names = FALSE,
               quote = TRUE
               );
         },
      writeRow = function(index)
         {
            write.table(
               x = self$stats[index,], 
               file = self$statsFile, 
               append = TRUE,
               sep = ",",
               col.names = FALSE,
               row.names = FALSE,
               quote = TRUE
               );
         }
      )
   );

# Class StatsLoggerBayes (R6) ####

StatsLoggerBayes <- R6Class(
   classname = "StatsLoggerBayes",
   inherit = StatsLogger,
   public = list(
      buildLog = function(numRows, ...)
         {
            super$buildLog(numRows, ...);
            names(self$stats)[1] <- "posterior";
            names(self$stats)[2] <- "propPosterior";
            self$stats <- cbind(
               self$stats,
               data.frame(
                  likelihood = numeric(length = self$numRows),
                  propLikelihood = numeric(length = self$numRows)
               )
            );
         },
      logProposed = function(index)
         {
            super$logProposed(index);
            self$stats[index, 5] <- self$objFunc$baseObjFunc$value;
         },
      logAccepted = function(index)
         {
            super$logAccepted(index);
            self$stats[index, 4] <- self$objFunc$baseObjFunc$value;
         },
      logRejected = function(index)
         {
            super$logRejected(index);
            self$stats[index, 4] <- self$stats[index - 1, 4];
         }
      )
   );

# Class AMMCMCSampler (R6) ####

#' An Adaptive Metropolis Markov Chain Monte Carlo Sampler
#' 
#' Provides the tools for executing an optimization using a
#' Markov Chain sampler with an adaptive covariance matrix to determine
#' the step size. The Metropolis algorithm is used to determine whether
#' a proposed parameter set is accepted into the ensemble estimate of
#' the posterior distributions of parameter estimates.
#' 
#' @export
#' @usage \code{AMMCMCSampler$new(...)}
#' @param baseObjFunc The objective function used to calculate the base
#'    likelihood.  The sum of the log prior likelihoods are added to this
#'    to generate the overall value of the objective function
#' @param initialParams A vector with initial values for the parameter 
#'    being estimated
#'    (initial location in parameter space for the Markov Chain)
#' @param burninCovariance The standard deviations used to stochastically 
#'    constrain Markov Chain step sizes during the burnin phase
#' @param burninRealizations Number of realizations for the burnin phase
#' @param staticCovariance The standard deviations used to stochastically
#'    constrain Markov Chain step sizes during the static Metropolis
#'    phase.  By default this will be the same as burninCovariance.
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
#' @return The object of class \code{AMMCMCSampler} created
#'    by the constructor
AMMCMCSampler <- R6Class(
   classname = "AMMCMCSampler",
   public = list(
      objFunc = NULL,
      prevProb = NULL,
      maxProb = NULL,
      maxProbIndex = NULL,
      initialParams = NULL,
      numParams = NULL,
      burninRealizations = NULL,
      startCovarianceIndex = NULL,
      staticRealizations = NULL,
      totalStaticRealizations = NULL,
      adaptiveRealizations = NULL,
      totalRealizations = NULL,
      adaptiveCovarianceFactor = NULL,
      paramSamples = NULL,
      paramSamplesFile = NULL,
      paramProposals = NULL,
      paramProposalsFile = NULL,
      statsLogger = NULL,
      filesPath = NULL,
      writeFiles = NULL,
      burninCovariance = NULL,
      staticCovariance = NULL,
      tinyIdentFactor = NULL,
      adaptiveCovariance = NULL,
      criterion = NULL,
      initialize = function(
         objFunc, 
         initialParams, 
         burninCovariance,
         burninRealizations,
         staticCovariance = burninCovariance,
         staticRealizations,
         adaptiveRealizations,
         criterion = CriterionLogLikelihood$new(),
         adaptiveCovarianceFactor = 1,
         tinyIdentFactor = 1e-20,
         writeFiles = TRUE,
         filesPath = "./output",
         paramProposalsFile = "paramProposals.csv",
         paramSamplesFile = "paramSamples.csv",
         statsLogger = StatsLogger$new()
         )
         {
            # Assign attributes according to arguments
            self$objFunc <- objFunc;   
            self$criterion <- criterion;
            self$initialParams <- initialParams;
            self$burninCovariance <- burninCovariance;
            self$burninRealizations <- burninRealizations;
            self$staticCovariance <- staticCovariance;
            self$staticRealizations <- staticRealizations;
            self$adaptiveRealizations <- adaptiveRealizations;
            self$adaptiveCovarianceFactor <- adaptiveCovarianceFactor;
            self$tinyIdentFactor <- tinyIdentFactor;
            self$filesPath <- filesPath;
            self$paramProposalsFile <- paste(
               filesPath,
               paramProposalsFile,
               sep = "/"
               );
            self$paramSamplesFile <- paste(
               filesPath,
               paramSamplesFile,
               sep = "/"
               );
            self$writeFiles <- writeFiles;
            
            # Derive attributes for indexing phases of AMMCMC algorithm
            self$numParams <- length(initialParams);
            self$startCovarianceIndex <- burninRealizations + 1;
            self$totalStaticRealizations <- 
               self$burninRealizations + self$staticRealizations;
            self$totalRealizations <-
               self$totalStaticRealizations + self$adaptiveRealizations;
            
            # Create the matrix for the parameter samples and
            # populate the first row
            self$paramSamples <- matrix(
               nrow = self$totalRealizations, 
               ncol = self$numParams,
               dimnames = list(NULL, names(initialParams))
            );
            self$paramSamples[1,] <- initialParams;

            # Create the matrix for the parameters proposed and
            # populate the first row
            self$paramProposals <- matrix(
               nrow = self$totalRealizations, 
               ncol = self$numParams,
               dimnames = list(NULL, names(initialParams))
               );
            self$paramProposals[1,] <- initialParams;
            
            # Configure the statistics logger object
            self$statsLogger <- statsLogger;
            self$statsLogger$buildLog(
               self$totalRealizations, 
               objFunc,
               self$filesPath
               );
   
            # Create the first row of samples and proposals from the
            # initial parameter set
            self$prevProb <- self$objFunc$propose(self$initialParams);
            self$maxProb <- self$prevProb;
            self$maxProbIndex <- 1;
            self$statsLogger$logProposed(1);
            self$statsLogger$logAccepted(1);
         },
      optimize = function()
         {
            # Set up the output files and write the first line
            if (self$writeFiles) {
               dir.create(
                  path = self$filesPath, 
                  showWarnings = FALSE,
                  recursive = TRUE
                  );
               write.table(
                  x = data.frame(self$paramSamples[1:2,])[1,], 
                  file = self$paramSamplesFile, 
                  append = FALSE,
                  sep = ",",
                  col.names = TRUE,
                  row.names = FALSE,
                  quote = TRUE
                  );
               write.table(
                  x = data.frame(self$paramProposals[1:2,])[1,], 
                  file = self$paramProposalsFile, 
                  append = FALSE,
                  sep = ",",
                  col.names = TRUE,
                  row.names = FALSE,
                  quote = TRUE
                  );
               self$statsLogger$writeFirstRow();
            }

            # Create a tiny identity matrix to avoid zeros in 
            # diagonal of covariance matrix
            tinyIdent = 
               diag(self$numParams) * 
               self$tinyIdentFactor * 
               self$adaptiveCovarianceFactor;
            
            # Start the static convariance burnin phase
            loop <- 2:self$burninRealizations;
            for(realizationCount in loop) {
               # Take a Markov Chain step in parameter space based on a
               # static covariance and propose the new parameter set
               self$paramProposals[realizationCount,] <- 
                  self$paramSamples[realizationCount - 1,] +
                  mvrnorm(
                     n = 1,
                     mu = rep(0, self$numParams),
                     Sigma = self$burninCovariance
                     );
               self$propose(realizationCount);
            }
            
            # Start the static covariance Metropolis phase
            loop <- (self$burninRealizations + 1):self$totalStaticRealizations;
            for(realizationCount in loop) {
               # Take a Markov Chain step in parameter space based on a
               # static covariance and propose the new parameter set
               self$paramProposals[realizationCount,] <- 
                  self$paramSamples[realizationCount - 1,] +
                  mvrnorm(
                     n = 1,
                     mu = rep(0, self$numParams),
                     Sigma = self$staticCovariance
                     );
               self$propose(realizationCount);
            }
            
            # Start the adaptive covariance Metropolis phase
            loop <- (self$totalStaticRealizations + 1):self$totalRealizations;
            for(realizationCount in loop) {
               prevRealization <- realizationCount - 1;
               # Adapt the covariance matrix based on the current parameter
               # ensemble
               covarianceIndices <- self$startCovarianceIndex:prevRealization;
               self$adaptiveCovariance <- 
                  cov(self$paramSamples[covarianceIndices,]) *
                  self$adaptiveCovarianceFactor +
                  tinyIdent;
               
               # Take a Markov Chain step in parameter space based on an
               # adapted covariance and propose the new parameter set 
               self$paramProposals[realizationCount,] <- 
                  self$paramSamples[prevRealization,] +
                  mvrnorm(
                     n = 1,
                     mu = rep(0, self$numParams),
                     Sigma = self$adaptiveCovariance
                  );
               self$propose(realizationCount);
            }
         },
      propose = function(index, prevIndex = index - 1)
         {
            # Use the criterion object to determine if proposal
            # should be accepted
            accept <- self$criterion$isAccepted(
                  self$objFunc$propose(self$paramProposals[index,]),
                  self$prevProb
               );
            self$statsLogger$logProposed(index);
            
            # Record results of current realization depending on whether
            # proposed parameter set is accepted or rejected relative to
            # previous Markov Chain step
            if(accept) {
               # Accept the proposed parameters
               self$paramSamples[index,] <- self$paramProposals[index,];
               self$prevProb <- self$objFunc$value;
               if(self$prevProb > self$maxProb) {
                  self$maxProb = self$prevProb;
                  self$maxProbIndex = index;
               }
               self$statsLogger$logAccepted(index);
            } else {
               # Reject the proposed parameters
               self$paramSamples[index,] <- self$paramSamples[prevIndex,];
               self$statsLogger$logRejected(index);
            }
            
            # Write results to output files
            if(self$writeFiles) {
               write(
                  self$paramSamples[index,], 
                  file = self$paramSamplesFile, 
                  ncolumns = self$numParams,
                  sep = ",",
                  append = TRUE
                  );
               write(
                  self$paramProposals[index,], 
                  file = self$paramProposalsFile, 
                  ncolumns = self$numParams,
                  sep = ",",
                  append = TRUE
                  );
               self$statsLogger$writeRow(index);
            }
            
         },
      plotTraces = function(indices = NULL, ...)
         {
            if(is.null(indices)) {
               indices <- 1:self$totalRealizations;
            } else if (indices == "adaptive") {
               indices <- (self$totalStaticRealizations + 1):
                  self$totalRealizations;
            } 
            par(mfrow = c(self$numParams, 1), mar = c(4, 5, 1, 1));
            for(paramIndex in 1:self$numParams) {
               plot(
                  self$paramSamples[indices,paramIndex],
                  ylab = colnames(self$paramSamples)[paramIndex],
                  ...
                  );
            }
         },
      plotPosteriorDensities = function(indices = "adaptive", ...)
         {
            if (indices == "adaptive") {
               indices <- (self$totalStaticRealizations + 1):
                  self$totalRealizations;
            } else if(is.null(indices)) {
               indices <- 1:self$totalRealizations;
            }
            par(mfrow = c(self$numParams, 1), mar = c(4, 5, 1, 1));
            for(paramIndex in 1:self$numParams) {
               plot(
                  density(self$paramSamples[indices,paramIndex]),
                  main = "",
                  xlab = colnames(self$paramSamples)[paramIndex],
                  ...
                  );   
            }
         },
      plotHighestPosterior = function(...)
         {
            numVars <- length(self$objFunc$observation);
            self$objFunc$propose(self$paramSamples[self$maxProbIndex,]);
            par(
               mfrow = c(numVars, 1), 
               mar = c(4, 5, 2, 1)
               );
            for(varIndex in 1:numVars) {
               plot(
                  self$objFunc$observation[[varIndex]],
                  ylab = names(self$objFunc$observation)[varIndex]
               );
               lines(
                  self$objFunc$baseObjFunc$prediction[[varIndex]],
                  col = "red",
                  lty = "dashed"
               );
            }
         },
      plotSummary = function(
         device = "pdf", 
         file = NULL,
         width = 8.5,
         height = 10
         ) 
         {
            if (device == "pdf") {
               pdf(file = file, width = width, height = height)
            }
            
            # Plot the full traces of the parameter samples
            if (device == "windows") {
               windows(width = width, height = height);
            } else if (device == "quartz") {
               quartz(width = width, height = height);
            }
            self$plotTraces();
            
            if (device == "windows") {
               windows(width = width, height = height);
            } else if (device == "quartz") {
               quartz(width = width, height = height);
            }
            self$plotTraces(indices = "adaptive");
            
            # Plot the posterior probability densities for parameter estimates
            if (device == "windows") {
               windows(width = width, height = height);
            } else if (device == "quartz") {
               quartz(width = width, height = height);
            }
            self$plotPosteriorDensities();
            
            # Plot the fit with the highest likelihood on the data
            if (device == "windows") {
               windows(width = width, height = height);
            } else if (device == "quartz") {
               quartz(width = width, height = height);
            }
            self$plotHighestPosterior();
            
            if (device == "pdf") {
               dev.off();
            }
         }
      )
   );
