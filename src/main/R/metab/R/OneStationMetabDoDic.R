# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class OneStationMetabDoDic (R6) ####

#' @export
#' 
#' @title
#'    Class OneStationMetabDoDic (R6)
#' 
#' @description
#'    A model for predicting dissolved oxygen and carbon dioxide concentrations in
#'    a stream using the one station method.
#'    Parameters desribed here are for the R6 constructor method ($new).
#' 
#' @param ... 
#'    Arguments passed to constructor \code{OneStationMetabDoDic$new(...)} will be 
#'    passed generically to the constructor for the superclass \code{OneStationMetabDo}. 
#'    See documentation for the class \code{\link{OneStationMetabDo}} for a description
#'    of these arguments.
#' @param initialDIC 
#'    initial DIC concentration in micromoles per liter
#'    (numerical vector, only first value will be used)
#'    Either initialDIC or initialpCO2 must be specified, but not both.
#' @param initialpCO2 
#'    initial pCO2 concentration in microatmospheres
#'    (numerical vector, only first value will be used)
#'    Either initialDIC or initialpCO2 must be specified, but not both.
#' @param pCO2air 
#'    partial pressure of CO2 in the air in microatmospheres
#'    (numerical vector)
#' @param alkalinity 
#'    alkalinity of stream water
#'    (numerical vector)
#' @param RQ 
#'    Respiratory quotient
#'    (single value numerical vector, default value is 0.85)
#' @param PQ 
#'    Photosynthetic quotient
#'    (single value numerical vector, default value is 1.22)
#' @return 
#'    The object of class \code{OneStationMetabDoDic} 
#'    instantiated by the constructor
#'    
#' @section Extends \code{\link{OneStationMetabDo}}:
#'   Overrides method \code{$run}
#'   \itemize{
#'     \item see \code{\link{Model_run}}
#'     \item see \code{\link{OneStationMetabDo_run}}
#'     \item see \code{\link{OneStationMetabDoDic_run}}
#'   }
#'   
OneStationMetabDoDic <- R6Class(
   classname = "OneStationMetabDoDic",
   inherit = OneStationMetabDo,
   public = list(
      initialDIC = NULL, 
      pCO2air = NULL, 
      alkalinity = NULL,
      RQ = NULL, 
      PQ = NULL,
      carbonateEq = NULL,
      initialize = function
         (
            ..., 
            initialDIC = NULL,
            initialpCO2 = NULL,
            pCO2air, 
            alkalinity,
            RQ = 0.85, 
            PQ = 1.22
         ) 
         {
            super$initialize(...);
            self$alkalinity <- alkalinity;
            self$carbonateEq <- CarbonateEq$new(tempC = self$temp[1]);
            if(!is.null(initialDIC)) {
               if(!is.null(initialpCO2)) {
                  stop(paste(
                     "Ambiguous arguments. Cannot specify both initialDIC",
                     "and initialpCO2."
                  ));
               }
               self$initialDIC <- initialDIC;
            } else {
               if(is.null(initialpCO2)) {
                  stop(paste(
                     "Cannot create a DO/DIC metabolism model without either",
                     "an initial DIC or and initial pCO2 (arguments initialDIC",
                     "or initialpCO2)"
                  ));
               } else {
                  self$initialDIC <- 1e6 * self$carbonateEq$optDICFromfCO2TotalAlk(
                        initialpCO2[1],
                        self$alkalinity[1] * 1e-6
                     )$concDIC;
               }
            }
            self$pCO2air <- pCO2air;
            self$RQ <- RQ;
            self$PQ <- PQ;
         }
   )
);

# Method OneStationMetabDoDic$run ####

#' @name OneStationMetabDoDic_run
#' 
#' @title 
#'    Runs the model (R6 method)
#' 
#' @description 
#'    Runs the model predicting the change in DO and DIC concentrations over
#'    time at a location along a stream.
#' 
#' @return 
#'    Data frame with incremental and final results of the simulation,
#'    with columns \cr
#'    \itemize{
#'      \item time: POSIXct simulation time (tzone attribute will not be set)
#'      \item do: dissolved oxygen concentration in micromolality
#'      \item doSat: Saturated dissolved oxygen concentration in micromolality
#'      \item doProduction: increase in DO concentration in micromolality during the time step
#'      \item doConsumption: decrease in DO concentration in micromolality during the time step
#'      \item k: DO gas exchange rate in per day
#'      \item temp: water temperature in degrees Celsius
#'      \item dt: length of time step in days
#'      \item co2Production: increase in co2 concentration during the time step in
#'            micromoles per liter
#'      \item co2Consumption: decrease in co2 concentration during the time step in
#'            micromoles per liter
#'      \item kH: Henry's constant during the time step in micromoles per liter per 
#'            microatmospheres
#'      \item co2Sat: saturation concentration for co2 in micromoles per liter
#'      \item fGas: change in co2 concentration due to air-water exchange in
#'            micromoles per liter
#'      \item pH: minus the log of the hydrogen ion concentration
#'      \item pCO2: partial pressure of co2 in water in microatmosphere
#'      \item dic: DIC concentration in water in moles per liter 
#'    }
#'    
#' @section Method of class:
#'   \code{\link{OneStationMetabDoDic}}
#'
#' @section Overrides method in super class:
#'   \code{\link{Model_run}}
#'   \code{\link{OneStationMetabDo_run}}
#'   
OneStationMetabDoDic$set(
   which = "public",
   name = "run",
   value = function()
      {
         # Run the superclass one station metabolism model for DO
         super$run();
         
         # Set up the data frame that will be returned
         dicPredLength <- length(self$time);
         self$output <- data.frame(
            self$output, 
            co2Production = 
               -self$output$doConsumption * self$RQ,
            co2Consumption = 
               -self$output$doProduction * self$PQ,
            kH = numeric(length = dicPredLength),
            co2Sat = numeric(length = dicPredLength),
            fGas = numeric(length = dicPredLength),
            pH = numeric(length = dicPredLength),
            pCO2 = numeric(length = dicPredLength),
            dic = numeric(length = dicPredLength)
         );
         
         # Create the carbonate equilibrium object with the initial 
         # temperature
         self$carbonateEq$resetFromTemp(tempC = self$temp[1]);
         self$output$kH[1] <- self$carbonateEq$kHenryCO2;
         
         # Set the initial DIC concentration, pCO2, and fgas
         self$output$dic[1] <- self$initialDIC[1];
         dicOptim <- self$carbonateEq$optfCO2FromDICTotalAlk(
            concDIC = self$initialDIC[1] * 1e-6, 
            totalAlk = self$alkalinity[1] * 1e-6
         );
         self$output$pCO2[1] <- dicOptim$fCO2;
         self$output$pH[1] <- dicOptim$pH;
         self$output$fGas[1] <- self$output$dt[1] * self$output$k[1] * 0.915 * 
            self$output$kH[1] * (self$pCO2air - self$output$pCO2[1]);
         
         # Iterate through time to predict the change in DIC and 
         # the consequent changes in pH, pCO2, and gas exchange
         for (i in 2:dicPredLength) {
            # Calculate current DIC based on previous state
            self$output$dic[i] <- self$output$dic[i - 1] +
               self$output$co2Production[i - 1] + 
               self$output$co2Consumption[i - 1] + 
               self$output$fGas[i - 1];
            
            # Calculate pCO2 and fGas based on new DIC
            self$carbonateEq$resetFromTemp(tempC = self$temp[i]);
            self$output$kH[i] <- self$carbonateEq$kHenryCO2; 
            optim <- self$carbonateEq$optfCO2FromDICTotalAlk(
               concDIC = self$output$dic[i] * 1e-6,
               totalAlk = self$alkalinity * 1e-6
               );
            self$output$pH[i] <- optim$pH;
            self$output$pCO2[i] <- optim$fCO2;
            self$output$fGas[i] <- self$output$dt[i] * self$output$k[i] * 0.915 * 
               self$output$kH[i] * (self$pCO2air - self$output$pCO2[i]);
         }
         
         self$output$co2Sat <- self$pCO2air * self$output$kH;
         
         return(self$output);
      }
)
