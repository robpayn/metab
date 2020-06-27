# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class OneStationMetabDoDic (R6) ####

#' @export
#' 
#' @title
#'    R6 class defining a one station metabolism model for DO and DIC
#' 
#' @description
#'    A model for predicting dissolved oxygen and carbon dioxide concentrations in
#'    a stream using the one station method.
#' 
OneStationMetabDoDic <- R6Class(
   classname = "OneStationMetabDoDic",
   inherit = OneStationMetabDo,
   public = list(
      
      #' @field ratioDicCfix
      #'    Ratio of carbon atoms in DIC consumed relative to carbon atoms fixed.
      #'    Defaults to 1.
      ratioDicCfix = NULL,
      
      #' @field dailyGPPDIC
      #'   Model parameter for daily gross primary production
      #'   based on C atoms in DIC consumed.
      #'   Units of micromoles per liter per day.
      dailyGPPDIC = NULL,
      
      #' @field ratioDicCresp
      #'    Ratio of carbon atoms in DIC produced relative to carbon atoms respired.
      #'    Defaults to 1.
      ratioDicCresp = NULL,
      
      #' @field dailyERDIC
      #'   Model parameter for daily aerobic ecosystem respiration
      #'   based on C atoms in DIC consumed.
      #'   Units of micromoles per liter per day.
      dailyERDIC = NULL,
      
      #' @field initialDIC
      #'   initial total dissolved inorganic carbon concentration
      initialDIC = NULL, 
      
      #' @field pCO2air
      #'   pCO2 in the atmosphere
      pCO2air = NULL, 
      
      #' @field alkalinity
      #'   Total acid neutralizing capacity of the water 
      alkalinity = NULL,
      
      #' @field carbonateEq
      #'   The carbonate equilibrium object to use for calculations
      carbonateEq = NULL,
      
      #' @field kSchmidtFuncCO2
      #'   Function to use to adjust carbon dioxide gas exchange for temperature
      kSchmidtFuncCO2 = NULL,
      
      # Method OneStationMetabDoDic$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #'   
      #' @param ... 
      #'    Arguments passed to constructor \code{OneStationMetabDoDic$new(...)} will be 
      #'    passed generically to the constructor for the superclass \code{OneStationMetabDo}. 
      #'    See documentation for the class \code{\link{OneStationMetabDo}} for a description
      #'    of these arguments.
      #' @param ratioDicCfix
      #'    Ratio of carbon atoms in DIC consumed relative to carbon atoms fixed.
      #'    Defaults to -1.
      #' @param ratioDicCresp
      #'    Ratio of carbon atoms in DIC produced relative to carbon atoms respired.
      #'    Defaults to 1.
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
      #' @param kSchmidtFuncCO2
      #'    Option to change the function used for calculating the carbon dioxide gas exchange
      #'    rate from the gas exchange parameter normalized to a Schmidt number.
      #'    Defaults to \code{\link{kSchmidtCO2}}
      #'    
      initialize = function
      (
         ...,
         ratioDicCfix = -1.0,
         ratioDicCresp = 1.0,
         initialDIC = NULL,
         initialpCO2 = NULL,
         pCO2air, 
         alkalinity,
         kSchmidtFuncCO2 = kSchmidtCO2
      ) 
      {
         # Execute the super class constructor
         super$initialize(...);
         
         # Populate attributes
         self$ratioDicCfix <- ratioDicCfix;
         self$ratioDicCresp <- ratioDicCresp;
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
         self$kSchmidtFuncCO2 <- kSchmidtFuncCO2;
      },
      
      # Method OneStationMetabDoDic$run ####
      #
      #' @description 
      #'    Runs the model predicting the change in DO and DIC concentrations over
      #'    time at a location along a stream.
      #' 
      #' @return 
      #'    Data frame with incremental and final results of the simulation,
      #'    with columns \cr
      #'    \itemize{
      #'      \item All columns generated by \code{\link{OneStationMetabDo}} run method
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
      run = function()
      {
         # Run the superclass one station metabolism model for DO
         super$run();
         
         # Set the effects of metabolism on DIC
         self$dailyGPPDIC <- self$dailyGPP * self$ratioDicCfix;
         self$dailyERDIC <- self$dailyER * self$ratioDicCresp;
         
         # Set up the data frame that will be returned
         dicPredLength <- length(self$time);
         self$output <- data.frame(
            self$output, 
            co2Production = 
               (self$output$doConsumption / self$ratioDoCresp) * self$ratioDicCresp,
            co2Consumption = 
               (self$output$doProduction / self$ratioDoCfix) * self$ratioDicCfix,
            kCO2 = self$kSchmidtFuncCO2(temp = self$temp, k600 = self$k600),
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
         self$output$fGas[1] <- self$output$dt[1] * self$output$kCO2[1] * 
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
            self$output$fGas[i] <- self$output$dt[i] * self$output$kCO2[i] * 
               self$output$kH[i] * (self$pCO2air - self$output$pCO2[i]);
         }
         
         self$output$co2Sat <- self$pCO2air * self$output$kH;
         
         return(self$output);
      }
      
   )
)
