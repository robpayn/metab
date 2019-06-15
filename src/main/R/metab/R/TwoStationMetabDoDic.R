# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#' @importFrom inferno Model
NULL

# Class TwoStationMetabDoDic ####

#' @export
#' 
TwoStationMetabDoDic <- R6Class(
   classname = "TwoStationMetabDoDic",
   inherit =TwoStationMetabDO,
   public = list(
      upstreamDIC = NULL, 
      upstreampCO2 = NULL,
      upstreampH = NULL,
      upstreamkH = NULL,
      upstreamCO2Sat = NULL,
      upstreamDICSat = NULL,
      downstreamkH = NULL,
      downstreamCO2Sat = NULL,
      downstreamDICSat = NULL,
      pCO2air = NULL, 
      alkalinity = NULL,
      RQ = NULL, 
      PQ = NULL,
      kQ = NULL,
      maxDIC = NULL,
      carbonateEq = NULL,
      initialize = function
         (
            ..., 
            upstreamDIC = NULL,
            upstreampCO2 = NULL,
            pCO2air, 
            alkalinity,
            RQ = 0.85, 
            PQ = 1.22,
            kQ = 0.915,
            maxDIC = 1e6
         ) 
         {
            super$initialize(...);
            self$alkalinity <- alkalinity;
            self$carbonateEq <- CarbonateEq$new(tempC = self$upstreamTemp[1]);
            self$pCO2air <- pCO2air;
            if(!is.null(upstreamDIC)) {
               self$upstreamDIC <- upstreamDIC;
               for (index in 1:length(self$upstreamTemp)) {
                  self$carbonateEq$resetFromTemp(tempC = self$upstreamTemp[index]);
                  equil <- self$carbonateEq$optfCO2FromDICTotalAlk(
                     concDIC = self$upstreamDIC[index],
                     totalAlk = self$alkalinity * 1e-6
                  );
                  self$upstreampCO2[index] <- equil$fCO2;
                  self$upstreampH[index] <- equil$pH;
                  
                  self$carbonateEq$resetFromTemp(tempC = self$upstreamTemp[index]);
                  self$upstreamkH[index] <- self$carbonateEq$kHenryCO2;
                  self$upstreamCO2Sat[index] <- self$upstreamkH[index] * self$pCO2air;
                  self$upstreamDICSat[index] <- self$carbonateEq$optDICFromfCO2TotalAlk(
                     fCO2 = self$upstreamCO2Sat[index],
                     totalAlk = self$alkalinity * 1e-6
                  )$concDIC * 1e6;
               }
            } else {
               if(is.null(upstreampCO2)) {
                  stop(paste(
                     "Cannot create a DO/DIC metabolism model without either",
                     "an upstream DIC or and upstream pCO2 (arguments upstreamDIC",
                     "or upstreampCO2)"
                  ));
               } else {
                  self$upstreampCO2 <- upstreampCO2;
                  for (index in 1:length(self$upstreamTemp)) {
                     self$carbonateEq$resetFromTemp(tempC = self$upstreamTemp[index]);
                     equil <- self$carbonateEq$optDICFromfCO2TotalAlk(
                        fCO2 = self$upstreampCO2[index],
                        totalAlk = self$alkalinity * 1e-6
                     );
                     self$upstreamDIC[index] <- 1e6 * equil$concDIC;
                     self$upstreampH[index] <- equil$pH;
                     
                     self$carbonateEq$resetFromTemp(tempC = self$upstreamTemp[index]);
                     self$upstreamkH[index] <- self$carbonateEq$kHenryCO2;
                     self$upstreamCO2Sat[index] <- self$upstreamkH[index] * self$pCO2air;
                     self$upstreamDICSat[index] <- self$carbonateEq$optDICFromfCO2TotalAlk(
                        fCO2 = self$upstreamCO2Sat[index],
                        totalAlk = self$alkalinity * 1e-6
                     )$concDIC * 1e6;
                  }
               }
            }
            for (index in 1:length(self$downstreamTemp)) {
               self$carbonateEq$resetFromTemp(tempC = self$downstreamTemp[index]);
               self$downstreamkH[index] <- self$carbonateEq$kHenryCO2;
               self$downstreamCO2Sat[index] <- self$downstreamkH[index] * self$pCO2air;
               self$downstreamDICSat[index] <- self$carbonateEq$optDICFromfCO2TotalAlk(
                  fCO2 = self$downstreamCO2Sat[index],
                  totalAlk = self$alkalinity * 1e-6
               )$concDIC * 1e6;
            }
            self$RQ <- RQ;
            self$PQ <- PQ;
            self$kQ <- kQ;
            self$maxDIC <- maxDIC;
         }
      )
);

TwoStationMetabDoDic$set(
   which = "public",
   name = "run",
   value = function() 
      {
         # Run the superclass one station metabolism model for DO
         super$run();
         
         # Set up the data frame that will be returned
         dicPredLength <- length(self$downstreamTime);
         self$output <- data.frame(
            self$output, 
            co2Production = 
               -self$output$doConsumption * self$RQ,
            co2Consumption = 
               -self$output$doProduction * self$PQ,
            co2Equilibration = numeric(length = dicPredLength),
            pH = numeric(length = dicPredLength),
            pCO2 = numeric(length = dicPredLength),
            dic = numeric(length = dicPredLength)
         );
      
         # Calculate the change in DIC for each parcel of water
         # being transported over the reach
         for (i in 1:dicPredLength) {
            
            # Calculate pCO2 and fGas based on new DIC
            upstreamCO2Deficit <- self$upstreamCO2Sat[i] -
               self$upstreampCO2[i] * self$upstreamkH[i];
            
            # Calculate the RHS target value of the equation for 
            # the DIC change over reach transport. The RHS is the
            # deterministic terms of the equation that are independent
            # of the downstream pCO2 measurements.
            kCO2 <- self$output$k[i] * self$kQ;
            self$output$co2Equilibration[i] <- self$output$residenceTime[i] * 
               kCO2 * 0.5 * (upstreamCO2Deficit + self$downstreamCO2Sat[i]);
            target <- 
               self$upstreamDIC[i] +
               self$output$co2Production[i] + 
               self$output$co2Consumption[i] + 
               self$output$co2Equilibration[i];
            
            # Find the combination of downstream DIC and CO2 concentrations
            # that allows the model to match the RHS calculated above. This
            # calculation provides the second equation to determine the second
            # unknown, based on known carbonate equilibrium relations.
            optim <- optimize(
               f = function(dic, alkalinity, kH, kCO2, dt, target) 
                  {
                     CO2 <- self$carbonateEq$optfCO2FromDICTotalAlk(
                        concDIC = dic * 1e-6,
                        totalAlk = alkalinity
                     )$fCO2 * kH;
                     guess <- dic + kCO2 * dt * CO2 * 0.5;
                     return( (target - guess)^2 );
                  },
               alkalinity = self$alkalinity * 1e-6,
               kH = self$downstreamkH[i],
               kCO2 = kCO2,
               dt = self$output$residenceTime[i],
               target = target,
               lower = 0,
               upper = self$maxDIC
            );
            self$output$dic[i] <- optim$minimum;
            equil <- self$carbonateEq$optfCO2FromDICTotalAlk(
               concDIC = self$output$dic[i] * 1e-6,
               totalAlk = self$alkalinity * 1e-6
            );
            self$output$pH[i] <- equil$pH;
            self$output$pCO2[i] <- equil$fCO2;

         }

         return(self$output);
      }
);
