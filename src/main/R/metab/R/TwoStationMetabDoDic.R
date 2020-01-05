# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class TwoStationMetabDoDic ####

#' @export
#' 
#' @title
#'    R6 class defining a two station metabolism model for DO and DIC
#'
#' @description
#'    A model for predicting changes in dissolved oxygen concentrations
#'    and dissolved inorganic carbon concentrations
#'    over a reach of a stream using the two-station method.
#' 
TwoStationMetabDoDic <- R6Class(
   classname = "TwoStationMetabDoDic",
   inherit =TwoStationMetabDo,
   public = list(
      
      #' @field upstreamDIC
      #'   Upstream DIC concentration
      upstreamDIC = NULL, 
      
      #' @field upstreampCO2
      #'   Upstream partial pressure of carbon dioxide
      upstreampCO2 = NULL,
      
      #' @field upstreampH
      #'   Upstream pH
      upstreampH = NULL,
      
      #' @field upstreamkH
      #'   Upstream Henry's constant
      upstreamkH = NULL,
      
      #' @field upstreamCO2Sat
      #'   Upstream saturation concentration for CO2
      upstreamCO2Sat = NULL,
      
      #' @field upstreamDICSat
      #'   Upstream DIC at saturation concentration for CO2
      upstreamDICSat = NULL,
      
      #' @field downstreamkH
      #'   Downstream Henry's constant
      downstreamkH = NULL,
      
      #' @field downstreamCO2Sat
      #'   Downstream saturation concentration for CO2
      downstreamCO2Sat = NULL,
      
      #' @field downstreamDICSat
      #'   Downstream DIC at saturation concentration for CO2
      downstreamDICSat = NULL,
      
      #' @field pCO2air
      #'   Partial pressure of CO2 in the air
      pCO2air = NULL, 
      
      #' @field alkalinity
      #'   Acid neutralizing capacity of the water
      alkalinity = NULL,
      
      #' @field RQ
      #'   Respiratory quotient for DIC change relative to DO change
      #'   caused by ecosystem respiration
      RQ = NULL, 
      
      #' @field PQ
      #'   Photosynthetic quotient for DIC change relative to DO change
      #'   caused by gross primary production
      PQ = NULL,
      
      #' @field kQ
      #'   Gas exchange quotient for DIC change relative to DO change
      #'   caused by gas exchange with the air
      kQ = NULL,
      
      #' @field maxDIC
      #'   Maximum DIC to allow in optimization
      maxDIC = NULL,
      
      #' @field carbonateEq
      #'   The carbonate equilibrium object to use for calculations
      carbonateEq = NULL,
      
      # Method TwoStationMetabDoDic$new ####
      #
      #' @description 
      #'   Construct a new instance of the class.
      #' 
      #' @param ... 
      #'    Arguments passed to constructor \code{TwoStationMetabDoDic$new(...)} will be 
      #'    passed generically to the constructor for the superclass \code{TwoStationMetabDo}. 
      #'    See documentation for the constructor of class \code{\link{TwoStationMetabDo}} for a description
      #'    of these arguments.
      #' @param upstreamDIC 
      #'    upstream DIC concentration in micromoles per liter
      #'    (numerical vector the same length as the upstream time vector)
      #'    Either upstreamDIC or upstreampCO2 must be specified, but not both.
      #' @param upstreampCO2 
      #'    upstream pCO2 concentration in microatmospheres
      #'    (numerical vector the same length as the upstream time vector)
      #'    Either upstreamDIC or upstreampCO2 must be specified, but not both.
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
      #' @param kQ
      #'    Gas exchange quotient
      #'    (single value numerical vector, default value is 0.915)
      #' @param maxDIC
      #'    Maximum DIC to allow in optimization.
      #'    Defaults to 1e6.
      #'    
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
                  concDIC = self$upstreamDIC[index] * 1e-6,
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
      },
      
      # Method TwoStationMetabDoDic$run ####
      #
      #' @description
      #'   Runs a two-station metabolism model
      #'
      #' @return 
      #'    Data frame with incremental and final results of the simulation,
      #'    with columns:
      #'    \itemize{
      #'      \item All columns generated by the run method of \code{\link{TwoStationMetabDo}}
      #'      \item co2Production: concentration change in co2 due to respiration
      #'      \item co2Consumption: concentration change in co2 due to GPP
      #'      \item co2Equilibration: two-station co2 equilibration term from gas exchange calculation
      #'      \item pH: the downstream pH (minus the log base 10 of hydrogen ion concentration)
      #'      \item pCO2: the downstream partial pressure of carbon dioxide
      #'      \item dic: the downstream total dissolved inorganic carbon concentration
      #'    }
      #'    
      run = function() 
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
      },
      
      # Method TwoStationMetabDoDic$plot ####
      #
      #' @description 
      #'   Plot the results of the model simulation.
      #' 
      #' @param obs.DO
      #'   vector of observed DO concentrations.
      #'   Defaults to an empty vector which disables plotting of observations.
      #' @param ylim.DO
      #'   The limits of the scale on the y axis for DO concentrations.
      #'   Defaults to an empty vector which calculates the min and max from
      #'   the simulated and observed values.
      #' @param mar.DO
      #'   The margins on the DO plot.
      #'   Defaults to c(2.5, 4.5, 2, 4).
      #' @param obs.pCO2
      #'   vector of observed CO2 partial pressures.
      #'   Defaults to an empty vector which disables plotting of observations.
      #' @param ylim.pCO2
      #'   The limits of the scale on the y axis for CO2 partial pressure.
      #'   Defaults to an empty vector which calculates the min and max from
      #'   the simulated and observed values.
      #' @param mar.pCO2
      #'   The margins on the CO2 partial pressure plot.
      #'   Defaults to the value of the mar.DO argument.
      #' @param mfrow
      #'   The structure for multi-panel plots.
      #'   Defaults to c(1, 2), which is one row and two columns.
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      plot = function
      (
         obs.DO = numeric(),
         ylim.DO = numeric(),
         mar.DO = c(2.5, 4.5, 2, 4),
         obs.pCO2 = numeric(),
         ylim.pCO2 = numeric(),
         mar.pCO2 = mar.DO,
         mfrow = c(1, 2)
      )
      {
         # Set the mfrow if now switched off in argument
         # with a NULL setting
         if (!is.null(mfrow))
         {
            par(mfrow = mfrow);
         }
         
         # Plot the DO panel
         
         self$plotDO(
            obs = obs.DO,
            ylim = ylim.DO,
            mar = mar.DO
         );
         par(new = TRUE);
         plot(
            x = self$output$time,
            y = self$par,
            col = "red",
            axes = FALSE,
            xlab = "",
            ylab = "",
            ylim = c(
               max(self$par),
               min(self$par)
            )
         );
         axis(
            side = 4, 
            col = "red",
            col.ticks = "red",
            col.axis = "red"
         );
         mtext(
            text = "PAR",
            side = 4,
            line = 2.5,
            col = "red"
         );
         
         # Add parameter values to margin
         
         mtext(
            text = bquote(paste(
               .(sprintf("GPP = %5.3e", self$dailyGPP))
            )),
            side = 3,
            line = 0.5,
            adj = 0
         );
         mtext(
            text = bquote(paste(
               .(sprintf("ER = %5.3e", self$dailyER))
            )),
            side = 3,
            line = 0.5,
            adj = 1
         );
         
         # Plot the pCO2 panel
         
         self$plotpCO2(
            obs = obs.pCO2,
            ylim = ylim.pCO2,
            mar = mar.pCO2
         );
         par(new = TRUE);
         plot(
            x = self$output$time,
            y = self$downstreamTemp,
            col = "red",
            axes = FALSE,
            xlab = "",
            ylab = ""
         );
         axis(
            side = 4, 
            col = "red",
            col.ticks = "red",
            col.axis = "red"
         );
         mtext(
            text = bquote(Temperature~.("(")*degree*C*.(")")),
            side = 4,
            line = 2.5,
            col = "red"
         )
         
         # Add parameter values to margin
         
         mtext(
            text = bquote(paste(
               k[600],
               .(sprintf(" = %5.3e", self$k600))
            )),
            side = 3,
            line = 0.5,
            adj = 0
         );
      },
      
      # Method TwoStationMetabDoDic$plotpCO2 ####
      #
      #' @description 
      #'   Plot the partial pressure of CO2 simulation from the model
      #' 
      #' @param obs
      #'   vector of observed CO2 partial pressures.
      #'   Defaults to an empty vector which disables plotting of observations.
      #' @param ylim
      #'   The limits of the scale on the y axis for CO2 partial pressures.
      #'   Defaults to an empty vector which calculates the min and max from
      #'   the simulated and observed values.
      #' @param mar
      #'   The margins of the CO2 partial pressure plot.
      #'   Defaults to c(2.5, 4.5, 1, 4).
      #' @param ...
      #'   Other abstract arguments are passed to the plot.default function.
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      plotpCO2 = function
      (
         obs = numeric(),
         ylim = numeric(),
         mar = c(2.5, 4.5, 1, 4),
         ...
      ) 
      {
         par(mar = mar);
         if (length(ylim) != 2) {
            ymin <- min(
               self$output$pCO2,
               self$upstreampCO2
            );
            ymax <- max(
               self$output$pCO2,
               self$upstreampCO2
            );
            if (length(obs) > 0) {
               ymin <- min(ymin, obs);
               ymax <- max(ymax, obs);
            }
            ylim <- c(ymin, ymax);
         } else {
            ymin <- ylim[1];
            ymax <- ylim[2];
         }
         plot(
            x = self$output$time,
            y = self$output$pCO2,
            type = "l",
            xaxt = "n",
            xlab = "",
            ylim = ylim,
            ylab = bquote(pCO[2]*.(" (")*mu*atm*.(")")),
            ...
         );
         points(
            x = as.POSIXct(
               self$upstreamTime * 86400, 
               origin = as.POSIXct("1970-01-01", tz = "UTC")
            ),
            y = self$upstreampCO2,
            pch = 20
         )
         if (length(obs) > 0) {
            points(
               x = self$output$time,
               y = obs
            );
         }
         axis.POSIXct(
            x = self$output$time,
            side = 1,
            format = "%H:%M"
         );
         abline(
            h = self$pCO2air,
            lty = "dashed"
         )
      }
   )
)
