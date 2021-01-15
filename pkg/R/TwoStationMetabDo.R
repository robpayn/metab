# Package dependencies ####
#
# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#
#' @importFrom R6 R6Class
NULL

# Class TwoStationMetabDo (R6) ####

#' @export
#' 
#' @title
#'    R6 class defining a two station metabolism model for DO
#'
#' @description
#'    A model for predicting changes in dissolved oxygen concentrations
#'    over a reach of a stream using the two-station method.
#' 
TwoStationMetabDo <- R6Class(
   classname = "TwoStationMetabDo",
   public = list(
      
      #' @field output
      #'   Data frame with output from the model
      output = NULL,
      
      #' @field dailyGPP
      #'   Model parameter for daily gross primary production
      #'   based on C atoms fixed.
      #'   Units of micromoles per liter per day.
      dailyGPP = NULL, 
      
      #' @field ratioDoCfix
      #'   Ratio of DO molecules produced relative to carbon atoms fixed.
      ratioDoCfix = NULL,
      
      #' @field dailyER
      #'   Model parameter for daily aerobic ecosystem respiration
      #'   based on carbon atoms respired.
      #'   Units of micromoles per liter per day.
      dailyER = NULL, 
      
      #' @field ratioDoCresp
      #'    Ratio of DO molecules consumed relative to carbon atoms respired.
      #'    Defaults to 1.
      ratioDoCresp = NULL,
      
      #' @field k600
      #'   Model parameter for the gas exchange rate
      k600 = NULL, 
      
      #' @field alphag
      #'   Model parameter for the turnover rate due to groundwater
      alphag = NULL, 
      
      #' @field airPressure
      #'   Air pressure
      airPressure = NULL,
      
      #' @field par
      #'   Vector of photosynthetically active radiation at the simulated times
      par = NULL, 
      
      #' @field parTotal
      #'   Total PAR over the simulation period
      parTotal = NULL, 
      
      #' @field upstreamTime
      #'   Numeric time in days a parcel of water is passing upstream station
      upstreamTime = NULL,
      
      #' @field upstreamTimePOSIX
      #'   POSIX time a parcel of water is passing upstream station
      upstreamTimePOSIX = NULL,
      
      #' @field upstreamTemp
      #'   Temperature of a parcel of water when passing upstream station
      upstreamTemp = NULL,
      
      #' @field upstreamPAR
      #'   Photosynthetically active radiation on a parcel of water when 
      #'   passing upstream station
      upstreamPAR = NULL,
      
      #' @field upstreamDO
      #'   DO concentration of a parcel of water when passing upstream station
      upstreamDO = NULL, 
      
      #' @field upstreamDOSat
      #'   The saturation concentration of DO of a parcel of water when
      #'   passing upstream station
      upstreamDOSat = NULL,
      
      #' @field downstreamTime
      #'   Time a parcel of water is passing downstream station
      downstreamTime = NULL, 
      
      #' @field downstreamTimePOSIX
      #'   POSIX time a parcel of water is passing upstream station
      downstreamTimePOSIX = NULL,
      
      #' @field downstreamTemp
      #'   Temperature of a parcel of water when passing downstream station
      downstreamTemp = NULL,
      
      #' @field downstreamPAR
      #'   Photosynthetically active radiation on a parcel of water when 
      #'   passing downstream station
      downstreamPAR = NULL,
      
      #' @field downstreamDOSat
      #'   The saturation concentration of DO of a parcel of water when
      #'   passing downstream station
      downstreamDOSat = NULL,
      
      #' @field gwC
      #'   Concentration of oxygen in inflowing groundwater
      gwC = NULL, 
      
      #' @field doSatCalculator
      #'   Function to use for do saturation calculations
      doSatCalculator = NULL,
      
      #' @field kSchmidtFunc
      #'   Function to use to adjust gas exchange for temperature
      kSchmidtFunc = NULL,
      
      # Method TwoStationMetabDo$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #'   
      #' @param dailyGPP 
      #'    Daily average influence of gross primary production on oxygen concentration.
      #'    Units depend on the unit conversion factor for DO saturation.
      #'    Default value of the unit conversion factor results in
      #'    units of micomolarity per day.
      #' @param ratioDoCfix
      #'    Ratio of DO molecules produced relative to carbon atoms fixed.
      #'    Defaults to 1.
      #' @param dailyER 
      #'    Daily average influence of ecosystem respiration on oxygen.
      #'    Units depend on the unit conversion factor for DO saturation.
      #'    Default value of the unit conversion factor results in
      #'    units of micomolarity per day.
      #' @param ratioDoCresp
      #'    Ratio of DO molecules consumed relative to carbon atoms respired.
      #'    Defaults to -1.
      #' @param k600 
      #'    Influence of atmospheric gas exchange on oxygen concentration as a first-order rate
      #'    depending on saturation deficit. Units are per day.
      #' @param alphag
      #'    Model parameter for the turnover rate due to groundwater in per day
      #' @param airPressure 
      #'    Barometric pressure.
      #'    Units must be consistent with the units of the optional standard air pressure
      #'    argument.
      #'    Default value for standard air pressure results in units of mm Hg.
      #' @param upstreamTime
      #'    Time vector for data at upstream location.
      #'    (POSIXct vector or character vector in a standard text format
      #'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
      #' @param upstreamTemp
      #'    Vector of temperatures in degrees C at upstream location.
      #' @param upstreamPAR
      #'    Optional vector of values for PAR at upstream station
      #' @param upstreamDO
      #'    Vector of DO concentrations at upstream location.
      #' @param downstreamTime
      #'    Time vector for data at downstream location.
      #'    In relation to upstream times, the downstream times should reflect
      #'    the travel times of water over the reach.
      #'    (POSIXct vector or character vector in a standard text format
      #'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss")
      #' @param downstreamTemp
      #'    Vector of temperatures in degrees C at the downstream location.
      #' @param downstreamPAR
      #'    Optional vector of values for PAR at downstream station
      #' @param par 
      #'    Photosynthetically active radiation. 
      #'    Units are arbitrary, but must be consistent with total PAR total if the
      #'    optional argument for total PAR is specified.
      #'    Defaults to the average of upstream and downstream PAR arguments
      #' @param parTotal 
      #'    Total PAR over the evaluation period. Units must be the same as the PAR
      #'    data argument.
      #'    Default is the integration of the PAR data provided over the evaluation period.
      #' @param stdAirPressure
      #'    The standard air pressure in the desired units. 
      #'    Defaults to 760 mm Hg.
      #' @param gwC
      #'    Concentration of oxygen in inflowing groundwater in micromolarity
      #' @param doSatCalculator
      #'    The DO saturation calculator used to estimate DO saturation concentrations
      #'    at a given temperature and air pressure. 
      #'    Defaults to a new instance of \code{\link{DoSatCalculator}} based on
      #'    the standard air pressure and saturation concentration unit conversion
      #'    arguments.
      #' @param kSchmidtFunc
      #'    Option to change the function used for calculating the gas exchange
      #'    rate from the gas exchange parameter normalized to a Schmidt number.
      #'    Defaults to \code{\link{kSchmidtDO}}
      #'
      initialize = function
      (
         dailyGPP, 
         ratioDoCfix = 1.0,
         dailyER, 
         ratioDoCresp = -1.0,
         k600,
         alphag = NA,
         airPressure, 
         upstreamTime, 
         upstreamTemp,
         upstreamPAR = NA,
         upstreamDO,
         downstreamTime,
         downstreamTemp,
         downstreamPAR = NA,
         par = 0.5 * (upstreamPAR + downstreamPAR),
         parTotal = NA,
         stdAirPressure = 760,
         gwC = NA,
         doSatCalculator = DoSatCalculator$new(
            stdAirPressure = stdAirPressure
         ),
         kSchmidtFunc = kSchmidtDO
      ) 
      {
         # Populate attributes
         self$dailyGPP <- dailyGPP;
         self$ratioDoCfix <- ratioDoCfix;
         self$dailyER <- dailyER;
         self$ratioDoCresp <- ratioDoCresp;
         self$k600 <- k600; 
         self$alphag <- alphag;
         self$airPressure <- airPressure;
         self$upstreamPAR <- upstreamPAR;
         self$downstreamPAR <- downstreamPAR;
         self$par <- par;
         self$parTotal <- parTotal; 
         self$gwC <- gwC;
         
         # Time attribute should be POSIXct
         self$upstreamTimePOSIX <- as.POSIXct(upstreamTime);
         self$upstreamTime <- 
            as.numeric(self$upstreamTimePOSIX) / 86400;
         self$upstreamTemp <- upstreamTemp;
         self$upstreamDO <- upstreamDO;
         
         # Time attribute should be POSIXct
         self$downstreamTimePOSIX <- as.POSIXct(downstreamTime);
         self$downstreamTime <- 
            as.numeric(self$downstreamTimePOSIX) / 86400;
         self$downstreamTemp <- downstreamTemp;
         
         self$doSatCalculator <- doSatCalculator;
         self$kSchmidtFunc <- kSchmidtFunc;

         # Use the default or provided saturation calculator to
         # calculate the saturation DO concentration attribute.
         self$upstreamDOSat <- self$doSatCalculator$calculate(
            self$upstreamTemp, 
            self$airPressure 
         );
         self$downstreamDOSat <- self$doSatCalculator$calculate(
            self$downstreamTemp, 
            self$airPressure
         );
         
         # If total PAR is not provided as an argument, total PAR is calculated
         # over the vector using trapezoidal integration
         # Calculates the time step between data points
         if (is.na(self$parTotal)) {
            dtUpstream <- c(
               self$upstreamTime[2:length(self$upstreamTime)] - 
                  self$upstreamTime[1:length(self$upstreamTime) - 1], 
               0
            );
            parAverage <- c(
               0.5 * (
                  self$par[1:length(self$upstreamTime) - 1] + 
                     self$par[2:length(self$upstreamTime)]
                  ),
               0
            );
            self$parTotal <- sum(parAverage * dtUpstream);
         }
      },
      
      # Method TwoStationMetabDo$getDailyGppDo ####
      #
      #' @description
      #'    Calculates the current daily GPP relative to DO
      #'
      #' @return 
      #'    Current daily GPP setting as a rate of change in DO concentration
      #'    
      getDailyGppDo = function()
      {
         return(self$dailyGPP * self$ratioDoCfix);
      },
      
      # Method TwoStationMetabDo$getDailyErDo ####
      #
      #' @description
      #'    Calculates the current daily ER relative to DO
      #'
      #' @return 
      #'    Current daily ER setting as a rate of change in DO concentration
      #'    
      getDailyErDo = function()
      {
         return(self$dailyER * self$ratioDoCresp);
      },
      
      # Method TwoStationMetabDo$run ####
      #
      #' @description
      #'   Runs a two-station metabolism model
      #'
      #' @return 
      #'    Data frame with incremental and final results of the simulation,
      #'    with columns:
      #'    \itemize{
      #'      \item time: POSIXct simulation time
      #'      \item residenceTime: length of time between upstream and downstream times
      #'      \item do: dissolved oxygen concentration
      #'      \item doProduction: increase in DO concentration in travel over the reach
      #'      \item doConsumption: decrease in DO concentration in travel over the reach
      #'      \item doEquilibration: two-station term for gas exchange in travel over the reach
      #'      \item k: DO gas exchange applied in travel over the reach
      #'    }
      #'    
      run = function() 
      {
         # Calculate the saturation deficit upstream
         upstreamDODeficit <- self$upstreamDOSat - self$upstreamDO;
         
         # Calculate the temperature adjusted gas exchange rate from 
         # the k600 rate at a Schmidt number of 600
         k <- 0.5 * (
            self$kSchmidtFunc(self$upstreamTemp, self$k600) + 
            self$kSchmidtFunc(self$downstreamTemp, self$k600)
         );
         
         # Calculate the residence time of each parcel of water
         residenceTime <- self$downstreamTime - self$upstreamTime;
         
         # Calculate the C fixation and respiration for each
         # time step
         cFixation <- self$dailyGPP * 
            ((self$par * residenceTime) / self$parTotal);
         cRespiration <- self$dailyER * residenceTime;
         
         # Calculate the DO production and consumption for
         # each time step
         doProduction <- cFixation * self$ratioDoCfix; 
         doConsumption <- cRespiration * self$ratioDoCresp;
         doEquilibration <- residenceTime * k * 
            (0.5 * (upstreamDODeficit + self$downstreamDOSat));
         
         # Calculate the downstream DO concentration 
         numerator <-
            self$upstreamDO +
            doProduction +
            doConsumption +
            doEquilibration;
         denominator <-
            1 + (0.5 * (residenceTime * k));
         
         if (!is.na(self$alphag)) {
            gwMixing <- residenceTime * self$alphag * 
               (self$gwC - (0.5 * self$upstreamDO));
            numerator <- numerator + gwMixing;
            denominator <- denominator + 
               0.5 * (residenceTime * self$alphag);
         }

         self$output <- data.frame(
            time = self$downstreamTimePOSIX,
            residenceTime = residenceTime,
            do = numerator / denominator,
            cFixation = cFixation,
            cRespiration = cRespiration,
            doProduction = doProduction,
            doConsumption = doConsumption,
            doEquilibration = doEquilibration,
            k = k
         );
         
         if (!is.na(self$alphag)) {
            self$output$gwMixing <- gwMixing;
         }
         return(self$output);
      },
      
      # Method TwoStationMetabDo$plot ####
      #
      #' @description 
      #'   Plots the results of a two station model simulation
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
      #' @param ylim.temp
      #'   The limits of the scale on the y axis for temperature.
      #'   Defaults to an empty vector which calculates the min and max from
      #'   the temperature data.
      #' @param mar.temp
      #'   The margins on the temperature plot.
      #'   Defaults to c(2.5, 4.5, 2, 4).
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
         ylim.temp = numeric(),
         mar.temp = c(2.5, 4.5, 2, 4),
         mfrow = c(1, 2)
      )
      {
         # Use the default mfrow if not turned off
         # in argument with a NULL setting
         
         if (!is.null(mfrow)) {
            par(mfrow = mfrow)
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
         
         
         # Plot the temperature panel
         
         self$plotTemp(
            ylim = ylim.temp,
            mar = mar.temp
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
               k[600],
               .(sprintf(" = %5.3e", self$k600))
            )),
            side = 3,
            line = 0.5,
            adj = 0
         );
      },
      
      # Method TwoStationMetabDo$plotDO ####
      #
      #' @description 
      #'   Plot the simulated DO concentrations.
      #'   
      #' @param obs
      #'   vector of observed DO concentrations.
      #'   Defaults to an empty vector which disables plotting of observations.
      #' @param ylim
      #'   The limits of the scale on the y axis for DO concentrations.
      #'   Defaults to an empty vector which calculates the min and max from
      #'   the simulated and observed values.
      #' @param mar
      #'   The margins of the DO plot.
      #'   Defaults to c(2.5, 4.5, 1, 4).
      #' @param ...
      #'   Other abstract arguments are passed to the plot.default function.
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      plotDO = function
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
               self$output$do,
               self$upstreamDO
            );
            ymax <- max(
               self$output$do,
               self$upstreamDO
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
            y = self$output$do,
            type = "l",
            xaxt = "n",
            xlab = "",
            ylim = ylim,
            ylab = bquote(.("[DO] (")*mu*mol~L^-1*.(")")),
            ...
         );
         points(
            x = self$upstreamTimePOSIX,
            y = self$upstreamDO,
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
         lines(
            x = self$output$time,
            y = self$downstreamDOSat,
            lty = "dashed"
         );
      },
      
      # Method TwoStationMetabDo$plotTemp ####
      #
      #' @description 
      #'   Plot the temperature.
      #'   
      #' @param ylim
      #'   The limits of the scale on the y axis for temperature.
      #'   Defaults to an empty vector which calculates the min and max from
      #'   the temperature data. 
      #' @param mar
      #'   The margins of the temperature plot.
      #'   Defaults to c(2.5, 4.5, 1, 4).
      #' @param ...
      #'   Abstract arguments to be passed to the plot.default function
      #' 
      #' @return 
      #'   No defined return value.
      #'   
      plotTemp = function
      (
         ylim = numeric(),
         mar = c(2.5, 4.5, 1, 4),
         ...
      ) 
      {
         par(mar = mar);
         if (length(ylim) != 2) {
            ymin <- min(
               self$upstreamTemp,
               self$downstreamTemp
            );
            ymax <- max(
               self$upstreamTemp,
               self$downstreamTemp
            );
            ylim <- c(ymin, ymax);
         } else {
            ymin <- ylim[1];
            ymax <- ylim[2];
         }
         plot(
            x = self$output$time,
            y = self$downstreamTemp,
            xaxt = "n",
            xlab = "",
            ylim = ylim,
            ylab = bquote(Temperature~.("(")*degree*C*.(")")),
            ...
         );
         points(
            x = self$upstreamTimePOSIX,
            y = self$upstreamTemp,
            pch = 20
         )
         axis.POSIXct(
            x = self$output$time,
            side = 1,
            format = "%H:%M"
         );
      }
   )
)
