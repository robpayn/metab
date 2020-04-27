# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#
#' @importFrom R6 R6Class
NULL

# Class OneStationMetabDo (R6) ####

#' @export
#' 
#' @title
#'    R6 Class defining a one station metabolism model for DO
#'
#' @description
#'    A model for predicting dissolved oxygen concentrations in
#'    a stream using the one-station method.
#' 
OneStationMetabDo <- R6Class(
   classname = "OneStationMetabDo",
   public = list(
      
      #' @field output
      #'   Model output data frame
      output = NULL,
      
      #' @field dailyGPP
      #'   Model parameter for daily gross primary production
      #'   based on C atoms fixed.
      #'   Units of micromoles per liter per day.
      dailyGPP = NULL, 
      
      #' @field ratioDoCfix
      #'    Ratio of DO molecules produced relative to carbon atoms fixed.
      ratioDoCfix = NULL,
      
      #' @field dailyGPPDO
      #'   Model parameter for daily gross primary production
      #'   based on oxygen molecules produced.
      #'   Units of micromoles per liter per day.
      dailyGPPDO = NULL,
      
      #' @field dailyER
      #'   Model parameter for daily aerobic ecosystem respiration
      #'   based on carbon atoms respired.
      #'   Units of micromoles per liter per day.
      dailyER = NULL, 
      
      #' @field ratioDoCresp
      #'    Ratio of DO molecules consumed relative to carbon atoms respired.
      #'    Defaults to 1.
      ratioDoCresp = NULL,
      
      #' @field dailyERDO
      #'   Model parameter for daily aerobic ecosystem respiration
      #'   based on oxygen molecules consumed.
      #'   Units of micromoles per liter per day.
      dailyERDO = NULL, 
      
      #' @field k600
      #'   Model parameter for the gas exchange rate
      k600 = NULL, 
      
      #' @field airPressure
      #'   Air pressure
      airPressure = NULL,
      
      #' @field initialDO
      #'   Initial dissolved oxygen concentration
      initialDO = NULL, 
      
      #' @field time
      #'   Vector of times for model predictions
      time = NULL, 
      
      #' @field temp
      #'   Vector of temperatures at the simulated times
      temp = NULL, 
      
      #' @field par
      #'   Vector of photosynthetically active radiation at the simulated times
      par = NULL, 
      
      #' @field parTotal
      #'   Total PAR over the simulation period
      parTotal = NULL, 
      
      #' @field dt
      #'   Vector of time steps
      dt = NULL,
      
      #' @field doSatCalculator
      #'   Function to use for do saturation calculations
      doSatCalculator = NULL,
      
      #' @field kSchmidtFunc
      #'   Function to use to adjust oxygen gas exchange for temperature
      kSchmidtFunc = NULL,
      
      # Method OneStationMetabDo$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #' 
      #' @param dailyGPP 
      #'    Model parameter for daily gross primary production
      #'    based on C atoms fixed.
      #'    Units of micromoles per liter per day.
      #' @param ratioDoCfix
      #'    Ratio of DO molecules produced relative to carbon atoms fixed.
      #'    Defaults to 1.
      #' @param dailyER
      #'    Model parameter for daily aerobic ecosystem respiration
      #'    based on carbon atoms respired.
      #'    Units of micromoles per liter per day.
      #' @param ratioDoCresp
      #'    Ratio of DO molecules consumed relative to carbon atoms respired.
      #'    Defaults to -1.
      #' @param k600 
      #'    Influence of atmospheric gas exchange on oxygen concentration as a first-order rate
      #'    depending on saturation deficit. Units are per day.
      #' @param airPressure 
      #'    Barometric pressure.
      #'    Units must be consistent with the units of the optional standard air pressure
      #'    argument.
      #'    Default value for standard air pressure results in units of mm Hg.
      #' @param initialDO 
      #'    Initial DO concentration at the beginning of the evaluation period.
      #'    Units depend on the unit conversion factor for DO saturation.
      #'    Default value of the unit conversion factor results in
      #'    units of micomolarity per day.
      #' @param time 
      #'    Vector of times associated with temperature and PAR data.
      #'    POSIXct vector or character vector in a standard text format
      #'    to be forced to POSIXct "YYYY-MM-DD HH:mm:ss".
      #' @param temp 
      #'    Vector of water temperatures in degrees Celsius.
      #'    Must be same length as the time vector.
      #' @param par 
      #'    Vector of photosynthetically active radiation values. 
      #'    Units are arbitrary, but must be consistent with total PAR if the
      #'    optional argument for total PAR is specified.
      #'    Must be the same length as the time vector.
      #' @param parTotal 
      #'    total PAR, see units note in par argument description
      #'    (numerical vector with single value, defaults to NA which forces
      #'    calculation of total PAR from PAR data vector)
      #' @param stdAirPressure
      #'    The standard air pressure in the desired units. Defaults to 760 mm Hg.
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
         airPressure, 
         initialDO, 
         time, 
         temp, 
         par, 
         parTotal = NA, 
         stdAirPressure = 760,
         doSatCalculator = DoSatCalculator$new(
            stdAirPressure = stdAirPressure
         ),
         kSchmidtFunc = kSchmidtDO
      ) 
      {
         # Assign attribute values from arguments
         
         self$dailyGPP <- dailyGPP;
         self$ratioDoCfix <- ratioDoCfix;
         self$dailyER <- dailyER;
         self$ratioDoCresp <- ratioDoCresp;
         self$k600 <- k600; 
         self$airPressure <- airPressure;
         self$initialDO <- initialDO;
         self$temp <- temp;
         self$par <- par;
         self$doSatCalculator <- doSatCalculator;
         self$kSchmidtFunc <- kSchmidtFunc;

         # Time argument is adjusted to units of days since the epoch
         # (assumed 1970-01-01) in UTC
         
         self$time <- as.numeric(as.POSIXct(time)) / 86400;
         
         # Calculate the time steps between data points
         
         self$dt <- c(
            self$time[2:length(self$time)] - self$time[1:length(self$time) - 1], 
            0
         );
         
         # If total PAR is not provided as an argument (or is NA), 
         # total PAR is calculated over the vector using trapezoidal integration
         
         if (is.na(parTotal)) {
            parAverage <- c(
               0.5 * (self$par[1:length(self$time) - 1] + self$par[2:length(self$time)]),
               0
            );
            self$parTotal <- sum(parAverage * self$dt);
         } else {
            self$parTotal <- parTotal; 
         }
      },
      
      # Method OneStationMetabDo$run ####
      #
      #' @description
      #'   Runs a one-station metabolism model
      #'
      #' @return 
      #'    Data frame with incremental and final results of the simulation,
      #'    with columns:
      #'    \itemize{
      #'      \item time: POSIXct simulation time (tzone attribute will not be set)
      #'      \item do: dissolved oxygen concentration in micromolality
      #'      \item doSat: Saturated dissolved oxygen concentration in micromolality
      #'      \item doProduction: increase in DO concentration in micromolality during the time step
      #'      \item doConsumption: decrease in DO concentration in micromolality during the time step
      #'      \item k: DO gas exchange rate in per day
      #'      \item temp: water temperature in degrees Celsius
      #'      \item dt: length of time step in days
      #'    }
      #'    
      run = function() 
      {
         # Caclulate the effect of metabolism on DO based
         # on stoichiometric efficiencies
         
         self$dailyGPPDO <- self$dailyGPP * self$ratioDoCfix;
         self$dailyERDO <- self$dailyER * self$ratioDoCresp;
         
         # Specify length to use for vectors and the data frame based 
         # on the length of time vector
         
         doPredLength <- length(self$time);
         
         # Creates a data frame to store simulated values at each
         # time step
         
         self$output <- data.frame(
            time = as.POSIXct(
               self$time * 86400, 
               origin = as.POSIXct("1970-01-01", tz = "GMT")
            ),
            do = rep(as.numeric(NA), doPredLength),
            doSat = self$doSatCalculator$calculate(
               temp = self$temp,
               airPressure = self$airPressure
            ),
            doProduction = self$dailyGPPDO * 
               ((self$par * self$dt) / self$parTotal),
            doConsumption = self$dailyERDO * 
               self$dt,
            k = self$kSchmidtFunc(self$temp, self$k600),
            temp = self$temp,
            dt = self$dt
         );
         
         # Creates first value for the vector doPred$do  
         
         self$output$do[1] <- self$initialDO[1];
         
         # Iterates over the time steps to model the change in dissolved
         # oxygen over time (one station method)
         for (i in 2:doPredLength) {
            self$output$do[i] <- self$output$do[i - 1] +
               self$output$doProduction[i - 1] +
               self$output$doConsumption[i - 1] + 
               self$dt[i - 1] * self$output$k[i - 1] * 
                  (self$output$doSat[i - 1] - self$output$do[i - 1]);
         }
         
         return(self$output);
      },
      
      # Method OneStationMetabDo$plot ####
      #
      #' @description
      #'    Plots the oxygen concentration, saturation oxygen concentration,
      #'    and PAR on the same axes. Plots to the current active device.
      #' 
      #' @param obsDO
      #'    Optional parameter to specify a vector of observations to plot
      #'    as points on the graph. The vector must be the same length as
      #'    the number of time steps in the model. Default value is an empty
      #'    vector, such that observations will only be plotted if a vector
      #'    is provided as an argument.
      #' @param mar
      #'    Optional parameter to change the inner margins of the plot. 
      #'    Default value is c(2.5, 4.5, 1, 4).
      #' @param plotPAR
      #'    Optional parameter to turn on/off plotting of PAR on the same axes.
      #'    Defaults to TRUE.
      #' @param colPAR
      #'    Optional parameter to set the color for the PAR plot.
      #'    Defaults to "red".
      #' @param labelPAR
      #'    Optional parameter for changing the PAR label.
      #'    Defaults to "PAR".
      #' @param ylim
      #'    Optional two-element vector to change the min/max scaling of the y axis
      #'    Defaults to empty vector, which results in the minimum and
      #'    maximum of oxygen data determining the axis scale
      #' @param ...
      #'    Any additional parameters will be passed to the R "plot.default"
      #'    function used to set up the axes for the oxygen plots.
      #' 
      #' @return 
      #'   No defined return value
      #'   
      plot = function
      (
         obsDO = numeric(),
         mar = c(2.5, 4.5, 1, 4),
         plotPAR = TRUE,
         colPAR = "red",
         labelPAR = "PAR",
         ylim = numeric(),
         ...
      ) 
      {
         par(
            mar = mar
         );
         if (length(ylim) != 2) {
            ymin <- min(self$output$do);
            ymax <- max(self$output$do);
            if (length(obsDO) > 0) {
               ymin <- min(ymin, obsDO);
               ymax <- max(ymax, obsDO);
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
         if (length(obsDO) > 0) {
            points(
               x = self$output$time,
               y = obsDO
            );
         }
         axis.POSIXct(
            x = self$output$time,
            side = 1,
            format = "%H:%M"
         );
         lines(
            x = self$output$time,
            y = self$output$doSat,
            lty = "dashed"
         );
         if (plotPAR) {
            par(new = TRUE);
            self$plotPAR(
               col = colPAR,
               axes = FALSE,
               ylab = ""
            );
            axis(
               side = 4
            );
            mtext(
               text = labelPAR,
               side = 4,
               line = 2.5
            )   
         }
      },
      
      # Method OneStationMetabDo$plotPAR ####
      #
      #' @description
      #'    Plots the PAR that was used for the model simulation.
      #' 
      #' @param ylab
      #'    Optional parameter to change the y axis label.
      #'    Defaults to "PAR".
      #' @param col
      #'    Optional parameter to change the color of the plot.
      #'    Defaults to "red".
      #' @param ...
      #'    Any additional parameters will be passed to the R "plot.default"
      #'    function used to set up the axes for the PAR plot.
      #' 
      #' @return 
      #'   No defined return value
      #'   
      plotPAR = function
      (
         ylab = "PAR",
         col = "red",
         ...
      ) 
      {
         plot(
            x = self$output$time,
            y = self$par,
            ylab = ylab,
            ylim = c(
               max(self$par),
               min(self$par)
            ),
            col = col,
            ...
         );
      }
      
   )
)
