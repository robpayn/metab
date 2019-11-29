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
#'    Class OneStationMetabDo (R6)
#'
#' @description
#'    A model for predicting dissolved oxygen concentrations in
#'    a stream using the one-station method.
#'    Parameters are for the R6 constructor method ($new).
#' 
#' @param dailyGPP 
#'    Daily average influence of gross primary production on oxygen concentration.
#'    Units are micomolality per day.
#' @param dailyER 
#'    Daily average influence of ecosystem respiration on oxygen.
#'    Units are micomolality per day.
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
#'    Defaults to \code{\link{kSchmidt}}
#'    
#' @return 
#'    Reference to a new OneStationMetabDo object configured with the provided arguments.
#'    
#' @section Implements \code{\link{Model}}:
#'   Method \code{$run}
#'   \itemize{
#'     \item see \code{\link{Model_run}}
#'     \item see \code{\link{OneStationMetabDo_run}}
#'   }
#'   
#' @section Methods:
#'   Method \code{$plot}
#'   \itemize{
#'     \item see \code{\link{OneStationMetabDo_plot}}
#'   }
#'   Method \code{$plotPAR}
#'   \itemize{
#'     \item see \code{\link{OneStationMetabDo_plotPAR}}
#'   }
#'   
OneStationMetabDo <- R6Class(
   classname = "OneStationMetabDo",
   public = list(
      output = NULL,
      dailyGPP = NULL, 
      dailyER = NULL, 
      k600 = NULL, 
      airPressure = NULL,
      initialDO = NULL, 
      time = NULL, 
      temp = NULL, 
      par = NULL, 
      parTotal = NULL, 
      dt = NULL,
      doSatCalculator = NULL,
      kSchmidtFunc = NULL,
      initialize = function
         (
            dailyGPP, 
            dailyER, 
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
            kSchmidtFunc = kSchmidt
         ) 
         {
            # Assign attribute values from arguments
            
            self$dailyGPP <- dailyGPP; 
            self$dailyER <- dailyER;
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
         }
      )
);

# Method OneStationMetabDo$run ####

#' @name OneStationMetabDo_run
#'
#' @title
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
#' @section Method of class:
#'   \code{\link{OneStationMetabDo}}
#'
#' @section Implementation of abstract method:
#'   \code{\link{Model_run}} - See abstract method for documentation
#'   
OneStationMetabDo$set(
   which = "public",
   name = "run",
   value = function
      () 
      {
         # Calculate the saturated oxygen concentration for the 
         # provided temperatures and air pressure
         
         doSat <- self$doSatCalculator$calculate(
            temp = self$temp,
            airPressure = self$airPressure
         );
         
         # Calculate the temperature adjusted gas exchange rate from 
         # the k600 rate at a Schmidt number of 600
         
         k <- self$kSchmidtFunc(self$temp, self$k600);
         
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
            do = numeric(doPredLength),
            doSat = doSat,
            doProduction = numeric(doPredLength),
            doConsumption = numeric(doPredLength),
            k = k,
            temp = self$temp,
            dt = self$dt
         );
         
         # Creates first value for the vector doPred$do  
         
         self$output$do[1] <- self$initialDO[1];
         
         # Calculate the DO production and consumption for
         # each time step
         
         self$output$doProduction <- self$dailyGPP * 
            ((self$par * self$dt) / self$parTotal); 
         self$output$doConsumption <- self$dt * self$dailyER;
         
         # Iterates over the time steps to model the change in dissolved
         # oxygen over time (one station method)
         for (i in 2:doPredLength) {
            self$output$do[i] <- self$output$do[i - 1] +
               self$output$doProduction[i - 1] +
               self$output$doConsumption[i - 1] + 
               self$dt[i - 1] * k[i - 1] * (doSat[i - 1] - self$output$do[i - 1]);
         }
         
         return(self$output);
      }
);

# Method OneStationMetabDo$plot ####

#' @name OneStationMetabDo_plot
#'
#' @title
#'    Plots the model output
#'
#' @description
#'    Plots the oxygen concentration, saturation oxygen concentration,
#'    and PAR on the same axes.
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
#' @section Method of class:
#'   \code{\link{OneStationMetabDo}}
#'   
OneStationMetabDo$set(
   which = "public",
   name = "plot",
   value = function
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
   }
);

# Method OneStationMetabDo$plotPAR ####

#' @name OneStationMetabDo_plotPAR
#'
#' @title
#'    Plots the PAR
#'
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
#' @section Method of class:
#'   \code{\link{OneStationMetabDo}}
#'   
OneStationMetabDo$set(
   which = "public",
   name = "plotPAR",
   value = function
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