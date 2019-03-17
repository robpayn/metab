# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Utility functions ####

#' @export
#' 
#' @title
#'    Calculates the density of water
#' 
#' @description 
#'    Density of water is calculated based on an empirical 
#'    relationship with the temperature of water
#'    
#' @param temp 
#'    Numerical vector of temperatures (deg C) 
#' @return 
#'    Numerical vector of densities of water in kilograms per liter
#'    
densityWater <- function(temp) 
{
   return (
      0.999842 +
         6.7940e-5 * temp -
         9.0953e-6 * temp^2 +
         1.0017e-7 * temp^3 -
         1.1201e-9 * temp^4 +
         6.5363e-12 * temp^5
   );
}

#' @export
#' 
#' @title
#'    Calculates temperature adjusted gas exchange
#' 
#' @description 
#'    Adjusts a gas exchange velocity or rate based on temperature
#'    and the provided gas exchange velocity or rate at a Schmidt number of 600
#'    
#' @param temp 
#'    Water temperature in degrees Celsius
#'    (numerical vector)
#' @param k600 
#'    Gas exchange rate (per time) or velocity (length per time)
#'    at a Schmidt number of 600
#'    (numerical vector)
#' @return 
#'    Numerical vector of temperature corrected gas exchange rates 
#'    in same units as k600 argument
#'    
kSchmidt <- function(temp, k600)
{
   schmidt <- 1800.6 -
      120.1 * temp +
      3.7818 * temp^2 -
      0.047608 * temp^3;
   return (k600 * (schmidt / 600)^-0.5);
}

# Class DoSatCalculator (R6) ####

#' @export
#' 
#' @title 
#'    Class to calculate saturated DO concentration
#'
#' @description 
#'    Calculates the saturation concentration of dissolved
#'    oxygen in water based on temperature and air pressure.
#'    Standard air pressure and a unit conversion factor for
#'    the return value are configurable so the calculator
#'    can be used for many different unit systems, though
#'    temperature units must be deg C.
#' 
#' @usage 
#'    DoSatCalculator$new(
#'       unitConvFactor, 
#'       densityWaterFunc = densityWater,
#'       stdAirPressure = 760
#'    )
#' @param unitConvFactor
#'    A unit conversion factor that will be multiplied by
#'    the base units of mol per liter to provide a
#'    concentration
#' @param densityWaterFunc
#'    The function used to estimate the density of water
#'    from temperature.  Defaults to densityWater provided as
#'    a utility function in this package.
#' @param stdAirPressure
#'    Standard air pressure at sea level in the same units
#'    that will be used for the saturation concentration
#'    calculation.  Defaults to 760 mm Hg.
#' @return 
#'    The object of class \code{DoSatCalculator} 
#'    instantiated by the constructor
#'    
DoSatCalculator <- R6Class(
   classname = "DoSatCalculator",
   public = list(
      densityWaterFunc = NULL,
      stdAirPressure = NULL,
      unitConvFactor = NULL,
      initialize = function(
         unitConvFactor,
         densityWaterFunc = densityWater,
         stdAirPressure = 760
      )
      {
         self$unitConvFactor <- unitConvFactor;
         self$densityWaterFunc <- densityWaterFunc;
         self$stdAirPressure <- stdAirPressure;
      },
      calculate = function(temp, airPressure)
      {
         normTemp <- log(
            (298.15 - temp) / 
               (273.15 + temp)
         );
         return(
            self$unitConvFactor *
               (airPressure / self$stdAirPressure) * 
               self$densityWaterFunc(temp) *
               exp(
                  5.80871 +
                     3.20291 * normTemp +
                     4.17887 * normTemp^2 +
                     5.10006 * normTemp^3 -
                     0.0986643 * normTemp^4 +
                     3.80369 * normTemp^5
               )
         );
      }
   )
);
