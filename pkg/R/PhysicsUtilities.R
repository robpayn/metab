# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Water density function ####

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
#'    
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

# DO gas exchange temperature correction function ####

#' @export
#' 
#' @title
#'    Adjusts air-freshwater dissolved oxygen exchange for temperature
#' 
#' @description 
#'    Adjusts a dissolved oxygen exchange velocity or rate based on temperature
#'    and the provided dissolved gas exchange velocity or rate at a Schmidt number of 600.
#'    The Schmidt number for oxygen calculated based on an empirical function of 
#'    temperature accoring to Wanninkhof (1992 Journal of Geophysical Research).
#'    The gas exchange rate at a Schmidt number of 600 is then coverted to the
#'    gas exchange rate at the Schmidt number estimated from the provided temperature
#'    according to Jahne et al. (1987, Journal of Geophysical Research).
#'    
#' @param temp 
#'    Water temperature in degrees Celsius
#'    (numerical vector)
#' @param k600 
#'    Gas exchange rate (per time) or velocity (length per time)
#'    at a Schmidt number of 600
#'    (numerical vector)
#'    
#' @return 
#'    Numerical vector of temperature corrected gas exchange rates 
#'    for oxygen in same units as k600 argument
#'    
kSchmidtDO <- function(temp, k600)
{
   schmidt <- 
      1800.6 -
      120.1 * temp +
      3.7818 * temp^2 -
      0.047608 * temp^3;
   return (k600 * (schmidt / 600)^-0.5);
}

# Dissolved carbon dioxide gas exchange temperature correction function ####

#' @export
#' 
#' @title
#'    Adjusts air-freshwater dissolved carbon dioxide exchange for temperature
#' 
#' @description 
#'    Adjusts a dissolved carbon dioxide exchange velocity or rate based on temperature
#'    and the provided dissolved gas exchange velocity or rate at a Schmidt number of 600.
#'    The Schmidt number for carbon dioxide is calculated based on an empirical function of 
#'    temperature accoring to Raymond et al. (2012 Limnology and Oceanography: Fluids and Environments).
#'    The exchange rate at a Schmidt number of 600 is then coverted to the
#'    gas exchange rate at the Schmidt number estimated from the provided temperature
#'    according to Jahne et al. (1987, Journal of Geophysical Research).
#'    
#' @param temp 
#'    Water temperature in degrees Celsius
#'    (numerical vector)
#' @param k600 
#'    Gas exchange rate (per time) or velocity (length per time)
#'    at a Schmidt number of 600
#'    (numerical vector)
#'    
#' @return 
#'    Numerical vector of temperature corrected gas exchange rates 
#'    for carbon dioxide in same units as k600 argument
#'    
kSchmidtCO2 <- function(temp, k600)
{
   schmidt <- 
      1742 -
      91.24 * temp +
      2.208 * temp^2 -
      0.0219 * temp^3;
   return (k600 * (schmidt / 600)^-0.5);
}

# Class DoSatCalculator (R6) ####

#' @export
#' 
#' @title 
#'    R6 class defining a DO Saturation concentration calculator
#'
#' @description 
#'    Calculates the saturation concentration of dissolved
#'    oxygen in water based on temperature and air pressure.
#' 
DoSatCalculator <- R6Class(
   classname = "DoSatCalculator",
   public = list(
      
      #' @field densityWaterFunc
      #'   The function used to calculate the density of water from temperature
      densityWaterFunc = NULL,
      
      #' @field stdAirPressure
      #'   The air pressure under standard conditions
      stdAirPressure = NULL,
      
      # Method DoSatCalculator$new
      #
      #' @description 
      #'   Construct an instance of the class.
      #'   
      #' @param densityWaterFunc
      #'    The function used to estimate the density of water
      #'    from temperature.  Defaults to densityWater provided as
      #'    a utility function in this package.
      #' @param stdAirPressure
      #'    Standard air pressure at sea level in the same units
      #'    that will be used for the saturation concentration
      #'    calculation.  Defaults to 760 mm Hg. Units here
      #'    must be consistent with units used in other methods.
      #'    
      initialize = function
      (
         densityWaterFunc = densityWater,
         stdAirPressure = 760
      )
      {
         self$densityWaterFunc <- densityWaterFunc;
         self$stdAirPressure <- stdAirPressure;
      },
      
      # Method DoSatCalculator$calculate ####
      #
      #' @description 
      #'   Calculates the saturated dissolved oxygen concentration based
      #'   on the provided water temperature and air pressure.
      #'   
      #' @param temp
      #'   Water temperature(s) in degrees Celsius
      #' @param airPressure
      #'   Air pressure in same units as the definition of standard
      #'   air pressure in the class attribute stdAirPressure
      #'   
      #' @return 
      #'   The dissolved oxygen concentration at saturation relative
      #'   to concentrations in the overlying atmosphere (Henry's law)
      #'   
      calculate = function(temp, airPressure)
      {
         normTemp <- log(
            (298.15 - temp) / 
               (273.15 + temp)
         );
         return(
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
)
