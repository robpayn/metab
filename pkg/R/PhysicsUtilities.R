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
);


# Class SolarRadiation ####

#' @export
#' 
#' @title
#'   R6 class defining incoming solar radiation
#' 
#' @description 
#'   Provides the ability to make solar radiation calculations
#'   for a given location on Earth.
#'   
SolarRadiation <- R6Class(
   classname = "SolarRadiation",
   public = list(
      
      #' @field latitudeAngle
      #'   Angle of the latitude
      latitudeAngle = NULL,
      
      #' @field longitudeAngle
      #'   Angle of the longitude
      longitudeAngle = NULL,
      
      #' @field differenceFromGMT
      #'   Number of hours different from UTC
      differenceFromGMT = NULL,
      
      #' @field adjustDST
      #'   Number of hours to adjust during daylight savings
      adjustDST = NULL,
      
      #' @field solarConstantFlux
      #'   The solar constant for flux of radiation (for Earth)
      solarConstantFlux = NULL,
      
      #' @field earthAngularVelocity
      #'   The angular velocity of Earth's rotation on its axis
      earthAngularVelocity = NULL,
      
      # Method SolarRadiation$new ####
      #
      #' @description 
      #'   Construct a new instance of the class
      #'   
      #' @param latitude
      #'   Latitude on earth in decimal degrees
      #' @param longitude
      #'   Longitude on earth in decimal degrees
      #' @param differenceFromGMT
      #'   Number of hours time zone is different from GMT
      #' @param adjustDST
      #'   Number of hours to adjust the difference from GMT during 
      #'   daylight savings time.
      #'   Default value is NA, which disables daylight savings adjustments.
      #' @param solarConstantFlux
      #'   Base flux of energy from the sun.
      #'   Default value is 1364 Watts per square meter
      #' @param earthAngularVelocity
      #'   Angular velocity of earth's rotation in radians per hour.
      #'   Default value is 0.2618.
      #'   
      initialize = function
      (
         latitude,
         longitude,
         differenceFromGMT,
         adjustDST = NA,
         solarConstantFlux = 1364,
         earthAngularVelocity = 0.2618
      ) 
      {
         self$latitudeAngle <- 2 * pi * (latitude / 360);
         self$longitudeAngle <- 2 * pi * (longitude / 360);
         self$differenceFromGMT <- differenceFromGMT;
         self$adjustDST <- adjustDST;
         self$solarConstantFlux <- solarConstantFlux;
         self$earthAngularVelocity <- earthAngularVelocity;
      },
      
      # Method SolarRadiation$getExtraterrestrialInsolation ####
      #
      #' @description 
      #'   Calculates the incoming extraterrestrial solar radiation at
      #'   the times provided
      #'   (i.e. before any influence by atmosphere) for the location
      #'   on earth represented by the SolarRadiation object.
      #' 
      #' @param time
      #'   The time(s) at which solar radiation should be calculated.
      #'   Must be coercable into a POSIX type time.
      #' @param timeCT
      #'   The time in POSIXct type.
      #'   Default value is time coerced into POSIXct class.
      #' @param timeLT
      #'   The time in POSXlt type.
      #'   Default value is timeCT coerced into POSIXlt class.
      #' @param solarNoonCorrectionTime
      #'   The time basis for calculating solar noon corrections.
      #'   Defaults to a conversion to radians from a version of the
      #'   day angle.
      #' @param solarNoonCorrectionEcc
      #'   The correction for eccentricities in Earth's orbit.
      #'   Defaults to an empirical approximation from solarNoonCorrectionTime
      #' @param dayAngle
      #'   The angle representing the day of the year, if the year is represented
      #'   by 2 * pi radians
      #'   Default value is calculated based on the day of year form timeLT.
      #' @param eccCoefficient
      #'   The coefficient of correction for radiation based on eccentricities in
      #'   Earth's distance from the sun during its orbit. 
      #'   Default value is an empirical calculation from the day angle.
      #' @param declinationAngle
      #'   The declination angle of the sun relative to Earth's equator.
      #'   Default value is an empirical calculation from the day angle.
      #'   
      #' @return 
      #'   The solar radiation flux at the top of Earth's atmosphere
      #'   
      getExtraterrestrialInsolation = function
      (
         time,
         timeCT = as.POSIXct(time),
         timeLT = as.POSIXlt(timeCT),
         solarNoonCorrectionTime = 
            2 * pi * (timeLT$yday - 81) / 365,
         solarNoonCorrectionEcc = 
            (
               9.87 * sin(2 * solarNoonCorrectionTime) - 
                  7.53 * cos(solarNoonCorrectionTime) -
                  1.50 * sin(solarNoonCorrectionTime) 
            ) /
            60,
         dayAngle = 2 * pi * (timeLT$yday / 365),
         eccCoefficient =
            1.000110 +
            0.034221 * cos(dayAngle) +
            0.001280 * sin(dayAngle) +
            0.000719 * cos(2 * dayAngle) +
            0.000077 * sin(2 * dayAngle),
         declinationAngle = 
            0.006918 -
            0.399912 * cos(dayAngle) +
            0.070257 * sin(dayAngle) -
            0.006758 * cos(2 * dayAngle) +
            0.000907 * sin(2 * dayAngle) -
            0.002697 * cos(3 * dayAngle) +
            0.001480 * sin(3 * dayAngle)
      )
      {
         meridianAngle = rep(
            0.2617994 * self$differenceFromGMT, 
            times = length(timeCT)
         );
         
         if (!is.na(self$adjustDST)) {
            meridianAngle[timeLT$isdst == 1] <- 
               0.2617994 * (self$differenceFromGMT + self$adjustDST);
         }
         
         solarNoonCorrection <- 
            solarNoonCorrectionEcc + 
            (self$longitudeAngle - meridianAngle) / 
            self$earthAngularVelocity;
         
         solarTime <- timeCT + (solarNoonCorrection * 3600);
         
         solarNoon <- as.POSIXlt(solarTime);
         solarNoon$hour <- 12;
         solarNoon$min <- 0;
         solarNoon$sec <- 0;
         solarNoon <- as.POSIXct(solarNoon);
         
         timeAfterNoon <- 
            (as.numeric(solarTime) - as.numeric(solarNoon)) / 3600;
         
         zenithAngle <-
            acos(
               sin(self$latitudeAngle) *
                  sin(declinationAngle) +
                  cos(self$latitudeAngle) *
                  cos(declinationAngle) *
                  cos(self$earthAngularVelocity * timeAfterNoon)
            );
         zenithAngle[zenithAngle > pi / 2] <- pi / 2;
         
         return(
            self$solarConstantFlux * 
               eccCoefficient *
               cos(zenithAngle)
         );
      }
   )
)
