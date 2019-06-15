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

# Gas exchange temperature correction function ####

#' @export
#' 
#' @title
#'    Adjusts air-freshwater dissolved oxygen exchange for temperature
#' 
#' @description 
#'    Adjusts a dissolved oxygen exchange velocity or rate based on temperature
#'    and the provided dissolved oxygen exchange velocity or rate at a 
#'    Schmidt number of 600.
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
#' @return 
#'    Numerical vector of temperature corrected gas exchange rates 
#'    in same units as k600 argument
#'    
kSchmidt <- function(temp, k600)
{
   schmidt <- 
      1800.6 -
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
#'    DoSatCalculator$new(<arguments>)
#' @param unitConvFactor
#'    A unit conversion factor that will be multiplied by
#'    the base units of micromoles per liter to provide a
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
#' @section Methods:
#'   \code{$new} - See usage section\cr
#'   \code{$calculate} - 
#'     See \code{\link{DoSatCalculator_calculate}}
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
         }
      )
);

# Method DoSatCalculator$calculate ####

#' @name DoSatCalculator_calculate 
#' 
#' @title 
#'   Calculate the saturated dissolved oxygen concentration
#'   
#' @description 
#'   Calculates the saturated dissolved oxygen concentration based
#'   on the provided water temperature and air pressure.
#'   
#' @usage 
#'   doSatCalculator$calculate(<arguments>)
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
#' @section Method of class: 
#'   \code{\link{DoSatCalculator}}
DoSatCalculator$set(
   which = "public",
   name = "calculate",
   value = function(temp, airPressure)
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
);

# Class SolarRadiation ####

#' @export
#' 
#' @title
#'   Theoretical solar radiation
#' 
#' @description 
#'   Provides the ability to make solar radiation calculations
#'   for a given location on Earth.\cr
#'   Usage describes the constructor for the class.
#'   
#' @usage 
#'   SolarRadiation$new(<arguments>)
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
#' @return 
#'   An R6 object of class SolarRadiation
#'   
#' @section Methods:
#'   \code{$new} - See usage section\cr
#'   \code{$getExtraterrestrialInsolation} - 
#'     See \code{\link{SolarRadiation_getExtraterrestrialInsolation}}
#'   
SolarRadiation <- R6Class(
   classname = "SolarRadiation",
   public = list(
      latitudeAngle = NULL,
      longitudeAngle = NULL,
      differenceFromGMT = NULL,
      adjustDST = NULL,
      solarConstantFlux = NULL,
      earthAngularVelocity = NULL,
      initialize = function(
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
         }
      )
);

# Method SolarRadiation$getExtraterrestrialInsolation ####

#' @name SolarRadiation_getExtraterrestrialInsolation
#' 
#' @title 
#'   Method for calculating extraterrestrial solar radiation
#'   
#' @description 
#'   Calculates the incoming extraterrestrial solar radiation at
#'   the times provided
#'   (i.e. before any influence by atmosphere) for the location
#'   on earth represented by the SolarRadiation object.
#' 
#' @usage 
#'   solarRadiation$getEgetExtraterrestrialInsolation(<arguments>)
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
#' @section Method of class: 
#'   \code{\link{SolarRadiation}}
#'   
SolarRadiation$set(
   which = "public",
   name = "getExtraterrestrialInsolation",
   value = function(
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
);
