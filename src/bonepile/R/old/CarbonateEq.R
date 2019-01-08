#' Calculate Henry's constant for CO2
#' 
#' Calculates Henry's constant for CO2 dissolution in 
#'    water based on the provided temperature. (Weiss 1974 - Marine Chemistry)
#'    
#' @export
#' @param tempK Temperature in Kelvin
#' @param convertC Boolean value specifying whether the provided temperature
#'    should be converted from Celsius to Kelvin (TRUE will convert the value,
#'    FALSE will not convert the value) Default value is FALSE.
#' @return Henry's constant for CO2 in moles per liter per atmosphere
kHenryCO2fromTemp <- function(tempK, convertC = FALSE)
{
   if (convertC) {
      tempK = tempK + 273.15;
   }
   return(
      exp(-60.2409 + 
         93.4517 * (100 / tempK)  + 
         23.3585 * log(tempK / 100))
         );
}

# CarbonateEQ Class (reference) ####

#' Class CarbonateEq
#' 
#' Reference class \code{CarbonateEq} defines a set of equilibrium
#' conditions for inorganic carbon at a given temperature 
#' in freshwater
#' 
#' @exportClass CarbonateEq
#' @seealso \code{\link{CarbonateEq_initialize}} for constructor

CarbonateEq <- setRefClass(
   Class = "CarbonateEq",
   fields = c(
      tempC = "numeric",
      tempK = "numeric",
      eConduct = "numeric",
      ionicStrength = "numeric",
      daviesParam = "numeric",
      daviesExponent = "numeric",
      activityCoeffH = "numeric",
      activityCoeffOH = "numeric",
      activityCoeffHCO3 = "numeric",
      activityCoeffCO3 = "numeric",
      kDissocH2CO3App = "numeric",
      kDissocHCO3App = "numeric",
      kDissocH2OApp = "numeric",
      kHenryCO2 = "numeric"
      ),
   );

#' Constructor method 
#' 
#' Overrides the initialize method for the CarbonateEq reference class
#'
#' @name CarbonateEq_initialize
#' @param tempC Temperature in degrees Celsius
#' @param eConduct Electrical conductivity in mS cm-1, default is set to zero
#'    which will result in the ideal case of activity equal to concentration
#' @param ionicStrength Ionic strength in mol L-1, default is set as a linear correlate
#'    of electrical conductivity as suggested by Griffin and Jurinak
#'    (1973 - Soil Science)
#' @param daviesParam Parameter for Davies equation derivation, defaults to 
#'    \code{0.5092 + (tempC - 25) * 0.00085}
#' @param daviesExponent Factor for exponent in Davies equation, defaults to
#'    \code{(ionicStrength ^ 0.5 / (1 + ionicStrength ^ 0.5) - 0.3 * ionicStrength)}
#' @return The object of class \code{CarbonateEq} created
#'      by the constructor
NULL

CarbonateEq$methods(
   initialize = function(
      tempC,
      tempK = tempC + 273.15,
      eConduct = 0,
      ionicStrength = 0.013 * eConduct,
      daviesParam = 0.5092 + (tempC - 25) * 0.00085,
      daviesExponent = -daviesParam * 
         (ionicStrength ^ 0.5 / (1 + ionicStrength ^ 0.5) - 
              0.3 * ionicStrength)
      )
      {
         newObject <- callSuper(
            tempC = tempC,
            tempK = tempK,
            eConduct = eConduct,
            ionicStrength = ionicStrength,
            daviesParam = daviesParam,
            daviesExponent = daviesExponent,
            activityCoeffH = NaN,
            activityCoeffOH = NaN,
            activityCoeffHCO3 = NaN,
            activityCoeffCO3 = NaN,
            kDissocH2CO3App = NaN,
            kDissocHCO3App = NaN,
            kDissocH2OApp = NaN,
            kHenryCO2 = NaN
            );
         newObject$resetFromTemp();

         return(newObject);
      }
   );

#' Reset the carbonate equlibrium 
#' 
#' Resets equilibrium constants based on the provided temperature
#'
#' @name CarbonateEq_resetFromTemp
#' @param argtempC Temperature in degrees Celsius
#' @param argeConduct Electrical conductivity in mS cm-1, default is set to zero
#'    which will result in the ideal case of activity equal to concentration
#' @param argionicStrength Ionic strength in mol L-1, default is set as a linear correlate
#'    of electrical conductivity as suggested by Griffin and Jurinak
#'    (1973 - Soil Science)
#' @param argdaviesParam Parameter for Davies equation derivation, defaults to 
#'    \code{0.5092 + (tempC - 25) * 0.00085}
#' @param argdaviesExponent Factor for exponent in Davies equation, defaults to
#'    \code{(ionicStrength ^ 0.5 / (1 + ionicStrength ^ 0.5) - 0.3 * ionicStrength)}
#' @return A named vector with 2 values: the previous temperature and the new temperature
NULL

CarbonateEq$methods(
   resetFromTemp = function(
      argtempC = NaN,
      argtempK = argtempC + 273.15,
      argeConduct = 0,
      argionicStrength = 0.013 * argeConduct,
      argdaviesParam = 0.5092 + (argtempC - 25) * 0.00085,
      argdaviesExponent = -argdaviesParam * 
         (argionicStrength ^ 0.5 / (1 + argionicStrength ^ 0.5) - 
             0.3 * argionicStrength)
      )
      {
         prevTemp <- tempC;
         
         if (!is.nan(argtempC))
         {
            tempC <<- argtempC;
            tempK <<- argtempK;
            eConduct <<- argeConduct;
            ionicStrength <<- argionicStrength;
            daviesParam <<- argdaviesParam;
            daviesExponent <<- argdaviesExponent;
         }
         
         # Activitity coefficients from Davies equation 
         # based on square of ionic charge
         activityCoeffH <<- 10 ^ daviesExponent; # Charge +1
         activityCoeffOH <<- activityCoeffH; # Charge -1
         activityCoeffHCO3 <<- activityCoeffH; # Charge -1
         activityCoeffCO3 <<- 10 ^ (4 * daviesExponent); # Charge -2
         
         logTempK <- log(tempK);
         
         # Ideal and apparent dissociation constant for H2CO3
         # (Millero 1979 - Geochimica et Cosmochimica Acta)
         kDissocH2CO3 <- exp(290.9097 - 14554.21 / tempK - 45.0575 * logTempK);
         kDissocH2CO3App <<- kDissocH2CO3 /
            (activityCoeffH * activityCoeffHCO3);
         
         # Ideal and apparent dissociation constant for HCO3
         # (Millero 1979 - Geochimica et Cosmochimica Acta)
         kDissocHCO3 <- exp(207.6548 - 11843.79 / tempK - 33.6485 * logTempK);
         kDissocHCO3App <<- kDissocHCO3 /
            (activityCoeffH * activityCoeffCO3 / activityCoeffHCO3);
         
         # Ideal and apparent dissociation constant for freshwater
         # (Millero 1995 - Geochimica et Cosmochimica Acta)
         kDissocH2O <- exp(-13847.26 / tempK + 148.9802 - 23.6521 * logTempK);
         kDissocH2OApp <<- kDissocH2O /
            (activityCoeffH * activityCoeffOH);
         
         kHenryCO2 <<- kHenryCO2fromTemp(tempK = tempK);
         
         return(c(Previous_Temp = prevTemp, New_Temp = tempC));
      }
   );

#' Get the total alkalinity
#' 
#' Method to get the total alkalinity for a known
#' equilibrium scenario based on a known dissolved inorganic carbon
#' concentration and pH
#'
#' @name CarbonateEq_getTotalAlk
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @param concDIC Dissolved inorganic carbon concentration in molality
#' @param pH The pH of the solution (minus the log10 of H+ concentration in molality)
#' @return Total alkalinity in charge molality
#' @aliases getTotalAlk.CarbonateEq
NULL

CarbonateEq$methods(
   getTotalAlk = function(concDIC, pH)
      {
         concH = 10 ^ -pH;
         concOH = kDissocH2OApp / concH;
         concCO2 = concDIC * (concH ^ 2) /
            ( concH ^ 2 +
                 kDissocH2CO3App * concH +
                 kDissocH2CO3App * kDissocHCO3App );
         concHCO3 = concCO2 * kDissocH2CO3App / concH;
         concCO3 = concHCO3 * kDissocHCO3App / concH;
         return(concHCO3 + 2 * concCO3 + concOH - concH);
      }
   );

#' Optimize to infer pH
#' 
#' Method to perform the optimization necessary to get the pH for a known
#' equilibrium scenario based on known dissolved inorganic carbon
#' and total alkalinity
#'
#' @name CarbonateEq_optimizepH
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @param concDIC Dissolved inorganic carbon concentration in molality
#' @param totalAlk The measured or otherwise known total alkalinity in charge molality
#' @param tolerance The tolerance used for convergence in the optimization algorithm,
#'    default value is 1e-5
#' @param range Range in pH values to constrain the optimization,
#'    default range is 2 < pH < 12
#' @return An optimized value of the pH based on finding the calculated total Alkalinity
#'    from DIC that matches the observed alkalinity provided
#' @aliases optimizepH.CarbonateEq
NULL

CarbonateEq$methods(
   optimizepH = function(
      concDIC,
      totalAlk,
      tolerance = 1e-5,
      range = c(12, 2)
      )
      {
         optr <- optimize(
            f = function(pH)
               {
                  return ( (totalAlk - getTotalAlk(concDIC, pH))^2 );
               },
            interval = range,
            tol = tolerance
            );
         return(optr$minimum);
      }
   );

#' Optimize to infer pCO2
#' 
#' Method to calculate the pCO2 derived from
#' the optimization necessary to get the pH for a known
#' equilibrium scenario based on known dissolved inorganic carbon
#' and total alkalinity
#'
#' @name CarbonateEq_optimizepCO2
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @param concDIC Dissolved inorganic carbon concentration in molality
#' @param totalAlk The measured or otherwise known total alkalinity in charge molality
#' @param tolerance The tolerance used for convergence in the optimization algorithm,
#'    default value is 1e-5
#' @param range Range in pH values to constrain the optimization,
#'    default range is 2 < pH < 12
#' @return A named list of the optimized values of the pCO2 and pH based on finding the
#'    calculated total Alkalinity from DIC that matches the observed alkalinity provided
#'    \code{list(pH = <optimized pH value>, pCO2 = <pCO2 value calculated from pH>)}.
#'    Value of pCO2 is in units of microatmospheres.
#' @aliases optimizepCO2.CarbonateEq
NULL

CarbonateEq$methods(
   optimizepCO2 = function(
      concDIC,
      totalAlk,
      tolerance = 1e-5,
      range = c(12, 2)
      )
      {
         pH <- optimizepH(
            concDIC = concDIC,
            totalAlk = totalAlk,
            tolerance = tolerance,
            range = range
            );
         concH <- 10 ^ -pH;
         concCO2 <- concDIC * concH ^ 2 * 10 ^ (6 * daviesExponent) /
            ( concH ^ 2 * 10 ^ (6 * daviesExponent) +
                kDissocH2CO3App * concH * 10 ^ (4 * daviesExponent) +
                kDissocH2CO3App * kDissocHCO3App );
         fCO2 <- 1e6 * (concCO2 / kHenryCO2);
         return( list(pH = pH, fCO2 = fCO2) );
      }
   );
