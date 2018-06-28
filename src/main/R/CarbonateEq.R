 # CarbonateEQ Class (S4) ####

#' Class CarbonateEq
#' 
#' S4 class \code{CarbonateEq} defines a set of equilibrium
#' conditions for inorganic carbon at a given temperature 
#' in freshwater
#' 
#' @export CarbonateEq
#' @exportClass CarbonateEq

CarbonateEq <- setClass(
   Class = "CarbonateEq",
   slots = c(
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
      )
   );

# CarbonateEQ constructor method (overrides S4 class initialize method) ####

#' S4 constructor method (override of \code{initialize()} method) 
#' for CarbonateEq class.
#' 
#' @rdname CarbonateEq-class
#' @param .Object Name of the object (for use with \code{new(...) constructor})
#' @param tempC Temperature in degrees Celsius
#' @param eConduct Electrical conductivity in mS cm-1, default is set to zero
#'    which will result in the ideal case of activity equal to concentration
#' @param ionicStrength Ionic strength in mol L-1, default is set as a linear correlate
#'    of electrical conductivity as suggested by Griffin and Jurinak 
#'    (1973 - Soil Science)
#' @return The object of class \code{HyperbolicAnalysis} created
#'      by the constructor

setMethod(
   f = "initialize",
   signature = "CarbonateEq",
   definition = function(
      .Object,
      tempC,
      eConduct = 0,
      ionicStrength = 0.013 * eConduct
      )
      {
         # Temperature in Kelvin
         tempK <- tempC + 273.15;
         
         # Parameter for Davies equation
         daviesParam <- 0.5092 + (tempC - 25) * 0.00085;
         
         # Exponent factor for Davies equation
         daviesExponent <- -daviesParam * 
            ( ionicStrength ^ 0.5 / (1 + ionicStrength ^ 0.5) - 
                 0.3 * ionicStrength );
         
         # Activitity coefficients from Davies equation 
         # based on ionic charge
         activityCoeffH <- 10 ^ daviesExponent; # Charge +1
         activityCoeffOH <- activityCoeffH; # Charge -1
         activityCoeffHCO3 <- activityCoeffH; # Charge -1
         activityCoeffCO3 <- 10 ^ (4 * daviesExponent); # Charge -2
         
         logTempK <- log(tempK);
         # Ideal and apparent dissociation constant for H2CO3 
         # (Millero 1979 - Geochimica et Cosmochimica Acta) 
         kDissocH2CO3 = exp(290.9097 - 14554.21 / tempK - 45.0575 * logTempK);
         kDissocH2CO3App = kDissocH2CO3 / 
            (activityCoeffH * activityCoeffHCO3); 
         
         # Ideal and apparent dissociation constant for HCO3 
         # (Millero 1979 - Geochimica et Cosmochimica Acta)
         kDissocHCO3 = exp(207.6548 - 11843.79 / tempK - 33.6485 * logTempK);
         kDissocHCO3App = kDissocHCO3 /
            (activityCoeffH * activityCoeffCO3 / activityCoeffHCO3); 
         
         # Ideal and apparent dissociation constant for freshwater 
         # (Millero 1995 - Geochimica et Cosmochimica Acta) 
         kDissocH2O = exp(-13847.26 / tempK + 148.9802 - 23.6521 * logTempK);
         kDissocH2OApp = kDissocH2O / 
            (activityCoeffH * activityCoeffOH);
          
         # Henry's constant for CO2 dissolution in freshwater
         # (Weiss 1974 - Marine Chemistry)  
         kHenryCO2 = exp(93.4517 * 100 / tempK - 60.2409 + 23.3585 * log(tempK / 100));

         return(callNextMethod(
            .Object,
            tempC = tempC,
            tempK = tempK,
            eConduct = eConduct,
            ionicStrength = ionicStrength,
            daviesParam = daviesParam,
            daviesExponent = daviesExponent,
            activityCoeffH = activityCoeffH,
            activityCoeffOH = activityCoeffOH,
            activityCoeffHCO3 = activityCoeffHCO3,
            activityCoeffCO3 = activityCoeffCO3,
            kDissocH2CO3App = kDissocH2CO3App,
            kDissocHCO3App = kDissocHCO3App,
            kDissocH2OApp = kDissocH2OApp,
            kHenryCO2 = kHenryCO2
            ));
      }
   );

# CarbonateEq.getTotalAlk method ####

#' Interface for a method to get the total alkalinity for a known
#' equilibrium scenario
#' 
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @return Total alkalinity in units determined by the equilibrium scenario
#' @exportMethod getTotalAlk

setGeneric(
   name = "getTotalAlk",
   def = function(carbonateEq, ...) { standardGeneric("getTotalAlk") }
   );

#' Method to get the total alkalinity for a known
#' equilibrium scenario based on a known dissolved inorganic carbon
#' concentration and pH
#' 
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @param concDIC Dissolved inorganic carbon concentration in molality
#' @param pH The pH of the solution (minus the log10 of H+ concentration in molality)
#' @return Total alkalinity in charge molality
#' @aliases getTotalAlk.CarbonateEq
#' @exportMethod getTotalAlk

setMethod(
   f = "getTotalAlk",
   signature = "CarbonateEq",
   definition = function(carbonateEq, concDIC, pH)
      {
         concH = 10 ^ -pH;
         concOH = carbonateEq@kDissocH2OApp / concH;
         concCO2 = concDIC * (concH ^ 2) / 
            ( concH ^ 2 + 
                 carbonateEq@kDissocH2CO3App * concH + 
                 carbonateEq@kDissocH2CO3App * carbonateEq@kDissocHCO3App );
         concHCO3 = concCO2 * carbonateEq@kDissocH2CO3App / concH;
         concCO3 = concHCO3 * carbonateEq@kDissocHCO3App / concH;
         return(concHCO3 + 2 * concCO3 + concOH - concH);
      }
   );

# CarbonateEq.optimizepH method ####

#' Interface for a method to get the pH for an
#' equilibrium scenario
#' 
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @return The pH based on an optimization of total alkalinity prediction
#' @exportMethod optimizepH

setGeneric(
   name = "optimizepH",
   def = function(carbonateEq, ...) { standardGeneric("optimizepH") }
   );

#' Method to perform the optimization necessary to get the pH for a known
#' equilibrium scenario based on known dissolved inorganic carbon
#' and total alkalinity
#' 
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
#' @exportMethod optimizepH

setMethod(
   f = "optimizepH",
   signature = "CarbonateEq",
   definition = function(
      carbonateEq, 
      concDIC, 
      totalAlk, 
      tolerance = 1e-5, 
      range = c(12, 2)
      )
      {
         optr <- optimize(
            f = function(pH) 
               { 
                  return ( (totalAlk - getTotalAlk(carbonateEq, concDIC, pH))^2 ); 
               },
            interval = range,
            tol = tolerance
            );
         return(optr$minimum);
      }
   );
      
# CarbonateEq.optimizepCO2 method ####

#' Interface for a method to get the pCO2 and pH for an
#' equilibrium scenario
#' 
#' @param carbonateEq \code{CarbonateEq} object defining equilibrium scenario
#' @return The pCO2 calculated from an optimization of pH 
#'    based on an optimization of total alkalinity prediction
#' @exportMethod optimizepCO2

setGeneric(
   name = "optimizepCO2",
   def = function(carbonateEq, ...) { standardGeneric("optimizepCO2") }
   );

#' Method to calculate the pCO2 derived from 
#' the optimization necessary to get the pH for a known
#' equilibrium scenario based on known dissolved inorganic carbon
#' and total alkalinity
#' 
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
#' @exportMethod optimizepCO2

setMethod(
   f = "optimizepCO2",
   signature = "CarbonateEq",
   definition = function(
      carbonateEq, 
      concDIC, 
      totalAlk, 
      tolerance = 1e-5, 
      range = c(12, 2)
      )
      {
         pH <- optimizepH(
            carbonateEq = carbonateEq, 
            concDIC = concDIC, 
            totalAlk = totalAlk,
            tolerance = tolerance,
            range = range
            );
         concH <- 10 ^ -pH;
         concCO2 <- concDIC * concH ^ 2 * 10 ^ (6 * carbonateEq@daviesExponent) / 
            ( concH ^ 2 * 10 ^ (6 * carbonateEq@daviesExponent) + 
                carbonateEq@kDissocH2CO3App * concH * 10 ^ (4 * carbonateEq@daviesExponent) + 
                carbonateEq@kDissocH2CO3App * carbonateEq@kDissocHCO3App );
         fCO2 <- 1e6 * (concCO2 / carbonateEq@kHenryCO2);
         return( list(pH = pH, fCO2 = fCO2) );
      }
   );

# Implementation notes:
# 
#    Use Henry's coefficient function from existing code?  Current code is 
#    reproducing a calculation that is similar to exisiting libraries.  Check
#    into normalizing to avoid the cut-and-paste antipattern.
