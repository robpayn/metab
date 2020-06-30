# Package dependencies ####

#' @importFrom R6 R6Class
#' @importFrom stats optimize
NULL


# Function kHenryCO2fromTemp ####

#' @export
#' 
#' @title
#'    Calculate Henry's constant for carbon dioxide
#' 
#' @description
#'    Calculates Henry's constant for carbon dioxide dissolution in 
#'    water based on the provided temperature. 
#'    (Weiss 1974 - Marine Chemistry)
#'    
#' @param tempK 
#'    Temperature in Kelvin
#'    
#' @return 
#'    Henry's constant for CO2 in moles per liter per atmosphere
#'    
kHenryCO2fromTemp <- function(tempK)
{
   return(exp(
      -60.2409 + 
         93.4517 * (100 / tempK)  + 
         23.3585 * log(tempK / 100)
   ));
}


# Class CarbonateEQ (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining carbonate equilibrium
#' 
#' @description 
#'   Defines a set of equilibrium
#'   conditions for inorganic carbon at a given temperature 
#'   in freshwater.
#' 
CarbonateEq <- R6Class(
   classname = "CarbonateEq",
   public = list(
      
      #' @field tempC
      #'   Temperature in deg Celsius
      tempC = NULL,
      
      #' @field tempK
      #'   Temperature in Kelvin
      tempK = NULL,
      
      #' @field eConduct
      #'   Electrical conductivity
      eConduct = NULL,
      
      #' @field ionicStrength
      #'   Ionic strength
      ionicStrength = NULL,
      
      #' @field daviesParam
      #'   Parameter of Davies equation
      daviesParam = NULL,
      
      #' @field daviesExponent
      #'   Exponent of Davies equation
      daviesExponent = NULL,
      
      #' @field activityCoeffH
      #'   Activity coefficient for hydrogen ions
      activityCoeffH = NULL,
      
      #' @field activityCoeffOH
      #'   Activity coefficient for hydroxide ions
      activityCoeffOH = NULL,
      
      #' @field activityCoeffHCO3
      #'   Activity coefficient for bicarbonate ions
      activityCoeffHCO3 = NULL,
      
      #' @field activityCoeffCO3
      #'   Activity coefficient for carbonate ions
      activityCoeffCO3 = NULL,
      
      #' @field kDissocH2CO3App
      #'   Apparent equilibrium constant for carbonic acid disassociation
      kDissocH2CO3App = NULL,
      
      #' @field kDissocHCO3App
      #'   Apparent equilibrium constant for bicarbonate disassociation
      kDissocHCO3App = NULL,
      
      #' @field kDissocH2OApp
      #'   Apparent equilibrium constant for water disassociation
      kDissocH2OApp = NULL,
      
      #' @field kHenryCO2
      #'   Henry's constant
      kHenryCO2 = NULL,
      
      #' @field kHenryCO2fromTempFunc
      #'   Function used to calculate Henry's constant
      kHenryCO2fromTempFunc = NULL,

      # Method CarbonatEq$new ####
      #
      #' @description 
      #'   Construct an instance of the class
      #' 
      #' @param tempC
      #'   Water temperature in degrees Celsius
      #' @param kHenryCO2fromTempFunc
      #'   Optional function to use for calculating Henry's constant.
      #'   Defaults to kHenryCO2fromTemp provided with the package.
      #' @param ... 
      #'   Additional arguments will be 
      #'   passed generically to the method \code{resetFromTemp()} 
      #'   when it is called to initialize the object. 
      #'   See documentation for \code{resetFromTemp()}
      #'   for description of arguments.
      #'   
      initialize = function
      (
         tempC, 
         kHenryCO2fromTempFunc = kHenryCO2fromTemp, 
         ...
      )
      {
         self$kHenryCO2fromTempFunc <- kHenryCO2fromTempFunc;
         self$resetFromTemp(tempC, ...);
      },
      
      # Method CarbonateEq$resetFromTemp ####
      #
      #' @description
      #'    Resets equilibrium constants in attributes of the object
      #'    based on the provided temperature
      #'
      #' @param tempC 
      #'    Water temperature in degrees Celsius
      #' @param tempK
      #'    Temperature in Kelvin. Defaults to tempC argument plus 273.15
      #' @param eConduct 
      #'    Electrical conductivity in milliSiemens per cm, default is set to zero
      #'    which will result in the ideal case of solute activities equal to 
      #'    their concentrations
      #' @param ionicStrength 
      #'    Ionic strength in moles per liter, default is set as a linear correlate
      #'    of electrical conductivity as suggested by 
      #'    Griffin and Jurinak (1973 - Soil Science)
      #' @param daviesParam 
      #'    Parameter for Davies equation derivation, defaults to
      #'    \code{0.5092 + (tempC - 25) * 0.00085}
      #' @param daviesExponent 
      #'    Factor for exponent in Davies equation, defaults to
      #'    \code{(ionicStrength ^ 0.5 / (1 + ionicStrength ^ 0.5) - 0.3 * ionicStrength)}
      #'    
      #' @return 
      #'    A named vector with 2 values: the previous temperature and the new temperature
      #'    
      resetFromTemp = function
      (
         tempC,
         tempK = tempC + 273.15,
         eConduct = 0,
         ionicStrength = 0.013 * eConduct,
         daviesParam = 0.5092 + (tempC - 25) * 0.00085,
         daviesExponent = -daviesParam *
            ( ionicStrength ^ 0.5 / (1 + ionicStrength ^ 0.5) -
                 0.3 * ionicStrength )
      )
      {
         prevTemp <- self$tempC;
         
         self$tempC <- tempC;
         self$tempK <- tempK;
         self$eConduct <- eConduct;
         self$ionicStrength <- ionicStrength;
         self$daviesParam <- daviesParam;
         self$daviesExponent <- daviesExponent;
         
         # Activitity coefficients from Davies equation
         # based on square of ionic charge
         self$activityCoeffH <- 10 ^ self$daviesExponent; # Charge +1
         self$activityCoeffOH <- self$activityCoeffH; # Charge -1
         self$activityCoeffHCO3 <- self$activityCoeffH; # Charge -1
         self$activityCoeffCO3 <- 10 ^ (4 * self$daviesExponent); # Charge -2
         
         logTempK <- log(self$tempK);
         
         # Ideal and apparent dissociation constant for H2CO3
         # (Millero 1979 - Geochimica et Cosmochimica Acta)
         kDissocH2CO3 <- exp(290.9097 - 14554.21 / self$tempK - 45.0575 * logTempK);
         self$kDissocH2CO3App <- kDissocH2CO3 /
            (self$activityCoeffH * self$activityCoeffHCO3);
         
         # Ideal and apparent dissociation constant for HCO3
         # (Millero 1979 - Geochimica et Cosmochimica Acta)
         kDissocHCO3 <- exp(207.6548 - 11843.79 / self$tempK - 33.6485 * logTempK);
         self$kDissocHCO3App <- kDissocHCO3 /
            (self$activityCoeffH * self$activityCoeffCO3 / self$activityCoeffHCO3);
         
         # Ideal and apparent dissociation constant for freshwater
         # (Millero 1995 - Geochimica et Cosmochimica Acta)
         kDissocH2O <- exp(-13847.26 / self$tempK + 148.9802 - 23.6521 * logTempK);
         self$kDissocH2OApp <- kDissocH2O /
            (self$activityCoeffH * self$activityCoeffOH);
         
         self$kHenryCO2 <- self$kHenryCO2fromTempFunc(tempK = self$tempK);
         
         return(c(Previous_Temp = prevTemp, New_Temp = self$tempC));
      },
      
      # Method CarbonateEq$calcTotalAlkFromDICpH ####
      #
      #' @description 
      #'   Calculates the total alkalinity based on a provided DIC concentration
      #'   and pH. This is a simplified alkalinity based only on carbonate species.
      #'
      #' @param concDIC 
      #'    Dissolved inorganic carbon concentration in molality
      #' @param pH 
      #'    The pH of the solution (minus the log10 of H+ concentration in molality)
      #'    
      #' @return 
      #'    Total alkalinity in charge molality
      #'    
      calcTotalAlkFromDICpH = function(concDIC, pH)
      {
         concH = 10 ^ -pH;
         concOH = self$kDissocH2OApp / concH;
         concCO2 = concDIC * (concH ^ 2) /
            ( concH ^ 2 +
                 self$kDissocH2CO3App * concH +
                 self$kDissocH2CO3App * self$kDissocHCO3App );
         concHCO3 = concCO2 * self$kDissocH2CO3App / concH;
         concCO3 = concHCO3 * self$kDissocHCO3App / concH;
         return(concHCO3 + 2 * concCO3 + concOH - concH);
      },
      
      # Method CarbonateEq$calcTotalAlkFromConcCO2pH ####
      #
      #' @description 
      #'   Calculates the total alkalinity based on a provided carbon dioxide concentration
      #'   and pH. This is a simplified alkalinity based only on carbonate species.
      #'
      #' @param concCO2 
      #'   Concentration of carbon dioxide in molality
      #' @param pH 
      #'   The pH of the solution (minus the log10 of H+ concentration in molality)
      #'    
      #' @return 
      #'   Total alkalinity in charge molality
      #'    
      calcTotalAlkFromConcCO2pH = function(concCO2, pH)
      {
         concH <- 10 ^ -pH;
         concOH <- self$kDissocH2OApp / concH;
         concHCO3 <- concCO2 * self$kDissocH2CO3App / concH;
         concCO3 <- concHCO3 * self$kDissocHCO3App / concH;
         return(concHCO3 + 2 * concCO3 + concOH - concH);
      },

      # Method CarbonateEq$calcDICFromConcCO2pH ####
      #
      #' @description 
      #'   Calculates dissolved inorganic carbon based on a provided carbon dioxide concentration
      #'   and pH.
      #'
      #' @param concCO2 
      #'   Concentration of carbon dioxide in molality
      #' @param pH 
      #'   The pH of the solution (minus the log10 of H+ concentration in molality)
      #'    
      #' @return 
      #'   Dissolved inorganic carbon concentration in molality of C
      #'    
      calcDICFromConcCO2pH = function(concCO2, pH)
      {
         concH <- 10 ^ -pH;
         return(
            concCO2 * concH ^ -2 * 10 ^ -(6 * self$daviesExponent) *
               ( concH ^ 2 * 10 ^ (6 * self$daviesExponent) +
                    self$kDissocH2CO3App * concH * 10 ^ (4 * self$daviesExponent) +
                    self$kDissocH2CO3App * self$kDissocHCO3App )
         );
      },

      # Method CarbonateEq$optpHFromDICTotalAlk ####
      #
      #' @description
      #'    Uses a single parameter optimization to infer the pH from a given
      #'    DIC concentration and total alkalinity. The pH is inferred by 
      #'    changing its value until the resulting calculated alkalinity
      #'    matches the provided alkalinity.
      #'
      #' @param concDIC 
      #'    Dissolved inorganic carbon concentration in molality
      #' @param totalAlk 
      #'    The total alkalinity in charge molality
      #' @param tolerance 
      #'    The tolerance used for convergence in the optimization algorithm.
      #'    Default value is 1e-5.
      #' @param range 
      #'    Range in pH values to constrain the optimization.
      #'    Default range is 2 < pH < 12.
      #'    
      #' @return 
      #'    The inferred pH.
      #'    
      optpHFromDICTotalAlk = function
      (
         concDIC,
         totalAlk,
         tolerance = 1e-5,
         range = c(12, 2)
      )
      {
         optr <- optimize(
            f = function(pH)
            {
               return ( (totalAlk - self$calcTotalAlkFromDICpH(concDIC, pH))^2 );
            },
            interval = range,
            tol = tolerance
         );
         return(optr$minimum);
      },

      # Method CarbonateEq$optpHFromConcCO2TotalAlk ####
      #
      #' @description
      #'    Uses a single parameter optimization to infer the pH from a given
      #'    carbon dioxide concentration and total alkalinity. The pH is inferred by 
      #'    changing its value until the resulting calculated alkalinity
      #'    matches the provided alkalinity.
      #'
      #' @param concCO2 
      #'    Dissolved carbon dioxide concentration in molality
      #' @param totalAlk 
      #'    The total alkalinity in charge molality
      #' @param tolerance 
      #'    The tolerance used for convergence in the optimization algorithm.
      #'    Default value is 1e-5.
      #' @param range 
      #'    Range in pH values to constrain the optimization.
      #'    Default range is 2 < pH < 12.
      #'    
      #' @return 
      #'    The inferred pH.
      #'    
      optpHFromConcCO2TotalAlk = function
      (
         concCO2,
         totalAlk,
         tolerance = 1e-5,
         range = c(12, 2)
      )
      {
         optr <- optimize(
            f = function(pH)
            {
               return ( (totalAlk - self$calcTotalAlkFromConcCO2pH(concCO2, pH))^2 );
            },
            interval = range,
            tol = tolerance
         );
         return(optr$minimum);
      },

      # Method CarbonateEq$optpHFromConcCO2DIC ####
      #
      #' @description
      #'    Uses a single parameter optimization to infer the pH from provided
      #'    carbon dioxide and dissolved inorganic carbon concentrations . 
      #'    The pH is inferred by 
      #'    changing its value until the resulting calculated dissolved inorganic
      #'    carbon matches the provided value.
      #'
      #' @param concCO2 
      #'    Dissolved carbon dioxide concentration in molality
      #' @param concDIC 
      #'    The dissolved inorganic carbon in molality of C
      #' @param tolerance 
      #'    The tolerance used for convergence in the optimization algorithm.
      #'    Default value is 1e-5.
      #' @param range 
      #'    Range in pH values to constrain the optimization.
      #'    Default range is 2 < pH < 12.
      #'    
      #' @return 
      #'    The inferred pH.
      #'    
      optpHFromConcCO2DIC = function
      (
         concCO2,
         concDIC,
         tolerance = 1e-5,
         range = c(12, 2)
      )
      {
         optr <- optimize(
            f = function(pH)
            {
               return ( (concDIC - self$calcDICFromConcCO2pH(concCO2, pH))^2 );
            },
            interval = range,
            tol = tolerance
         );
         return(optr$minimum);
      },

      # Method CarbonateEq$optfCO2FromDICTotalAlk ####
      #
      #' @description
      #'    Uses a pH optimization to infer the fugacity of carbon dioxide based
      #'    on the provided DIC concentration and total alkalinity
      #'
      #' @param concDIC 
      #'    Dissolved inorganic carbon concentration in molality
      #' @param totalAlk 
      #'    The measured or otherwise known total alkalinity in charge molality
      #' @param tolerance 
      #'    The tolerance used for convergence in the optimization algorithm,
      #'    default value is 1e-5
      #' @param range 
      #'    Range in pH values to constrain the optimization,
      #'    default range is 2 < pH < 12
      #'    
      #' @return 
      #'    A named list of the inferred pCO2 and pH.
      #'    Value of pCO2 is in units of microatmospheres.
      #'    
      optfCO2FromDICTotalAlk = function
      (
         concDIC,
         totalAlk,
         tolerance = 1e-5,
         range = c(12, 2)
      )
      {
         pH <- self$optpHFromDICTotalAlk(
            concDIC = concDIC,
            totalAlk = totalAlk,
            tolerance = tolerance,
            range = range
         );
         concH <- 10 ^ -pH;
         concCO2 <- concDIC * concH ^ 2 * 10 ^ (6 * self$daviesExponent) /
            ( concH ^ 2 * 10 ^ (6 * self$daviesExponent) +
                 self$kDissocH2CO3App * concH * 10 ^ (4 * self$daviesExponent) +
                 self$kDissocH2CO3App * self$kDissocHCO3App );
         fCO2 <- 1e6 * (concCO2 / self$kHenryCO2);
         return( list(pH = pH, fCO2 = fCO2) );
      },

      # Method CarbonateEq$optDICFromfCO2TotalAlk ####
      #
      #' @description
      #'    Uses a pH optimization to infer the dissolved inorganic carbon based
      #'    on the provided fugacity of carbon dioxide and total alkalinity
      #'
      #' @param fCO2 
      #'    Fugacity of carbon dioxide in microatmospheres
      #' @param totalAlk 
      #'    The measured or otherwise known total alkalinity in charge molality
      #' @param tolerance 
      #'    The tolerance used for convergence in the optimization algorithm,
      #'    default value is 1e-5
      #' @param range 
      #'    Range in pH values to constrain the optimization,
      #'    default range is 2 < pH < 12
      #'    
      #' @return 
      #'    A named list of the inferred DIC concentration in molality and pH.
      #'    
      optDICFromfCO2TotalAlk = function 
      (
         fCO2,
         totalAlk,
         tolerance = 1e-5,
         range = c(12, 2)
      )
      {
         concCO2 <- fCO2 * 1e-6 * self$kHenryCO2;
         pH <- self$optpHFromConcCO2TotalAlk(
            concCO2 = concCO2,
            totalAlk = totalAlk,
            tolerance = tolerance,
            range = range
         );
         return(list(
            pH = pH, 
            concDIC = self$calcDICFromConcCO2pH(concCO2 = concCO2, pH = pH)
         ));
      },

      # Method CarbonateEq$optTotalAlkFromfCO2DIC ####
      #
      #' @description
      #'    Uses a pH optimization to infer the total alkalinity based
      #'    on the provided fugacity of carbon dioxide and dissolved
      #'    inorganic carbon. Total alkalinity is assumed to be dominated
      #'    by carbonate species.
      #'
      #' @param fCO2 
      #'    Fugacity of carbon dioxide in microatmospheres
      #' @param concDIC 
      #'    The dissolved inorganic carbon in molality of C
      #' @param tolerance 
      #'    The tolerance used for convergence in the optimization algorithm,
      #'    default value is 1e-5
      #' @param range 
      #'    Range in pH values to constrain the optimization,
      #'    default range is 2 < pH < 12
      #'    
      #' @return 
      #'    A named list of the inferred alkalinity in charge molality and pH.
      #'    
      optTotalAlkFromfCO2DIC = function 
      (
         fCO2,
         concDIC,
         tolerance = 1e-5,
         range = c(12, 2)
      )
      {
         concCO2 <- fCO2 * 1e-6 * self$kHenryCO2;
         pH <- self$optpHFromConcCO2DIC(
            concCO2 = concCO2,
            concDIC = concDIC,
            tolerance = tolerance,
            range = range
         );
         return(list(
            pH = pH, 
            totalAlk = self$calcTotalAlkFromConcCO2pH(concCO2 = concCO2, pH = pH)
         ));
      }

   )
)
