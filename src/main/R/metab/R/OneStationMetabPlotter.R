# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

# Class OneStationMetabPlotter (R6) ####

#' @export
#' 
#' @title 
#'   R6 class defining a plotter for a one station metabolism DO model
#' 
#' @description 
#'   Plots a signal of DO and pCO2 from a one station model of metabolism
#'   
OneStationMetabPlotter <- R6Class(
   classname = "OneStationMetabPlotter",
   inherit = disco::SignalPlotter,
   public = list(
      
      #' @field timeTicks
      #'   Number of ticks on the time axis
      timeTicks = NULL,
      
      #' @field resultFile
      #'   Base file name for the results.
      resultFile = NULL,
      
      #' @field results
      #'   The results object to be plotted
      results = NULL,
      
      #' @field plotDIC
      #'   Flag to determine if DIC should be plotted rather
      #'   than pCO2.
      plotDIC = NULL,
      
      #' @field subTitle
      #'   Subtitle for headings on plots, will appear over right
      #'   panel on legend.
      subTitle = NULL,
      
      #' @field format
      #'   Data frame aggregating the formats for all the plots
      format = NULL,
      
      # Method OneStationMetabPlotter$new ####
      #
      #' @description 
      #'   Construct a new instance of the class.
      #' 
      #' @param ...
      #'   Arguments passed to the constructor of the super class
      #' @param timeTicks
      #'   Number of ticks on the time axis
      #' @param resultFile
      #'   Base file name for the results.
      #'   Defaults to "results".
      #' @param plotDIC
      #'   Flag to determine if DIC should be plotted rather
      #'   than pCO2.
      #'   Defaults to FALSE.
      #' @param subTitle
      #'   Subtitle for headings on plots, will appear over right
      #'   panel on legend.
      #'   Defaults to an empty string (blank).
      #' @param format.doObs
      #'   Format for DO observations.
      #'   See defaults in usage.
      #' @param format.pCO2Obs
      #'   Format for pCO2 observations.
      #'   See defaults in usage.
      #' @param format.dicObs
      #'   Format for DIC observations.
      #'   See defaults in usage.
      #' @param format.doPred
      #'   Format for DO predictions
      #'   See defaults in usage.
      #' @param format.pCO2Pred
      #'   Format for pCO2 predictions
      #'   See defaults in usage.
      #' @param format.dicPred
      #'   Format for DIC predictions
      #'   See defaults in usage.
      #' @param format.doSat
      #'   Format for saturation DO concentrations
      #'   See defaults in usage.
      #' @param format.par
      #'   Format for PAR
      #'   See defaults in usage.
      #' @param format.temp
      #'   Format for temperature
      #'   See defaults in usage.
      #'   
      initialize = function
      (
         ..., 
         timeTicks, 
         resultFile = "results",
         plotDIC = FALSE,
         subTitle = "",
         format.doObs = list(
            name = "[DO] observed",
            lty = NA,
            pch = 1,
            col = "black",
            lwd = 1
         ),
         format.pCO2Obs = list(
            name = "pCO2 observed",
            lty = NA,
            pch = 1,
            col = "green3",
            lwd = 1
         ),
         format.dicObs = list(
            name = "DIC Obs",
            lty = NA,
            pch = 1,
            col = "green3",
            lwd = 1
         ),
         format.doPred = list(
            name = "[DO] modeled",
            lty = "solid",
            pch = NA,
            col = "black",
            lwd = 2
         ),
         format.pCO2Pred = list(
            name = "pCO2 modeled",
            lty = "solid",
            pch = NA,
            col = "green3",
            lwd = 2
         ),
         format.dicPred = list(
            name = "DIC Pred",
            lty = "solid",
            pch = NA,
            col = "green3",
            lwd = 2
         ),
         format.doSat = list(
            name = "Saturated [DO]",
            lty = "dotted",
            pch = NA,
            col = "black",
            lwd = 2
         ),
         format.par = list(
            name = "PAR",
            lty = "dashed",
            pch = NA,
            col = "red",
            lwd = 2
         ),
         format.temp = list(
            name = "Temperature",
            lty = NA,
            pch = 1,
            col = "black",
            lwd = 1
         )
      )
      {
         # Call the constructor of the super class
         
         super$initialize(...);
         
         # Populate attributes
         
         self$timeTicks <- timeTicks;
         self$resultFile <- resultFile;
         self$plotDIC <- plotDIC;
         self$subTitle <- subTitle;
         
         # Set the format scheme for plots
         self$format <- rbind.data.frame(
            doObs = format.doObs,
            pCO2Obs = format.pCO2Obs,
            dicObs = format.dicObs,
            doPred = format.doPred,
            pCO2Pred = format.pCO2Pred,
            dicPred = format.dicPred,
            doSat = format.doSat,
            par = format.par,
            temp = format.temp,
            stringsAsFactors = FALSE
         );
      },
      
      # Method OneStationMetabPlotter$open ####
      #
      #' @description 
      #'   Opens the pdf for writing the plots
      #'   
      #' @param path
      #'   The path where the pdf of figures should be written
      #' 
      #' @return 
      #'   No defined return value
      #'   
      open = function
      (
         path
      )
      {
         super$open(path);
         
         plot(
            x = NULL,
            xaxt = 'n',
            xlim = 0:1, 
            xlab = '', 
            yaxt = 'n',
            ylim = 0:1,
            ylab = '',
            bty = 'n',
            main = "One Station Metabolism Analysis Summary"
         );
         text(
            x = 0.5,
            y = 0.9,
            labels = "Left panel legend:",
            font = 2
         );
         legend(
            x = "center",
            bty = 'n',
            legend = c(
               self$format["doObs", "name"], 
               self$format["doPred", "name"], 
               self$format["pCO2Obs", "name"],
               self$format["pCO2Pred", "name"], 
               self$format["doSat", "name"],
               self$format["par", "name"]
            ),
            lty = c(
               self$format["doObs", "lty"], 
               self$format["doPred", "lty"], 
               self$format["pCO2Obs", "lty"],
               self$format["pCO2Pred", "lty"], 
               self$format["doSat", "lty"],
               self$format["par", "lty"]
            ),
            lwd = c(
               self$format["doObs", "lwd"], 
               self$format["doPred", "lwd"], 
               self$format["pCO2Obs", "lwd"],
               self$format["pCO2Pred", "lwd"], 
               self$format["doSat", "lwd"],
               self$format["par", "lwd"]
            ),
            pch = c(
               self$format["doObs", "pch"], 
               self$format["doPred", "pch"], 
               self$format["pCO2Obs", "pch"],
               self$format["pCO2Pred", "pch"], 
               self$format["doSat", "pch"],
               self$format["par", "pch"]
            ),
            col = c(
               self$format["doObs", "col"], 
               self$format["doPred", "col"], 
               self$format["pCO2Obs", "col"],
               self$format["pCO2Pred", "col"], 
               self$format["doSat", "col"],
               self$format["par", "col"]
            )
         );
         
         plot(
            x = NULL,
            xaxt = 'n',
            xlim = 0:1, 
            xlab = '', 
            yaxt = 'n',
            ylim = 0:1,
            ylab = '',
            bty = 'n',
            main = self$subTitle
         );
         text(
            x = 0.5,
            y = 0.7,
            labels = "Right panel legend:",
            font = 2
         );
         legend(
            x = "center", 
            bty = 'n',
            legend = c(
               self$format["temp", "name"],
               self$format["par", "name"]
            ),
            lty = c(
               self$format["temp", "lty"],
               self$format["par", "lty"]
            ),
            lwd = c(
               self$format["temp", "lwd"],
               self$format["par", "lwd"]
            ),
            pch = c(
               self$format["temp", "pch"],
               self$format["par", "pch"]
            ),
            col = c(
               self$format["temp", "col"],
               self$format["par", "col"]
            )
         );
      },
      
      # Method OneStationMetabPlotter$summarize ####
      #
      #' @description 
      #'   Summarize the data in a two panel visualization
      #'   
      #' @param signal
      #'   The signal object that contains observations
      #' @param outputPath
      #'   A path to results of a metabolism analysis.
      #'   Not necessary if summarizer does not need output.
      #' @param label
      #'   A label for the summary
      #' @param timeBounds  
      #'   The temporal bounds on the summary
      #'   
      #' @return 
      #'   No defined return value
      #'   
      summarize = function
      (
         signal = NULL,
         outputPath = NULL,
         label, 
         timeBounds
      ) 
      {
         # Set the signal if it is provided
         if (!is.null(signal)) {
            self$signal <- signal;
         } else {
            if (is.null(self$signal)) {
               stop(paste(
                  "No signal provided for the OneStationMetabPlotter$summarize method."
               ));
            }
         }
         
         # Set the output path if provided
         if(!is.null(outputPath)) {
            self$outputPath <- outputPath;
         }
         # Results are not plotted if output path is not provided
         plotResults <- !is.null(self$outputPath);
         
         # Plot DO
         
         # Determine y axis scale for DO
         ylim <- c(
            min(self$signal$getVariable("do"), na.rm = TRUE),
            max(self$signal$getVariable("do"), na.rm = TRUE)
         );
         # Load results if plotted and alter y axis scale
         # accordingly for results
         if (plotResults) {
            self$results <- readRDS(file = sprintf(
               fmt = "%s/%s.RData",
               self$outputPath,
               self$resultFile
            ));
            ylim <- c(
               min(
                  ylim[1], 
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat,
                  na.rm = TRUE
               ),
               max(
                  ylim[2], 
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat,
                  na.rm = TRUE
               )
            );
         }
         # Plot the DO observations
         self$signal$plot(
            variableName = "do",
            xaxt = "n",
            ylim = ylim,
            ylab = "",
            pch = self$format["doObs", "pch"],
            col = self$format["doObs", "col"],
            lwd = self$format["doObs", "lwd"]
         );
         mtext(
            text = bquote(.("[DO] (")*mu*mol~L^-1*.(")")),
            side = 2,
            line = 2,
            cex = 0.7,
            col = self$format["doObs", "col"]
         );
         axis.POSIXct(
            side = 1,
            at = seq(
               timeBounds[1], 
               timeBounds[2], 
               length.out = self$timeTicks
            ),
            format = "%H:%M"
         );
         mtext(
            side = 3,
            line = 0.5,
            adj = 0,
            text = label,
            cex = 0.7
         );
         
         # Plot the best fit DO model if results are plotted
         if (plotResults) {
            lines(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$output$doSat,
               lty = self$format["doSat", "lty"],
               col = self$format["doSat", "col"],
               lwd = self$format["doSat", "lwd"]
            );
            lines(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$output$do,
               lty = self$format["doPred", "lty"],
               col = self$format["doPred", "col"],
               lwd = self$format["doPred", "lwd"]
            );
            mtext(
               text = bquote(paste(
                  .(sprintf(
                     fmt = "GPP = %1.2e",
                     self$results$objFunc$params[1]
                  )),
                  ~mu*mol~L^-1~d^-1
               )),
               side = 3,
               line = 0.5,
               cex = 0.6,
               adj = 1
            );
         }
         
         # Reset the axes scaling for inorganic carbon
         par(new = TRUE);
         
         if (!self$plotDIC) {
            
            # Determine y axis scale for pCO2
            ylim <- c(
               min(self$signal$getVariable("pCO2"), na.rm = TRUE),
               max(self$signal$getVariable("pCO2"), na.rm = TRUE)
            );
            
            # Alter y axis scale
            # accordingly for results, if they are plotted
            if (plotResults) {
               ylim <- c(
                  min(
                     ylim[1], 
                     self$results$objFunc$model$output$pCO2,
                     na.rm = TRUE
                  ),
                  max(
                     ylim[2], 
                     self$results$objFunc$model$output$pCO2,
                     na.rm = TRUE
                  )
               );
            }
            
            # Plot the pCO2 observations
            self$signal$plot(
               variableName = "pCO2",
               xaxt = "n",
               yaxt = "n",
               ylim = ylim,
               ylab = "",
               pch = self$format["pCO2Obs", "pch"],
               col = self$format["pCO2Obs", "col"],
               lwd = self$format["pCO2Obs", "lwd"]
            );
            axis(
               side = 4
            );
            mtext(
               text = bquote(pCO[2]~.("(")*mu*atm*.(")")),
               side = 4,
               line = 2.5,
               cex = 0.7,
               col = self$format["pCO2Obs", "col"]
            );
            if (plotResults) {
               if (!is.null(self$results$objFunc$model$output$pCO2)) {
                  lines(
                     x = self$results$objFunc$model$output$time,
                     y = self$results$objFunc$model$output$pCO2,
                     lty = self$format["pCO2Pred", "lty"],
                     col = self$format["pCO2Pred", "col"],
                     lwd = self$format["pCO2Pred", "lwd"]
                  );
               }
            }
            
         } else {
            
            # Determine y axis scale for DIC
            ylim <- c(
               min(self$signal$getVariable("dic"), na.rm = TRUE),
               max(self$signal$getVariable("dic"), na.rm = TRUE)
            );
            
            # Alter y axis scale
            # accordingly for results, if they are plotted
            if (plotResults) {
               ylim <- c(
                  min(
                     ylim[1], 
                     self$results$objFunc$model$output$dic,
                     na.rm = TRUE
                  ),
                  max(
                     ylim[2], 
                     self$results$objFunc$model$output$dic,
                     na.rm = TRUE
                  )
               );
            }
            
            # Plot the DIC observations
            self$signal$plot(
               variableName = "dic",
               xaxt = "n",
               ylim = ylim,
               yaxt = "n",
               ylab = "",
               pch = self$format["dicObs", "pch"],
               col = self$format["dicObs", "col"],
               lwd = self$format["dicObs", "lwd"]
            );
            axis(
               side = 4
            );
            mtext(
               text = sprintf(
                  "%s (%s)",
                  "dic",
                  self$signal$dataFrame$metaColumns["dic",]$units
               ),
               side = 4,
               line = 2.5,
               cex = 0.7,
               col = self$format["dicObs", "col"]
            );
            if (plotResults & !is.null(self$results$objFunc$model$output$pCO2)) {
               lines(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$output$dic,
                  lty = self$format["dicPred", "lty"],
                  col = self$format["dicPred", "col"],
                  lwd = self$format["dicPred", "lwd"]
               );
            }         
            
         }
         
         # Reset the axes scaling for PAR reference
         par(new = TRUE);
         self$signal$plot(
            variableName = "par",
            type = "l",
            lty = self$format["par", "lty"],
            col = self$format["par", "col"],
            lwd = self$format["par", "lwd"],
            yaxt = "n",
            ylab = "",
            xaxt = "n",
            xlab = ""
         );
         
         # Plot temperature on a new plot
         self$signal$plot(
            variableName = "temp",
            pch = self$format["temp", "pch"],
            col = self$format["temp", "col"],
            lwd = self$format["temp", "lwd"],
            xaxt = "n"
         );
         axis.POSIXct(
            side = 1,
            at = seq(
               timeBounds[1], 
               timeBounds[2], 
               length.out = self$timeTicks
            ),
            format = "%H:%M"
         );
         if (plotResults) {
            mtext(
               text = bquote(paste(
                  .(sprintf(
                     fmt = "ER = %1.2e",
                     self$results$objFunc$params[2]
                  )),
                  ~mu*mol~L^-1~d^-1
               )),
               side = 3,
               line = 0.5,
               cex = 0.6,
               adj = 0
            );
            mtext(
               text = bquote(paste(
                  k[600],
                  .(sprintf(
                     fmt = " = %1.2e",
                     self$results$objFunc$params[3]
                  )),
                  ~d^-1
               )),
               side = 3,
               line = 0.5,
               cex = 0.6,
               adj = 1
            );
         }
         
         # Reset axes scaling for PAR
         par(new = TRUE);
         # Plot PAR on the temperature plot
         self$signal$plot(
            variableName = "par",
            type = "l",
            lty = self$format["par", "lty"],
            col = self$format["par", "col"],
            lwd = self$format["par", "lwd"],
            yaxt = "n",
            ylab = "",
            xaxt = "n",
            xlab = ""
         );
         axis(
            side = 4
         );
         mtext(
            text = sprintf(
               "par (%s)",
               self$signal$dataFrame$metaColumns["par",]$units
            ),
            side = 4,
            line = 2.5,
            cex = 0.7,
            col = self$format["par", "col"]
         );
      }
   )
)
