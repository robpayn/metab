# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
#' @importFrom disco SignalPlotter
NULL

# Class OneStationMetabPlotter (R6) ####

#' @export
#' 
#' @title 
#'   Plots a signal of DO and pCO2
#'   
OneStationMetabPlotter <- R6Class(
   classname = "OneStationMetabPlotter",
   inherit = SignalPlotter,
   public = list(
      timeTicks = NULL,
      resultFile = NULL,
      results = NULL,
      initialize = function
         (
            ..., 
            timeTicks, 
            resultFile = "results"
         )
         {
            super$initialize(...);
            self$timeTicks <- timeTicks;
            self$resultFile <- resultFile;
         }
      )
);

# Method OneStationMetabPlotter$plot ####

OneStationMetabPlotter$set(
   which = "public",
   name = "plot",
   value = function
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
                  "No signal provided for the OneStationMetabPlotter$plot method."
               ));
            }
         }
         
         # Set the output path if provided
         if(!is.null(outputPath)) {
            self$outputPath <- outputPath;
         }
         # Results are not plotted if output path is not provided
         plotResults <- !is.null(self$outputPath);

         # Set the format scheme for plots
         ld <- rbind.data.frame(
            doObs = list(
               name = "DO Obs",
               lty = NA,
               pch = 1,
               col = "black",
               lwd = 1
               ),
            pCO2Obs = list(
               name = "pCO2 Obs",
               lty = NA,
               pch = 1,
               col = "green3",
               lwd = 1
            ),
            doPred = list(
               name = "DO Pred",
               lty = "solid",
               pch = NA,
               col = "black",
               lwd = 2
               ),
            pCO2Pred = list(
               name = "pCO2 Pred",
               lty = "solid",
               pch = NA,
               col = "green3",
               lwd = 2
            ),
            doSat = list(
               name = "DO Sat",
               lty = "dotted",
               pch = NA,
               col = "black",
               lwd = 2
               ),
            par = list(
               name = "PAR",
               lty = "dashed",
               pch = NA,
               col = "red",
               lwd = 2
               ),
            temp = list(
               name = "Temperature",
               lty = NA,
               pch = 1,
               col = "black",
               lwd = 1
               ),
            stringsAsFactors = FALSE
         );
         
         # Plot DO
         
         # Determine y axis scale for DO
         ylim <- c(
            min(self$signal$dataFrame$data$do),
            max(self$signal$dataFrame$data$do)
         );
         # Load results if plotted and alter y axis scale
         # accordingly for results
         if (plotResults) {
            load(file = sprintf(
               fmt = "%s/%s.RData",
               self$outputPath,
               self$resultFile
            ));
            self$results <- results;
            ylim <- c(
               min(
                  ylim[1], 
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat
               ),
               max(
                  ylim[2], 
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat
               )
            );
         }
         # Plot the DO observations
         self$signal$plot(
            variableName = "do",
            xaxt = "n",
            ylim = ylim,
            pch = ld["doObs", "pch"],
            col = ld["doObs", "col"],
            lwd = ld["doObs", "lwd"]
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
               lty = ld["doSat", "lty"],
               col = ld["doSat", "col"],
               lwd = ld["doSat", "lwd"]
            );
            lines(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$output$do,
               lty = ld["doPred", "lty"],
               col = ld["doPred", "col"],
               lwd = ld["doPred", "lwd"]
            );
            mtext(
               text = sprintf(
                  fmt = "GPP = %1.2e",
                  results$objFunc$params[1]
               ),
               side = 3,
               line = 0.5,
               cex = 0.6,
               adj = 1
            );
         }
         
         # Plot pCO2
         
         # Reset the axes scaling for pCO2 plot
         par(new = TRUE);
         # Plot the pCO2 observations
         self$signal$plot(
            variableName = "pCO2",
            xaxt = "n",
            yaxt = "n",
            ylab = "",
            pch = ld["pCO2Obs", "pch"],
            col = ld["pCO2Obs", "col"],
            lwd = ld["pCO2Obs", "lwd"]
         );
         axis(
            side = 4
         );
         mtext(
            text = sprintf(
               "%s (%s)",
               "pCO2",
               self$signal$dataFrame$metaColumns["pCO2",]$units
            ),
            side = 4,
            line = 2.5,
            cex = 0.7,
            col = ld["pCO2Obs", "col"]
         );
         if (plotResults) {
            if (!is.null(self$results$objFunc$model$output$pCO2)) {
               lines(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$output$pCO2,
                  lty = ld["pCO2Pred", "lty"],
                  col = ld["pCO2Pred", "col"],
                  lwd = ld["pCO2Pred", "lwd"]
               );
            }
         }

         # Reset the axes scaling for PAR reference
         par(new = TRUE);
         self$signal$plot(
            variableName = "par",
            type = "l",
            lty = ld["par", "lty"],
            col = ld["par", "col"],
            lwd = ld["par", "lwd"],
            yaxt = "n",
            ylab = "",
            xaxt = "n",
            xlab = ""
         );
         
         # Plot temperature on a new plot
         self$signal$plot(
            variableName = "temp",
            pch = ld["temp", "pch"],
            col = ld["temp", "col"],
            lwd = ld["temp", "lwd"],
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
               text = sprintf(
                  fmt = "ER = %1.2e",
                  results$objFunc$params[2]
               ),
               side = 3,
               line = 0.5,
               cex = 0.6,
               adj = 0
            );
            mtext(
               text = sprintf(
                  fmt = "k600 = %1.2e",
                  results$objFunc$params[3]
               ),
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
            lty = ld["par", "lty"],
            col = ld["par", "col"],
            lwd = ld["par", "lwd"],
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
            col = ld["par", "col"]
         );
      }
);
