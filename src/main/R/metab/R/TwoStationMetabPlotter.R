# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
TwoStationMetabPlotter <- R6Class(
   classname = "TwoStationMetabPlotter",
   inherit = disco::TransferFunctionPlotter,
   public = list(
      timeTicks = NULL,
      resultFile = NULL,
      results = NULL,
      subTitle = NULL,
      initialize = function
         (
            ..., 
            timeTicks, 
            resultFile = "results",
            subTitle = ""
         )
         {
            super$initialize(...);
            self$timeTicks <- timeTicks;
            self$resultFile <- resultFile;
            self$subTitle <- subTitle;
         }
      )
);


# Method TwoStationMetabPlotter$open ####

TwoStationMetabPlotter$set(
   which = "public",
   name = "open",
   value = function
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
            main = "Two Station Metabolism Analysis Summary"
         );
         text(
            x = 0.5,
            y = 0.8,
            labels = "Left panel legend:",
            font = 2
            );
         legend(
            x = "center",
            bty = 'n',
            legend = c(
               '[DO] upstream observed', 
               '[DO] downstream observed', 
               '[DO] downstream modeled (if available)', 
               'PAR upstream',
               'PAR downstream'
               ),
            lty = c(
               NA,
               NA,
               'solid',
               NA,
               NA
               ),
            lwd = 2,
            pch = c(
               46,
               176,
               NA,
               46,
               176
               ),
            col = c(
               'black',
               'black',
               'black',
               'red',
               'red'
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
            y = 0.8,
            labels = "Right panel legend:",
            font = 2
            )
         legend(
            x = "center", 
            bty = 'n',
            legend = c(
               'pCO2 upstream observed',
               'pCO2 downstream observed',
               'pCO2 downstream modeled (if available)', 
               'Temperature upstream',
               'Temperature downstream'
               ),
            lty = c(
               NA,
               NA,
               'solid',
               NA,
               NA
               ),
            lwd = 2,
            pch = c(
               46,
               176,
               NA,
               46,
               176
               ),
            col = c(
               'black',
               'black',
               'black',
               'red',
               'red'
               )
         );
      }
);


# Method OneStationMetabPlotter$summarize ####

TwoStationMetabPlotter$set(
   which = "public",
   name = "summarize",
   value = function
      (
         signalIn = NULL,
         signalOut = NULL,
         outputPath = NULL,
         label, 
         timeBounds
      ) 
      {
         # Set the output path if provided
         if(!is.null(outputPath)) {
            self$outputPath <- outputPath;
         }
         # Results are not plotted if output path is not provided
         plotResults <- !is.null(self$outputPath);
         
         # Plot DO
         
         doIn <- signalIn$getVariable("do");
         doOut <- signalOut$getVariable("do");
         ymin <- min(doIn, doOut, na.rm = TRUE);
         ymax <- max(doIn, doOut, na.rm = TRUE);
         
         # Load results if plotted and alter y axis scale
         # accordingly for results
         if (plotResults) {
            load(file = sprintf(
               fmt = "%s/%s.RData",
               self$outputPath,
               self$resultFile
            ));
            self$results <- results;
            ymin <- min(
               ymin, 
               self$results$objFunc$model$output$do,
               self$results$objFunc$model$output$doSat
            );
            ymax <- max(
               ymax, 
               self$results$objFunc$model$output$do,
               self$results$objFunc$model$output$doSat
            );
         }
         
         signalOut$plot(
            variableName = "do",
            pch = 176,
            xaxt = "n",
            xlab = "",
            xlim = timeBounds,
            ylim = c(ymin, ymax)
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
         points(
            x = signalOut$time,
            y = doIn,
            pch = 46
         );
         mtext(
            side = 3,
            line = 0.5,
            text = label,
            cex = 0.7,
            adj = 0
         );
         
         # Plot the best fit DO model if results are plotted
         if (plotResults) {
            lines(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$output$doSat,
               lty = "dotted",
               lwd = 2
            );
            lines(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$output$do,
               lwd = 2
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
         
         # Plot PAR
         
         par(new = TRUE);
         parIn <- signalIn$getVariable("par");
         parOut <- signalOut$getVariable("par");
         ymin <- min(parIn, parOut, na.rm = TRUE);
         ymax <- max(parIn, parOut, na.rm = TRUE);
         plot(
            x = signalOut$time,
            y = parOut,
            pch = 176,
            xaxt = "n",
            xlab = "",
            xlim = timeBounds,
            yaxt = "n",
            ylab = "",
            ylim = c(ymax, ymin),
            col = "red"
         );
         axis(
            side = 4
         );
         mtext(
            side = 4,
            line = 2.5,
            cex = 0.7,
            text = "PAR (relative intensity)",
            col = "red"
         );
         points(
            x = signalOut$time,
            y = parIn,
            pch = 46,
            col = "red"
         );
         
         # Plot pCO2
         
         pCO2In <- signalIn$getVariable("pCO2");
         pCO2Out <- signalOut$getVariable("pCO2");
         ymin <- min(pCO2In, pCO2Out, na.rm = TRUE);
         ymax <- max(pCO2In, pCO2Out, na.rm = TRUE);
         signalOut$plot(
            variableName = "pCO2",
            pch = 176,
            xaxt = "n",
            xlab = "",
            xlim = timeBounds,
            ylim = c(ymin, ymax)
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
         points(
            x = signalOut$time,
            y = pCO2In,
            pch = 46
         );
         if (plotResults) {
            if (!is.null(self$results$objFunc$model$output$pCO2)) {
               lines(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$output$pCO2,
                  lwd = 2
               );
            }
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
         
         # Plot temperature
         
         par(new = TRUE);
         tempIn <- signalIn$getVariable("temp");
         tempOut <- signalOut$getVariable("temp");
         ymin <- min(tempIn, tempOut, na.rm = TRUE);
         ymax <- max(tempIn, tempOut, na.rm = TRUE);
         plot(
            x = signalOut$time,
            y = tempOut,
            pch = 176,
            xaxt = "n",
            xlab = "",
            xlim = timeBounds,
            yaxt = "n",
            ylab = "",
            ylim = c(ymin, ymax),
            col = "red"
         );
         axis(
            side = 4
         );
         mtext(
            side = 4,
            line = 2.5,
            cex = 0.7,
            text = "Temperature (degrees C)",
            col = "red"
         );
         points(
            x = signalOut$time,
            y = tempIn,
            pch = 46,
            col = "red"
         );

      }
);
