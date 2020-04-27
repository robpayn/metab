# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
#' @title 
#'   R6 class defining a plotter for a two station metabolism model
#'   
#' @description 
#'   Plots a signal of DO and pCO2 from a two station model of metabolism
#'   
TwoStationMetabPlotter <- R6Class(
   classname = "TwoStationMetabPlotter",
   inherit = disco::TransferFunctionPlotter,
   public = list(
      #' @field timeTicks
      #'   Number of ticks on the time axis
      timeTicks = NULL,
      
      #' @field plotDelta
      #'   Logical value indicating if the change in concentration
      #'   should be plotted instead of the upstream/downstream
      #'   concentrations
      plotDelta = NULL,
      
      #' @field resultFile
      #'   Base file name for the results.
      resultFile = NULL,
      
      #' @field results
      #'   The results object to be plotted
      results = NULL,
      
      #' @field subTitle
      #'   Subtitle for headings on plots, will appear over right
      #'   panel on legend.
      subTitle = NULL,
      
      #' @field format
      #'   Data frame aggregating the formats for all the plots
      format = NULL,
      
      # Method TwoStationMetabPlotter$new ####
      #
      #' @description 
      #'   Construct a new instance of the class.
      #' 
      #' @param ...
      #'   Arguments passed to the constructor of the super class
      #' @param timeTicks
      #'   Number of ticks on the time axis
      #' @param plotDelta
      #'   Logical value indicating if the change in concentration
      #'   should be plotted instead of the upstream/downstream
      #'   concentrations
      #' @param resultFile
      #'   Base file name for the results.
      #'   Defaults to "results".
      #' @param subTitle
      #'   Subtitle for headings on plots, will appear over right
      #'   panel on legend.
      #'   Defaults to an empty string (blank).
      #' @param format.doObsUp
      #'   Format for upstream DO observations.
      #'   See defaults in usage.
      #' @param format.doObsDown
      #'   Format for downstream DO observations.
      #'   See defaults in usage.
      #' @param format.doPred
      #'   Format for downstream DO predictions
      #'   See defaults in usage.
      #' @param format.parUp
      #'   Format for upstream PAR
      #'   See defaults in usage.
      #' @param format.parDown
      #'   Format for downstream PAR
      #'   See defaults in usage.
      #' @param format.pCO2ObsUp
      #'   Format for upstream pCO2 observations.
      #'   See defaults in usage.
      #' @param format.pCO2ObsDown
      #'   Format for downstream pCO2 observations.
      #'   See defaults in usage.
      #' @param format.pCO2Pred
      #'   Format for pCO2 predictions
      #'   See defaults in usage.
      #' @param format.tempUp
      #'   Format for upstream temperature
      #'   See defaults in usage.
      #' @param format.tempDown
      #'   Format for downstream temperature
      #'   See defaults in usage.
      #' @param format.doSatDown
      #'   Format for downstream saturation DO concentrations
      #'   See defaults in usage.
      initialize = function
      (
         ..., 
         timeTicks,
         plotDelta = FALSE,
         resultFile = "results",
         subTitle = "",
         format.doObsUp = list(
            name = "[DO] observed upstream",
            lty = NA,
            pch = 46,
            col = "black",
            lwd = 1
         ),
         format.doObsDown = list(
            name = "[DO] observed downstream",
            lty = NA,
            pch = 176,
            col = "black",
            lwd = 1
         ),
         format.doPred = list(
            name = "[DO] modeled downstream",
            lty = "solid",
            pch = NA,
            col = "black",
            lwd = 2
         ),
         format.parUp = list(
            name = "PAR upstream",
            lty = NA,
            pch = 46,
            col = "red",
            lwd = 1
         ),
         format.parDown = list(
            name = "PAR downstream",
            lty = NA,
            pch = 176,
            col = "red",
            lwd = 1
         ),
         format.pCO2ObsUp = list(
            name = "pCO2 observed upstream",
            lty = NA,
            pch = 46,
            col = "green3",
            lwd = 1
         ),
         format.pCO2ObsDown = list(
            name = "pCO2 observed downstream",
            lty = NA,
            pch = 176,
            col = "green3",
            lwd = 1
         ),
         format.pCO2Pred = list(
            name = "pCO2 modeled downstream",
            lty = "solid",
            pch = NA,
            col = "green3",
            lwd = 2
         ),
         format.tempUp = list(
            name = "Temperature upstream",
            lty = NA,
            pch = 46,
            col = "black",
            lwd = 1
         ),
         format.tempDown = list(
            name = "Temperature downstream",
            lty = NA,
            pch = 176,
            col = "black",
            lwd = 1
         ),
         format.doSatDown = list(
            name = "[DO] saturated downstream",
            lty = "dotted",
            pch = NA,
            col = "black",
            lwd = 1
         )
      )
      {
         super$initialize(...);
         self$timeTicks <- timeTicks;
         self$plotDelta <- plotDelta;
         self$resultFile <- resultFile;
         self$subTitle <- subTitle;
         
         self$format <- rbind.data.frame(
            doObsUp = format.doObsUp,
            doObsDown = format.doObsDown,
            doPred = format.doPred,
            parUp = format.parUp,
            parDown = format.parDown,
            pCO2ObsUp = format.pCO2ObsUp,
            pCO2ObsDown = format.pCO2ObsDown,
            pCO2Pred = format.pCO2Pred,
            tempUp = format.tempUp,
            tempDown = format.tempDown,
            doSatDown = format.doSatDown,
            stringsAsFactors = FALSE
         );
      },
      
      # Method TwoStationMetabPlotter$open ####
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
      open = function(path)
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
            y = 0.9,
            labels = "Left panel legend:",
            font = 2
         );

         if(self$plotDelta) {
            legend(
               x = "center",
               bty = 'n',
               legend = sapply(
                  c(
                     bquote(Delta*DO~observed), 
                     bquote(Delta*DO~modeled),
                     bquote(.(self$format["parUp", "name"])),
                     bquote(.(self$format["parDown", "name"]))
                  ), 
                  as.expression
               ),
               lty = c(
                  self$format["doObsDown", "lty"], 
                  self$format["doPred", "lty"], 
                  self$format["parUp", "lty"],
                  self$format["parDown", "lty"]
               ),
               lwd = c(
                  self$format["doObsDown", "lwd"], 
                  self$format["doPred", "lwd"], 
                  self$format["parUp", "lwd"],
                  self$format["parDown", "lwd"]
               ),
               pch = c(
                  self$format["doObsDown", "pch"], 
                  self$format["doPred", "pch"], 
                  self$format["parUp", "pch"],
                  self$format["parDown", "pch"]
               ),
               col = c(
                  self$format["doObsDown", "col"], 
                  self$format["doPred", "col"], 
                  self$format["parUp", "col"],
                  self$format["parDown", "col"]
               )
            );
         } else {
            legend(
               x = "center",
               bty = 'n',
               legend = c(
                  self$format["doObsUp", "name"],
                  self$format["doObsDown", "name"], 
                  self$format["doSatDown", "name"], 
                  self$format["doPred", "name"], 
                  self$format["parUp", "name"],
                  self$format["parDown", "name"]
               ),
               lty = c(
                  self$format["doObsUp", "lty"],
                  self$format["doObsDown", "lty"], 
                  self$format["doSatDown", "lty"], 
                  self$format["doPred", "lty"], 
                  self$format["parUp", "lty"],
                  self$format["parDown", "lty"]
               ),
               lwd = c(
                  self$format["doObsUp", "lwd"],
                  self$format["doObsDown", "lwd"], 
                  self$format["doSatDown", "lwd"], 
                  self$format["doPred", "lwd"], 
                  self$format["parUp", "lwd"],
                  self$format["parDown", "lwd"]
               ),
               pch = c(
                  self$format["doObsUp", "pch"],
                  self$format["doObsDown", "pch"], 
                  self$format["doSatDown", "pch"], 
                  self$format["doPred", "pch"], 
                  self$format["parUp", "pch"],
                  self$format["parDown", "pch"]
               ),
               col = c(
                  self$format["doObsUp", "col"],
                  self$format["doObsDown", "col"], 
                  self$format["doSatDown", "col"], 
                  self$format["doPred", "col"], 
                  self$format["parUp", "col"],
                  self$format["parDown", "col"]
               )
            );
         }
         
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
         
         if(self$plotDelta) {
            legend(
               x = "center", 
               bty = 'n',
               legend = sapply(
                  c(
                     bquote(Delta*pCO[2]~observed), 
                     bquote(Delta*pCO[2]~modeled),
                     bquote(.(self$format["tempUp", "name"])),
                     bquote(.(self$format["tempDown", "name"]))
                  ),
                  as.expression
               ),
               lty = c(
                  self$format["pCO2ObsDown", "lty"], 
                  self$format["pCO2Pred", "lty"], 
                  self$format["tempUp", "lty"],
                  self$format["tempDown", "lty"]
               ),
               lwd = c(
                  self$format["pCO2ObsDown", "lwd"], 
                  self$format["pCO2Pred", "lwd"], 
                  self$format["tempUp", "lwd"],
                  self$format["tempDown", "lwd"]
               ),
               pch = c(
                  self$format["pCO2ObsDown", "pch"], 
                  self$format["pCO2Pred", "pch"], 
                  self$format["tempUp", "pch"],
                  self$format["tempDown", "pch"]
               ),
               col = c(
                  self$format["pCO2ObsDown", "col"], 
                  self$format["pCO2Pred", "col"], 
                  self$format["tempUp", "col"],
                  self$format["tempDown", "col"]
               )
            );
         } else {
            legend(
               x = "center", 
               bty = 'n',
               legend = c(
                  self$format["pCO2ObsUp", "name"],
                  self$format["pCO2ObsDown", "name"], 
                  self$format["pCO2Pred", "name"], 
                  self$format["tempUp", "name"],
                  self$format["tempDown", "name"]
               ),
               lty = c(
                  self$format["pCO2ObsUp", "lty"],
                  self$format["pCO2ObsDown", "lty"], 
                  self$format["pCO2Pred", "lty"], 
                  self$format["tempUp", "lty"],
                  self$format["tempDown", "lty"]
               ),
               lwd = c(
                  self$format["pCO2ObsUp", "lwd"],
                  self$format["pCO2ObsDown", "lwd"], 
                  self$format["pCO2Pred", "lwd"], 
                  self$format["tempUp", "lwd"],
                  self$format["tempDown", "lwd"]
               ),
               pch = c(
                  self$format["pCO2ObsUp", "pch"],
                  self$format["pCO2ObsDown", "pch"], 
                  self$format["pCO2Pred", "pch"], 
                  self$format["tempUp", "pch"],
                  self$format["tempDown", "pch"]
               ),
               col = c(
                  self$format["pCO2ObsUp", "col"],
                  self$format["pCO2ObsDown", "col"], 
                  self$format["pCO2Pred", "col"], 
                  self$format["tempUp", "col"],
                  self$format["tempDown", "col"]
               )
            );
         }
      },
      
      # Method TwoStationMetabPlotter$summarize ####
      #
      #' @description 
      #'   Summarize the data in a two panel visualization
      #'   
      #' @param signalIn
      #'   The input signal object that contains upstream observations
      #' @param signalOut
      #'   The output signal object that contains downstream observations
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
         
         if(self$plotDelta) {
            ymin <- min(doOut - doIn, na.rm = TRUE);
            ymax <- max(doOut - doIn, na.rm = TRUE);
         } else {
            ymin <- min(doIn, doOut, na.rm = TRUE);
            ymax <- max(doIn, doOut, na.rm = TRUE);
         }
         
         # Load results if plotted and alter y axis scale
         # accordingly for results
         if (plotResults) {
            self$results <- readRDS(file = sprintf(
               fmt = "%s/%s.RData",
               self$outputPath,
               self$resultFile
            ));
            if(self$plotDelta) {
               ymin <- min(
                  ymin, 
                  self$results$objFunc$model$output$do - doIn,
                  na.rm = TRUE
               );
               ymax <- max(
                  ymax, 
                  self$results$objFunc$model$output$do - doIn,
                  na.rm = TRUE
               );
            } else {
               ymin <- min(
                  ymin, 
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat,
                  na.rm = TRUE
               );
               ymax <- max(
                  ymax, 
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat,
                  na.rm = TRUE
               );
            }
         }
         
         if(self$plotDelta) {
            plot(
               x = signalOut$time,
               y = doOut - doIn,
               pch = self$format["doObsDown", "pch"],
               lwd = self$format["doObsDown", "lwd"],
               col = self$format["doObsDown", "col"],
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = timeBounds,
               ylim = c(ymin, ymax)
            );
            mtext(
               text = bquote(Delta*.("DO (")*mu*mol~L^-1*.(")")),
               side = 2,
               line = 2,
               cex = 0.7,
               col = self$format["doObsDown", "col"]
            );
         } else {
            signalOut$plot(
               variableName = "do",
               pch = self$format["doObsDown", "pch"],
               lwd = self$format["doObsDown", "lwd"],
               col = self$format["doObsDown", "col"],
               xaxt = "n",
               xlab = "",
               ylab = "",
               xlim = timeBounds,
               ylim = c(ymin, ymax)
            );
            points(
               x = signalOut$time,
               y = doIn,
               pch = self$format["doObsUp", "pch"],
               lwd = self$format["doObsUp", "lwd"],
               col = self$format["doObsUp", "col"]
            );
            mtext(
               text = bquote(.("[DO] (")*mu*mol~L^-1*.(")")),
               side = 2,
               line = 2,
               cex = 0.7,
               col = self$format["doObsDown", "col"]
            );
         }
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
            text = label,
            cex = 0.7,
            adj = 0
         );
         
         # Plot the best fit DO model if results are plotted
         if (plotResults) {
            if(self$plotDelta) {
               lines(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$output$do - doIn,
                  lty = self$format["doPred", "lty"],
                  lwd = self$format["doPred", "lwd"],
                  col = self$format["doPred", "col"]
               );
            } else {
               lines(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$downstreamDOSat,
                  lty = self$format["doSatDown", "lty"],
                  lwd = self$format["doSatDown", "lwd"],
                  col = self$format["doSatDown", "col"]
               );
               lines(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$output$do,
                  lty = self$format["doPred", "lty"],
                  lwd = self$format["doPred", "lwd"],
                  col = self$format["doPred", "col"]
               );
            }
            mtext(
               text = bquote(paste(
                  .(sprintf(
                     fmt = "GPP = %1.2e",
                     self$results$objFunc$model$dailyGPP
                  )),
                  ~mu*mol~L^-1~d^-1
               )),
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
            pch = self$format["parDown", "pch"],
            lwd = self$format["parDown", "lwd"],
            col = self$format["parDown", "col"],
            xaxt = "n",
            xlab = "",
            xlim = timeBounds,
            yaxt = "n",
            ylab = "",
            ylim = c(ymax, ymin)
         );
         axis(
            side = 4
         );
         mtext(
            side = 4,
            line = 2.5,
            cex = 0.7,
            text = "PAR (relative intensity)",
            col = self$format["parDown", "col"]
         );
         points(
            x = signalOut$time,
            y = parIn,
            pch = self$format["parUp", "pch"],
            lwd = self$format["parUp", "lwd"],
            col = self$format["parUp", "col"]
         );
         
         # Plot pCO2
         
         pCO2In <- signalIn$getVariable("pCO2");
         pCO2Out <- signalOut$getVariable("pCO2");
         
         if(self$plotDelta) {
            ymin <- min(pCO2Out - pCO2In, na.rm = TRUE);
            ymax <- max(pCO2Out - pCO2In, na.rm = TRUE);
            signalOut$plot(
               x = signalOut$time,
               y = pCO2Out - pCO2In,
               pch = self$format["pCO2ObsDown", "pch"],
               lwd = self$format["pCO2ObsDown", "lwd"],
               col = self$format["pCO2ObsDown", "col"],
               xaxt = "n",
               xlab = "",
               xlim = timeBounds,
               ylim = c(ymin, ymax),
               ylab = ""
            );
            mtext(
               text = bquote(Delta*pCO[2]~.("(")*mu*atm*.(")")),
               side = 2,
               line = 2.5,
               cex = 0.7,
               col = self$format["pCO2ObsDown", "col"]
            );
         } else {
            ymin <- min(pCO2In, pCO2Out, na.rm = TRUE);
            ymax <- max(pCO2In, pCO2Out, na.rm = TRUE);
            signalOut$plot(
               variableName = "pCO2",
               pch = self$format["pCO2ObsDown", "pch"],
               lwd = self$format["pCO2ObsDown", "lwd"],
               col = self$format["pCO2ObsDown", "col"],
               xaxt = "n",
               xlab = "",
               xlim = timeBounds,
               ylim = c(ymin, ymax),
               ylab = ""
            );
            points(
               x = signalOut$time,
               y = pCO2In,
               pch = self$format["pCO2ObsUp", "pch"],
               lwd = self$format["pCO2ObsUp", "lwd"],
               col = self$format["pCO2ObsUp", "col"]
            );
            mtext(
               text = bquote(pCO[2]~.("(")*mu*atm*.(")")),
               side = 2,
               line = 2.5,
               cex = 0.7,
               col = self$format["pCO2ObsDown", "col"]
            );
         }
         
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
            if (!is.null(self$results$objFunc$model$output$pCO2)) {
               if(self$plotDelta) {
                  lines(
                     x = self$results$objFunc$model$output$time,
                     y = self$results$objFunc$model$output$pCO2 - pCO2In,
                     lty = self$format["pCO2Pred", "lty"],
                     lwd = self$format["pCO2Pred", "lwd"],
                     col = self$format["pCO2Pred", "col"]
                  );
               } else {
                  lines(
                     x = self$results$objFunc$model$output$time,
                     y = self$results$objFunc$model$output$pCO2,
                     lty = self$format["pCO2Pred", "lty"],
                     lwd = self$format["pCO2Pred", "lwd"],
                     col = self$format["pCO2Pred", "col"]
                  );
               }
            }
            mtext(
               text = bquote(paste(
                  .(sprintf(
                     fmt = "ER = %1.2e",
                     self$results$objFunc$model$dailyER
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
                     self$results$objFunc$model$k600
                  )),
                  ~d^-1
               )),
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
            pch = self$format["tempDown", "pch"],
            lwd = self$format["tempDown", "lwd"],
            col = self$format["tempDown", "col"],
            xaxt = "n",
            xlab = "",
            xlim = timeBounds,
            yaxt = "n",
            ylab = "",
            ylim = c(ymin, ymax)
         );
         axis(
            side = 4
         );
         mtext(
            side = 4,
            line = 2.5,
            cex = 0.7,
            text = "Temperature (degrees C)",
            col = self$format["tempDown", "col"]
         );
         points(
            x = signalOut$time,
            y = tempIn,
            pch = self$format["tempUp", "pch"],
            lwd = self$format["tempUp", "lwd"],
            col = self$format["tempUp", "col"]
         );
      }
   )
)
