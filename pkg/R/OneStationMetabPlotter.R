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
      
      #' @field doHeader
      #' Character string reperesenting the header for dissolved oxygen data
      doHeader = NULL,
      
      #' @field pCO2Header
      #' Character string reperesenting the header for dissolved carbon dioxide data.
      pCO2Header = NULL,
      
      #' @field dicHeader
      #' Character string reperesenting the header for dissolved inorganic carbon data.
      dicHeader = NULL,

      #' @field parHeader
      #' Character string reperesenting the header for PAR data.
      parHeader = NULL,
      
      #' @field tempHeader
      #' Character string reperesenting the header for temperature data.
      tempHeader = NULL,
      
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
      #' @param doHeader
      #'   Optional character string reperesenting the header for dissolved oxygen data
      #'   Defaults to "do".
      #' @param dicHeader
      #'   Optional character string reperesenting the header for dissolved inorganic carbon data.
      #'   Defaults to "dic".
      #' @param pCO2Header
      #'   Optional character string reperesenting the header for dissolved carbon dioxide data.
      #'   Defaults to "pCO2".
      #' @param parHeader
      #'   Optional character string reperesenting the header for PAR data.
      #'   Defaults to "par".
      #' @param tempHeader
      #'   Optional character string reperesenting the header for PAR data.
      #'   Defaults to "temp"
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
         doHeader = "do",
         pCO2Header = "pCO2",
         dicHeader = "dic",
         parHeader = "par",
         tempHeader = "temp",
         format.doObs = list(
            name = "[DO] observed",
            lty = NA,
            pch = 1,
            col = "black",
            lwd = 1
         ),
         format.pCO2Obs = list(
            name = "pCO2 or DIC observed",
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
            name = "pCO2 or DIC modeled",
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
         self$doHeader <- doHeader;
         self$pCO2Header <- pCO2Header;
         self$dicHeader <- dicHeader;
         self$parHeader <- parHeader;
         self$tempHeader <- tempHeader;
         
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
         } 
         # The signal is not plotted if the signal is not provided
         plotSignal <- !is.null(self$signal);
         
         # Set the output path if provided
         if(!is.null(outputPath)) {
            self$outputPath <- outputPath;
         }
         # Results are not plotted if output path is not provided
         plotResults <- !is.null(self$outputPath);
         
         # Load the results file if model results are to be plotted
         if(plotResults) {
            self$results <- readRDS(file = sprintf(
               fmt = "%s/%s.RData",
               self$outputPath,
               self$resultFile
            ));
         }
         
         if(!(plotSignal || plotResults)) {
            stop("Plotter needs at least one of signal or model results to plot.")
         }
         
         # Plot DO
         
         # Plot the DO observations
         if(plotSignal) {
            ylim <- c(
               min(self$signal$getVariable(self$doHeader), na.rm = TRUE),
               max(self$signal$getVariable(self$doHeader), na.rm = TRUE)
            );
            if(plotResults) {
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
            self$signal$plot(
               header = self$doHeader,
               xaxt = "n",
               ylim = ylim,
               ylab = "",
               pch = self$format["doObs", "pch"],
               col = self$format["doObs", "col"],
               lwd = self$format["doObs", "lwd"]
            );
            if(plotResults) {
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
         } else {
            ylim <- c(
               min(
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat,
                  na.rm = TRUE
               ),
               max(
                  self$results$objFunc$model$output$do,
                  self$results$objFunc$model$output$doSat,
                  na.rm = TRUE
               )
            );
            plot(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$output$doSat,
               type = "l",
               xaxt = "n",
               xlab = "time",
               ylim = ylim,
               ylab = "",
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
         
         # Reset the axes scaling for inorganic carbon
         par(new = TRUE);
         
         if (plotSignal) {
            plotDICSignal <- !is.null(self$signal$getVariable(self$pCO2Header));
         } else {
            plotDICSignal <- FALSE;
         }
         if (plotResults) {
            plotDICResults <- !is.null(self$results$objFunc$model$output$pCO2);
         } else {
            plotDICResults <- FALSE;
         }
         
         if (!self$plotDIC) {
            
            # Plot the pCO2 observations
            if (plotSignal && plotDICSignal) {
               ylim <- c(
                  min(self$signal$getVariable(self$pCO2Header), na.rm = TRUE),
                  max(self$signal$getVariable(self$pCO2Header), na.rm = TRUE)
               );
               if(plotResults && plotDICResults) {
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
               self$signal$plot(
                  header = self$pCO2Header,
                  xaxt = "n",
                  yaxt = "n",
                  ylim = ylim,
                  ylab = "",
                  pch = self$format["pCO2Obs", "pch"],
                  col = self$format["pCO2Obs", "col"],
                  lwd = self$format["pCO2Obs", "lwd"]
               );
               if (plotResults && plotDICResults) {
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
            } else if (plotDICResults) {
               ylim <- c(
                  min(
                     self$results$objFunc$model$output$pCO2,
                     na.rm = TRUE
                  ),
                  max(
                     self$results$objFunc$model$output$pCO2,
                     na.rm = TRUE
                  )
               );
               if (!is.null(self$results$objFunc$model$output$pCO2)) {
                  plot(
                     x = self$results$objFunc$model$output$time,
                     y = self$results$objFunc$model$output$pCO2,
                     type = "l",
                     xaxt = "n",
                     xlab = "",
                     yaxt = "n",
                     ylim = ylim,
                     ylab = "",
                     lty = self$format["pCO2Pred", "lty"],
                     col = self$format["pCO2Pred", "col"],
                     lwd = self$format["pCO2Pred", "lwd"]
                  );
               }
            }
            if(plotDICSignal || plotDICResults) {
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
            }

         } else {
            
            # Plot the DIC observations
            if(plotSignal && plotDICSignal) {
               ylim <- c(
                  min(self$signal$getVariable(self$dicHeader), na.rm = TRUE),
                  max(self$signal$getVariable(self$dicHeader), na.rm = TRUE)
               );
               if(plotResults && plotDICResults) {
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
               self$signal$plot(
                  header = self$dicHeader,
                  xaxt = "n",
                  xlab = "",
                  ylim = ylim,
                  yaxt = "n",
                  ylab = "",
                  pch = self$format["dicObs", "pch"],
                  col = self$format["dicObs", "col"],
                  lwd = self$format["dicObs", "lwd"]
               );
               if(plotResults && plotDICResults) {
                  lines(
                     x = self$results$objFunc$model$output$time,
                     y = self$results$objFunc$model$output$dic,
                     lty = self$format["dicPred", "lty"],
                     col = self$format["dicPred", "col"],
                     lwd = self$format["dicPred", "lwd"]
                  );
               }
            } else if (plotDICResults) {
               ylim <- c(
                  min(
                     self$results$objFunc$model$output$dic,
                     na.rm = TRUE
                  ),
                  max(
                     self$results$objFunc$model$output$dic,
                     na.rm = TRUE
                  )
               );
               plot(
                  x = self$results$objFunc$model$output$time,
                  y = self$results$objFunc$model$output$dic,
                  type = "l",
                  xaxt = "n",
                  xlab = "",
                  ylim = ylim,
                  yaxt = "n",
                  ylab = "",
                  lty = self$format["dicPred", "lty"],
                  col = self$format["dicPred", "col"],
                  lwd = self$format["dicPred", "lwd"]
               );
            }
            if(plotDICSignal || plotDICResults) {
               axis(
                  side = 4
               );
               mtext(
                  text = bquote(.("[DIC] (")*mu*mol~L^-1*.(")")),
                  side = 4,
                  line = 2.5,
                  cex = 0.7,
                  col = self$format["dicObs", "col"]
               );
            }

         }
         
         # Reset the axes scaling for PAR reference
         par(new = TRUE);
         
         if(plotSignal) {
            self$signal$plot(
               header = self$parHeader,
               type = "l",
               lty = self$format["par", "lty"],
               col = self$format["par", "col"],
               lwd = self$format["par", "lwd"],
               yaxt = "n",
               ylab = "",
               xaxt = "n",
               xlab = "",
               ylim = c(
                  max(self$signal$getVariable(self$parHeader)),
                  min(self$signal$getVariable(self$parHeader))
               )
            );
         } else {
            plot(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$par,
               type = "l",
               lty = self$format["par", "lty"],
               col = self$format["par", "col"],
               lwd = self$format["par", "lwd"],
               yaxt = "n",
               ylab = "",
               xaxt = "n",
               xlab = "",
               ylim = c(
                  max(self$results$objFunc$model$par),
                  min(self$results$objFunc$model$par)
               )
            );
         }
         
         # Plot temperature on a new plot
         if(plotSignal) {
            self$signal$plot(
               header = self$tempHeader,
               pch = self$format["temp", "pch"],
               col = self$format["temp", "col"],
               lwd = self$format["temp", "lwd"],
               xaxt = "n"
            );
         } else {
            plot(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$temp,
               ylab = "temp (deg C)",
               pch = self$format["temp", "pch"],
               col = self$format["temp", "col"],
               lwd = self$format["temp", "lwd"],
               xaxt = "n",
               xlab = "time"
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
         
         # Reset axes scaling for PAR
         par(new = TRUE);
         
         # Plot PAR on the temperature plot
         if(plotSignal) {
            self$signal$plot(
               header = self$parHeader,
               type = "l",
               lty = self$format["par", "lty"],
               col = self$format["par", "col"],
               lwd = self$format["par", "lwd"],
               yaxt = "n",
               ylab = "",
               xaxt = "n",
               xlab = "",
               ylim = c(
                  max(self$signal$getVariable(self$parHeader)),
                  min(self$signal$getVariable(self$parHeader))
               )
            );
         } else {
            plot(
               x = self$results$objFunc$model$output$time,
               y = self$results$objFunc$model$par,
               type = "l",
               lty = self$format["par", "lty"],
               col = self$format["par", "col"],
               lwd = self$format["par", "lwd"],
               yaxt = "n",
               ylab = "",
               xaxt = "n",
               xlab = "",
               ylim = c(
                  max(self$results$objFunc$model$par),
                  min(self$results$objFunc$model$par)
               )
            );
         }
         axis(
            side = 4
         );
         mtext(
            text = "PAR (relative units)",
            side = 4,
            line = 2.5,
            cex = 0.7,
            col = self$format["par", "col"]
         );
      }
   )
)
