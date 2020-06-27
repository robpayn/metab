# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
#' @title 
#'   R6 class for extracting results from one station metabolism models
#' 
#' @description 
#'   Extracts parameters used from metabolism models
#'   
OneStationMetabExtractor <- R6Class(
   classname = "OneStationMetabExtractor",
   inherit = disco::SignalSummarizer,
   public = list(
      
      #' @field table
      #'   Table of summary data
      table = NULL,
      
      #' @field index
      #'   Index for tracking the row in the table
      index = NULL,
      
      #' @field resultFile
      #'   The name of the file with analysis results
      resultFile = NULL,
      
      #' @field summaryFile
      #'   The name of the file to create with the summary
      summaryFile = NULL,
      
      #' @field path
      #'   Table of summary data
      path = NULL,
      
      # Method OneStationMetabExtractor$new ####
      #
      #' @description 
      #'   Constructs a new instance of the class
      #'   
      #' @param rows
      #'   number of rows in the table of extracted parameter values
      #' @param resultFile
      #'   name of the results files to read
      #' @param summaryFile
      #'   name of the summary file to create
      #'   
      initialize = function
      (
         rows,
         resultFile = "results",
         summaryFile = "summary"
      )
      {
         self$table <- data.frame(
            GPP = numeric(length = rows),
            ER = numeric(length = rows),
            k600 = numeric(length = rows)
         );
         self$index <- 1;
         self$resultFile <- resultFile;
         self$summaryFile <- summaryFile;
      },
      
      # Method OneStationMetabExtractor$open ####
      #
      #' @description 
      #'   Prepare to write the summary file
      #'   
      #' @param path
      #'   Path where the summary file will be written
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      open = function(path) 
      {
         self$path = path;
      },
      
      # Method OneStationMetabExtractor$summarize ####
      #
      #' @description 
      #'   Collects the data for the summary
      #'   
      #' @param signal
      #'   The signal being summarized
      #' @param outputPath
      #'   The path where analysis output is located
      #' @param label
      #'   A label for the summary
      #' @param timeBounds
      #'   The temporal bounds on the summary
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      summarize = function
      (
         signal,
         outputPath,
         label,
         timeBounds
      )
      {
         results <- readRDS(file = sprintf(
            fmt = "%s/%s.RData",
            outputPath,
            self$resultFile
         ));
         self$table$GPP[self$index] <- results$objFunc$model$dailyGPP;
         self$table$ER[self$index] <- results$objFunc$model$dailyER;
         self$table$k600[self$index] <- results$objFunc$model$k600;
         self$index <- self$index + 1;
      },
      
      # Method OneStationMetabExtractor$close ####
      #
      #' @description 
      #'   Closes the summary output
      #'   
      #' @return 
      #'   No defined return value.
      #'   
      close = function() 
      {
         table <- self$table;
         saveRDS(
            table, 
            file = sprintf(
               fmt = "%s/%s.RData",
               self$path,
               self$summaryFile
            )
         );
      }

   )
)
