# Package dependencies ####

# R oxygen code for importing the proper classes used in this file
# Used for robust namespace management in R packages
#' @importFrom R6 R6Class
NULL

#' @export
#' 
#' @title 
#'   Extracts parameters from metabolism models
#'   
OneStationMetabExtractor <- R6Class(
   classname = "OneStationMetabExtractor",
   inherit = SignalSummarizer,
   public = list(
      table = NULL,
      index = NULL,
      resultFile = NULL,
      summaryFile = NULL,
      path = NULL,
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
         }
      )
);

OneStationMetabExtractor$set(
   which = "public",
   name = "open",
   value = function(path) 
      {
         self$path = path;
      }
);

OneStationMetabExtractor$set(
   which = "public",
   name = "summarize",
   value = function
      (
         signal,
         outputPath,
         label,
         timeBounds
      )
      {
         load(file = sprintf(
            fmt = "%s/%s.RData",
            outputPath,
            self$resultFile
         ));
         self$table$GPP[self$index] <- results$objFunc$model$dailyGPP;
         self$table$ER[self$index] <- results$objFunc$model$dailyER;
         self$table$k600[self$index] <- results$objFunc$model$k600;
         self$index <- self$index + 1;
      }
);

OneStationMetabExtractor$set(
   which = "public",
   name = "close",
   value = function () 
      {
         table <- self$table;
         save(
            table, 
            file = sprintf(
               fmt = "%s/%s.RData",
               self$path,
               self$summaryFile
               )
      );
   }
);
