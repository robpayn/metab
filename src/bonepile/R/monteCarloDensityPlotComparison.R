# This script creates a 9 panel plot of the density distribution from given data frames containing 
# multiple iterations of monte carlo analysis
#
# It loads csv files of 1000 iterations of monte carlo analysis for the coupled, pCO2, and DO
# models at high and low GPP scenarios 

# set size of plotting window and number of plotting pannels 
windows(width= 12, height = 8)
par(mfrow = c(3,3), mar = c(2.6, 4.1, 0.5, 0.5), lwd =2, oma = c(3,5,3,0))

#### plotting function ####
# This function plots the density distribution of GPP, ER, and k600 for high and low GPP scenarios
# High and low GPP scenarios are on the same plot pannel 
# It also also calculates the 95% confidence of each distributions and plots the line at each on of the 
# interval
#
# Arguments
#   monteCarloEnsemble - data frame of GPP, ER, and k600 values. Columns must be names dailyGPP, dailyER, and k600
#   monteCarloEnsemble2 - 2nd data frame of GPP, ER, and k600 values. Columns must be names dailyGPP, dailyER, and k600
#   color1 - color of plots pertaining to monteCarloEnsemble
#   color2 - color of plots pertaining to monteCarloEnsemble2
#   title - title of each plot
#   ER.xlim - range of x axis on ER plot
#             - if NA, xlim is calculated from min and max values in data frame
#   k600.xlim - range of x axis on k600 plot
#               - if NA, xlim is calculated from min and max values in data frame
# Value
# Returns 3 density distribution plots - GPP, ER, and k600. Each plot contains distributions of high and low GPP scenarios
# as well as those distribution's 95% conidence interval lines
monteCarloDistributionDensity <- function(monteCarloEnsemble, monteCarloEnsemble2, color1 = "black",
                                          color2 = "blue", title, ER.xlim = NA, k600.xlim = NA, ...)
{
  xlim <- c(min(monteCarloEnsemble$dailyGPP, monteCarloEnsemble2$dailyGPP),
            max(monteCarloEnsemble$dailyGPP, monteCarloEnsemble2$dailyGPP));
  density1 <- density(monteCarloEnsemble$dailyGPP)
  density2 <- density(monteCarloEnsemble2$dailyGPP)
  
  ylim <- c(min(density1$y, density2$y), max(density1$y, density2$y))
  
  plot(density1, xlim = xlim, ylim = ylim, xlab = "", ylab = "",
       main = title, col = color1);
  legend(x = 3, y = max(density1$y, density2$y), legend = c( "GPP = 10", "GPP = 2", "95% Confidence Interval","95% Confidence Interval"),
         col = c(color1, color2, color1, color2), lty = c(1,1,2,2), bty = "n", cex = 1.2)
  lines(density2, xlim = xlim, ylim = ylim, col = color2)
  
 if (is.na(ER.xlim[1])){
  xlim <- c(min(monteCarloEnsemble$dailyER, monteCarloEnsemble2$dailyER),
            max(monteCarloEnsemble$dailyER, monteCarloEnsemble2$dailyER))
  }
  else {xlim <- ER.xlim};
  density1 <- density(monteCarloEnsemble$dailyER)
  density2 <- density(monteCarloEnsemble2$dailyER)
  ylim <- c(min(density1$y, density2$y), max(density1$y, density2$y))
  confidence1 <- quantile(x = monteCarloEnsemble$dailyER, probs = c(0.025, 0.975))
  confidence2 <- quantile(x = monteCarloEnsemble2$dailyER, probs = c(0.025, 0.975))
  
  
  plot(density1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", main = title, col = color1)
  lines(density2, xlim = xlim, ylim = ylim, col = color2)
  abline(v = c(confidence1, confidence2),
         col = c(color1, color1, color2, color2), lty = 2, lwd = .05 )
  
  if (is.na(k600.xlim[1])){
  xlim <- c(min(monteCarloEnsemble$k600, monteCarloEnsemble2$k600),
            max(monteCarloEnsemble$k600, monteCarloEnsemble2$k600))
  }
  else {xlim <- k600.xlim};
  density1 <- density(monteCarloEnsemble$k600)
  density2 <- density(monteCarloEnsemble2$k600)
  ylim <- c(min(density1$y, density2$y), max(density1$y, density2$y))
  confidence1 <- quantile(x = monteCarloEnsemble$k600, probs = c(0.025, 0.975))
  confidence2 <- quantile(x = monteCarloEnsemble2$k600, probs = c(0.025, 0.975))
  
  
  plot(density1, xlim = xlim, ylim = ylim, xlab = "", ylab = "",
       main =title, col = color1)
  lines(density2, xlim = xlim, ylim = ylim, col = color2)
  abline(v = c(confidence1, confidence2),
         col = c(color1, color1, color2, color2), lty = 2, lwd = .05 )
}

#### Multi ####

#import datasets
monteCarloEnsembleNlmrMulti_GPPof10 <- 
  read.csv("./monteCarloEnsembleNlmrMulti_GPPof10.csv")
monteCarloEnsembleNlmrMultiGPPof2 <- 
  read.csv("./monteCarloEnsembleNlmrMultiGPPof2.csv")
#plot distribution
monteCarloDistributionDensity(monteCarloEnsemble = monteCarloEnsembleNlmrMulti_GPPof10,
                              monteCarloEnsemble2 = monteCarloEnsembleNlmrMultiGPPof2,
                              color1 = "darkorchid1",
                              color2 = "darkmagenta",
                              title ="" , 
                              ER.xlim = c(-5.6,-4.4),
                              k600.xlim = c(11, 13))


#### pCO2 ####
#import datasets
monteCarloEnsembleNlmrpCO2GPPof10 <- 
  read.csv("./monteCarloEnsembleNlmrpCO2GPPof10.csv")
monteCarloEnsembleNlmrpCO2GPPof2 <- 
  read.csv("./monteCarloEnsembleNlmrpCO2GPPof2.csv")
#plot distribution
monteCarloDistributionDensity(monteCarloEnsemble = monteCarloEnsembleNlmrpCO2GPPof10,
                              monteCarloEnsemble2 = monteCarloEnsembleNlmrpCO2GPPof2,
                              color1 = "red",
                              color2 = "red4",
                              title = "", 
                              ER.xlim = c(-5.6,-4.4),
                              k600.xlim = c(11, 13))


#### DO ####
#import datasets
monteCarloEnsembleNlmrDO_GPPof10 <- 
  read.csv("./monteCarloEnsembleNlmrDO_GPPof10.csv")
monteCarloEnsembleNlmrDO_GPPof2 <- 
  read.csv("./monteCarloEnsembleNlmrDO_GPPof2.csv")
#plot distribution
monteCarloDistributionDensity(monteCarloEnsemble = monteCarloEnsembleNlmrDO_GPPof10,
                              monteCarloEnsemble2 = monteCarloEnsembleNlmrDO_GPPof2,
                              color1 = " royalblue",
                              color2 = "dark blue",
                              title = "", 
                              ER.xlim = c(-5.6,-4.4),
                              k600.xlim = c(11, 13))

# name columns and rows of plot window, provide units on x axis
mtext(text = expression(paste("pCO", phantom()[2])), 
      side = 2, 
      outer = TRUE, 
      adj = 0.53, 
      line = 2, 
      cex = 1.2)


mtext(text = expression(paste("DO and pCO", phantom()[2])), 
      side = 2, 
      outer = TRUE, 
      adj = 0.95, line = 2, cex = 1.2)

mtext(text = "density",  
      side = 2, 
      outer = TRUE,
      adj = c(0.9, 0.53, 0.18), 
      line = 0)

mtext(text = "DO", 
      side = 2, 
      outer = TRUE, 
      adj = 0.195, 
      line = 2, 
      cex = 1.2)

mtext(text = c(expression(paste(
  "GPP ( ",
  mg~L^-1,
  ")"
)),
expression(paste(
  "ER ( ",
  mg~L^-1,
  ")"
)),
expression(paste(
  k[600],
  " (",
  day^-1,
  ")"
))),
side = c(1,1,1,3,3,3),
outer = TRUE,
adj = c(0.15, 0.52, 0.9), 
padj = c(.4,.4,.4,0,0,0))


#### k600 box plot ####
windows(width= 15, height = 10)
par(mfrow = c(2,1), mar = c(0.5, 3, 0.5, 0.5), oma = c(3,5,3,0))

# 
boxplot(x = quantile(x = monteCarloEnsembleNlmrDO_GPPof2$k600, probs = c(0.025, 0.975)), 
            quantile(x = monteCarloEnsembleNlmrDO_GPPof10$k600, probs = c(0.025, 0.975)),
            quantile(x = monteCarloEnsembleNlmrpCO2GPPof2$k600, probs = c(0.025, 0.975)),
            quantile(x = monteCarloEnsembleNlmrpCO2GPPof10$k600, probs = c(0.025, 0.975)),
            quantile(x = monteCarloEnsembleNlmrMultiGPPof2$k600, probs = c(0.025, 0.975)),
            quantile(x = monteCarloEnsembleNlmrMulti_GPPof10$k600, probs = c(0.025, 0.975)),
        col = c("Dark Blue", "royal blue", "red4", "red", "darkmagenta", "darkorchid1"), 
        xlab = "k600 DO",
        range = 0,
        xaxt = "n")

boxplot(x = quantile(x = monteCarloEnsembleNlmrDO_GPPof2$dailyER, probs = c(0.025, 0.975)), 
        quantile(x = monteCarloEnsembleNlmrDO_GPPof10$dailyER, probs = c(0.025, 0.975)),
        quantile(x = monteCarloEnsembleNlmrpCO2GPPof2$dailyER, probs = c(0.025, 0.975)),
        quantile(x = monteCarloEnsembleNlmrpCO2GPPof10$dailyER, probs = c(0.025, 0.975)),
        quantile(x = monteCarloEnsembleNlmrMultiGPPof2$dailyER, probs = c(0.025, 0.975)),
        quantile(x = monteCarloEnsembleNlmrMulti_GPPof10$dailyER, probs = c(0.025, 0.975)),
        col = c("Dark Blue", "royal blue", "red4", "red", "darkmagenta", "darkorchid1"), 
        xlab = "k600 DO",
        range = 0)

mtext( text = c(expression(paste(k[600],
                               " (",
                               day^-1,
                               ")")), expression(paste(
                                 "ER ( ",
                                 mg~L^-1,
                                 ")"
                               )), "95% Confidence Intervals"),
       side = c(2,2,3),
       outer = TRUE,
       adj = c(.75, .25, .5))
       
adj <- c(0.16, .32, .48, .64, .8, .96)-.02

mtext(text = c("DO Model", "DO Model", "DIC Model", "DIC Model", "Coupled Model", "Coupled Model"),
  side = 1,
outer = TRUE,
adj = adj, 
padj = 0)

mtext( text = c("GPP = 2", "GPP = 10"),
                side = 1,
                outer = TRUE,
                adj = adj,
                padj = 2)
