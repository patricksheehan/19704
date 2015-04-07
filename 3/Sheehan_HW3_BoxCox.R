# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Box cox plot

require(AER) # Require data package
require(MASS) # Require studres package
data(HousePrices) # Put the data in the workspace
benchmark.reg <- lm(price ~ driveway + recreation + fullbase + gasheat 
                            + aircon + garage + prefer + log(lotsize) 
                            + bedrooms + bathrooms + stories,
                    data= HousePrices)
pdf("boxcox.pdf")
lambdas <- boxcox(benchmark.reg, plotit=TRUE)
dev.off()