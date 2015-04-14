# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Jacknife residual plots

require(AER) # Require data package
require(MASS) # Require studres package
data(HousePrices) # Put the data in the workspace
benchmark.reg <- lm(price ~ driveway + recreation + fullbase + gasheat 
                            + aircon + garage + prefer + log(lotsize) 
                            + bedrooms + bathrooms + stories,
                    data= HousePrices)
jackknife.residuals <- studres(benchmark.reg)
fitted.values <- fitted(benchmark.reg)
pdf("residuals.pdf")
plot(fitted.values,jackknife.residuals,
     main="Jackknife residuals vs. Fitted values",
     xlab="Fitted values (house price in $1987)",
     ylab="Jackknife residual",
     pch=19,
     col=rgb(0, 0, 0, 0.5))
dev.off()