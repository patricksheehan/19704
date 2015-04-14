# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Component plus residual plot & Box-tidwell result (lot size)

require(AER) # Require data package
require(car) # Require crPlot package
data(HousePrices) # Put the data in the workspace
benchmark.reg <- lm(log(price) ~ driveway + recreation + fullbase + gasheat 
                                 + aircon + garage + prefer + lotsize
                                 + bedrooms + bathrooms + stories,
                    data= HousePrices)
pdf("componentresidual.pdf")
crPlots(benchmark.reg, terms="lotsize",
        xlab ="Lot size (square feet)",
        pch=19,
        col=rgb(0, 0, 0, 0.5))
dev.off()
# Now find the Box-Tidwell result
results <- boxTidwell(log(price) ~ lotsize,
                      other.x= ~ driveway + recreation + fullbase + gasheat
                                 + aircon + garage + prefer + bedrooms
                                 + bathrooms + stories,
                      data= HousePrices)
print(results)
