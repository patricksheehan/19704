# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Partial residual plot (lot size) from GAM and CV comparison

require(AER) # Require data package
require(mgcv) # Require GAM package
require(cvTools) # Require CV package
require(gamclass) # Require CV for gam package
data(HousePrices) # Put the data in the workspace
# Produce gam model with lotsize smoothed
benchmark.gam <- gam(log(price) ~ s(lotsize) + recreation + fullbase + gasheat 
                                  + aircon + garage + prefer + driveway
                                  + bedrooms + bathrooms + stories,
                     data= HousePrices)
pdf("gam.pdf")
plot(benchmark.gam, 
     residuals=TRUE, # Some parameters copied from lecture note code
     shade=TRUE,
     xlab="Lot size (square feet)",
     pch=19,
     col=rgb(0, 0, 0, 0.5),
     cex=.5)
dev.off()
# Now get the fit for the model with log(lotsize)
benchmark.log <- lm(log(price) ~ driveway + recreation + fullbase + gasheat 
                                 + aircon + garage + prefer + log(lotsize) 
                                 + bedrooms + bathrooms + stories,
                    data= HousePrices)
# Now CV to compare the two
gam.cv <- CVgam(benchmark.gam$formula, 
                data=HousePrices, 
                nfold= nrow(HousePrices) - 1, # leave one out CV
                printit= FALSE)
log.cv <- cvFit(benchmark.log,
                data=HousePrices,
                y=log(HousePrices$price),
                K= nrow(HousePrices) - 1, # leave one out cv
                cost=rmspe) # Evaluate based on the RMSE
print(sqrt(gam.cv$cvscale)) # Print RMSE of GAM model
print(log.cv$cv) # Print RMSE of log(lotsize) benchmark
