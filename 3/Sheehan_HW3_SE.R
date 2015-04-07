# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Robust and classical standard errors for Table III model

require(AER) # Require data package
require(sandwich) # For robust SE
data(HousePrices) # Put the data in the workspace
benchmark <- lm(log(price) ~ log(lotsize) + recreation + fullbase + gasheat 
                     + aircon + garage + prefer + driveway
                     + bedrooms + bathrooms + stories,
                     data= HousePrices)
# Now compute Standard and Robust errors
SE.classical <- summary(benchmark)
SE.robust <- coeftest(benchmark, # code from the lecture notes
                      vcov = vcovHC(benchmark, type = "HC0"),
                      df = df.residual(benchmark))
print(SE.classical)
print(SE.robust)


