# Author: Patrick Sheehan
# Purpose: Replication of Table II and III
#          from Anglin and Gencay (1996)     
# Input: None
# Output(s): Table II and III regression summaries

require(AER) # Require data package
data(HousePrices) # Put the data in the workspace
# Now replicate Table II & III regressions
Table_II <- lm(log(price) ~ driveway + recreation + fullbase + gasheat + aircon
                            + garage + prefer + log(lotsize) + log(bedrooms)
                            + log(bathrooms) + log(stories),
               data= HousePrices)
Table_III <- lm(log(price) ~ driveway + recreation + fullbase + gasheat + aircon
                             + garage + prefer + log(lotsize) + bedrooms
                             + bathrooms + stories,
                data= HousePrices)
# Print regression summaries
print(summary(Table_II))
print(summary(Table_III))