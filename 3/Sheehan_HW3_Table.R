# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Dummy variable tables

require(AER) # Require data package
data(HousePrices) # Put the data in the workspace
dummy.vars <- HousePrices[c(6:10, 12)] # Get dummy vars
dummy.tables <- lapply(dummy.vars,table) # Create table for each
print(dummy.tables) # Print tables