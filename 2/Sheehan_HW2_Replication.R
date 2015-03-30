# Author: Patrick Sheehan
# Purpose: Replicate Table 1.A and 2.A from Bierens and Ginther (2001)
# Input: None
# Output(s): t-values and estimates from Table 1.A (Table.1A) and 2.A (Table.2A)

# Load the necessary packages and get the data
require(AER) # Data package
require(quantreg) # LAD regression
data("CPS1988") # Put data in workspace
# Simulate the LAD median regression of Table 1.A:
Table.1A <- rq(log(wage) ~ education + experience + I(experience^2) + ethnicity,
           tau=0.5, data=CPS1988, model=TRUE, method="fn")
# Simulate the LAD median regression of Table 2.A:
Table.2A <- rq(log(wage) ~ education + experience + I(experience^2) + 
                       I(experience^3) + I(experience^4) + ethnicity,
           tau=0.5, data=CPS1988, model=TRUE, method="fn")
print("Table 1.A Replication:")
print(summary(Table.1A)) # Get details of the Table 1.A LAD regression
print("Table 2.A Replication:")
print(summary(Table.2A)) # Get details of the Table 2.A LAD regression


