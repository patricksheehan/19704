# Author: Patrick Sheehan
# Purpose: Replicate Table 1.A and 2.A from Bierens and Ginther (2001)
# Input: None
# Output(s): t-values and estimates from Table 1.A (T.1A) and 2.A (T.2A)

# Load the necessary packages and get the data
library(AER) # Data
library(quantreg) # LAD regression
data(CPS1988)

# Simulate the LAD median regression of Table 1.A:
T.1A <- rq(log(wage) ~ education + experience + I(experience^2) + ethnicity,
           tau=0.5, data=CPS1988, model=TRUE, method="fn")
summary(T.1A) # Get details of the LAD regression
# Simulate the LAD median regression of Table 2.A:
T.2A <- rq(log(wage) ~ education + experience + I(experience^2) + 
                       I(experience^3) + I(experience^4) + ethnicity,
           tau=0.5, data=CPS1988, model=TRUE, method="fn")
summary(T.2A) # Get detailss of the LAD regression
