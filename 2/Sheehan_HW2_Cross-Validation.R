# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Cross-validation

require(AER) # Require data package
data("CPS1988") # Put data in workspace
# Part 5: Backcast plot from linear regression as in Table 1.A
Table.1A <- lm(log(wage) ~ education + experience + I(experience^2) + ethnicity,
               data= CPS1988)

# Now, cross-validate
if(! require(cvTools)){
  install.packages("cvTools")
  library(cvTools)
}
Table.1A.CV <- cvFit(Table.1A, data=CPS1988, K=10)
print(summary(Table.1A.CV))