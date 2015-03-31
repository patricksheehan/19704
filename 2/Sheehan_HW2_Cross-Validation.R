# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Cross-validation results in the order of the "models" list

require(AER) # Require data package
require(cvTools) # Require cv Tools
data("CPS1988") # Put data in workspace
# Original Table 1.A model with linear regression
Table.1A <- lm(log(wage) ~ education + experience + I(experience^2) + ethnicity,
               data= CPS1988)
# Now, create models with one less independent variable
Table.1A.noEthnicity <- lm(log(wage) ~ education + experience + I(experience^2),
                           data= CPS1988)
Table.1A.noExpsquared <- lm(log(wage) ~ education + experience + ethnicity,
                            data= CPS1988)
Table.1A.noExperience <- lm(log(wage) ~ education + I(experience^2) + ethnicity,
                            data= CPS1988)
Table.1A.noEducation <- lm(log(wage) ~ experience + I(experience^2) + ethnicity,
                           data= CPS1988)
# Create list with all the models
models <- list(Table.1A,
               Table.1A.noEthnicity, 
               Table.1A.noExpsquared, 
               Table.1A.noExperience,
               Table.1A.noEducation)
# Cross validate each model
Table.1A.CV <- lapply(models,
                     cvFit,
                     data=CPS1988,
                     y=log(CPS1988$wage),
                     K=5, # 5 folds,
                     R=10, # 1 Replication for now
                     cost=rmspe) # Evaluate based on the RMSE
# Get the results, in the order of the "models" list
for(result in Table.1A.CV)
  print(summary(result))


