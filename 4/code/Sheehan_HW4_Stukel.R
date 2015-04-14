# Author: Patrick Sheehan
# Purpose: Extension of Gerfin (1996)   
# Input: None
# Output(s): Stukel's test on logistic and GAM regression of Table I model

require(AER) # Require data package
require(mgcv) # Require GAM package
data("SwissLabor") # get the data
# Define variables as in Gerfin (1996)
participation <- SwissLabor$participation;
AGE <- SwissLabor$age;
AGESQ <- (AGE^2);
EDUC <- SwissLabor$education;
NYC <- SwissLabor$youngkids;
NOC <- SwissLabor$oldkids;
NLINC <- SwissLabor$income;
FOREIGN <- SwissLabor$foreign;
# Run GAM
GAM <- gam(participation ~ s(AGE) + s(AGESQ) + s(EDUC) + NYC + NOC + NLINC
           + FOREIGN,
           family=binomial(link = "logit"))
# Run logistic regression
logit <- glm(participation ~ AGE + AGESQ + EDUC + NYC + NOC + NLINC + FOREIGN,
             family = binomial(link = "logit"))
# Now run Stukel's test on each. Code adapted from the lecture notes
eta.hat.GAM <- predict(GAM)
eta.hat.logit <- predict(logit)
pos.GAM <- ifelse(eta.hat.GAM >= 0, 1, 0)
neg.GAM <- ifelse(eta.hat.GAM < 0, 1, 0)
pos.logit <- ifelse(eta.hat.logit >= 0, 1, 0)
neg.logit <- ifelse(eta.hat.logit < 0, 1, 0)
eta.hat.GAM.sq <- eta.hat.GAM^2
eta.hat.logit.sq <- eta.hat.logit^2
s.logit <- glm(participation ~ AGE + AGESQ + EDUC + NYC + NOC + NLINC + FOREIGN
               + eta.hat.logit.sq:pos.logit + eta.hat.logit.sq:neg.logit,
               family = binomial(link = "logit"))
s.GAM <- gam(participation ~ s(AGE) + s(AGESQ) + s(EDUC) + NYC + NOC + NLINC
             + FOREIGN + eta.hat.GAM.sq:pos.GAM + eta.hat.GAM.sq:neg.GAM,
             family=binomial(link = "logit"))





