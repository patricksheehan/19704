# Author: Patrick Sheehan
# Purpose: Replicate the top of Table I of Gerfin (1996) for Switzerland data   
# Input: None
# Output(s): Table I replication (logistic regression coefficients and SEs)

require(AER) # Require data package
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
# Run probit regression
Table.I <- glm(participation ~ AGE + AGESQ + EDUC + NYC + NOC + NLINC + FOREIGN,
               family = binomial(link = "probit"))
# require(xtable) # code for LaTeX table output
# print.xtable(xtable(summary(Table.I)))
