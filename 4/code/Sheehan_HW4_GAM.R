# Author: Patrick Sheehan
# Purpose: Extension of Gerfin (1996)   
# Input: None
# Output(s): GAM with smoothing on age and education, partial residual plots

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
Table.I.GAM <- gam(participation ~ s(AGE) + s(AGESQ) + s(EDUC) + NYC 
                   + NOC + NLINC + FOREIGN,
                   family=binomial(link = "logit"))
pdf("gam%d.pdf", onefile=FALSE)
plot(Table.I.GAM, 
     residuals=TRUE, # Some parameters copied from lecture note code
     shade=TRUE,
     pch=19,
     col=rgb(0, 0, 0, 0.5),
     cex=.5)
dev.off()


