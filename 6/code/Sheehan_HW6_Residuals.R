# Author: Patrick Sheehan
# Purpose: Extension of Baltagi et al. (2000)     
# Input: None
# Output(s): Residual plots

require(Ecdat)
require(MASS)
# Get the models from the replication
source("../code/Sheehan_HW6_Replication.R")
# Next, plot the fitted values and jacknife studres for each model
pdf("residuals%d.pdf", onefile=FALSE)
plot(fitted(OLS), studres(OLS),
     pch=19,
     col=rgb(0, 0, 0, 0.5))
abline(lm(studres(OLS) ~ fitted(OLS)), col="red")
plot(fitted(OLS.D), studres(OLS.D),
     pch=19,
     col=rgb(0, 0, 0, 0.5))
abline(lm(studres(OLS.D) ~ fitted(OLS.D)), col="red")
plot(fitted(LSDV), studres(LSDV),
     pch=19,
     col=rgb(0, 0, 0, 0.5))
abline(lm(studres(LSDV) ~ fitted(LSDV)), col="red")
dev.off()

