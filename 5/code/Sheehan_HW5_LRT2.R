# Author: Patrick Sheehan
# Purpose: Extension of Goldstein et al. (1993)     
# Input: None
# Output(s): complete/partial/no pool regressions with standardized LRT scores
#            and group-level average intake LRT scores

# Some code adapted from lecture notes (section 5)
require(mlmRev) # Require data package
require(arm) # mixed effects regression
data("Exam", package = "mlmRev") # Get the data
# multi-level model with school average intake LRT
multi.LRT <- lmer(normexam ~ 1 + standLRT + schavg + (1|school), data=Exam)
summary(multi.LRT) # to get the variance of the intercepts
multi.LRT.intercepts <- as.matrix(coef(multi.LRT)$school[,1])
pdf("group_level.pdf")
hist(multi.LRT.intercepts + fixef(multi.LRT)[1], # have to add in fixed interc.
     breaks="FD",
     main="Histogram of school intercepts",
     xlab="School intercept value",
     col="gray")
dev.off()