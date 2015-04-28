# Author: Patrick Sheehan
# Purpose: Extension of Goldstein et al. (1993)     
# Input: None
# Output(s): complete/partial/no pool regressions with standardized LRT scores

# Some code adapted from lecture notes (section 5)
require(mlmRev) # Require data package
require(arm) # mixed effects regression
data("Exam", package = "mlmRev") # Get the data

# First, complete pooling
complete.pooling.LRT <- lm(normexam ~ 1 + standLRT, data=Exam)
complete.pooling.LRT.intercept <- coef(complete.pooling.LRT)[1]

# Next, no pooling
no.pooling.LRT <- lm(normexam ~ standLRT + factor(school) - 1, data=Exam)
pdf("no_pooling_LRT.pdf")
hist(coef(no.pooling.LRT)[-1],
     breaks="FD",
     main="Histogram of no-pooling intercepts",
     xlab="School intercept value",
     col="gray")
dev.off()

# Finally, partial pooling
partial.pooling.LRT <- lmer(normexam ~ 1 + standLRT + (1|school), data=Exam)
pdf("partial_pooling_LRT.pdf")
hist(as.matrix(coef(partial.pooling.LRT)$school[,1]),
     breaks="FD",
     main="Histogram of partial-pooling school regression intercepts",
     xlab="School intercept value",
     col="gray")
dev.off()

# Compare partial pooling and no pooling distributions
pdf("comparison_LRT.pdf")
qqplot(coef(no.pooling.LRT)[-1], as.matrix(coef(partial.pooling.LRT)$school[,1]),
       main="QQ plot for school regression intercepts",
       xlab="No pooling quantiles",
       ylab="Partial pooling quantiles",
       pch=19,
       col=rgb(0,0,0,.5))
abline(0,1)
abline(v=complete.pooling.LRT.intercept, lty=2)
dev.off()

print(coef(complete.pooling.LRT)["standLRT"])
print(coef(no.pooling.LRT)["standLRT"]) 
print(fixef(partial.pooling.LRT)["standLRT"]) 


