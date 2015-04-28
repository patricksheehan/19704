# Author: Patrick Sheehan
# Purpose: Extension of Goldstein et al. (1993)     
# Input: None
# Output(s): intercept-only regression results (GCSE exam scores)

# Some code adapted from lecture notes (section 5)
require(mlmRev) # Require data package
require(arm) # mixed effects regression
data("Exam", package = "mlmRev") # Get the data

# First, complete pooling
complete.pooling <- lm(normexam ~ 1, data=Exam)
# Make predictions for the intercept for each group with 95% conf.
school.intercepts <- predict(complete.pooling,
                             newdata  = Exam$school,
                             interval = c("confidence"),
                             level    = 0.95, type = "response")
pdf("complete_pooling.pdf")
plot(as.factor(Exam$school), Exam$normexam,
     xlim  = c(min(as.numeric(Exam$school)), max(as.numeric(Exam$school))),
     pch  = 20,
     col  = rgb(0, 0, 0, .2),
     main = "Normalized GCSE exam scores by school",
     ylab = "Normalized exam score",
     xlab = "School number")
# Add 95% confidence interval lines
lines(Exam$school, school.intercepts[, 2], col ="blue", lwd = 3)
lines(Exam$school, school.intercepts[, 3], col = "blue", lwd = 3)
# Add complete-pooling intercept
abline(h = coef(complete.pooling), col = "red", lty = 2, lwd = 4)
dev.off()

# Next, no pooling
no.pooling <- lapply(unique(Exam$school),
                     function(x)lm(normexam ~ 1, data=Exam[Exam$school == x, ]))
std.errors <- sapply(no.pooling,function(x)coef(summary(x))[1,2])
# Using the dummy variable method is easier to grab the means
no.pooling.onereg <- lm(normexam ~ factor(school) - 1, data=Exam)
pdf("no_pooling.pdf")
no.pooling.dist <- hist(coef(no.pooling.onereg),
                        breaks="FD",
                        main="Histogram of no-pooling mean GSCE exam score",
                        xlab="Mean normalized exam score",
                        col="gray")
dev.off()
pdf("no_pooling_se.pdf")
no.pooling.errors <- plot(coef(no.pooling.onereg), std.errors,
                          main="Standard errors for no-pooling means",
                          xlab="Mean normalized exam score for school",
                          ylab="Standard error of intercept coefficient",
                          pch=19)
dev.off()

# Finally, partial pooling
partial.pooling <- lmer(normexam ~ 1 + (1|school), data=Exam)
pdf("partial_pooling.pdf")
partial.pooling.dist <- hist(as.matrix(coef(partial.pooling)$school),
                             breaks="FD",
                             main="Histogram of partial-pooling 
                                   school mean GSCE exam score",
                             xlab="Mean normalized exam score",
                             col="gray")
dev.off()

# Compare partial pooling and no pooling distributions
pdf("no_vs_partial.pdf")
qqplot(coef(no.pooling.onereg), as.matrix(coef(partial.pooling)$school),
       main="QQ plot for mean normalized exam scores by school",
       xlab="No pooling quantiles",
       ylab="Partial pooling quantiles",
       pch=19,
       col=rgb(0,0,0,.5))
abline(0,1)
dev.off()

# Example school: 42
pdf("42.pdf")
school.42 <-hist(Exam$normexam[Exam$school == 42],
                 breaks="FD",
                 main="Histogram of normalized GCSE exam scores for school 42",
                 xlab="Normalized exam score",
                 col="gray")
abline(v=as.matrix(coef(partial.pooling)$school)[42], col="blue", lwd=3)
abline(v=coef(no.pooling.onereg)[42], col="red", lty=2, lwd=3)
dev.off()

