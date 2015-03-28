# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Scatterplots (one PDF file with all)

# First, run my simulation replicating Table 1.A and 2.a
require(AER) # Require data package
# Set up data
data("CPS1988")
wages <- CPS1988$wage 
log.wages <- log(wages)
education <- CPS1988$education
experience <- CPS1988$experience
# Part 2: Scatterplots (wages, log(wages) vs. education, experience)
pdf("scatterplots.pdf") # Begin PDF graphics driver
# Education vs. Wages
plot(education, wages,
     main="Wage vs. Education",
     xlab="Education (years)",
     ylab="Wage ($ per week)",
     col=rgb(0, 0, 0, 0.1),
     pch=19,
     xaxp=c(min(education), max(education), 18),
     cex.axis=0.8)
abline(lm(wages ~ education), col="red")
points(x=mean(education), y=mean(wages), pch=8, col="green", lwd=2, cex=1.5)
# Alternative: hexbin
# ed.bins <- hexbin(education, wages, 
#                   xbins=19,
#                   xlab= "Education (years)",
#                   ylab= "")
# plot(ed.bins, main= "Wage vs. Education")
# Education vs. log(wages)
plot(education, log.wages,
     main="log(Wage) vs. Education",
     xlab="Education (years)",
     ylab="log(Wage)",
     col=rgb(0, 0, 0, 0.1),
     pch=19,
     xaxp=c(min(education), max(education), 18),
     cex.axis=0.8)
abline(lm(log.wages~education), col="red")
# Experience vs. wages
plot(experience, wages,
     main="Wage vs. Experience",
     xlab="Experience (years)",
     ylab="Wage ($ per week)",
     col=rgb(0, 0, 0, 0.1),
     pch=19)
abline(lm(wages~experience), col="red")
# Experience vs. log.wages
plot(experience, log.wages,
     main="log(Wage) vs. Experience",
     xlab="Experience (years)",
     ylab="log(Wage)",
     col=rgb(0, 0, 0, 0.1),
     pch=19)
abline(lm(log.wages~experience), col="red")
dev.off() # Done with pdf file