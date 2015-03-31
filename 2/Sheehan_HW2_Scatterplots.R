# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Scatterplots (one PDF file with all)

# First, run my simulation replicating Table 1.A and 2.a
require(AER) # Require data package
if(! require(hexbin)){ # Require hexbin package
  install.packages("hexbin")
  library("hexbin")
}
# Set up data
data("CPS1988")
wages <- CPS1988$wage 
log.wages <- log(wages)
education <- CPS1988$education
experience <- CPS1988$experience
# Part 2: Scatterplots (wages, log(wages) vs. education, experience)
pdf("scatterplots%d.pdf", onefile=FALSE) # Begin PDF graphics driver
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
ed.hex <- hexbinplot(wages ~ education,
                     aspect= 1,
                     main= "Wage vs. Education",
                     xlab= "Education (years)",
                     ylab= "Wage ($ per week)",
                     type= c("r"),
                     col.line= "red")
plot(ed.hex)
# Education vs. log(Wage)
plot(education, log.wages,
     main="log(Wage) vs. Education",
     xlab="Education (years)",
     ylab="log(Wage)",
     col=rgb(0, 0, 0, 0.1),
     pch=19,
     xaxp=c(min(education), max(education), 18),
     cex.axis=0.8)
abline(lm(log.wages~education), col="red")
points(x=mean(education), y=mean(log.wages), pch=8, col="green", lwd=2, cex=1.5)
# Alternative: hexbin
ed.hex.log <- hexbinplot(log.wages ~ education,
                         main= "log(Wage) vs. Education",
                         xlab= "Education (years)",
                         ylab= "log(Wage)",
                         type=c("r"),
                         col.line= "red",)
plot(ed.hex.log)
# Experience vs. wages
plot(experience, wages,
     main="Wage vs. Experience",
     xlab="Experience (years)",
     ylab="Wage ($ per week)",
     col=rgb(0, 0, 0, 0.1),
     pch=19)
abline(lm(wages~experience), col="red")
points(x=mean(experience), y=mean(wages), pch=8, col="green", lwd=2, cex=1.5)
# Alternative: hexbin
exp.hex <- hexbinplot(wages ~ experience,
                      xbins= 120,
                      aspect= 1,
                      main= "Wage vs. Experience",
                      xlab= "Experience (years)",
                      ylab= "Wage ($ per week)",
                      type=c("r"),
                      col.line= "red",)
plot(exp.hex)
# Experience vs. log.wages
plot(experience, log.wages,
     main="log(Wage) vs. Experience",
     xlab="Experience (years)",
     ylab="log(Wage)",
     col=rgb(0, 0, 0, 0.1),
     pch=19)
abline(lm(log.wages~experience), col="red")
points(x=mean(experience), y=mean(log.wages), pch=8, col="green", lwd=2, cex=1.5)
# Alternative: hexbin
exp.hex.log <- hexbinplot(log.wages ~ experience,
                          aspect= 1,
                          xbins= 120,
                          main= "log(Wage) vs. Experience",
                          xlab= "Experience (years)",
                          ylab= "log(Wage)",
                          type=c("r"),
                          col.line= "red",)
plot(exp.hex.log)
dev.off() # Done with pdf file