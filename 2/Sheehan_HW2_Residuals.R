# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Linear regression residuals plots (one PDF file with all)

require(AER) # Require data package
if(! require(hexbin)){ # Require hexbin
  install.packages("hexbin")
  library(hexbin)
}
data("CPS1988") # Put data in workspace
# Part 4: Regression residual plots from linear regression as in T1.A & T2.A
# First, Table 1.A dependent variables with linear regression
Table.1A <- lm(log(wage) ~ education + experience + I(experience^2) + ethnicity,
               data= CPS1988)
Table.1A.residuals <- residuals(Table.1A) # get the residuals
Table.1A.fitted <- fitted(Table.1A)
# Next, Table 2.A dependent variables with linear regression
Table.2A <- lm(log(wage) ~ education + experience + I(experience^2) + 
                           I(experience^3) + I(experience^4) + ethnicity,
               data= CPS1988)
Table.2A.residuals <- residuals(Table.2A)
Table.2A.fitted <- fitted(Table.2A)
pdf("residuals%d.pdf", onefile=FALSE) # Begin PDF graphics driver
# plot 1A residuals
plot(Table.1A.fitted, Table.1A.residuals,
     main="Linear Regression Residuals vs. Predicted log(Wage) Values",
     xlab="Predicted log(Wage)",
     ylab="Residual",
     col=rgb(0, 0, 0, 0.1),
     pch=19,)
abline(0,0) # add line for perfect fit reference
# hexbin plot for 1A residuals
Table.1A.hex <- hexbinplot(Table.1A.residuals ~ Table.1A.fitted,
                           aspect=1,
                           main="Linear Regression Residuals vs. 
                                 Predicted log(Wage) Values",
                           xlab="Predicted log(Wage)",
                           ylab="Residual",)
plot(Table.1A.hex)
# influence plots for 1A regression
influenceIndexPlot(Table.1A, vars=c("Cook","Studentized","hat"), id.n=5)
# plot 2A residuals
plot(Table.2A.fitted, Table.2A.residuals,
     main="Linear Regression Residuals vs. Predicted log(Wage) Values",
     xlab="Predicted log(Wage)",
     ylab="Residual",
     col=rgb(0, 0, 0, 0.1),
     pch=19,)
abline(0,0) # add line for perfect fit reference
# hexbin plot for 2A residuals
Table.2A.hex <- hexbinplot(Table.2A.residuals ~ Table.2A.fitted,
                           aspect=1,
                           main="Linear Regression Residuals vs. 
                                 Predicted log(Wage) Values",
                           xlab="Predicted log(Wage)",
                           ylab="Residual",)
plot(Table.2A.hex)
# influence plots for 2A regression
influenceIndexPlot(Table.2A, vars=c("Cook","Studentized","hat"), id.n=5)
dev.off() # Done with pdf file
