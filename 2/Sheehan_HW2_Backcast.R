# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Backcasting plot

require(AER) # Require data package
require(lattice)
if(! require(hexbin)){ # Require hexbin
  install.packages("hexbin")
  library(hexbin)
}
data("CPS1988") # Put data in workspace
# Part 5: Backcast plot from linear regression as in Table 1.A
Table.1A <- lm(log(wage) ~ education + experience + I(experience^2) + ethnicity,
               data= CPS1988)
log.wage.actual <- log(CPS1988$wage) # Actual log(Wage) data
log.wage.predicted <- fitted(Table.1A) # Model predictions
min.log.wage <- min(log.wage.actual, log.wage.predicted)
max.log.wage <- max(log.wage.actual, log.wage.predicted)
pdf("backcast.pdf") # Begin PDF graphics driver
Table.1A.Backcastplot <- hexbinplot(log.wage.actual ~ log.wage.predicted,
                                    main="",
                                    xlab="Predicted log(Wage)",
                                    ylab="Actual log(Wage)",
                                    xlim=c(min.log.wage, max.log.wage),
                                    ylim=c(min.log.wage, max.log.wage),
                                    panel= function(...) {
                                              panel.hexbinplot(...)
                                              panel.abline(0,1)
                                           },
                                    xbins= 50,
                                    aspect= 1)
plot(Table.1A.Backcastplot)
dev.off() # Done with pdf file