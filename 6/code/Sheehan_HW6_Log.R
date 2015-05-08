# Author: Patrick Sheehan
# Purpose: Extension of Baltagi et al. (2000)     
# Input: None
# Output(s): Box Cox and CR plots to determine if log transform appropriate

require(Ecdat)
require(MASS)
require(car)
data("Cigar", package="Ecdat")
source("../code/Sheehan_HW6_Replication.R")
# First, set up a panel data set so R can handel the i,t indexes
Cigar.pdata <- pdata.frame(Cigar, index=c("state", "year"))
# Since boxcox() is built to handle lm objects, use the LSDV model
# but without a transformed dependent variable
LSDV <- lm(sales ~ factor(year) + factor(state) 
           + log(lag(Cigar.pdata$sales)) + log(price*100/cpi)
           + log(pimin*100/cpi) + log(ndi*100/cpi),
           data=Cigar)
pdf("BoxCox.pdf")
boxcox(LSDV) # boxcox to find the log-likelihood of lambda values for sales  
dev.off()
# Now diagnose possible transformations for the dependent variables
# again, use the LSDV model for analysis
LSDV <- lm(log(sales) ~ factor(year) + factor(state) 
           + log(lag(Cigar.pdata$sales)) + log(price*100/cpi)
           + log(pimin*100/cpi) + log(ndi*100/cpi),
           data=Cigar)
# Now, plot the CR plots for all independent variables except for DVs
pdf("CR%d.pdf", onefile=FALSE)
crPlots(LSDV, terms=~ . - factor(year) - factor(state))
crPlots(OLS)
crPlots(OLS.D, terms=~ .- factor(year))
dev.off()