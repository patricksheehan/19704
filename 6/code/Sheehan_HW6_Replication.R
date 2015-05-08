# Author: Patrick Sheehan
# Purpose: Extension of Baltagi et al. (2000)     
# Input: None
# Output(s): Replication of Table I (select rows)

require(Ecdat)
require(plm)
data("Cigar", package = "Ecdat")
# First, set up a panel data set to get the lag function working
Cigar.pdata <- pdata.frame(Cigar, index=c("state", "year"))
# Next, run the OLS complete pooling as described by Baltagi et al. (2000)
# All dollar values adjusted to 1983
OLS <- lm(log(sales) ~ log(lag(Cigar.pdata$sales)) + log(price*100/cpi) 
                        + log(pimin*100/cpi) + log(ndi*100/cpi),
           data=Cigar)
# Now add time dummy
OLS.D <- lm(log(sales) ~ factor(year) + log(lag(Cigar.pdata$sales)) 
                          + log(price*100/cpi) + log(pimin*100/cpi) 
                          + log(ndi*100/cpi),
             data=Cigar)
# Now use the within, two possible methods:
# 1. Direct within model with year dummy
Within <- plm(log(sales) ~ factor(year) + log(lag(sales)) 
                           + log(price*100/cpi) + log(pimin*100/cpi)
                           + log(ndi*100/cpi),
              model="within",
              data=Cigar.pdata)
# 2. LSDV (Within model accomplished)
LSDV <- lm(log(sales) ~ factor(year) + factor(state) 
                        + log(lag(Cigar.pdata$sales)) + log(price*100/cpi)
                        + log(pimin*100/cpi) + log(ndi*100/cpi),
           data=Cigar)