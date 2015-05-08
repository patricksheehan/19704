# Author: Patrick Sheehan
# Purpose: Extension of Baltagi et al. (2000)     
# Input: None
# Output(s): homoskedastic, heteroskedasticity, and serial-correlation
# robust standard errors

require(Ecdat)
require(plm)
require(lmtest)
require(sandwich)
data("Cigar", package = "Ecdat")
# Some code adapted from the lecture notes
# First, set up a panel data set to get the lag function working
Cigar.pdata <- pdata.frame(Cigar, index=c("state", "year"))
# Now use the within, two possible methods:
Within <- plm(log(sales) ~ factor(year) + log(lag(sales)) 
              + log(price*100/cpi) + log(pimin*100/cpi)
              + log(ndi*100/cpi),
              model="within",
              effect="individual",
              data=Cigar.pdata)
# Get the homoskedastic standard errors
homoskedastic <-summary(Within)
# Now get the heteroskedastic standard errors
heteroskedastic <- coeftest(Within, 
                            vcov = function(x) vcovHC(x,
                                                      method  = "white1",
                                                      type    = "HC0",
                                                      cluster = "group"))
# Now get the serial-correlation robust standard errors
serial <- coeftest(Within, 
                   vcov = function(x) vcovHC(x,
                                             method  = "arellano",
                                             type    = "HC0",
                                             cluster = "group"))
