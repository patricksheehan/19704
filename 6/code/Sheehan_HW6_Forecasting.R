# Author: Patrick Sheehan
# Purpose: Extension of Baltagi et al. (2000)     
# Input: None
# Output(s): Forecasting 1992 data

require(Ecdat)
require(plm)
data("Cigar", package = "Ecdat")
# As in the replication:
# First, set up a panel data set to get the lag function working
Cigar.pdata <- pdata.frame(Cigar, index=c("state", "year"))
# Next, run the OLS complete pooling as described by Baltagi et al. (2000)
# But this time only train the model with data for years less than 1992
training <- Cigar[Cigar$year != 92, ]
log.lagged.sales <- log(lag(Cigar.pdata$sales)[Cigar.pdata$year != 92])
OLS <- lm(log(sales) ~ log.lagged.sales + log(price*100/cpi) 
          + log(pimin*100/cpi) + log(ndi*100/cpi),
          data=training)
# 2. LSDV (Within model), again using only data years < 1992
LSDV <- lm(log(sales) ~ factor(year) + factor(state) 
           + log.lagged.sales + log(price*100/cpi)
           + log(pimin*100/cpi) + log(ndi*100/cpi),
           data=training)
# Now, make predictions for 1992, some code adapted from lecture notes
test <- Cigar[Cigar$year == 92, ] # get the '92 data
test$year <- 91 # don't have an intercept in the model for '92, so we use '91
test$log.lagged.sales <- log(Cigar$sales[Cigar$year == 91]) # last year's sales
OLS.pred <- predict(OLS, newdata=test) # make predictions
LSDV.pred <- predict(LSDV, newdata=test)
OLS.residual <- log(test$sales) - OLS.pred # find residuals
LSDV.residual <- log(test$sales) - LSDV.pred
OLS.rMSE <- mean(OLS.residual^2) # calculate rMSE
LSDV.rMSE <- mean(LSDV.residual^2)
