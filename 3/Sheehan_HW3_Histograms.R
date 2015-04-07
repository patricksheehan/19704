# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Histograms

# Set up function for the histograms which are discrete
discreteHist <- function(x){
  # Computes the breakpoints between histogram cells for a discrete variable
  # then outputs a histogram with discrete axis values
  #
  # Args:
  #   x: vector of values for a discrete variable
  #
  # Returns:
  #   A histogram with one bin for each value of x
  x.hist <- hist(x,
                 main=paste("Histogram of", deparse(substitute(x))),
                 xlab=deparse(substitute(x)),
                 breaks=seq(min(x), max(x)+1, by=1),
                 right=FALSE,
                 xaxt="n",
                 col="gray")
  axis(1, at=x.hist$mids, labels=seq(min(x), max(x), by=1), tick=FALSE, line=-2)
  return(x.hist)
}

require(AER) # Require data package
data(HousePrices) # Put the data in the workspace
pdf("histograms%d.pdf", onefile=FALSE) # Save on individual PDFs
# Histogram for house prices
price <- HousePrices$price
price.hist <- hist(price,
                   main="Histogram of house prices",
                   xlab="House price ($1987)",
                   breaks="Scott",
                   col="gray")
# Histogram for log house prices
log.price.hist <- hist(log(price),
                       main="Histogram of log of house prices",
                       xlab="log(price)",
                       breaks="Scott",
                       col="gray")
# Histogram for lot size
lot.size <- HousePrices$lotsize
lot.size.hist <- hist(lot.size,
                      main= "Histogram of lot size",
                      xlab="Lot size (square feet)",
                      breaks="Scott",
                      col="gray")
bedrooms <- HousePrices$bedrooms
bedrooms.hist <- discreteHist(bedrooms) # Histogram for number of bedrooms
bathrooms <- HousePrices$bathrooms
bathrooms.hist <- discreteHist(bathrooms) # Histogram for number of bathrooms
stories <- HousePrices$stories
stories.hist <- discreteHist(stories) # Histogram for number of stories
dev.off() # done
