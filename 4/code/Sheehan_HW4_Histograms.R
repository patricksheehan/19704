# Author: Patrick Sheehan
# Purpose: Extension of Anglin and Gencay (1996)     
# Input: None
# Output(s): Histograms and tables (for factor variables)

# Set up function for the histograms which are discrete
discreteHist <- function(x, main, xlab, increment=1){
  # Computes the breakpoints between histogram cells for a discrete variable
  # then outputs a histogram with discrete axis values
  #
  # Args:
  #   x: vector of values for a discrete variable
  #
  # Returns:
  #   A histogram with one bin for each value of x
  x.hist <- hist(x,
                 main=main,
                 xlab=xlab,
                 breaks=seq(min(x), max(x)+increment, by=increment),
                 right=FALSE,
                 xaxt="n",
                 col="gray")
  if(increment == 1){
    axis(1, 
         at=x.hist$mids, 
         labels=seq(min(x), max(x), by=1), 
         tick=FALSE, 
         line=-2)
  } else {
    axis(1)
  }
  return(x.hist)
}
# Now create the histograms
require(AER) # Require data package
data(SwissLabor) # Put the data in the workspace
pdf("histograms%d.pdf", onefile=FALSE) # Save on individual PDFs
age.hist <- discreteHist(SwissLabor$age, 
                         main="Histogram of age", 
                         xlab="Age (in decades)",
                         increment=0.1)
age.hist <- hist(SwissLabor$age, 
                 main="Histogram of age", 
                 xlab="Age (in decades)",
                 breaks="Scott",
                 col="gray")
education.hist <- discreteHist(SwissLabor$education, 
                               main="Histogram of education", 
                               xlab="Education (years of formal education)")
young.children.hist <- discreteHist(SwissLabor$youngkids, 
                                    main="Histogram of young children", 
                                    xlab="Number of young children (age < 7)")
# Non-discrete histogram for non-labor income
income.hist <- hist(SwissLabor$income,
                    main="Histogram of yearly non-labor income",
                    xlab="Log of yearly non-labor income",
                    breaks="fd",
                    col="gray")
old.kids.hist <- discreteHist(SwissLabor$oldkids, 
                              main="Histogram of old children", 
                              xlab="Number of old children (age > 7)")
dev.off() # done
participation.table <- table(SwissLabor$participation)
foreign.table <- table(SwissLabor$foreign)
