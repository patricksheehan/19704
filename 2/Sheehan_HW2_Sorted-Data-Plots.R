# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Sorted Data Plots (one PDF file with all)

require(AER) # Require data package
data("CPS1988") # Put data in workspace
# Part 3: Sorted data plots (wages, log(wages) vs. normal dist)
wages <- CPS1988$wage 
log.wages <- log(wages)
# Draw from normal dist. with same sample size, mean, and SD
norm.wages <- rnorm(length(wages), mean(wages), sd(wages))
norm.log.wages <- rnorm(length(log.wages), mean(log.wages), sd(log.wages))
# Plot the sorted data
pdf("sorted-data-plots%d.pdf", onefile=FALSE) # Begin PDF graphics driver
# Wage vs. normal 
min1 <- min(wages, norm.wages)  # Find the smallest value (From lecture notes)
max1 <- max(wages, norm.wages)  # Find the largest value (From lecture notes)
plot(sort(norm.wages),sort(wages),
     main= "Wages vs. Normal Distribution",
     xlab= "Sorted Draws from a Normal Distribution",
     ylab= "Sorted Wage Data",
     xlim= c(min1, max1),
     ylim= c(min1, max1),
     pch= 19,
     col= rgb(0,0,0,0.1))
abline(0,1) # split the plot where the two distributions would be considered ==
# log(Wage) vs. normal
min2 <- min(log.wages, norm.log.wages)  # Find the smallest value
max2 <- max(log.wages, norm.log.wages)  # Find the largest value
plot(sort(norm.log.wages),sort(log.wages),
     main= "log(Wages) vs. Normal Distribution",
     xlab= "Sorted Draws from a Normal Distribution",
     ylab= "Sorted log(Wage) Data",
     xlim= c(min2, max2),
     ylim= c(min2, max2),
     pch= 19,
     col= rgb(0,0,0,0.1))
abline(0,1) # split the plot where the two distributions would be considered ==
dev.off() # Done with pdf file
