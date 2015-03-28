# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)      
# Input: None
# Output(s): Histograms (one PDF file with all)

# First, run my simulation replicating Table 1.A and 2.a
require(AER) # Require data package
data("CPS1988") # Put data in workspace
# Part 1: Histograms (wages, log(wages), education, experience)
# Get the data 
wages <- CPS1988$wage 
log.wages <- log(wages)
education <- CPS1988$education
experience <- CPS1988$experience
pdf("histograms.pdf") # Begin PDF graphics driver
wage.hist <- hist(wages,
     breaks = c(seq(0, 3500, 50),max(wages)), # FD bins + Outlier bin
     freq = TRUE,
     main = "Wages",
     xlim = c(0, 3.5e3),
     xlab = "Wages (in dollars per week)")
axis(1, at=c(0, 50, seq(500, 3500, 500)))
hist(log.wages,
     breaks = "Scott",
     main = "log(Wages)",
     xlab = "log(Wages)")
ed.hist <- hist(education,
                breaks = seq(0, max(education)+1, 1), # One bin for each year
                right = FALSE, # Mandate one year for each bin
                main = "Education",
                xlab = "Education (discrete years)",
                xaxt = "n")
axis(1, at=ed.hist$mids, labels=seq(0, max(education), 1), cex.axis=0.8,
     tick=FALSE, line=-2)
hist(experience,
     main = "Experience",
     xlab = "Experience (discrete years)",
     breaks = seq(min(experience), max(experience)+1, 1), # One bin per year
     right = FALSE,) # Mandate one year per bin
dev.off() # Done with pdf file
