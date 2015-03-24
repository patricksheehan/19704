# Author: Patrick Sheehan
# Purpose: Extend the replication of Table 1.A and 2.A 
#          from Bierens and Ginther (2001)          
# Input: None (But requires "Birens_Ginther_Simulation.R" in working directory)
# Output(s): Histograms

# First, run my simulation replicating Table 1.A and 2.a
source("Birens_Ginther_Simulation.R")
# 1: Histograms (wages, log(wages), education, experience)
wages <- CPS1988$wage # Get the data 
log.wages <- log(wages)
education <- CPS1988$education
experience <- CPS1988$experience
pdf("histograms.pdf") # Begin PDF graphics driver
hist(wages,
     breaks = "FD",
     main = "Wages",
     xlim = c(0,3.5e3),
     xlab = "Wage (in dollars per week)")
hist(log.wages,
     breaks = "Scott",
     main = "log(Wages)",
     xlab = "log(Wages)")
ed.hist <- hist(education,
                breaks = seq(0,max(education),1), # One bin for each year
                main = "Education",
                xlab = "Education (Number of years of education)",
                xaxt = "n")
axis(1, at= ed.hist$mids, labels= seq(0,max(education)-1,1), cex.axis=0.8)
hist(experience)
dev.off()
