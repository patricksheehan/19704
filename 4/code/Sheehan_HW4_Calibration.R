# Author: Patrick Sheehan
# Purpose: Extension of Gerfin (1996)   
# Input: None
# Output(s): Calibration table and plot

require(AER) # Require data package
data("SwissLabor") # get the data
# Define variables as in Gerfin (1996)
participation <- SwissLabor$participation;
AGE <- SwissLabor$age;
AGESQ <- (AGE^2);
EDUC <- SwissLabor$education;
NYC <- SwissLabor$youngkids;
NOC <- SwissLabor$oldkids;
NLINC <- SwissLabor$income;
FOREIGN <- SwissLabor$foreign;
# Run regression with Table I model
Table.I <- glm(participation ~ AGE + AGESQ + EDUC + NYC + NOC + NLINC + FOREIGN,
               family = binomial(link = "logit"))
# Code adapted from lecture notes
# get the raw fitted probabilities
predicted.probabilities <- predict(Table.I, type="response")
# Get the deciles of the fitted probabilities
decile.cutpoints <- quantile(predicted.probabilities, probs = seq(0, 1, .1))
# Create a new variable that identifies
# the quantile that each fitted probibility falls in
decile.ID <- cut(predicted.probabilities,
                 breaks = decile.cutpoints,
                 labels = 1:10,
                 include.lowest = TRUE)
# Next calculate the number which actually participated in labor force
# Calculate the number which participated in each decile
tab <- table(decile.ID, SwissLabor$participation)
# To turn the table into a data.frame, use as.data.frame.matrix
observed <- as.data.frame.matrix(tab)
# To calculate the expected values for each decile,
# we need to sum the fitted probabilities for each decile
# We can do this by using tapply to compute
# the sum of predicted.probabilities within each level of decileID:
expected1 <- tapply(predicted.probabilities, decile.ID, FUN = sum)
# Create a summary calibration table
# Create cutpoints that represent the 9 intervals that exist for deciles
interval.cutpoints <- round(quantile(predicted.probabilities, 
                                     probs = seq(.1, 1, .1),
                                     type = 7)
                            , 2)
# Create a dataframe with these cutpoints:
cal <- data.frame(interval.cutpoints)
# Add a column of observed switches
cal$observed1 <- observed[, 2]
# Add a column of expected switches
cal$expected1 <- round(expected1, 0)
# Add columns for observed and expected non-switches:
cal$observed0 <- observed[ , 1]
cal$expected0 <-round(87 - expected1, 0)
# Add a column for the total # of observations in each decile
cal$total <- table(decile.ID)
# require(xtable) # code for LaTeX
# print.xtable(xtable(cal))

# Now create calibration plot
observed.decile.probabilities = as.numeric(cal$observed1/cal$total)
expected.decile.probabilities = as.numeric(cal$expected1/cal$total)
pdf("calibration.pdf")
plot(expected.decile.probabilities, observed.decile.probabilities,
     main="",
     xlab="Predicted probabilities",
     ylab="Observed probabilities",
     pch=19)
abline(0,1)
dev.off()
