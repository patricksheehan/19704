# Author: Patrick Sheehan
# Purpose: Extension of Gerfin (1996)   
# Input: None
# Output(s): ROC

require(AER) # Require data package
require(ROCR) # Require ROC package
require(cvTools) # Require cv Tools
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
# code adapted from lecture notes
# Create an empty data frame to store the predictions
without.preds <- data.frame(rep(NA, nrow(SwissLabor)/4))
with.preds <- data.frame(rep(NA, nrow(SwissLabor)/4))
# Remove the column of NAs
without.preds <- without.preds[, -1]
with.preds <- with.preds[, -1]
# Create an empty data frame to store the actual outcomes
labels <- data.frame(rep(NA, nrow(SwissLabor)/4))
# Remove the column of NAs
labels <- labels[, -1]
for(i in 1:100){
  # Take a random sample, without replacement from 4/5 of the rows
  samp1 <- sample(rownames(SwissLabor), 3*nrow(SwissLabor)/4, replace = FALSE)
  # Training data takes the sampled rows
  training <- SwissLabor[samp1, ]
  # Test data takes the remaining rows
  testing <- SwissLabor[setdiff(rownames(SwissLabor), samp1), ]
  # Run logistic regression without transformations on training data
  without <-  glm(participation ~ age + education + youngkids +
                    oldkids + income + foreign,
                  data=training,
                  family = binomial(link = "logit"))
  # Make probability predictions for test data
  without.pred <- predict(without, testing, type = "response")
  # Add the predictions for this iteration to the data frame
  without.preds <- cbind(without.preds, without.pred)
  # Run logistic regression with transformations on training data
  with <-  glm(participation ~ age + I(age^2) + youngkids +
                    oldkids + income + foreign + education,
                  data=training,
                  family = binomial(link = "probit"))
  # Make probability predictions for test data
  with.pred <- predict(with, testing, type = "response")
  # Add the predictions for this iteration to the data frame
  with.preds <- cbind(with.preds, with.pred)
  # Add the actual outcomes for this iteration to the data frame
  labels <- cbind(labels, testing$participation)
}
# Create a list with the predictions and labels
without.cvdata <- list(without.preds, labels)
with.cvdata <- list(with.preds, labels)
# Run the ROCR prediction and performance measures
without.err <- prediction(without.cvdata[[1]], without.cvdata[[2]])
without.perf <- performance(without.err, measure="tpr", x.measure="fpr")
# Run the ROCR prediction and performance measures
without.err <- prediction(without.cvdata[[1]], without.cvdata[[2]])
without.perf <- performance(without.err, measure="tpr", x.measure="fpr")
# This gives a vector of AUCs
without.auc <- performance(without.err, measure = "auc")
# Unlist the AUCs
without.auc <- unlist(without.auc@y.values)
# Take the average
without.auc <- mean(without.auc)
# Now with the other model
with.err <- prediction(with.cvdata[[1]], with.cvdata[[2]])
with.perf <- performance(with.err, measure="tpr", x.measure="fpr")
with.err <- prediction(with.cvdata[[1]], with.cvdata[[2]])
with.perf <- performance(with.err, measure="tpr", x.measure="fpr")
with.auc <- performance(with.err, measure = "auc")
with.auc <- unlist(with.auc@y.values)
with.auc <- mean(with.auc)

# print results
print(without.auc)
print(with.auc)

# Plot
pdf("ROC%d.pdf", onefile=FALSE)
plot(without.perf,
     colorize         = TRUE,
     color            = rainbow(10),
     main             = "Cross-Validated ROC Curve: Without transformations",
     avg              = 'threshold',
     spread.estimate  = 'stddev',
     print.cutoffs.at = seq(.0, .9, by = 0.1),
     text.adj         = c(-.5, 1.2),
     xlab             = "Average False Positive Rate",
     ylab             = "Average True Positive Rate")
abline(0, 1)
plot(with.perf,
     colorize         = TRUE,
     color            = rainbow(10),
     main             = "Cross-Validated ROC Curve: With transformations",
     avg              = 'threshold',
     spread.estimate  = 'stddev',
     print.cutoffs.at = seq(.0, .9, by = 0.1),
     text.adj         = c(-.5, 1.2),
     xlab             = "Average False Positive Rate",
     ylab             = "Average True Positive Rate")
abline(0, 1)
dev.off()





