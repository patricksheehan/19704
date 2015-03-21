# Author: Patrick Sheehan
# Purpose: Replicate Situation B in Simons et al. (2011)
# Input: None
# Output(s): % of simulated experiments with p < .1, .05, .01

# First, create function which mimics Situation B
SituationB <- function(pval){
  # Runs a single test of Situation B in Simons et al. (2011) where a t-test is
  # performed on two populations which are sampled randomly from normal 
  # distributions. While the initial sample size is 20, if that test proves not
  # significant as determined by the p-value given (pval), then a second
  # t-test is done with 10 additional observations.
  #
  # Args:
  #   pval: p-value which will be used for the t-tests for significance
  # 
  # Returns:
  #   1: if either of the tests is significant as determined by pvalue
  #   0: if neither of the tests is significant as determined by pvalue

  # Set up initial experiment, two conditions, 20 observations each
  condition1 <- rnorm(n=20, mean=0, sd=1) # Observations for first condition
  condition2 <- rnorm(n=20, mean=0, sd=1) # Observations for second condition
  test1 <- t.test(condition1, condition2, var.equal=TRUE) # Perform t-test
  # If t-test significant at pval level, return 1
  if (test1$p.value < pval)
    return (1)
  # Otherwise, run test with 10 additional observations
  condition1 <- c(condition1, rnorm(10,0,1)) # include original observations
  condition2 <- c(condition2, rnorm(10,0,1)) # include original observations
  test2 <- t.test(condition1, condition2, var.equal=TRUE)
  # If this t-test significant at pval level, return 1
  if (test2$p.value < pval){
    return (1)
  } else { # Otherwise, no tests were significant, return 0
    return (0)
  }
}

# Now, run 15,000 experiments of the format in Situation B as done in
# Simons et al. (2011) then determine the percent with "significant" results
simulation.p10 <- replicate(15000,SituationB(pval=0.10)) # p-value = .1
percent.signif.p10 <- mean(simulation.p10)
simulation.p05 <- replicate(15000,SituationB(pval=0.05)) # p-value = .05
percent.signif.p05 <- mean(simulation.p05)
simulation.p01 <- replicate(15000,SituationB(pval=0.01)) # p-value = .01
percent.signif.p01 <- mean(simulation.p01)