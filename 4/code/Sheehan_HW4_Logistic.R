# Author: Patrick Sheehan
# Purpose: Extend Gerfin (1996)   
# Input: None
# Output(s): Table I model logistic regression with logit link

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
# Run probit regression
Table.I <- glm(participation ~ AGE + AGESQ + EDUC + NYC + NOC + NLINC + FOREIGN,
               family = binomial(link = "logit"))
# require(xtable) # code for LaTeX table
# print.xtable(xtable(summary(Table.I)))

# Now plot participation vs. age holding the others at their means
pdf("probability_vs_age.pdf")
curve(1/(1+exp(-(coef(Table.I)[1] +
                   coef(Table.I)[2]*x +
                   coef(Table.I)[3]*x^2 +
                   coef(Table.I)[4]*mean(EDUC) +
                   coef(Table.I)[5]*mean(NYC) +
                   coef(Table.I)[6]*mean(NOC) +
                   coef(Table.I)[7]*mean(NLINC) +
                   coef(Table.I)[8]*0))),
      main="Probability of labor force participation vs. Respondent age",
      xlab="Respondent age (decades)",
      ylab="Probability of labor force participation",
      xlim=c(0,8),
      add=NA)
dev.off()
