# Author: Patrick Sheehan
# Purpose: Extension of Goldstein et al. (1993)     
# Input: None
# Output(s): Histograms and tables (for factor variables)

require(mlmRev) # Require data package
data("Exam", package = "mlmRev") # Get the data
# Get the number of students in each school
student.counts <- sapply(unique(Exam$school), function(x) sum(Exam$school == x))
# Get the school average LRT intake score
if(length(unique(Exam$school)) == length(unique(Exam$schavg)))
  school.LRT <- unique(Exam$schavg) # if each school has a unique avg LRT score
pdf("histogram_%d.pdf", onefile=FALSE)
hist(student.counts,
     breaks="Scott",
     main="Histogram of number of students in a school",
     xlab="Number of students",
     col="gray")
hist(Exam$normexam,
     breaks="Scott",
     main="Histogram of normalized exam score",
     xlab="Normalized exam score",
     col="gray")
hist(school.LRT,
     breaks="Scott",
     main="Histogram of average school LRT scores",
     xlab="Average school LRT intake score",
     col="gray")
hist(Exam$standLRT,
     breaks="Scott",
     main="Histogram of normalized exam score",
     xlab="Normalized exam score",
     col="gray")
dev.off()

# Now get the table values
school.gender <- table(Exam$schgend)
student.VR <- table(Exam$vr)
student.LRT <- table(Exam$intake)
student.gender <- table(Exam$sex)
school.type <- table(Exam$type)

