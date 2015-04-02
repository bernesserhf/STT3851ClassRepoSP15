# 3/31/15#

choose(4,2)
choose(5,2)
choose(6,2)
combn(1:4,2)

library (MASS)
str(quine)

#Chi square test is between two CATEGORICAL variable

#Null Hypothesis:  MuF0 = MuF1 = MuF2 = MuF3

summary(aov(Days ~ Age, data = quine))
# p-value of 0.02, so we can reject the null, at least one of these is not equal to the others

TukeyHSD(aov(Days ~ Age, data = quine))
# The only significant one is F2-F1 which has a padj of 0.02

T1 <- xtabs(~Lrn + Age, data = quine)
T1
addmargins(T1)
#For when you are too lazy to add  ^-^

#P(AL) = 83/146   P(F0) = 27/146
# 83/146 * 27/146 does not equal 19/146
# Expected is (83/146)*(27/146)*146  and can be done for each one.  This was for top row, first column

(63/146) * (40/146) * 146  # Second row, 3rd column etc....

#chi square (observed - expected)^2 divided by expected
#(19-15.35)^2 / 15.35

chisq.test(T1)
# get a value of 42.71
# would be a right skewed graph
# 0.05 / 6 = 0.0083
# ^ alpha_B = alpha / # of comparisions (6) = .0083
# only significant if it is below .0083

T1[,c(1,2)] #first arguement for the row, blank for all rows, then the columns
chisq.test(T1[,c(1,2)]) #pvalue = 0.0039
chisq.test(T1[,c(1,3)]) #pvalue = 0.0283
chisq.test(T1[,c(1,4)]) #pvalue = 0.0029, warning if values are not at least 5 and we have a 0 in that column
chisq.test(T1[,c(2,3)]) #pvalue = 0.6263
chisq.test(T1[,c(2,4)]) #pvalue = 6.015e-09
chisq.test(T1[,c(3,4)]) #pvalue = 2.208e-07


