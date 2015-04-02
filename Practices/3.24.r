# Ho : Mw - Mm = 0
# Ha : Mw - Mm < 0

gender <- c(rep("Female", 5), rep("Male", 5))
gender
Height <- c(68,61,63,67,64,72,72,74,74,70)
Height
DF <- data.frame(Height,gender)
head(DF)
rm(Height,gender)

tapply(DF$Height,DF$gender,mean)
64.6 - 72.4   #mean of females minus mean of men

sims <- 10000
meandiff <- numeric(sims)
set.seed(123)
ANS <- tapply(DF$Height,DF$gender,mean)
ANS
OBSDIFF <- ANS[1] - ANS[2]
OBSDIFF

for(i in 1:sims) {
  ANS <- tapply(DF$Height,
                sample(DF$gender),mean)
  meandiff[i] <- ANS[1] - ANS[2]
}

hist(meandiff)

pvalue <- (sum(meandiff <= OBSDIFF)+1)/(sims+1)
pvalue  #alpha = 0.05

t.test(Height~gender, data = DF, a = "l")
# looking at (xbar - ybar) - (Mux - Muy) / sqrt(S^2x / nx) + (S^2y / ny)
pt(-5.235,6.423)
# gives the value to the left of the 't = -5.235

t.test(Height~gender, data=DF, var.equal=TRUE)
#pvalue = 0.00079
summary(aov(Height~gender, data=DF))
#pvaule = 0.00079 two sample t test gives the same as analysis of varience
#need to aov for 2 or more variable
# t value from t test squared is the same as f value on aov
# -5.235^2 = 27.41
mod1 <- aov(Height~gender, data=DF)

TukeyHSD(mod1)
