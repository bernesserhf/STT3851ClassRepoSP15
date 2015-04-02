library(MASS)
head(quine)
quine$SEX <- ifelse(quine$Sex=="F",TRUE,FALSE)
quine$AGE <- ifelse((quine$Age=="F0"|quine$Age=="F1"),TRUE,FALSE)
quine$COMP <- quine$SEX + quine$AGE
head(quine)
