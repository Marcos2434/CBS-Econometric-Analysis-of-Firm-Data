# Solutions to Problem set 3

rm(list=ls())


###Solution to Problem 1

# load data
library(haven)
wage2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2024/Problem Sets/PS3/WAGE2.DTA")


#a)
#lecture's model (2)
reg1<-lm(lwage ~ educ + exper + tenure + married + south + urban + black + IQ, data=wage2)
summary(reg1)

# use KWW
reg2<-lm(lwage ~ educ + exper + tenure + married + south + urban + black + KWW, data=wage2)
summary(reg2)


#b)
reg3<-lm(lwage ~ educ + exper + tenure + married + south + urban + black + IQ + KWW, data=wage2)
summary(reg3)


#c)
library(car)
linearHypothesis(reg3, c("IQ=KWW","KWW=0"))


rm(list=ls())



###Solution to Problem 2

wage2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2024/Problem Sets/PS3/WAGE2.DTA")


#a)
library(AER)

reg_IV<-ivreg(lwage~educ|sibs ,data=wage2)
summary(reg_IV)

reg1<-lm(lwage ~ sibs, data=wage2)
summary(reg1)


#b)
reg2<-lm(educ ~ brthord, data=wage2)
summary(reg2)


#c)
reg_IV.2<-ivreg(lwage~educ|brthord ,data=wage2)
summary(reg_IV.2)


#d)
reg3<-lm(educ ~ sibs + brthord, data=wage2)
summary(reg3)


#e)
reg_IV.3<-ivreg(lwage~educ + sibs|brthord +sibs,data=wage2)
summary(reg_IV.3)


#f)
educhat<-reg3$fitted.values
sibs.naomit<-subset(wage2$sibs, !is.na(wage2$brthord)) #remove obs in sibs for which there are not fitted values (to compute correlation the two vectors need to have the same dimension)

cor(educhat, sibs.naomit)


rm(list=ls())




###Solution to Problem 3

Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2024/Problem Sets/PS3/401KSUBS.DTA")

#a)
reg1<-lm(pira ~ p401k + inc + incsq + age + agesq, data=Data)
summary(reg1)
coeftest(reg1, vcov=hccm(reg1, type="hc1"))


#d)
reg2<-lm(p401k ~ e401k + inc + incsq + age + agesq, data=Data)
summary(reg2)
coeftest(reg2, vcov=hccm(reg2, type="hc1"))


#e)
reg_IV<-ivreg(pira~p401k + inc + incsq + age + agesq|e401k + inc + incsq + age + agesq, data=Data)
summary(reg_IV)

#the diagonal elements are the se:
sqrt(vcovHC(reg_IV, type = "HC0"))

#alternatively: ivpack currently not supported by R
#library(ivpack)
#robust.se(reg_IV) #to obtain robust standard errors


#f)
Data$nuhat<-reg2$residuals

reg3<-lm(pira ~ p401k + inc + incsq + age + agesq + nuhat, data=Data)
summary(reg3)
coeftest(reg3, vcov=hccm(reg3, type="hc1"))


