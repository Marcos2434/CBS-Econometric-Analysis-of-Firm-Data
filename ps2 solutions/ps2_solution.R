# Solutions to Problem set 2

rm(list=ls())


###Solution to Problem 2

# load data
library(haven)
hprice1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2023/Problem Sets/PS2/hprice1.dta")


#a) 
reg1<-lm(lprice ~ llotsize + lsqrft + bdrms, data=hprice1)
summary(reg1)

# regression with heteroscedasticity robust standard errors:
library(car)
library(lmtest)
coeftest(reg1, vcov=hccm(reg1, type="hc1"))


#b)

hprice1$resid<-reg1$residuals

hprice1$residsq<-hprice1$resid^2

hprice1$llotsizesq<-hprice1$llotsize^2
hprice1$lsqrftsq<-hprice1$lsqrft^2
hprice1$bdrmssq<-hprice1$bdrms^2
hprice1$llotsizelsqrft<-hprice1$llotsize*hprice1$lsqrft
hprice1$llotsizebdrms<-hprice1$llotsize*hprice1$bdrms
hprice1$lsqrftbdrms<-hprice1$lsqrft*hprice1$bdrms

reg_White<-lm(residsq ~ llotsize + lsqrft + bdrms + llotsizesq + lsqrftsq + bdrmssq + llotsizelsqrft + llotsizebdrms + lsqrftbdrms, data=hprice1)
output<-summary(reg_White)
output

F_stat<-output$fstatistic[1]

#The p-value is:
p_value<-pf(F_stat,9,78,lower.tail = F)
p_value


rm(list=ls())


###Solution to Problem 3

ceosal2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Problem Sets/PS2/ceosal2.dta")


#a)
reg1<-lm(lsalary ~ lsales + lmktval + profmarg + ceoten + comten, data=ceosal2)
summary(reg1)

#b)
reg2<-lm(lsalary ~ lsales + lmktval + profmarg + ceoten + comten + comtensq + ceotensq, data=ceosal2)
summary(reg2)

linearHypothesis(reg2, c("comtensq=ceotensq","ceotensq=0"))


#c)

ceosal2$yhat<-reg1$fitted.values

ceosal2$yhatsq<-yhat^2

ceosal2$yhatcub<-yhat^3

reg_RESET<-lm(lsalary ~ lsales + lmktval + profmarg + ceoten + comten + yhatsq + yhatcub, data=ceosal2)
summary(reg_RESET)

linearHypothesis(reg_RESET, c("yhatsq=yhatcub","yhatcub=0"))


rm(list=ls())



###Solution to Problem 4

gpa2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/Problem Sets/PS2/GPA2.dta")


#a)
reg1<-lm(sat ~ hsize + hsizesq, data=gpa2)
summary(reg1)


#b)
opt_size<-coef(reg1)[2]/(-2*coef(reg1)[3])
opt_size


#c)
gpa2$lsat<-log(gpa2$sat)

reg2<-lm(lsat ~ hsize + hsizesq, data=gpa2)
summary(reg2)

opt_size2<-coef(reg2)[2]/(-2*coef(reg2)[3])
opt_size2


#d)

gpa2$size<-gpa2$hsize*100
gpa2$sizesq<-gpa2$size^2

reg3<-lm(sat ~ size + sizesq, data=gpa2)
summary(reg3)

opt_size3<-coef(reg3)[2]/(-2*coef(reg3)[3])
opt_size3


rm(list=ls())

