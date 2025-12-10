# Solutions to Problem set 4

rm(list=ls())


###Solution to Problem 1

# load data
library(haven)
smoke <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2023/Problem Sets/PS4/SMOKE.DTA")


#d)
reg1<-lm(cigs ~ educ + age + agesq + lcigpric + restaurn, data=smoke)
summary(reg1)

library(car)
linearHypothesis(reg1, c("lcigpric=restaurn","restaurn=0"))


#e)
library(AER)

reg_IV<-ivreg(lincome~cigs + educ +  age + agesq|lcigpric + restaurn  + educ + age + agesq, data=smoke)
summary(reg_IV)

reg2<-lm(lincome ~ cigs + educ + age + agesq, data=smoke)
summary(reg2)


rm(list=ls())



###Solution to Problem 2

airfare <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Problem Sets/PS4/airfare.dta")
airfare97<-subset(airfare,year==1997)


#b)
reg1<-lm(lpassen ~ lfare + ldist + ldistsq, data=airfare97)
summary(reg1)


#d)
reg2<-lm(lfare ~ concen + ldist + ldistsq, data=airfare97)
summary(reg2)


#e)
reg_IV<-ivreg(lpassen ~ lfare + ldist + ldistsq |concen + ldist + ldistsq, data=airfare97)
summary(reg_IV)


#f)
#minimum in ldist
coef(reg_IV)[3]/(-2*coef(reg_IV)[4])

#minimum in dist
exp(coef(reg_IV)[3]/(-2*coef(reg_IV)[4]))


sum(airfare97$dist<336)

sum(airfare97$dist<336)/nrow(airfare97)


rm(list=ls())



###Solution to Problem 3

kielmc <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Problem Sets/PS4/KIELMC.DTA")


#b)
reg1<-lm(lprice ~ y81 + ldist + y81ldist, data=kielmc)
summary(reg1)


#c)
reg2<-lm(lprice ~ y81 + ldist + y81ldist + age + agesq + rooms + baths + lintst + lland + larea, data=kielmc)
summary(reg2)

tvalue <- 1.24 # t value for y81ldist

pvalue<-pt(tvalue, 310, lower.tail = F)
pvalue # pvalue for H1: y81*ldist>0 


rm(list=ls())


