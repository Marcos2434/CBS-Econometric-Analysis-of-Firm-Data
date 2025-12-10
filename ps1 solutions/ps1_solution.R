# Solutions to Problem set 1

rm(list=ls())


###Solution to Problem 1

# load data
library(haven)
wage1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/Problem Sets/PS1/wage1.dta")

#a)

summary(wage1$educ)

sum(wage1$educ==0) # count number of people with 0 years of educ
sum(wage1$educ==18)

#b) 

mean(wage1$wage)

#c)

wage1$wage_2003=184/56.9*wage1$wage
summary(wage1$wage_2003)

#d)

sum(wage1$female==1) # count number of female
sum(wage1$female==0)

rm(list=ls())


###Solution to Problem 2

meap01 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/Problem Sets/PS1/meap01.dta")

#a) 
summary(meap01$math4)

#b)
numb_max<-sum(meap01$math4==100)

numb_max/length(meap01$dcode) # percentage of total schools

#c)
sum(meap01$math4==50)

#d)
mean(meap01$math4)
mean(meap01$read4)

#e)
cor(meap01$math4,meap01$read4) 

#f)
summary(meap01$exppp)
mean(meap01$exppp)
sd(meap01$exppp)

rm(list=ls())


###Solution to Problem 3

ceosal2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/Problem Sets/PS1/CEOSAL2.dta")

#a) 
reg1<-lm(lsalary ~ lsales + lmktval, data=ceosal2)
summary(reg1)

#b)
reg2<-lm(lsalary ~ lsales + lmktval + profits, data=ceosal2)
summary(reg2)

#c)
reg3<-lm(lsalary ~ lsales + lmktval + profits + ceoten, data=ceosal2)
summary(reg3)

#d)
cor(ceosal2$lmktval,ceosal2$profits)

rm(list=ls())


###Solution to Problem 4

bwght2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/Problem Sets/PS1/BWGHT2.dta")

#a) 
reg1<-lm(lbwght ~ cigs + npvis, data=bwght2)
summary(reg1)

#b)
sd_npvis<-sd(bwght2$npvis,na.rm = T)
b_npvis<-summary(reg1)$coefficients[3,1]

sd_npvis*b_npvis

#c)
reg2<-lm(lbwght ~ cigs, data=bwght2)
summary(reg2)

#d)
cor(bwght2$npvis, bwght2$cigs, use = "na.or.complete")  

#e)
reg3<-lm(lbwght ~ cigs + npvis + mage + meduc + fage + feduc, data=bwght2)
summary(reg3)


rm(list=ls())


