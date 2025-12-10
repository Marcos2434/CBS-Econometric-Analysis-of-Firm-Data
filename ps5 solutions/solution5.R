# Solutions to Problem set 5

rm(list=ls())



###Solution to Problem 1

# load data
library(haven)
motion <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2024/Problem Sets/PS5/motion.dta")

#declare panel data structure
library(plm)
pdata <- pdata.frame(motion, index=c("id", "period"))
pdim(pdata)

#b)
###########################################
  #frontalasymmetry
#POLS
pols_frontalas <- lm(frontalasymmetry ~ -1 + female + as.matrix(pdata[,13:23]) + as.matrix(pdata[,24:67]), data=pdata)
summary(pols_frontalas)

library(lmtest)
coeftest(pols_frontalas, vcov=vcovCR(pols_frontalas,type="CR1S",cluster=pdata$id)) # same s.e. as in Stata

###########################################
  #joy
#POLS
pols_joy <- lm(joy ~ -1 + female + as.matrix(pdata[,13:23]) + as.matrix(pdata[,24:67]), data=pdata)
summary(pols_joy)

coeftest(pols_joy, vcov=vcovCR(pols_joy,type="CR1S",cluster=pdata$id)) # same s.e. as in Stata

###########################################
 #browfurrow
#POLS
pols_brow <- lm(browfurrow  ~ -1 + female + as.matrix(pdata[,13:23]) + as.matrix(pdata[,24:67]), data=pdata)
summary(pols_brow)

coeftest(pols_brow, vcov=vcovCR(pols_brow,type="CR1S",cluster=pdata$id)) # same s.e. as in Stata


#c)
#FD
#difference the data
variables<-data.frame(pdata$id,pdata$female,pdata$browfurrow,pdata$joy,pdata$frontalasymmetry,pdata[,13:23],pdata[,24:67])

library(dplyr)
pdata2<-variables %>%
  group_by(pdata.id) %>%
  mutate_at(vars(pdata.browfurrow:logo4_dur_10s), funs(.-lag(.)))

### frontal asymmetry
FD_frontalas <- lm(pdata.frontalasymmetry  ~ -1 + as.matrix(pdata2[,6:16]) + as.matrix(pdata2[,17:60]), data=pdata2)
summary(FD_frontalas)

# Note that R and STATA automatically exclude one variable because of collinearity. To obtain the same coefficients estimates 
# the excluded regressor has to be same. E.g. in STATA D.dur_6s is excluded, then do the same in R: 
FD_frontalas <- lm(pdata.frontalasymmetry  ~ -1 + as.matrix(pdata2[,6:11]) + as.matrix(pdata2[,13:16]) + as.matrix(pdata2[,17:60]), data=pdata2)
summary(FD_frontalas) # same coefficients estimates as in STATA


#males
pdata2_m<-subset(pdata2,pdata2$pdata.female==0)
FD_frontalas_m<-lm(pdata.frontalasymmetry  ~ -1 + as.matrix(pdata2_m[,6:16]) + as.matrix(pdata2_m[,17:60]), data=pdata2_m)
summary(FD_frontalas_m) # 3 omissions because of collinearity. Same remark as above

#females
pdata2_f<-subset(pdata2,pdata2$pdata.female==1)
FD_frontalas_f<-lm(pdata.frontalasymmetry  ~ -1 + as.matrix(pdata2_f[,6:16]) + as.matrix(pdata2_f[,17:60]), data=pdata2_f)
summary(FD_frontalas_f) # 3 omissions because of collinearity. Same remark as above


### joy
FD_joy <- lm(pdata.joy  ~ -1 + as.matrix(pdata2[,6:16]) + as.matrix(pdata2[,17:60]), data=pdata2)
summary(FD_joy) # 1 omission because of collinearity


#males
FD_joy_m<-lm(pdata.joy  ~ -1 + as.matrix(pdata2_m[,6:16]) + as.matrix(pdata2_m[,17:60]), data=pdata2_m)
summary(FD_joy_m) # 3 omissions because of collinearity. Same remark as above

#females
FD_joy_f<-lm(pdata.joy  ~ -1 + as.matrix(pdata2_f[,6:16]) + as.matrix(pdata2_f[,17:60]), data=pdata2_f)
summary(FD_joy_f) # 3 omissions because of collinearity


### browfurrow
FD_browfurr <- lm(pdata.browfurrow  ~ -1 + as.matrix(pdata2[,6:16]) + as.matrix(pdata2[,17:60]), data=pdata2)
summary(FD_browfurr)

#males
FD_browfurr_m<-lm(pdata.browfurrow  ~ -1 + as.matrix(pdata2_m[,6:16]) + as.matrix(pdata2_m[,17:60]), data=pdata2_m)
summary(FD_browfurr_m) # 3 omissions because of collinearity. Same remark as above

#females
FD_browfurr_f<-lm(pdata.browfurrow  ~ -1 + as.matrix(pdata2_f[,6:16]) + as.matrix(pdata2_f[,17:60]), data=pdata2_f)
summary(FD_browfurr_f) # 3 omissions because of collinearity


rm(list=ls())




###Solution to Problem 2

gpa3 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2024/Problem Sets/PS5/GPA3.DTA")


#a)
reg1 <- lm(trmgpa ~ spring + sat + hsperc + female + black + white + frstsem + tothrs + crsgpa + season, data=gpa3)
summary(reg1)

#c)
#create the missing differenced variable:
variables<-data.frame(gpa3$id,gpa3$frstsem)

D<-variables %>%
  group_by(gpa3.id) %>%
  mutate_at(vars(gpa3.frstsem), funs(.-lag(.)))

names(D)[2]<-"cfrstsem"
gpa3[,24]<-D[,2]

reg2 <- lm(ctrmgpa ~ cfrstsem + ctothrs  +  ccrsgpa + cseason, data=gpa3)
summary(reg2)


rm(list=ls())




###Solution to Problem 3

airfare <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2024/Problem Sets/PS5/airfare.dta")

#a)
#pooled OLS
reg1 <- lm(lfare ~ y98 + y99 + y00 + concen + ldist + ldistsq, data=airfare)
summary(reg1)


#b)
#pooled OLS with robust SE
coeftest(reg1, vcov=vcovHC(reg1,type="HC1"))

#pooled OLS with "fully" robust SE 
library(clubSandwich)
coeftest(reg1, vcov=vcovCR(reg1,type="CR1S",cluster = airfare$id)) # CR1S gives same dof correction as in Stata


#c)
#log(dist) turning point
-coef(reg1)[6]/(2*coef(reg1)[7])

#dist turning point
exp(-coef(reg1)[6]/(2*coef(reg1)[7]))

summary(airfare["dist"])


#d)
airfare <- pdata.frame(airfare, index=c("id", "year"))
reg_FE<-plm(lfare ~ y98 + y99 + y00 + concen + ldist + ldistsq, data=airfare, model = "within")
summary(reg_FE)



rm(list=ls())
