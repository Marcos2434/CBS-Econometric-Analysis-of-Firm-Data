# DID estimator

rm(list=ls())

# load data
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/sales.dta")


# regression for y2014
lm1<-lm(salespc ~ urban, data=subset(Data, y2014==1))
summary(lm1)

gamma14<-summary(lm1)$coef[2,1]


# regression for y2013
lm2<-lm(salespc ~ urban, data=subset(Data, y2014==0))
summary(lm2)

gamma13<-summary(lm2)$coef[2,1]


#DID estimator
did<-gamma14-gamma13


#direct estimation of the did effect (campaign is a dummy that takes value 1 if y2014=1 and urban=1)
lmDID<-lm(salespc ~ y2014+urban+campaign, data=Data)
summary(lmDID)


##other model
lmDID2<-lm(salespc ~ y2014 + urban + campaign + age + agesq + inc + incsq + marr + fsize, data=Data)
summary(lmDID2)

