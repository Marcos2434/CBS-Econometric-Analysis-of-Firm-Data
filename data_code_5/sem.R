rm(list=ls(all=TRUE))
library(foreign)
library(AER)

Data<-read.dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/5 SEM/MROZ.DTA")

Data_omit<-Data[1:428,]

# check identification conditions for labour supply equation:
# rank condition tested on reduced form equation for lwage
reg1<-lm(lwage~educ+age+kidslt6+nwifeinc+exper+expersq,data=Data)
summary(reg1)

# excluded exogenous variables from labour supply equation need to have nonzero coefficients:
linearHypothesis(reg1, c("exper = 0", "expersq= 0"))


# check identification conditions for wage offer equation:
# rank condition tested on reduced form equation for hours
reg2<-lm(hours~educ+age+kidslt6+nwifeinc+exper+expersq,data=Data)
summary(reg2)

# excluded exogenous variables from wage offer equation need to have nonzero coefficients:
linearHypothesis(reg2, c("age = 0", "kidslt6= 0", "nwifeinc= 0"))


# estimate the labour demand equation
reg_IV<-ivreg(lwage~ hours + educ + exper + expersq |.-hours +age + kidslt6 + nwifeinc, data=Data)
summary(reg_IV)


