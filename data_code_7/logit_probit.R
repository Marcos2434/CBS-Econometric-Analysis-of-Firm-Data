# Logit - Probit

rm(list=ls()) 

# open data
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/7 LDV/MROZ.dta")


#LPM
reg1<-lm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, data=Data)
summary(reg1)

library(plm)
coeftest(reg1, vcov = vcovHC(reg1, type = "HC1")) # robust standard errors

Data$fit<-fitted(reg1)
sum(Data$fit<0)
sum(Data$fit>1)

as.data.frame(table(Data$fit[Data$fit<0]))
as.data.frame(table(Data$fit[Data$fit>1]))

# compute percent correctly predicted
Data$tinlf<-as.numeric(Data$fit>0.5)

as.data.frame(table(Data$inlf,Data$tinlf,dnn = list("inlf","tinlf")))

perc.correct.pred<-(203+350)/753


#logit model
logit<-glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, family = binomial(link=logit), data=Data)
summary(logit)

logLik(logit) # Log Likelihood value
pseudoR2_logit<-1-logit$deviance/logit$null.deviance
pseudoR2_logit  # McFadden (1974) Pseudo R-squared
library(lmtest)
lrtest(logit) # LR test for overall significance (as in STATA output)

#probit model
probit<-glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, family = binomial(link=probit), data=Data)
summary(probit)

logLik(probit) # Log Likelihood value
pseudoR2_probit<-1-probit$deviance/probit$null.deviance
pseudoR2_probit  # McFadden (1974) Pseudo R-squared
lrtest(probit) # LR test for overall significance (as in STATA output)


#estimate marginal effects at mean
library(mfx)
logitmfx(logit, atmean=T, data = Data) # PEA: Partial Effects at the Average
probitmfx(probit, atmean=T, data = Data) # PEA: Partial Effects at the Average
#alternatively:
library(margins)
lev<-data.frame(mean(Data$nwifeinc),mean(Data$educ),mean(Data$exper),mean(Data$expersq),mean(Data$age),mean(Data$kidslt6),mean(Data$kidsge6))
names(lev)<-c("nwifeinc","educ","exper","expersq","age","kidslt6","kidsge6")
margins(logit, at=lev)
margins(probit, at=lev)


#you can specify specific values for several variables
margins(probit) # without specifying any regressor level it gives the APE: Average Partial Effects
margins(probit, at=list(kidslt6=0)) # if we specify the level of only 1 regressor it is not clear what is the assumed level of other regressors (they are not the sample means as in STATA)
# Then specify levels for all regressors:
lev<-data.frame(mean(Data$nwifeinc),mean(Data$educ),mean(Data$exper),mean(Data$expersq),mean(Data$age),0,mean(Data$kidsge6))
names(lev)<-c("nwifeinc","educ","exper","expersq","age","kidslt6","kidsge6")
margins(probit, at=lev)
margins(logit, at=lev)

#APE
logitmfx(logit, atmean=F, data = Data) # APE: Average Partial Effects
probitmfx(probit, atmean=F, data = Data) # APE: Average Partial Effects

#This corresponds to multiplying the estimated coefficient by the scale factor:
xb.logit<-predict(logit)
xb.probit<-predict(probit)
factor.log<-mean(dlogis(xb.logit)) #scale factor in APE formula for logit
factor.prob<-mean(dnorm(xb.probit)) #scale factor in APE formula for probit

factor.log*logit$coefficients # APEs for logit
factor.prob*probit$coefficients # APEs for probit


#APE for kidslt6 (change from 0 to 1) using step by step
#APE for discrete variable
xb0<-probit$coefficients[1]+probit$coefficients[2]*Data$nwifeinc+probit$coefficients[3]*Data$educ+probit$coefficients[4]*Data$exper+probit$coefficients[5]*Data$expersq+probit$coefficients[6]*Data$age+probit$coefficients[8]*Data$kidsge6
pe<-pnorm(xb0+probit$coefficients[7])-pnorm(xb0)
#The mean is the APE. This method does not give a correct standard error:
mean(pe)

#take square of mean rather than mean of square
lev<-data.frame(mean(Data$nwifeinc),mean(Data$educ),mean(Data$exper),mean(Data$exper)^2,mean(Data$age),mean(Data$kidslt6),mean(Data$kidsge6))
names(lev)<-c("nwifeinc","educ","exper","expersq","age","kidslt6","kidsge6")
margins(probit, at=lev)

