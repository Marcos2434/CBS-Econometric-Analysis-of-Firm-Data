# Ordered Probit

rm(list=ls()) 

# open data
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/7 LDV/pension.dta")

#OLS
reg1<-lm(pctstck ~ choice + age + educ + female + black + married + finc25 + finc35 + finc50 + finc75 + finc100 + finc101 + wealth89 + prftshr, data=Data) 
summary(reg1)

#ordered probit
library(MASS)
oprobit<-polr(factor(pctstck) ~ choice + age + educ + female + black + married + finc25 + finc35 + finc50 + finc75 + finc100 + finc101 + wealth89 + prftshr, data=Data, method = "probit")
summary(oprobit)


#compute fitted values for all observations
Data$fit<-predict(oprobit)
as.data.frame(table(Data$pctstck,Data$fit,dnn = list("pctstck","fit")))

#compute % correctly predicted
#PCP overall
(33+31+22)/194

#PCP mostly bonds
33/64

#PCP mixed
31/72

#PCP mostly stocks
22/58
