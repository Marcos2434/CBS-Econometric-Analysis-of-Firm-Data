# Poisson regression

rm(list=ls()) 
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/7 LDV/failure.dta")


#distribution of dependent variable
table1 <- list(count=table(Data$failure),percent=round(as.numeric(table(Data$failure)/length(Data$failure)),3))
as.data.frame(table1)

hist(Data$failure)


#OLS
reg1<-lm(failure ~ age + price + tech_issue + train, data=Data)
summary(reg1)

coeftest(reg1,vcov=vcovHC(reg1,type = "HC1")) #robust standard errors

#Poisson regression
poisson<-glm(failure ~ age + price + tech_issue + train, family = poisson(link=log), data=Data)
summary(poisson)

logLik(poisson) # Log Likelihood value
pseudoR2_poisson<-1-poisson$deviance/poisson$null.deviance
pseudoR2_poisson  # McFadden (1974) Pseudo R-squared
library(lmtest)
lrtest(poisson) # LR test for overall significance (as in STATA output)


#APE: mean of dep variable x Beta
round(mean(Data$failure)*poisson$coefficients,5) # to compare with OLS estimates

#prediction
Data$fit<-predict(poisson,type="response")

# Adjusted variance
Data$resid2<-(Data$failure-Data$fit)^2
h<-Data$resid2/Data$fit
df=nrow(Data)-length(poisson$coefficients)
hsigma<-sqrt(sum(h)/df)
#the following coefficient has to be multiplied by the usual MLE SE to obtain QMLE SE
hsigma

hsigma*coeftest(poisson)[,2]

#alternatively, compute robust s.e. (more general, produces similar result)
coeftest(poisson, vcov=vcovHC(poisson, type = "HC0"))


########################
#negative binomial regression: estimates an additional parameter of the variance function
nbinom<-glm.nb(failure ~ age + price + tech_issue + train, data=Data)
summary(nbinom)
# theta is the parameter of the Gamma distribution used to model overdispersion. This correspond to 1/alpha in STATA output

logLik(nbinom) # Log Likelihood value
pseudoR2_nbinom<-1-nbinom$deviance/nbinom$null.deviance
pseudoR2_nbinom  # McFadden (1974) Pseudo R-squared
library(lmtest)
lrtest(nbinom) # LR test for overall significance (as in STATA output)

