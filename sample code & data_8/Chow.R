#Chow test

rm(list=ls()) 
library(haven)
Data <-read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/8 Decompositions/wage1.dta")

#perform Chow-test for equality of population regression functions between males and females

Data$femeduc<-Data$female*Data$educ
Data$femexper<-Data$female*Data$exper
Data$femtenure<-Data$female*Data$tenure

reg1<-lm(lwage ~ female + educ + femeduc + exper + femexper + tenure + femtenure, data=Data) 
urss<-sum(resid(reg1)^2) 

reg2<-lm(lwage ~ educ + exper + tenure, data=Data)
rrss<-sum(resid(reg2)^2) 

n<-nrow(Data)
df1<-4
df2<-n-2*(3+1)
F_stat<-((rrss - urss)/df1)/((urss)/df2)
F_stat

#p-value for F-test
pvalue<-pf(F_stat,df1,df2,lower.tail = F)
pvalue

#or simply
library(car)
linearHypothesis(reg1, c("female=femeduc","femeduc=femexper","femexper=femtenure","femtenure=0"))


#version that avoids interaction terms
regA<-lm(lwage ~educ + exper + tenure, data=subset(Data, female==0))
rss_0<-sum(resid(regA)^2) 

regB<-lm(lwage ~educ + exper + tenure, data=subset(Data, female==1))
rss_1<-sum(resid(regB)^2) 

F_stat2<-((rrss - (rss_0+rss_1))/df1)/((rss_0+rss_1)/df2)
F_stat2

#p-value for F-test
pvalue2<-pf(F_stat2,df1,df2,lower.tail = F)
pvalue2

