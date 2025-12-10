# OLS, FD, FE, DV & RE estimation in R

rm(list=ls())

# load data
library(haven)
WAGEPAN <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/WAGEPAN.dta")

# define cross-section and time-series dimension:
# unitid: nr
# period: year

# set data as panel data:
library(plm) # linear models for panel data
library(lmtest)
pdata <- pdata.frame(WAGEPAN, index=c("nr", "year"))

# OLS estimator
OLS <- lm(lwage~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata)
summary(OLS)

# OLS estimator, robust standard errors
# different commands yield different results, therefore a few suggestions:
coeftest(OLS, vcov = vcovHC(OLS, type = "HC1")) 
#or
coeftest(OLS, vcovHC)

# pooled OLS estimator (same results as OLS estimator above)
pooling <- plm(lwage~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata, model= "pooling")
summary(pooling)


# FD regression

#difference the data
variables<-data.frame(pdata$nr,pdata$lwage,pdata$educ,pdata$black,pdata$hisp,pdata$exper,pdata$expersq,pdata$married,pdata$union)

library(dplyr)
pdata2<-variables %>%
  group_by(pdata.nr) %>%
  mutate_at(vars(pdata.lwage:pdata.union), funs(.-lag(.)))

pdata2<-data.frame(pdata2,pdata[,37:43])
colnames(pdata2)<-c("nr","dlwage","deduc","dblack","dhisp","dexper","dexpersq","dmarried","dunion","d81","d82","d83","d84","d85","d86","d87")

#estimate model in first differences
lm.FD<-lm(dlwage~ deduc + dblack + dhisp + dexpersq + dmarried + dunion +  d82 + d83+ d84 + d85 +d86 + d87, data = pdata2)
summary(lm.FD) 


# fixed effect (FE) estimator
# note that exper is not included because after transforming the data and including year dummies there is not variation left
fixed <- plm(lwage~ educ + black + hisp + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata, model= "within")
summary(fixed) # note that educ, black and hisp are authomatically omitted. Also the constant cannot be estimated and it is not reported (STATA reports
# the average of individual effects, computed from post-estimation results)


# DV (dummy variable) regression
# alternative way to find FE parameter estimates to the use of within transformation
# time-invariant covariates have to be excluded as in FE estimator
# similarly to FE estimator, in the following specification the coefficient on exper is not identifiable
options(max.print=100000)
dummy <- lm(lwage~ expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87 + factor(nr), data=pdata)
summary(dummy)


# random effects (RE) estimator
random <- plm(lwage~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata, model= "random")
summary(random)

# Table of selected results:
library("stargazer")
stargazer(OLS,fixed,dummy, random, type="text",
          column.labels=c("OLS", "FE","Dummy", "RE"), keep.stat=c("n","rsq"),
          keep=c("ed","bl","hi","exp","mar","un"))




           