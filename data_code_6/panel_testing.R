### Panel estimation and testing 

rm(list=ls())

library(haven)
wagepan <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2020/Lectures/6 Panel/wagepan.dta")


# define cross-section and time-series dimension:
# unitid: nr
# period: year

# set data as panel data:
library(plm)
pdata <- pdata.frame(wagepan, index=c("nr", "year"))


# Test for individual effects to be relevant for explaining variation in y
# model of interest
lm1 <- lm(lwage~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata)
summary(lm1)

fe <- plm(lwage~ educ + black + hisp + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata, model= "within")
summary(fe)

# dummy variable estimation
# create dummy variable for each individual
df<-data.frame(pdata$nr)

n <- nrow(df)
nlevels <- sapply(df, nlevels)
i <- rep(seq_len(n), ncol(df))
j <- unlist(lapply(df, as.integer)) +
  rep(cumsum(c(0, head(nlevels, -1))), each = n)
x <- 1

indic_matrix<-sparseMatrix(i = i, j = j, x = x)

options(max.print=100000)
dummy <- lm(lwage~ -1 + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87 + as.matrix(indic_matrix), data=pdata)
summary(dummy)


#########
 #F-test
########
urss <- sum(resid(dummy)^2)
df<- dummy$df.residual # degrees of freedom of the unrestricted model = n - k_unres - 1

rrss <- sum(resid(lm1)^2)
q <- 544 # number of restrictions

F_stat<-((rrss - urss)/q)/((urss)/(df))
F_stat

# p-value for F-test
p_value <- pf(F_stat,q,df,lower.tail = F)
p_value # The p-value for the F-test for joint significance of individual dummies

### an alternative F test for significance of individual effects, comparing the fixed effect estimator and the pooled ols:
pFtest(fe,lm1)


##############
  # Comparing FE vs RE estimators
##############
  
re <- plm(lwage~ educ + black + hisp + exper + expersq + married + union, data=pdata, model= "random")
summary(re)

phtest(fe, re, method="aux") # Hausman test with null that the 2 models are not statistically different (i.e. random effect is not inconsistent)
# method="aux" selects the auxiliary-regression-based version in Wooldridge (2010, Sec. 10.7.3.)


# when we also use period dummies:
re1 <- plm(lwage~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata, model= "random")
summary(re1)

phtest(fe, re1, method="aux")


# Sargan Test
# work in progress

# do it manually
# calculate time averages by unit for relevant variables
pdata$experbar = ave(pdata$exper, pdata$nr)
pdata$expersqbar = ave(pdata$expersq, pdata$nr)
pdata$marriedbar = ave(pdata$married, pdata$nr)
pdata$unionbar = ave(pdata$union, pdata$nr)

# multicollineariy between the experience related variables. One has to be dropped
random_ur <- plm(as.numeric(lwage)~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87 + expersqbar + marriedbar + unionbar, data=pdata, model= "random")
random_r <- plm(as.numeric(lwage)~ educ + black + hisp + exper + expersq + married + union + d81 + d82 + d83 + d84 + d85 + d86 + d87, data=pdata, model= "random")
waldtest(random_r, random_ur, test="Chisq")

