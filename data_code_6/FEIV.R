# FEIV

rm(list=ls())

library(haven)
airfare <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/airfare.dta")



# This is to produce the results in Table 11.1, page 357, Wooldridge (2010)

# define cross-section and time-series dimension:
# unitid: id
# period: year

# set data as panel data:
library(plm)
pdata <- pdata.frame(airfare, index=c("id", "year"))


# Non IV methods:


# FE estimator
fixed <- plm(lpassen~ y98 + y99 + y00 + lfare + ldist + ldistsq, data=pdata, model= "within")
summary(fixed)


##### panel data IV methods:
install.packages("AER")
library(AER)
install.packages("systemfit")
library (systemfit)



#Fixed effects IV
FEIV <- plm(formula=lpassen~ y98 + y99 + y00 + ldist + ldistsq + lfare | y98 + y99 + y00 + ldist + ldistsq + concen, data = airfare, index = c("id", "year"), random.method = c("swar"), 
            inst.method = c("bvk"), model="within") 
summary(FEIV)


