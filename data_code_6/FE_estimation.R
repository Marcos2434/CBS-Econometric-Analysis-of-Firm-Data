# FE or "within" estimation with R

rm(list=ls())

# open data
library(haven)
JTRAIN1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/JTRAIN1.dta")

library(plm) # panel data package
library(lmtest)

# define cross-section and time-series dimension:
# unitid: fcode
# period: year

# set data as panel data:
pdata1 <- pdata.frame(JTRAIN1, index=c("fcode", "year"))

# FE estimation
# Instead of applying the within transformation to all variables and then running lm, we can simply use plm on the original data with the option: model = "within"
fixed <- plm(lscrap ~ d88 + d89 + grant + grant_1, data=pdata1, model= "within")
summary(fixed)
# note: STATA reports an intercept, which is rather the average of individual intercepts

# With robust/white standard errors
coeftest(fixed, vcov = vcovHC(fixed, type = "HC3"))

