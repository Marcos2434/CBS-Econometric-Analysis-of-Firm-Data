# DV estimation with R

rm(list=ls())

# open data
library(haven)
JTRAIN1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/JTRAIN1.dta")

library(plm) # panel data package
library(lmtest)

lmDV<-lm(lscrap ~ -1 + d88 + d89 + grant + grant_1 + as.factor(fcode), data = JTRAIN1) # remove constant; as.factor creates dummies for firms
summary(lmDV)
