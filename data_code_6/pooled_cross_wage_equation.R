# code for estimating pooled cross section wage equation

rm(list=ls())

# load data
library(haven)
CPS78_85 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/CPS78_85.dta")

# browse data for lwage, y85, educ, y85educ, exper, expersq, union, female and y85fem
myvars <- c("lwage", "y85", "educ", "y85educ", "exper", "expersq", "union", "female", "y85fem")
CPS78_85new <- CPS78_85[myvars]
View(CPS78_85new)

s.lm.1<-summary(lm.1<-lm(lwage ~ y85 + educ + y85educ + exper + expersq + union + female + y85fem, data=CPS78_85))
s.lm.1

# calculating tvalue for y85fem
s.lm.1$coefficients
tvalue<-s.lm.1$coefficient[9,1]/s.lm.1$coefficient[9,2]
tvalue

# p-value
pt(tvalue, 1075, lower=FALSE)


