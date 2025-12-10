### t-value example

rm(list=ls())

# load data
library(haven)
gpa1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2023/Lectures/1 linear regression/GPA1.DTA")


# estimate model:
reg1 <- lm(colGPA ~ hsGPA + ACT + skipped, data=gpa1)
summary(reg1)


# determine critical values of the normal distribution
# qnorm: quantile function of the normal distribution (default is standard normal). Also called "inverse cumulative distribution function"

# 5%
qnorm(0.975) 
# 1%
qnorm(0.995)


# determine value of the test statistic with null that parameter is zero:

coef_hsGPA<-summary(reg1)$coefficients[2,1]
se_hsGPA<-summary(reg1)$coefficients[2,2]
tvalue<-coef_hsGPA/se_hsGPA



