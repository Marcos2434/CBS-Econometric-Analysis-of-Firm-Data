# Test linear restrictions example

rm(list=ls())

# load data
library(haven)
gpa1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/1 linear regression/GPA1.DTA")

# estimate model:
reg1 <- lm(colGPA ~ hsGPA + ACT + skipped, data=gpa1)
summary(reg1)


# test for various linear restrictions
library(car)

linearHypothesis(reg1, "hsGPA = 0")

linearHypothesis(reg1, "hsGPA = 0.5")

linearHypothesis(reg1, "hsGPA = ACT")

linearHypothesis(reg1, c("hsGPA = ACT", "hsGPA= skipped", "hsGPA= 0"))  # note that this is equivalent to an F-test for the overal significance of the regeression


