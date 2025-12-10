# F test code
# compute the F value with 2 restrictions (q=2)


rm(list=ls())

# load data
library(haven)
gpa1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/1 linear regression/GPA1.DTA")

# Unrestricted model
reg1 <- lm(colGPA ~ hsGPA + ACT + skipped + male, data=gpa1)
summary(reg1)

urss <- sum(resid(reg1)^2) # sum of squared residuals of the Unrestricted model
# alternative 1: urss <- deviance(reg1)
# alternative 2: urss <- anova(reg1)["Residuals", "Sum Sq"]

# Restricted model
reg2 <- lm(colGPA ~ hsGPA + ACT, data=gpa1)
summary(reg2)

rrss <- sum(resid(reg2)^2) # sum of squared residuals of the Restricted model

df<- reg1$df.residual # degrees of freedom of the unrestricted model = n - k_unres - 1

# compute the F statistic
F_stat <- ((rrss - urss)/2)/((urss)/(df))

# p-value for F-test
p_value <- pf(F_stat,2,df,lower.tail = F)


####
#Note that the code above perform the same test as the following built-in command:
linearHypothesis(reg1, c("skipped=male","skipped=0"))


