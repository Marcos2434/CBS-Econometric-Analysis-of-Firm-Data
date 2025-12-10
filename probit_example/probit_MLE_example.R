# MLE for probit model

rm(list=ls()) 
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/7 LDV/401ksubs.dta")

# dependent variable e401k
#  =1 if individual is eligible for a pension scheme
#  =0 otherwise

probit<-glm(e401k ~ inc + marr + male + age, family = binomial(link=probit), data=Data)
summary(probit)


### Use ML estimation method for Probit model
X<-as.matrix(cbind(1,Data[,2:5]))
colnames(X)<-c("constant","inc","marr","male","age")
Y<-as.matrix(Data$e401k)

#write probit negative log-likelihood
probit.nll <- function (beta) {
  eta <- X %*% beta
  p <- pnorm(eta)
   -sum((1 - Y) * log(1 - p) + Y * log(p))  # negative log-likelihood
}


# gradient of the log-likelihood can be provided to the optim() function, otherwise an approximation will be computed
probit.gr <- function (beta) {
  eta <- X %*% beta # nx1
  p <- pnorm(eta)
  u <- dnorm(eta) * (Y - p) / (p * (1 - p)) # chain rule 
  -crossprod(X, u) # gradient: kx1
}


b0<-rep(0,ncol(X)) # initial values
fit <- optim(b0, probit.nll, gr = probit.gr, method = "BFGS", hessian = TRUE) # BFGS algorithm is a quasi-Newton optimization method

beta<-round(fit$par,6)
se<-round(sqrt(diag(solve(fit$hessian))),6) # obtain standard errors from the Hessian
z_value<-round(beta/se,6)
pvalue<-round(2*pnorm(abs(z_value),lower.tail = F),6)

data.frame(colnames(X),beta,se,z_value,pvalue) # output



