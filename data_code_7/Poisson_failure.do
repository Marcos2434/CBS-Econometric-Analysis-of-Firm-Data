clear
use failure.dta


*distribution of dependent variable
tab failure

histogram failure, bin(5)


*OLS
reg failure age price tech_issue train

reg failure age price tech_issue train, robust

*Poisson MLE

poisson failure age price tech_issue train

*predict probabilities of events
predict p_0,pr(0)
predict p_1,pr(1)
predict p_2,pr(2)
predict p_3,pr(3)
predict p_4,pr(4)


*Poisson QMLE

glm failure age price tech_issue train, family(Poisson) link(log)

predict uhat, res
gen uhatsq=uhat^2
predict yhat

gen h=(uhatsq/yhat)

scalar df=_N-3

qui su h
scalar hsigma=sqrt(r(sum)/df)

* the following coefficient has to be multiplied by the usual MLE SE to obtain QMLE SE:
di hsigma

* alternatively, use the robust option (more general, produces similar result)
glm failure age price tech_issue train, family(Poisson) link(log) robust


************************************************************************************
*negative binomial regression: estimates additional parameter of the variance function
glm failure age price tech_issue train, fam(nb ml)
nbreg failure age price tech_issue train
