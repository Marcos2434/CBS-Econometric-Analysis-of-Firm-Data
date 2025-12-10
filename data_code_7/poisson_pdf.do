*computes poisson probability mass function for different values of the parameter lambda

clear
set obs 21

*generate grid on outcome support
gen y=_n-1

local lambda=1

gen f_`lambda'=(`lambda'^y*exp(-`lambda'))/exp(lnfactorial(y))
*remark: exp(lnfactorial(y)) gives the factorial of y, i.e. y! 

local lambda=4

gen f_`lambda'=(`lambda'^y*exp(-`lambda'))/exp(lnfactorial(y))

local lambda=10

gen f_`lambda'=(`lambda'^y*exp(-`lambda'))/exp(lnfactorial(y))

twoway (connected f_1 y) (connected f_4 y) (connected f_10 y), title(Poisson Probability mass function)
