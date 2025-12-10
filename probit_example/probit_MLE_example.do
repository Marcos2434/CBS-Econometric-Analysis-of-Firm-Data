clear all
use 401ksubs.dta


* dependent variable e401k
* =1 if individual is eligible for a pension scheme
* =0 otherwise

/****************************/
/*    start with probit     */
/****************************/

probit e401k inc marr male age

/****************************/
/*    MLE: probit regression */
/****************************/

* drops the programme "mlr_mle"
capture program drop probit_mle
*define the programme "mlr_mle"
program probit_mle
  *these are the arguments of the log-likelihood: (log(f),x*beta)
  args logf xb 
  *stata internally handels the dependent variable as a golobal "$ML_y1".
  *for convenience we call it y.
  local y "$ML_y1"
  *this specifies the conditonal log density of y given x.
  *log of standard normal cdf [N(0,1)] evaluated at xb or log of (1 - the same).
  quietly replace `logf' = ln(  normal(`xb')) if `y'==1
  quietly replace `logf' = ln(1-normal(`xb')) if `y'==0
end 

 *These lines execute the MLE
 ml model lf probit_mle (xb: e401k = inc marr male age)
 ml maximize
 
 