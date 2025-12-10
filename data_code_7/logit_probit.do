clear

use MROZ.dta

*LPM
regress inlf nwifeinc educ exper expersq age kidslt6 kidsge6
regress inlf nwifeinc educ exper expersq age kidslt6 kidsge6, robust

predict fit

tab fit if fit<0
tab fit if fit>1

*compute percent correctly predicted
gen tinlf=0

replace tinlf=1 if fit>0.5

tab inlf tinlf, col

di (203+350)/753


*logit
logit inlf nwifeinc educ exper expersq age kidslt6 kidsge6

*probit
probit inlf nwifeinc educ exper expersq age kidslt6 kidsge6

*estimate marginal effects at mean
mfx compute, at(mean)

* you can specify specific values for several variables
mfx compute, at(mean, kidslt6=0)


*APE using margin
margins , dydx(kidslt6)

* This corresponds to multiplying the scale factor with the coefficient:
di 0.3*_b[kidslt6]

*APE for kidslt6 (change from 0 to 1) using step by step
*APE for discrete variable
generate double xb0 = _b[nwifeinc]*nwifeinc + _b[educ]*educ + _b[exper]*exper + _b[expersq]*expersq ///
+ _b[age]*age + _b[kidsge6]*kidsge6 + _b[_cons]

generate double pe = normal(xb0 + _b[kidslt6]) - normal(xb0)

qui su pe
*The mean is the APE. This method does not give a correct standard error.
di r(mean)


drop pe xb0

* take square of mean rather than mean of square
summ exper,meanonly

local exp2=r(mean)^2

mfx compute, at(mean expersq=`exp2')
