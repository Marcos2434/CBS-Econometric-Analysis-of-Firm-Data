clear
cap log close
set more off

cd "H:\Teaching\CBS\AEF\KAN-CCMVV2401U\2017\Problem Sets\PS4"

log using solution4.log, text replace



***Solution to Problem 1


use SMOKE.DTA


* d)

regress cigs educ age agesq lcigpric restaurn

* e)

ivreg lincome ( cigs = lcigpric restaurn ) educ age agesq

regress lincome cigs educ age agesq


clear


***Solution to Problem 2

use airfare.dta

keep if year==1997

* b)

reg lpassen lfare ldist ldistsq

* d)

reg lfare concen ldist ldistsq

* e)

ivreg lpassen (lfare=concen) ldist ldistsq

* f)

*minimum in ldist
di _b[ldist]/(-2*(_b[ldistsq]))

*minimum in dist
di exp(_b[ldist]/(-2*(_b[ldistsq])))

count if dist<336

di r(N)/_N

***Solution to Problem 3

use KIELMC.DTA


* b)

regress lprice y81 ldist y81ldist

* c)

regress lprice y81 ldist y81ldist  age agesq rooms baths lintst lland larea

scalar tvalue = 1.24

scalar pvalue=ttail(310,tvalue)

display "H1: y81*ldist>0, P=value: " pvalue

clear

log close


