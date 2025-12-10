clear
cap log close
set more off

cd "H:\Teaching\CBS\AEF\KAN-CCMVV2401U\2017\Problem Sets\PS3"

log using solution3.log, text replace


***Solution to Problem 1

use WAGE2.DTA

* a)

*lecture's model (2)
regress lwage educ exper tenure married south urban black IQ

* use KWW
regress lwage educ exper tenure married south urban black KWW


* b)

regress lwage educ exper tenure married south urban black IQ KWW


* c)

test IQ KWW


clear

***Solution to Problem 2

use WAGE2.DTA

* a)

ivreg lwage (educ = sibs )

regress lwage sibs

* b)

regress educ brthord

* c)

ivreg lwage (educ = brthord )

* d)

regress educ sibs brthord

* e)

ivreg lwage (educ = brthord ) sibs

* f)

regress educ sibs brthord

predict educhat

cor educhat sibs


clear

***Solution to Problem 3

use 401KSUBS.DTA

* a)

reg pira p401k inc incsq age agesq, robust

* d)

reg p401k e401k inc incsq age agesq, robust

*e)

ivreg pira (p401k=e401k) inc incsq age agesq, robust 

*f)

reg p401k e401k inc incsq age agesq, robust
predict nuhat, resid

reg pira p401k inc incsq age agesq nuhat, robust

clear

log close


