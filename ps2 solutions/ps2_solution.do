clear all
cap log close
set more off

*cd "H:\Teaching\CBS\AEF\KAN-CCMVV2401U\2017\Problem Sets\PS2"


log using solution2.log, text replace


***Solution to Problem 2


use hprice1.dta

*a) 

regress lprice llotsize lsqrft bdrms

regress lprice llotsize lsqrft bdrms, robust

*b)

regress lprice llotsize lsqrft bdrms

predict double resid, residuals

gen residsq=resid^2

gen llotsizesq=llotsize^2
gen lsqrftsq=lsqrft^2
gen bdrmssq=bdrms^2
gen llotsizelsqrft=llotsize*lsqrft
gen llotsizebdrms=llotsize*bdrms
gen lsqrftbdrms=lsqrft*bdrms

regress residsq llotsize lsqrft bdrms llotsizesq-lsqrftbdrms


scalar F=e(F)

* The p-value is

display "The p-value for the F-test for joint significance is " Ftail(9,78,F)


clear

***Solution to Problem 3

use ceosal2.dta


*a)
 
regress lsalary lsales lmktval profmarg ceoten comten

*b)
 
regress lsalary lsales lmktval profmarg ceoten comten comtensq ceotensq

test comtensq ceotensq

*c)

regress lsalary lsales lmktval profmarg ceoten comten

predict yhat

gen yhatsq=yhat^2

gen yhatcub=yhat^3

regress lsalary lsales lmktval profmarg ceoten comten yhatsq yhatcub 

test yhatsq yhatcub

clear


***Solution to Problem 4

use GPA2.dta

* a)

regress sat hsize hsizesq

* b)

di _b[hsize]/(-2*(_b[hsizesq]))

* c)

gen lsat=log(sat)

regress lsat hsize hsizesq

di _b[hsize]/(-2*(_b[hsizesq]))

* d)

gen size=hsize*100
gen sizesq=size^2

regress sat size sizesq

di _b[size]/(-2*(_b[sizesq]))


clear



log close
