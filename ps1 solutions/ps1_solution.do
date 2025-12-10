clear all
cap log close
set more off

*cd "H:\Teaching\CBS\AEF\KAN-CCMVV2401U\2016\Problem Sets\PS1\solutions.zip"


log using solution1.log, text replace


***Solution to Problem 1

use wage1.dta

*a)

su educ

di r(min)
di r(max)

count if educ==0
count if educ==18

*b) 

mean(wage)

*c)

gen wage_2003=184/56.9*wage

su wage_2003




*d)

count if female==1
count if female==0

clear

***Solution to Problem 2

use meap01.dta

*a) 

su math4

di r(min)
di r(max)

*b)

count if math4==100

di r(N)/_N

*c)

count if math4==50

*d)

mean(math4)
mean(read4)

*e)

cor math4 read4


*f)

su exppp

clear

***Solution to Problem 3

use CEOSAL2.dta

*a) 

reg lsalary lsales lmktval

*b)

reg lsalary lsales lmktval profits

*c)

reg lsalary lsales lmktval profits ceoten


*d)

cor lmktval profits

clear

***Solution to Problem 4

use BWGHT2.dta

*a) 

reg lbwght cigs npvis


*b)

su npvis

di r(sd)*_b[npvis]


*c)

reg lbwght cigs

*d)

cor npvis cigs 

*e)

reg lbwght cigs npvis mage meduc fage feduc

clear



log close
