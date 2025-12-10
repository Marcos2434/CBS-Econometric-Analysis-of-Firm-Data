clear
cap log close
set more off

cd "H:\Teaching\CBS\AEF\KAN-CCMVV2401U\2018\Problem Sets\PS5"

log using solution5.log, text replace


***Solution to Problem 1


use motion.dta

*declare panel data structure
xtset id period

*a)
******************************************
*frontalasymmetry
*POLS

reg  frontalasymmetry female dur_0s-logo4_dur_10s, cluster(id) nocons

***********************************
*joy
*POLS

reg  joy female dur_0s-logo4_dur_10s, cluster(id) nocons

*************************************
*browfurrow
*POLS
reg   browfurrow female dur_0s-logo4_dur_10s, cluster(id) nocons
 
*b)
*FD

reg  d.frontalasymmetry D.(dur_0s-logo4_dur_10s), cluster(id) nocons

* males
reg  d.frontalasymmetry D.(dur_0s-logo4_dur_10s) if female==0, cluster(id) nocons
* females
reg  d.frontalasymmetry D.(dur_0s-logo4_dur_10s) if female==1, cluster(id) nocons

reg  d.joy D.(dur_0s-logo4_dur_10s), cluster(id) nocons

* males
reg  d.joy D.(dur_0s-logo4_dur_10s) if female==0, cluster(id) nocons
* females
reg  d.joy D.(dur_0s-logo4_dur_10s) if female==1, cluster(id) nocons

reg  d.browfurrow D.(dur_0s-logo4_dur_10s), cluster(id) nocons

* males
reg  d.browfurrow D.(dur_0s-logo4_dur_10s) if female==0, cluster(id) nocons
* females
reg  d.browfurrow D.(dur_0s-logo4_dur_10s) if female==1, cluster(id) nocons
 
clear

***Solution to Problem 2

use GPA3.DTA

* a)

regress trmgpa spring sat hsperc female black white frstsem tothrs crsgpa season

* c)

gen cfrstsem=frstsem[_n]-frstsem[_n-1] if cseason!=.

regress ctrmgpa cfrstsem ctothrs ccrsgpa cseason

*or use panel comands

tsset id term

regress d.trmgpa d.frstsem d.tothrs d.crsgpa d.season

clear

***Solution to Problem 3

use airfare.dta

* a)

*pooled OLS
regress lfare y98 y99 y00 concen ldist ldistsq

* b)

* pooled OLS with robust SE
regress lfare y98 y99 y00 concen ldist ldistsq, robust

*pooled OLS with "fully" robust SE 
regress lfare y98 y99 y00 concen ldist ldistsq, robust cluster(id)

* c)

*log(dist) turning point
di -_b[ldist]/(2*_b[ldistsq])
*dist turning point
di exp(-_b[ldist]/(2*_b[ldistsq]))

su dist

* d)

tsset id year, yearly
xtreg lfare y98 y99 y00 concen ldist ldistsq, fe




clear

log close
