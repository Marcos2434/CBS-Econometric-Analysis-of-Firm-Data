*** FE & FD & DV estimation with STATA

clear
*open data

use wagepan.dta

*uniid: nr
*period: year

* determine panel structure for panel commands

tsset nr year, yearly

regress lwage educ black hisp exper expersq married union d81-d87

* first difference the data
sort nr year

local varnames lwage educ black hisp exper expersq married union

foreach var in `varnames'{

gen d`var'=.
replace d`var'=`var'-`var'[_n-1] if year!=1980
}


reg dlwage deduc dblack dhisp dexpersq dmarried dunion d82-d87 if year!=1980

*or use STATA's differencing operator

reg d.lwage d.educ d.black d.hisp d.expersq d.married d.union d82-d87

*FE estimation

xtreg lwage educ black hisp expersq married union d81-d87, fe

* dummy variable estimation
* create dummy variable for each individual
sort nr year
qui tab nr if lwage!=., gen(id)

set matsize 600
regress lwage educ black hisp exper expersq married union d81-d87 id2-id545, noconstant
*the results on the time constant variables will be sensitive to which dummy variables are omitted from the model
*they should not be interpreted without making futher restrictions on the model such as combining dummies if the estimated fixed effect is similar.
