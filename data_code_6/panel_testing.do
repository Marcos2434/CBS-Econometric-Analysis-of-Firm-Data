*** panel estimation and testing with STATA

clear
*open data

use wagepan.dta

*uniid: nr
*period: year

* determine panel structure for panel commands

tsset nr year, yearly
*or
*xtset nr year, yearly

*model of interest
regress lwage educ black hisp exper expersq married union d81-d87

qui xtreg lwage educ black hisp expersq married union d81-d87, fe

*store FE results for later use
est store fixed

* dummy variable estimation
* create dummy variable for each individual
sort nr year
qui tab nr if lwage!=., gen(id)

set matsize 600
qui regress lwage educ black hisp exper expersq married union d81-d87 id1-id545, noconstant

*************************
*F-test
*************************

scalar df_ur = e(df_r)
scalar df_m_ur=e(df_m)
scalar urss = e(rss)

qui regress lwage educ black hisp exper expersq married union d81-d87
scalar df_m_r=e(df_m)
scalar rrss = e(rss)

scalar restr=df_m_ur-df_m_r
*Note that the number of restrictions is less than the number of units, because several dummy variables are dropped in ur.
scalar F = [(rrss - urss)/restr]/[(urss)/(df_ur)]
display "The value of the F statistic is " F

*p-value for F-test

display "The p-value for the F-test for joint significance is " Ftail(restr,df_ur,F)



*************************
* Breusch Pagan LM Test
*************************
qui xtreg lwage educ black hisp exper expersq married union d81-d87, re
xttest0 

*************************
* Hausmann Test
*************************
*postestimation command after RE model. Requires stored results for FE model.

*Uses variance matrix of both estimators as in the equation in the lecture slides
hausman fixed 
*This may result in a negative statistic because the difference in the two variance matrices is not necessarily positive definite.

*solution:
*use only the variance matrix of the more efficient estimator
hausman fixed, sigmamore
*use only the variance matrix of the consistent but less efficient estimator
hausman fixed, sigmaless

************************
*Sargan test
************************

qui xtreg lwage educ black hisp exper expersq married union, re

* If you do not have write permission on system drive of your computer, 
* choose other system folders in which external packages can be installed:
sysdir set PLUS "H:\Teaching\CBS\Statauser"
sysdir set PERSONAL "H:\Teaching\CBS\Statauser"

* xtoverid requires two external packages:
ssc install ivreg2, replace
ssc install xtoverid, replace

xtoverid
* why only 4 degrees of freedom? Because only exper expersq married union are time varying. 

* when we also use period dummies:
qui xtreg lwage educ black hisp exper expersq married union d81-d87, re
xtoverid
* Why now 9 degrees of freedom? 
* df should not increase if there is no variation in a variable across i 
*(which is the case for a period dummy)

*do it manually
*calculate time averages by unit for relevant variables
egen experbar = mean(exper), by(nr)
egen expersqbar = mean(expersq), by(nr)
egen marriedbar = mean(married), by(nr)
egen unionbar = mean(union), by(nr)

*multicollineariy between the experience related variables. One has to be dropped

qui xtreg lwage educ black hisp exper expersq married union d81-d87 expersqbar marriedbar unionbar, re 
test expersqbar marriedbar unionbar
