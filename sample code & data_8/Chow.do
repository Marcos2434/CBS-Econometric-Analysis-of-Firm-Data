*Chow test

use wage1.dta

*perform Chow-test for equality of population regression functions between males and females

gen femeduc=female*educ
gen femexper=female*exper
gen femtenure=female*tenure

qui reg lwage female educ femeduc exper femexper tenure femtenure
scalar urss = e(rss)

qui reg lwage educ exper tenure
scalar rrss = e(rss)

scalar F = [(rrss - urss)/4]/[(urss)/(526-2*(3+1))]
display F

*p-value for F-test
display Ftail(4,526-2*(3+1),F)


*or simply
reg lwage female educ femeduc exper femexper tenure femtenure
test female femeduc femexper femtenure


*version that avoids interaction terms

qui reg lwage educ exper tenure if female==0
scalar rss_0 = e(rss)
qui reg lwage educ exper tenure if female==1
scalar rss_1 = e(rss)

scalar F = [(rrss - (rss_0+rss_1))/4]/[(rss_0+rss_1)/(526-2*(3+1))]
display F

*p-value for F-test
display Ftail(4,526-2*(3+1),F)
