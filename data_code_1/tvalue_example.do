clear all

*set mem 50m *may be required for older Stata versions

*load data

use GPA1.DTA

* run regression

regress colGPA hsGPA ACT skipped

*determine cirtical value of the normal distribution
* invnormal: inverse cumulative standard normal distribution

* 5%
display invnormal(0.975)
* 1%
display invnormal(0.995)

* for the t distribution it is
local df = e(df_r) 
display invt(`df', 0.975)
display invt(`df', 0.995)



*determine value of the test statistic with null that parameter is zero:

scalar tvalue=(_b[hsGPA])/_se[hsGPA]

display "T-value hsGPA: " tvalue "

