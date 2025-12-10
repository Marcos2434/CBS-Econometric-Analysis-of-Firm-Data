clear

*computation of the p-value for two sided alternative and small number of observations

* ttail(n,t) return P(T>t) for the t-distribution with n degrees of freedom

* as an example, we set the t-value=1.85

scalar tvalue = 1.85

scalar pvalue=2*ttail(40,tvalue)

display "T-value: " tvalue ", P=value: " pvalue
