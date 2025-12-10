* code for estimating pooled cross section wage equation

clear
use CPS78_85.dta

br lwage y85 educ y85educ exper expersq union female y85fem

regress lwage y85 educ y85educ exper expersq union female y85fem

scalar tvalue = 1.67

scalar pvalue=ttail(1075,tvalue)

display "H1: y85fem>0"
display "T-value: " tvalue ", P=value: " pvalue
