clear

use pension

reg pctstck choice age educ female black married finc25 finc35 finc50 finc75 finc100 finc101 wealth89 prftshr


oprobit pctstck choice age educ female black married finc25 finc35 finc50 finc75 finc100 finc101 wealth89 prftshr 

*compute fitted values for all observations
	
gen pclass=.


predict c1 if e(sample), outcome(0)

predict c2 if e(sample), outcome(50)

predict c3 if e(sample), outcome(100)

*compute % correctly predicted

egen atest = rmax(c1 c2 c3)


foreach n of numlist 1/3 {
	replace pclass = `n' if c`n' ==atest
		}

tab pclass pctstck if e(sample)

*PCP overall
di (33+31+22)/194

*PCP mostly bonds
di 33/64

*PCP mixed
di 31/72

*PCP mostly stocks
di 22/58
