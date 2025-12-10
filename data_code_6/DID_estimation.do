* DiD estimation of the average treatment effect of a marketing campaign on sales per capita

clear
use sales.dta

regress salespc urban if y2014==1

scalar gamma14=_b[urban]

regress salespc urban if y2014==0

scalar gamma13=_b[urban]

*DID estimator

scalar did=gamma14-gamma13

di did

*direct estimation of the did effect

regress salespc y2014 urban campaign


**other model

regress salespc y2014 urban campaign age agesq inc incsq marr fsize


