* simple panel data analysis

clear
use crime2.dta

br year d87 crmrte unem area

tab area

regress crmrte unem if d87==1

regress crmrte d87 unem

br crmrte ccrmrte unem cunem

regress ccrmrte cunem
