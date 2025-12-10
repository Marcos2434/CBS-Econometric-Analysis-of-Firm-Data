*** FE estimation with STATA

clear
*open data

use JTRAIN.dta

*uniid: fcode
*period: year


* create dummy variable for each firm
sort fcode year
qui tab fcode if lscrap!=., gen(firm)

regress lscrap d88 d89 grant grant_1 firm1-firm54, noconstant

