*** FE estimation with STATA

clear
*open data

use jtrain.dta

*uniid: fcode
*period: year

* determine panel structure for panel commands



tsset fcode year, yearly

xtreg lscrap d88 d89 grant grant_1, fe
