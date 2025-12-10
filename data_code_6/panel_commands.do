*** selection of useful panel commands with STATA

*open data

use JTRAIN1.dta

*unitid: fcode
*period: year


*bring data into order: sort by unitid period

sort fcode year



* determine panel structure for panel commands

tsset fcode year, yearly

*or

xtset fcode year, yearly

*describe panel data
xtdescribe

*or summarise

xtsum


* commands by unitid or period to create for example dummy variables in i or t dimension:

*compute averages by id or by period
by fcode (year), sort: su lscrap
by year (fcode), sort: su lscrap

*firm dummies
qui tab fcode, gen(id)
