*FE and FEIV

use airfare.dta


* This is to partly reproduce the results in Table 11.1, page 357, Wooldridge (2010)

tsset id year, yearly


xtreg lpassen y98 y99 y00 lfare ldist ldistsq, fe

*b)

xtivreg lpassen y98 y99 y00 ldist ldistsq (lfare = concen), fe
