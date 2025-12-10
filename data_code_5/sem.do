clear

use MROZ.dta

*check identification conditions for labour supply equation
reg lwage educ age kidslt6 nwifeinc exper expersq

test exper expersq

* check identification conditions for wage offer equation
reg hours educ age kidslt6 nwifeinc exper expersq

test age kidslt6 nwifeinc

*estimate the labour supply equation
ivreg lwage (hours = age kidslt6 nwifeinc) educ exper expersq

