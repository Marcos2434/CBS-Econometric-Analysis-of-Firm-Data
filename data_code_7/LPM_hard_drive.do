* data from: https://www.backblaze.com/hard-drive-test-data.html
* sample: 31 DEC 2013

use hard_disk.dta

tab failure

reg failure temperature_C capacity_GB operation_years past_error unstable_sectors

predict fit

count if fit<0
count if fit>1

*compute precent correctly predicted
gen tfail=0

replace tfail=1 if fit>0.5

tab failure tfail, col

di (27209+0)/27222
