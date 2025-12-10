clear all

set mem 50m

*load data

use GPA1.DTA

* run regression

regress colGPA hsGPA ACT skipped

* test for various linear restrictions

test hsGPA=0

test hsGPA=0.5

test hsGPA=ACT

test hsGPA=ACT=skipped=0
* note that this is equivalent to an F-test for the overal significance of the regeression
