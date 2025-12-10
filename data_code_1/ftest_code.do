*compute the F value with q=2 and n-k-1=396

regress y x1 x2 x3 x4
scalar urss = e(rss)

regress y x1 x2
scalar rrss = e(rss)

scalar F = [(rrss - urss)/2]/[(urss)/(396)]
display F

*p-value for F-test

display Ftail(2,396,F)
