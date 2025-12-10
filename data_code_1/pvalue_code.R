# computation of the p-value for two sided alternative and small number of observations

rm(list=ls())


# pt(q,df,lower.tail=F) return P(T>t) for the t-distribution with df degrees of freedom

# as an example, we set the t-value=1.85; df=40

tvalue <- 1.85

pvalue <- 2*pt(tvalue,40, lower.tail = F)  # option lower.tail=T (default) gives P(T<t); see help document