#computes poisson probability mass function for different values of the parameter lambda

rm(list=ls()) 

n<-21

#generate grid on outcome support
y<-c(1:n)

lambda<-1
f_1<-(lambda^y*exp(-lambda))/factorial(y)

lambda<-4
f_4<-(lambda^y*exp(-lambda))/factorial(y)

lambda<-10
f_10<-(lambda^y*exp(-lambda))/factorial(y)

df<-data.frame(f_1,f_4,f_10)
matplot(df,type = c("b"),pch=1,col = 1:3)