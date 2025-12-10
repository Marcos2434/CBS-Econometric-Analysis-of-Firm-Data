# LPM hard drive

rm(list=ls())

# load data
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/7 LDV/hard_disk.dta")

#data from: https://www.backblaze.com/hard-drive-test-data.html
#sample: 31 DEC 2013

# explore variable failure
table1 <- list(count=table(Data$failure),percent=table(Data$failure)/length(Data$failure))
as.data.frame(table1)

reg1<-lm(failure ~ temperature_C + capacity_GB + operation_years + past_error + unstable_sectors, data=Data) 
summary(reg1)

Data$fit<-fitted(reg1)
sum(Data$fit<0)
sum(Data$fit>1)

max(Data$fit)
min(Data$fit)

#compute percent correctly predicted (a measure of goodness of fit)
Data$tfail<-as.numeric(Data$fit>0.5)


as.data.frame(table(Data$tfail,Data$failure,dnn = list("tfail","failure")))

perc.correct.pred<-27209/27222
