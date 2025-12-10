# simple panel data analysis

rm(list=ls()) 

# open data
library(haven)
crime2 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/CRIME2.dta")

# browse data for year, d87, crmrte, unem and area
myvars <- c("year", "d87", "crmrte", "unem", "area")
crime2new <- crime2[myvars]
View(crime2new)

# explore variable area
table1 <- list(count=table(crime2$area),percent=table(crime2$area)/length(crime2$area))
as.data.frame(table1)

s.lm.1<-summary(lm.1<-lm(crmrte ~ unem, data=crime2[crime2$year==87,]))
s.lm.1

s.lm.2<-summary(lm.2<-lm(crmrte ~ d87 + unem, data=crime2))
s.lm.2

# browse data for crmrte, ccrmrte, unem and cunem
myvars <- c("crmrte", "ccrmrte", "unem", "cunem")
crime2new2 <- crime2[myvars]
View(crime2new2)

s.lm.3<-summary(lm.3<-lm(ccrmrte ~ cunem, data=crime2))
s.lm.3


