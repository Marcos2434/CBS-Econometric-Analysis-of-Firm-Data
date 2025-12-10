# Decomposition methods

rm(list=ls()) 
library(haven)
Data <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/8 Decompositions/nlsy00_ind.dta")

library(oaxaca)
Data<-Data[!(Data$ind3<=0 | Data$ind3>=990 | is.na(Data$ind3)),]

#######

Data$afqtp89<-Data$afqtp89/10.0
#### generate dummies for industrial sectors
Data$primary<-as.numeric(Data$indd1==1 | Data$indd2==1 | Data$indd7==1)
Data$manuf<-as.numeric(Data$indd3==1 | Data$indd4==1)
Data$eduheal<-as.numeric(Data$indd11==1 | Data$indd13==1)
Data$othind<-as.numeric(Data$indd5==1 | Data$indd6==1 | Data$indd8==1 | Data$indd9==1 | Data$indd10==1 | Data$indd12==1)

Data$ind5sum<-Data$primary+Data$manuf+Data$eduheal+Data$othind
summary(Data$ind5sum) #check that each obs is within one industry classification
Data<-Data[!(Data$ind5sum==0),]  # no drop in obs, as expected


summary(Data$female)


###Means in Table 2, Column 1
variables<-data.frame(Data$female, Data$black, Data$age00, Data$msa, Data$ctrlcity, Data$north_central, Data$south00, Data$west, Data$hispanic, Data$black,
  Data$sch_10, Data$sch10_12, Data$diploma_hs, Data$ged_hs, Data$smcol, Data$bachelor_col, Data$master_col, Data$doctor_col, Data$afqtp89,
  Data$famrspb, Data$wkswk_18, Data$yrsmil78_00, Data$pcntpt_22, Data$primary, Data$manuf, Data$eduheal, Data$othind, Data$lropc00)

Data.female<-subset(variables, variables$Data.female==1)
Data.male<-subset(variables, variables$Data.female==0)

summary(Data.female[,-1])
summary(Data.male[,-1])

###Table 2, Column 2 
reg1<-lm(lropc00 ~ age00 + msa + ctrlcity + north_central + south00 + west + hispanic + black +
           sch_10  + diploma_hs + ged_hs + smcol + bachelor_col + master_col + doctor_col + afqtp89 +
          famrspb + wkswk_18 + yrsmil78_00 + pcntpt_22 + manuf  + eduheal + othind, data=subset(Data, Data$female==0))
summary(reg1)


###Table 2, Column 3  
reg2<-lm(lropc00 ~ age00 + msa + ctrlcity + north_central + south00 + west + hispanic + black +
           sch_10  + diploma_hs + ged_hs + smcol + bachelor_col + master_col + doctor_col + afqtp89 +
           famrspb + wkswk_18 + yrsmil78_00 + pcntpt_22 + manuf  + eduheal + othind, data=subset(Data, Data$female==1))
summary(reg2)


###Table 2, Column 4
reg3<-lm(lropc00 ~ age00 + msa + ctrlcity + north_central + south00 + west + hispanic + black +
           sch_10  + diploma_hs + ged_hs + smcol + bachelor_col + master_col + doctor_col + afqtp89 +
           famrspb + wkswk_18 + yrsmil78_00 + pcntpt_22  + primary + manuf + othind, data=subset(Data, Data$female==0))
summary(reg3)


###Table 2, Column 5   
reg4<-lm(lropc00 ~ female + age00 + msa + ctrlcity + north_central + south00 + west + hispanic + black +
           sch_10  + diploma_hs + ged_hs + smcol + bachelor_col + master_col + doctor_col + afqtp89 +
           famrspb + wkswk_18 + yrsmil78_00 + pcntpt_22 + manuf + eduheal + othind, data=Data)
summary(reg4)


###########
# Blinder-Oaxaca Decomposition
###########

### Remarks on the R-implementation:
### R command does not allow to subsume some variables in a group, therefore it provides the detailed variable by variable decomposition.
### Bootstrap standard errors are computed and reported. Asymptotic Standard errors not yet implemented.


### Table 3, Column 1 
oaxaca1<-oaxaca(lropc00 ~ age00 + msa + ctrlcity + north_central + south00 + west + hispanic + black +
         sch_10  + diploma_hs + ged_hs + smcol + bachelor_col + master_col + doctor_col + afqtp89 +
         famrspb + wkswk_18 + yrsmil78_00 + pcntpt_22 + manuf + eduheal + othind | female, data=Data) 

oaxaca1$y # overall means group A, group B and difference
oaxaca1$twofold$overall[2,] #twofold decomposition: explained/unexplained (second row gives results with weights=1, i.e. male)
oaxaca1$twofold$variables[[2]] #twofold decomposition for all regressors (all weight to male group)


### Table 3, Column 2  
oaxaca2<-oaxaca(lropc00 ~ age00 + msa + ctrlcity + north_central + south00 + west + hispanic + black +
                  sch_10  + diploma_hs + ged_hs + smcol + bachelor_col + master_col + doctor_col + afqtp89 +
                  famrspb + wkswk_18 + yrsmil78_00 + pcntpt_22 + primary + eduheal + othind | female, data=Data) 

oaxaca2$y # overall means group A, group B and difference
oaxaca2$twofold$overall[2,] #twofold decomposition: explained/unexplained (second row gives results with weights=1, i.e. male)
oaxaca2$twofold$variables[[2]] #twofold decomposition for all regressors (all weight to male group) 


### Table 3, Column 3  
oaxaca1$y # overall means group A, group B and difference
oaxaca1$twofold$overall[1,] #twofold decomposition: explained/unexplained (first row gives results with weights=0, i.e. female)
oaxaca1$twofold$variables[[1]] #twofold decomposition for all regressors (all weight to female group) 


### Table 3, Column 4  
oaxaca1$y # overall means group A, group B and difference
oaxaca1$twofold$overall[4,] #twofold decomposition: explained/unexplained (fourth row gives results with weights based on the number of observations by group)
oaxaca1$twofold$variables[[4]] #twofold decomposition for all regressors 


### Table 3, Column 5  
oaxaca1$y # overall means group A, group B and difference
oaxaca1$twofold$overall[6,] #twofold decomposition: explained/unexplained (sixth row gives results from a pooled regression that includes the group indicator)
oaxaca1$twofold$variables[[6]] #twofold decomposition for all regressors 






