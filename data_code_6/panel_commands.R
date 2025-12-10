# Selection of useful panel commands with R

rm(list=ls())

# Open data
library(haven)
JTRAIN1 <- read_dta("H:/Teaching/CBS/AEF/KAN-CCMVV2401U/2018/Lectures/6 Panel/JTRAIN1.dta")


# Load required packages for panel data analysis
library(plm)


# Define panel data frame: define cross-section and time-series dimension:
# unitid: fcode
# period: year


# Sort by unitid period & determine panel structure for panel commands 
pdata <- pdata.frame(JTRAIN1, index=c("fcode", "year"))
View(pdata)

# Return Panel dimensions:
pdim(JTRAIN1)

# Observation  1 - 12: new "unitid" and "time" and some other variables:
JTRAIN1[1:12,c("fcode","year", "employ", "sales", "avgsal")]


# create firm dummies from fcode (sparse matrix of firm indicators)
df<-data.frame(pdata$fcode)

n <- nrow(df)
nlevels <- sapply(df, nlevels)
i <- rep(seq_len(n), ncol(df))
j <- unlist(lapply(df, as.integer)) +
  rep(cumsum(c(0, head(nlevels, -1))), each = n)
x <- 1

indic_matrix<-sparseMatrix(i = i, j = j, x = x)

dim(indic_matrix)
View(as.matrix(indic_matrix))

