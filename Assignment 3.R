
library(HMDHFDplus)

##read HMD

DAT <- readHMDweb("USA","bltper_1x1","yqin56@wisc.edu","Demography2023.")

##keep data needed
DAT <- subset(DAT,Year < 2006 )
DAT <- subset(DAT,Year > 2004 )
DAT <- subset(DAT,Age > 15 )
DAT <- subset(DAT, Age < 32)

DAT <- subset(DAT, select = c("Age", "qx"))

library(dplyr)

##probability of experiencing a non-fatal motor vehicle accident
DAT <- 
  DAT |>
  select(Age) |>
  mutate(DAT,nqx_M = 0.062 - 0.000053 * Age ^ 2)


##get lx 

DAT <- 
  DAT |>
  select(qx, nqx_M) |>
  mutate(DAT,npx = 1-qx-nqx_M)


DAT$cum_npx <- cumprod(DAT$npx)

DAT$cum_npx = lag(DAT$cum_npx)

DAT$cum_npx[is.na(DAT$cum_npx)] <- 1

DAT$lx <- 85000 * DAT$cum_npx


##Q1
DAT$lx[16]/DAT$lx[1]



##ndx due to mortor vehicle accident only

DAT$ndx_M = DAT$nqx_M * DAT$lx


##Q2
sum(DAT$ndx_M[10:15])/DAT$lx[10]


##ndx due to mortality only
DAT$ndx_D = DAT$qx * DAT$lx


##Q3
sum(DAT$ndx_D[1:15])/DAT$lx[1]






