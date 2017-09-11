
# Calculate percentage space use per community (from SOCPROG, SRI eigenvector mod1 method)
# = percentage of visits made by all foxes in each community to different patches

setwd("G:/Statistics 2016/Social network analysis_synced with uni up to 180416 12noon/SOCPROG/Communities")

mydata <- read.csv(file = "Community space use_SRI.csv", header = TRUE) #import dataset for SRI

mydata <- read.csv(file = "Community space use_SAI.csv", header = TRUE) #import datase for SAI

library(plyr)
summary <- ddply(mydata, c("SNACommName", "SiteID", "StationCode", "PatchPositionOrder"), 
             summarise,
             N=mean(comm.size),
             percent = ((sum(PercentagePatchVisits)/N)*100),
             sd      = sd(PercentagePatchVisits),
             se      = sd / sqrt(N)) 
summary

# REORGANISE INTO CROSSTAB
library(tidyr)

#== 1. Percentage data:

# extract main columns of interest
simplesum_percent<-data.frame(summary$SNACommName, summary$PatchPositionOrder, summary$percent)
  # rename columns
  head(simplesum_percent)
  simplesum_percent2<- dplyr::rename(simplesum_percent, CommName = summary.SNACommName,
                               patchpos = summary.PatchPositionOrder, 
                               percent = summary.percent) 
# make xtab
xtab_percents<-tidyr::spread(simplesum_percent2, patchpos, percent)


#== 2. SDs:

# extract main columns of interest
simplesum_sd<-data.frame(summary$SNACommName, summary$PatchPositionOrder, summary$sd)
# rename columns
head(simplesum_sd)
simplesum_sd2<- dplyr::rename(simplesum_sd, CommName = summary.SNACommName,
                                   patchpos = summary.PatchPositionOrder, 
                                   sd = summary.sd) 
# make xtab
xtab_sd<-tidyr::spread(simplesum_sd2, patchpos, sd)


#== 3. SEs:

# extract main columns of interest
simplesum_se<-data.frame(summary$SNACommName, summary$PatchPositionOrder, summary$se)
# rename columns
head(simplesum_se)
simplesum_se2<- dplyr::rename(simplesum_se, CommName = summary.SNACommName,
                              patchpos = summary.PatchPositionOrder, 
                              se = summary.se) 
# make xtab
xtab_se<-tidyr::spread(simplesum_se2, patchpos, se)
