
# 21/04/16
# Summarise survey effort - from table in chapter 4

setwd("G:/Statistics 2016/Social network analysis_synced with uni up to 180416 12noon/Summarising association data")
data <- read.csv(file = "SNA_surveyeffort.csv", header = TRUE)
head(data)

# organise headings
data$threshfox<-data$N.foxes.seen..4.times
data$obs.samp<-data$Mean.observations.per.sampling.period
data$obs.tot<-data$N.observations
data$inds<-data$Mean.individuals.observed.per.sampling.period
data$trueassocs<-data$N.true.associations

levels(data$Season)
data$Season <- factor(data$Season,levels(data$Season)[c(2,3,1,4)])


# summarise by season - means
library(plyr)
sum.seas<- ddply(data, c("Season"), summarise,
         mean.foxesn4    = mean(threshfox),
         meanobs.samp    = mean(obs.samp),
         meanobs.tot     = mean(obs.tot),
         meaninds.samp   = mean(inds),
         meantrueassocs  = mean(trueassocs))
sum.seas

# summarise by season - SDs
sum.seasd<- ddply(data, c("Season"), summarise,
                 sd.foxesn4    = sd(threshfox),
                 sdobs.samp    = sd(obs.samp),
                 sdobs.tot     = sd(obs.tot),
                 sdinds.samp   = sd(inds),
                 sd.trueassocs  = sd(trueassocs))
sum.seasd

# summarise by territory - means
sum.ter<- ddply(data, c("Territory"), summarise,
            mean.foxesn4    = mean(threshfox),
            meanobs.samp    = mean(obs.samp),
            meanobs.tot     = mean(obs.tot),
            meaninds.samp   = mean(inds),
            meantrueassocs  = mean(trueassocs))
sum.ter

# summarise by territory - SDs
sum.terd<- ddply(data, c("Territory"), summarise,
                  sd.foxesn4    = sd(threshfox),
                  sdobs.samp    = sd(obs.samp),
                  sdobs.tot     = sd(obs.tot),
                  sdinds.samp   = sd(inds),
                 sd.trueassocs  = sd(trueassocs))
sum.terd

# overall means for whole dataset
mean(data$inds) # mean foxes per day
sd(data$inds)

mean(data$threshfox) # mean foxes whole survey
sd(data$threshfox)

mean(data$obs.samp) # mean observations per day
sd(data$obs.samp)

mean(data$obs.tot) # mean obs total per survey
sd(data$obs.tot)
sum(data$obs.tot) # total overall

mean(data$trueassocs)# mean true associations per survey
sd(data$trueassocs)
sum(data$trueassocs) # total overall
