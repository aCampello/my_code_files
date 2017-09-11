rm(list=ls()) 
getwd() # view current working directory

save.image("PRT models.RData")

setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/PRT vs. food, sex, season and status")

load("PRT models.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## WHICH PATCH ATTRIBUTES AFFECT DAILY PATCH RESIDENCE TIME (PRT)?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation")
# import without converting to factor
visitdata <- read.csv("MultilevelModels_PatchResTimes_NoCubs28surveys.CSV", header=T, stringsAsFactors = FALSE)
str(visitdata)

#== Replace empty cells with NAs (won't work if imported data with stringsAsFactors=TRUE (default))
visitdata[visitdata==''] <- NA

#== THEN Make factors
visitdata$SocialStatus <- factor(visitdata$SocialStatus)
visitdata$Sex <- factor(visitdata$Sex)
visitdata$fTerritory <- factor(visitdata$Territory)
visitdata$fSeason <- factor(visitdata$SeasonID)
visitdata$Season <- factor(visitdata$Season)
visitdata$AgeClassID <- factor(visitdata$AgeClassID)
visitdata$AgeClass <- factor(visitdata$AgeClass)
visitdata$FeedingDay <- factor(visitdata$FeedingDay)
visitdata$TRANSYear <- factor(visitdata$TRANSYear)
visitdata$ShortCode <- factor(visitdata$ShortCode)
visitdata$StationCode <- factor(visitdata$StationCode)
visitdata$Core <- factor(visitdata$Core)

# Convert datetimes from character to POSIXct
visitdata$cDTVisitStart <-as.POSIXct(paste(visitdata$cDTVisitStart),format="%d/%m/%Y %H:%M:%S", tz="UTC") 
visitdata$VisitStart <-as.POSIXct(paste(visitdata$VisitStart),format="%d/%m/%Y %H:%M:%S", tz="UTC") 
# Did not run yet
visitdata$DAY <- as.Date(visitdata$TRANScDate, format="%d/%m/%Y") 

# Convert kJ to MJ
visitdata$MeanMJPerDay <- visitdata$PatchMeankJPerDay*0.001


# CALCULATE TOTAL DAILY PATCH RESIDENCE TIME (SUM OF VISIT DURATIONS /FOX/PATCH/DAY)

library(plyr)
# NB the total PRT is only calc for days when fox visit patch at least once, so excludes days when fox didn't visit
dailyPRT <- ddply(visitdata, c("Territory", "fSeason", "Season", "StationID", "StationCode", 
                               "AnimalID", "ShortCode", "Sex", "SocialStatus", "Core", "TRANScDate", "CameraDay", 
                               "FeedingDay", "PetDog"), # can include FeedingDay and PetDog as they're binary
                  summarise,
                  totalPRT = sum(VisDurS))

# Add patch attributes (couldn't include continuous varibles in the ddply function or it'd make sep rows for every possible number)
setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding")
patchdata <- read.csv("Patch feeding freq_28surveys.csv")



### CENTER THE VARIABLES DAYS FED & MJ (did not use in thesis or paper)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (to reduce collinearity, improve interpretation of intercept & speed model fit)
# OK to center these as they're continuous and have no meaningful zero within the dataset
patchdata$DF_centered <- scale(patchdata$DaysFedPerWeek, center = TRUE, scale = FALSE)
patchdata$MeanMJpfd_centered <- scale(patchdata$MeanMJperFeedingDay, center = TRUE, scale = FALSE)
# Plot to check it worked 
plot(patchdata$DF_centered, patchdata$DaysFedPerWeek) 
plot(patchdata$MeanMJpfd_centered, patchdata$MeanMJperFeedingDay) 
# view patch data frame: attributes of the scaled variables contains info metadata for the transformation
str(patchdata) # shows that the meanMJ=1.3 and the meanDF=5.38, these values were deducted from all values to do the centering
# storing these attribute details prevents using the predict() function: see http://bit.ly/2aDDN5W
# so convert to vectors before using in the model
patchdata$DF_centeredVEC <- as.vector(patchdata$DF_centered)
patchdata$MeanMJpfd_centeredVEC <- as.vector(patchdata$MeanMJpfd_centered)
# centering didnt make difference to model fit so did not use centered variables in the end


# Add patch attributes ####

# make identifier column
dailyPRT$TSS <- interaction(dailyPRT$Territory, as.numeric(dailyPRT$fSeason), dailyPRT$StationID)
patchdata$TSS <- interaction(patchdata$Territory, as.numeric(patchdata$SeasonID), patchdata$StationID)

# add food info
dailyPRT$DaysFedPerWeek <- patchdata[match(dailyPRT$TSS, patchdata$TSS),8] 
dailyPRT$MeanMJperFeedingDay  <- patchdata[match(dailyPRT$TSS, patchdata$TSS),14] 
dailyPRT$DF_centered <- patchdata[match(dailyPRT$TSS, patchdata$TSS),16] 
dailyPRT$MeanMJpfd_centered <- patchdata[match(dailyPRT$TSS, patchdata$TSS),17] 
dailyPRT$DF_centeredVEC <- patchdata[match(dailyPRT$TSS, patchdata$TSS),18] 
dailyPRT$MeanMJpfd_centeredVEC <- patchdata[match(dailyPRT$TSS, patchdata$TSS),19] 

# make factors
dailyPRT$fSeason <- factor(dailyPRT$fSeason)
dailyPRT$Core <- factor(dailyPRT$Core)


# Calculate the MEAN total daily PRT (for interest / initial plots ####
library(plyr)
MEANdailyPRT <- ddply(dailyPRT, c("Territory", "fSeason", "Season", "StationCode", 
                                  "AnimalID", "ShortCode", "Sex", "SocialStatus", "Core", 
                                  "DaysFedPerWeek", "MeanMJperFeedingDay", "DF_centered",
                                  "MeanMJpfd_centered", "DF_centeredVEC", "MeanMJpfd_centeredVEC"), 
                      summarise,
                      meanPRT = mean(totalPRT))

# Get median daily PRT - WHOLE DATA SET
summary(MEANdailyPRT$meanPRT) # median = 180.7
# FOR CORE FOXES ONLY
summary(subset(MEANdailyPRT, Core=="1")$meanPRT) # median=282.3

# Get 95% CIs for medians
# all foxes
sort(MEANdailyPRT$meanPRT)[qbinom(c(.025,.975), length(MEANdailyPRT$meanPRT), 0.5)]
# residents only
sort(subset(MEANdailyPRT, Core=="1")$meanPRT)[qbinom(c(.025,.975), length(subset(MEANdailyPRT, Core=="1")$meanPRT), 0.5)]

# alternative method:
library(asbio)
ci.median(subset(MEANdailyPRT, Core=="1")$meanPRT)

# get MAD (median absolute deviation):
mad(subset(MEANdailyPRT, Core=="1")$meanPRT) # 273.2592
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### PLOT RAW DATA ####

par(mfrow=c(1,2))

# SEX*SEASON: ALL FOXES #### - in thesis, but don't use in paper

library(ggplot2)
# subset without unknown sex
meanPRTsex <- subset(MEANdailyPRT, !is.na(Sex))
# reorder sex
meanPRTsex$Sexord <- ordered(meanPRTsex$Sex, levels(meanPRTsex$Sex)[c(2,1)])

# plot in colour
ggplot(meanPRTsex, aes(x=fSeason, y=meanPRT,fill=factor(Sexord))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="Sex") + #set males to blue and females to red
  xlab("") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT sex X season_all foxes_ggplot.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper
ggplot(meanPRTsex, aes(x=fSeason, y=meanPRT,fill=factor(Sexord))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour="black"),
        legend.position = c(0.94,0.91),
        legend.background = element_rect(fill = NA)) +
  scale_fill_manual(values = c("gray50","white"), name="Sex") + #set males to blue and females to red
  xlab("") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT sex X season_all foxes_ggplot_BW.jpeg", res=700, height=6, width=7, units="in") 



## Raw data plots: SEX*SEASON: CORE ONLY ####

# subset without unknown sex
meanPRTsex <- subset(MEANdailyPRT, !is.na(Sex) & Core=="1")
# OR subset without unknown sex and unknown status
meanPRTsex <- subset(MEANdailyPRT, !is.na(Sex) & !is.na(SocialStatus) & Core=="1")

# reorder sex
meanPRTsex$Sexord <- ordered(meanPRTsex$Sex, levels(meanPRTsex$Sex)[c(2,1)])

# plot in colour
ggplot(meanPRTsex, aes(x=fSeason, y=meanPRT,fill=factor(Sexord))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="Sex") + #set males to blue and females to red
  xlab("") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT sex X season CORE ONLY_ggplot.jpeg", res=700, height=6, width=8, units="in") 

# B&W 
ggplot(meanPRTsex, aes(x=fSeason, y=meanPRT,fill=factor(Sexord))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.94,0.91),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_fill_manual(values = c("gray50","white"), name="Sex") + #set males to blue and females to red
  xlab("") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT sex X season, known sex & status & CORE_BW.jpeg", res=700, height=6, width=7, units="in") 



# STATUS*FOOD: ALL FOXES #### - in thesis but don't use in paper

meanPRTdom<-subset(MEANdailyPRT, SocialStatus=="Dom")
meanPRTsub<-subset(MEANdailyPRT, SocialStatus=="Sub")

# DF
boxplot(meanPRTdom$meanPRT ~ round(meanPRTdom$DaysFedPerWeek),  ylim=c(0,2500),
        ylab="Patch residence time (s)", xlab="Days fed per week", main="Dominant")
boxplot(meanPRTsub$meanPRT ~ round(meanPRTsub$DaysFedPerWeek), 
        ylab="Patch residence time (s)", ylim=c(0,2500), xlab="Days fed per week", main="Subordinate")
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status DF all foxes.jpeg", res=700, height=6, width=10, units="in") 

# plot in colour
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus)), 
       aes(x=factor(round(DaysFedPerWeek)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="Social\nstatus") + #set males to blue and females to red
  xlab("\nDays fed per week") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status DF all foxes_ggplot.jpeg", res=700, height=6, width=8, units="in") 

# B&W 
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus)), 
       aes(x=factor(round(DaysFedPerWeek)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA)) +
  scale_fill_manual(values = c("gray50","white"), name="Social status") + #set males to blue and females to red
  xlab("Days fed per week") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status DF all foxes_ggplot_BW2.jpeg", res=700, height=6, width=7, units="in") 



# MJ
boxplot(meanPRTdom$meanPRT ~ ceiling(meanPRTdom$MeanMJperFeedingDay),  ylim=c(0,2500),
        ylab="Patch residence time (s)", xlab="MJ per feeding day", main="Dominant")
boxplot(meanPRTsub$meanPRT ~ ceiling(meanPRTsub$MeanMJperFeedingDay), 
        ylab="Patch residence time (s)", ylim=c(0,2500), xlab="MJ per feeding day", main="Subordinate")
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status MJ all foxes.jpeg", res=700, height=6, width=10, units="in") 

# plot in colour
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus)), 
       aes(x=factor(ceiling(MeanMJperFeedingDay)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="Social\nstatus") + #set males to blue and females to red
  xlab("\nMJ per feeding day") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status MJ all foxes_ggplot.jpeg", res=700, height=6, width=8, units="in") 

# B&W 
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus)), 
       aes(x=factor(ceiling(MeanMJperFeedingDay)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("gray35","gray65"), name="Social\nstatus") + #set males to blue and females to red
  xlab("\nMJ per feeding day") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status MJ all foxes_ggplot_BW.jpeg", res=700, height=6, width=8, units="in") 




# Raw data plots: STATUS*FOOD: CORE ONLY ####
meanPRTdomCORE <- subset(MEANdailyPRT, SocialStatus=="Dom" & Core=="1")
meanPRTsubCORE <- subset(MEANdailyPRT, SocialStatus=="Sub" & Core=="1")

# DF
boxplot(meanPRTdomCORE$meanPRT ~ round(meanPRTdomCORE$DaysFedPerWeek),  ylim=c(0,2500),
        ylab="Patch residence time (s)", xlab="Days fed per week", main="Dominant")
boxplot(meanPRTsubCORE$meanPRT ~ round(meanPRTsubCORE$DaysFedPerWeek), 
        ylab="Patch residence time (s)", ylim=c(0,2500), xlab="Days fed per week", main="Subordinate")
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status DF CORE.jpeg", res=700, height=6, width=10, units="in") 

# plot in colour 
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus) & Core=="1"), 
       aes(x=factor(round(DaysFedPerWeek)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="Social\nstatus") + #set males to blue and females to red
  xlab("\nDays fed per week") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status DF CORE_ggplot.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper  # excl. unknown sex AND unknown status
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus) & !is.na(Sex) & Core=="1"), 
       aes(x=factor(round(DaysFedPerWeek)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_fill_manual(values = c("gray35","white"), name="Social status") + 
  xlab("Days fed per week") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status X DF, known sex & status & CORE_BW.jpeg", res=700, height=6, width=7, units="in") 


# MJ
boxplot(meanPRTdomCORE$meanPRT ~ ceiling(meanPRTdomCORE$MeanMJperFeedingDay),  ylim=c(0,2500),
        ylab="Patch residence time (s)", xlab="MJ per feeding day", main="Dominant")
boxplot(meanPRTsubCORE$meanPRT ~ ceiling(meanPRTsubCORE$MeanMJperFeedingDay), 
        ylab="Patch residence time (s)", ylim=c(0,2500), xlab="MJ per feeding day", main="Subordinate")
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status MJ CORE.jpeg", res=700, height=6, width=10, units="in") 

# plot in colour
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus) & Core=="1"), 
       aes(x=factor(ceiling(MeanMJperFeedingDay)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="Social\nstatus") + #set males to blue and females to red
  xlab("\nMJ per feeding day") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT status MJ CORE_ggplot.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper # excl. unknown sex AND unknown status
ggplot(subset(MEANdailyPRT, !is.na(SocialStatus) & !is.na(Sex) & Core=="1"), 
       aes(x=factor(ceiling(MeanMJperFeedingDay)), y=meanPRT,fill=factor(SocialStatus))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.77,0.89),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_fill_manual(values = c("gray50","white"), name="Social status") + #set males to blue and females to red
  xlab("MJ per feeding day") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
# dev.print(jpeg, "RPlot_RAW mean daily PRT status X MJ, known sex & status & CORE_BW.jpeg", res=700, height=6, width=7, units="in") 



### RAW MEAN PRT accounting for FEEDING DAY (for paper): ####

# New meanPRT data frame (RESIDENTS ONLY)
MEANdailyPRT_FD <- ddply(subset(dailyPRT, Core=="1"), c("Territory", "fSeason", "Season", "StationCode", 
                                                                 "AnimalID", "ShortCode", "Sex", "SocialStatus", "Core", 
                                                                 "DaysFedPerWeek", "MeanMJperFeedingDay", "FeedingDay"), 
                         summarise,
                         meanPRT = mean(totalPRT))

# rename variables for plot labels
MEANdailyPRT_FD$FD <- ordered(MEANdailyPRT_FD$FeedingDay, levels = c("TRUE", "FALSE"), labels=c("Provisioning day", "Non-provisioning day")) 
MEANdailyPRT_FD$StatusLong <- factor(MEANdailyPRT_FD$SocialStatus, levels = c("Dom", "Sub"), labels=c("Dominant", "Subordinate")) 

# Simple boxplot of raw mean daily PRT ~ status + feeding day
ggplot(MEANdailyPRT_FD, aes(x=factor(round(DaysFedPerWeek)), y=meanPRT, 
                            fill=factor(StatusLong))) + 
  facet_wrap(~FD) +
  geom_boxplot(size=0.7) +
  ylim(0,1500) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.12,0.91), 
        legend.background = element_rect(fill = NA),
      legend.title=element_blank(),
        axis.text = element_text(colour = "black"),
        strip.background = element_rect(color = "black", size = 1.2)) +
  scale_fill_manual(values = c("gray50","white"), name="Social status") + 
  xlab("Days provisioned per week") + ylab("Patch residence time (s)")
#dev.print(jpeg, "RawPRT-DFXStatus_FacetFeedingDay_Core.jpeg", res=700, height=6, width=9, units="in") 


#~~~


# Raw data plots: CORE VS NOT CORE * FOOD ####
meanPRTCORE<-subset(MEANdailyPRT, Core=="1")
meanPRTNOTCORE <-subset(MEANdailyPRT, Core=="0")

# DF
boxplot(meanPRTCORE$meanPRT ~ round(meanPRTCORE$DaysFedPerWeek), 
        ylab="Patch residence time (s)", xlab="Days fed per week", main="Resident")
boxplot(meanPRTNOTCORE$meanPRT ~ round(meanPRTNOTCORE$DaysFedPerWeek), 
        ylab="Patch residence time (s)", xlab="Days fed per week", main="Non-resident")
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT COREvNOT DF RESIDENT.jpeg", res=700, height=6, width=10, units="in") 

# plot in colour
ggplot(MEANdailyPRT, aes(x=factor(round(DaysFedPerWeek)), y=meanPRT,fill=factor(CORElong))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="") + 
  xlab("\nDays fed per week") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT COREvNOT DF_ggplot RESIDENT.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper
ggplot(MEANdailyPRT, aes(x=factor(round(DaysFedPerWeek)), y=meanPRT,fill=factor(CORElong))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.16,0.96),
        legend.background = element_rect(fill = NA)) +
  scale_fill_manual(values = c("gray50","white"), name="") + #"gray35","gray65" for BW1
  xlab("Days fed per week") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT COREvNOT DF_ggplot RESIDENT_BW2.jpeg", res=700, height=6, width=7, units="in") 



# MJ
boxplot(meanPRTCORE$meanPRT ~ ceiling(meanPRTCORE$MeanMJperFeedingDay), 
        ylab="Patch residence time (s)",  xlab="MJ per feeding day", main="Resident")
boxplot(meanPRTNOTCORE$meanPRT ~ ceiling(meanPRTNOTCORE$MeanMJperFeedingDay), 
        ylab="Patch residence time (s)",  xlab="MJ per feeding day", main="Non-resident")
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT COREvNOT MJ RESIDENT.jpeg", res=700, height=6, width=10, units="in") 

# plot in colour
ggplot(MEANdailyPRT, aes(x=factor(ceiling(MeanMJperFeedingDay)), y=meanPRT,fill=factor(CORElong))) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), name="") + 
  xlab("\nMJ per feeding day") + ylab("Patch residence time (s)\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT COREvNOT MJ_ggplot RESIDENT.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper
ggplot(MEANdailyPRT, aes(x=factor(ceiling(MeanMJperFeedingDay)), y=meanPRT,fill=factor(CORElong))) + 
  geom_boxplot(size=0.8) +
  scale_y_continuous(breaks=seq(0,2500,500)) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.83,0.96),
        legend.background = element_rect(fill = NA)) +
  scale_fill_manual(values = c("gray50","white"), name="") + #"gray35","gray65" for BW1
  xlab("MJ per feeding day") + ylab("Patch residence time (s)") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_RAW mean daily PRT COREvNOT MJ_ggplot RESIDENT_BW2.jpeg", res=700, height=6, width=7, units="in") 

#### END OF PLOTTING RAW DATA ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### MODELLING ####

# Take subset excluding NA status and sex
dailyPRT_sub <- subset(dailyPRT, !(is.na(SocialStatus)) & !is.na(Sex))
write.csv(dailyPRT, file="dailyPRT.csv", row.names = FALSE) # saves as csv file in current working directory

#=== Check distribution
library(fitdistrplus)
require(MASS)
descdist(dailyPRT$totalPRT, discrete = F, boot=5000) # gamma/lognormal
hist(log10(dailyPRT$totalPRT), breaks=100) # log10 transforming the daily data doesn't work very well

library(car)
qqp(dailyPRT$totalPRT, "norm", main="normal") 
qqp(dailyPRT$totalPRT, "lnorm", main="lognormal")
gamma <- fitdist(dailyPRT$totalPRT, "gamma", meth="mme")
qqp(dailyPRT$totalPRT, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")
# gamma best but not great

# log10 transformation shows a bimodal distribution, but this is OK: it compresses the range, which
# will allow model coefficients to be a function of most of the data rather than a few extreme values
# - it compresses the data into a 'typical positive example' and a 'typical negative example' while
# stil alowing magnitudes to enter the model in some form. See: http://www.r-bloggers.com/modeling-trick-the-signed-pseudo-logarithm/
hist(log10(dailyPRT$totalPRT))
plot(density(dailyPRT$totalPRT))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## MODELS USED IN THESIS ####

# based on daily PRT rather than mean daily PRT, to avoid bias from foxes that had a few days with high PRT and then disappeared from the territory or one-off visitors with a short PRT

# transform response to use package influence.ME to check for overly-influential datapoints
dailyPRT_sub$log10totalPRT <- log10(dailyPRT_sub$totalPRT)

# TRIED GAMMA BUT ALWAYS GOT THIS ERROR " Model failed to converge: degenerate Hessian with 2 negative eigenvalues"
# LOGNORMALS FIT OK
# NOTE: when tried to fit gamma model again when repeating stats for Beh Ecol PAPER, the gamma did fit (see below)


# QUESTION 1: DO FEMALES SPEND MORE TIME IN PATCHES IN SPRING AND SUMMER? ####

# Thesis model: PRT ~ Sex * Season - ALL FOXES ####

# Take subset excluding unknown sex
dailyPRT_subsex <- subset(dailyPRT, !is.na(Sex))

dailyPRTmod_SSint <- lmer(log10(totalPRT) ~ 
                            fSeason*Sex +
                            (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                          data=dailyPRT_subsex, REML=F,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05)))

dailyPRTmod_SSadd <- lmer(log10(totalPRT) ~ 
                            fSeason + Sex +
                            (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                          data=dailyPRT_subsex, REML=F,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05)))
anova(dailyPRTmod_SSint, dailyPRTmod_SSadd) # 55.154,3,6.364e-12  - need interaction # REPORT THIS IN MODEL TABLE

# check model fit
plot(fitted(dailyPRTmod_SSint), resid(dailyPRTmod_SSint)) # OK
lines(smooth.spline(fitted(dailyPRTmod_SSint), resid(dailyPRTmod_SSint)), col="red" )
hist(resid(dailyPRTmod_SSint))
visreg::visreg(dailyPRTmod_SSint)
a <- coef(summary(dailyPRTmod_SSint))

# LR tests to get chisq values to report
dailyPRTmod_noSex <- lmer(log10(totalPRT) ~ 
                            fSeason  +
                            (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                          data=dailyPRT_subsex, REML=F,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05)))
dailyPRTmod_noseas <- lmer(log10(totalPRT) ~ 
                             Sex +
                             (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                           data=dailyPRT_subsex, REML=F,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05)))
anova(dailyPRTmod_SSint, dailyPRTmod_noSex) # 55.536      4  2.508e-11 ***
anova(dailyPRTmod_SSint, dailyPRTmod_noseas) #153.18      6  < 2.2e-16 ***


dailyPRTmod_null <- lmer(log10(totalPRT) ~ 1 +
                           (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                         data=dailyPRT_subsex, REML=F,
                         control=lmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05)))
anova(dailyPRTmod_SSint,dailyPRTmod_null) # 153.94      7  < 2.2e-16 ***


# Plot 
lsm_ss <- lsmeans::lsmeans(dailyPRTmod_SSint, ~fSeason|Sex, type="response") 
lsmSS <- data.frame(summary(lsm_ss))

# rename CORE for ggplot2 facet labels
lsmSS$Sexord <- ordered(lsmSS$Sex, levels(lsmSS$Sex)[c(2,1)])

library(ggplot2)
ggp <- ggplot(lsmSS, aes(x=fSeason, y=response, colour=factor(Sexord))) 
ggp + 
  xlab("") + ylab("Patch residence time (s)\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.y = element_text(size=NULL, face="bold")) +
  scale_colour_manual(name="Sex", values = c("royalblue3","firebrick1")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=Sexord), position=position_dodge(0.2)) +
  geom_line(size=1, aes(group=Sexord), position=position_dodge(0.2))

# save above in high resolution for publications
setwd("G:/Statistics 2016/Patch visitation/Patch utilisation intensity vs. food and status")
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT_sex X season_all foxes.jpeg", res=700, height=6, width=8, units="in") 

# plot in B&W for paper
ggp + 
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.94,0.91),
        legend.background = element_rect(fill = NA)) +
  scale_colour_manual(name="Sex", values = c("gray30","gray65")) + 
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=Sexord), position=position_dodge(0.2)) +
  geom_line(size=1.2, aes(group=Sexord), position=position_dodge(0.2))
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT_sex X season_all foxes_BW.jpeg", res=700, height=6, width=7, units="in") 


# test
lsm_ss1 <- pairs(lsmeans::lsmeans(dailyPRTmod_SSint, ~fSeason|Sex) )
lsm_ss2 <- pairs(lsmeans::lsmeans(dailyPRTmod_SSint, ~Sex|fSeason) )
# combine diff post hoc tests into same data frame  and correct for multiple testing
lsm_tukey <- lsmeans::test(rbind(lsm_ss1, lsm_ss2), type="response", adjust="tukey")

#~~~~~~~~~~~~~~~~~~~~~~~~ 

## Thesis model: PRT ~ Sex * Season - CORE ONLY #### - known sex, but not necessarily known status (in thesis)

dailyPRTmod_SSint_CORE <- lmer(log10(totalPRT) ~ 
                                 fSeason*Sex +
                                 (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                               data=subset(dailyPRT_subsex, Core=="1"), REML=F,
                               control=lmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=100000), 
                                                   check.conv.grad=.makeCC("warning",0.05)))

dailyPRTmod_SSadd_CORE <- lmer(log10(totalPRT) ~ 
                                 fSeason + Sex +
                                 (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                               data=subset(dailyPRT_subsex, Core=="1"), REML=F,
                               control=lmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=100000), 
                                                   check.conv.grad=.makeCC("warning",0.05)))
anova(dailyPRTmod_SSint_CORE, dailyPRTmod_SSadd_CORE) #x2(3)=35.195,p=1.108e-07*** # need interaction

a <- coef(summary(dailyPRTmod_SSint_CORE))

# LR TESTS FOR MODEL REPORTING
# no sex
dailyPRTmod_SS_CORE_nosex <- lmer(log10(totalPRT) ~ 
                                    fSeason + 
                                    (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                  data=subset(dailyPRT_subsex, Core=="1"), REML=F,
                                  control=lmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05)))
anova(dailyPRTmod_SSint_CORE,dailyPRTmod_SS_CORE_nosex) # 35.48      4  3.701e-07 ***

# no season
dailyPRTmod_SS_CORE_noseas <- lmer(log10(totalPRT) ~ 
                                     Sex +
                                     (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                   data=subset(dailyPRT_subsex, Core=="1"), REML=F,
                                   control=lmerControl(optimizer="bobyqa",
                                                       optCtrl = list(maxfun=100000), 
                                                       check.conv.grad=.makeCC("warning",0.05)))

anova(dailyPRTmod_SSint_CORE,dailyPRTmod_SS_CORE_noseas) # 122.23      6  < 2.2e-16 ***

## test
lsm_ss1 <- pairs(lsmeans::lsmeans(dailyPRTmod_SSint_CORE, ~fSeason|Sex) )
lsm_ss2 <- pairs(lsmeans::lsmeans(dailyPRTmod_SSint_CORE, ~Sex|fSeason) )
# combine diff post hoc tests into same data frame  and correct for multiple testing
lsm_tukey <- lsmeans::test(rbind(lsm_ss1, lsm_ss2), type="response", adjust="tukey")

## preds
lsm_ss <- lsmeans::lsmeans(dailyPRTmod_SSint_CORE, ~fSeason|Sex, type="response") 
lsmSS <- data.frame(summary(lsm_ss))
lsmSS$Sexord <- ordered(lsmSS$Sex, levels(lsmSS$Sex)[c(2,1)])

ggp <- ggplot(lsmSS, aes(x=fSeason, y=response, colour=factor(Sexord))) 
# plot in colour
ggp + 
  xlab("") + ylab("Patch residence time (s)\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.y = element_text(size=NULL, face="bold")) +
  scale_colour_manual(name="Sex", values = c("royalblue3","firebrick1")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=Sexord), position=position_dodge(0.2)) +
  geom_line(size=1, aes(group=Sexord), position=position_dodge(0.2))
# save
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT_sex X season_CORE ONLY.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper 
ggp + 
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.94,0.91),
        legend.background = element_rect(fill = NA)) +
  scale_colour_manual(name="Sex", values = c("gray30","gray65")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=Sexord), position=position_dodge(0.2)) +
  geom_line(size=1.2, aes(group=Sexord), position=position_dodge(0.2))
# save
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT_sex X season_CORE ONLY_BW.jpeg", res=700, height=6, width=7, units="in") 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### QUESTION 2: Do foxes spend more time in better quality patches, and does this depend on status? ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Thesis model:  PRT ~ Status * Patch quality - ALL FOXES ####

# subset to exclude status
dailyPRT_substat <- subset(dailyPRT, !(is.na(SocialStatus)))

# Need to fit with pre-transformed y to run influence.ME function sigtest
dailyPRT_substat$totalPRT_log10 <- log10(dailyPRT_substat$totalPRT)

# change reference category to subordinate as has more samples to try and reduce collinearity
dailyPRT_substat$StatusRef <- relevel(dailyPRT_substat$SocialStatus, ref = 2) # 2 as alphabetical
# NOTE: this reduced the VIF for several predictors in the model, but didnt change SEs or model fit so 
# continue using dom as the ref category - to be consistent with all previous models

# FULL MODEL 
dailyPRTmod <- lmer(totalPRT_log10 ~ 
                      DaysFedPerWeek*SocialStatus + 
                      MeanMJperFeedingDay*SocialStatus +
                      (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                    data=dailyPRT_substat, REML=F,
                    control=lmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun=100000), 
                                        check.conv.grad=.makeCC("warning",0.05))) 

plot(fitted(dailyPRTmod), resid(dailyPRTmod), main="lnorm")
lines(smooth.spline(fitted(dailyPRTmod), resid(dailyPRTmod)), col="red")
hist(resid(dailyPRTmod))

# Check for multicollinearity: variance inflation factor (VIF)
## values >5 are troubling.
vif.mer(dailyPRTmod) # all OK apart from social status but this is categorical + doesn't affect model much: tried swapping ref categories but didnt help much so kept as is.

# Assess leverage (outliers with extreme x values likely to influence regression estimates)
library(influence.ME) 
inf <- influence(model=dailyPRTmod, count=TRUE, group="StationCode")
sigtest(inf, test=-1.96) # NO INFLUENTIAL PATCHES


# Stepwise model refinement
car::Anova(dailyPRTmod)

# no DF*status 
dailyPRTmod_noDFstatusint <- lmer(log10(totalPRT) ~ 
                                    DaysFedPerWeek + 
                                    MeanMJperFeedingDay*SocialStatus +
                                    (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                  data=dailyPRT_substat, REML=F,
                                  control=lmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05))) 
anova(dailyPRTmod, dailyPRTmod_noDFstatusint) # x2=8.4658,1,0.003619    # need DF*STAT

# no MJ*status
dailyPRTmod_noMJstatint <- lmer(totalPRT_log10 ~ 
                                  DaysFedPerWeek*SocialStatus + 
                                  MeanMJperFeedingDay +
                                  (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                data=dailyPRT_substat, REML=F,
                                control=lmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05))) 
anova(dailyPRTmod, dailyPRTmod_noMJstatint) # 2.8045,1,0.094 .   # DONT need MJ*STAT


dailyPRTmod_noMJ <- lmer(totalPRT_log10 ~ 
                           DaysFedPerWeek*SocialStatus + 
                           (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                         data=dailyPRT_substat, REML=F,
                         control=lmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05))) 
anova(dailyPRTmod_noMJstatint, dailyPRTmod_noMJ) # 33.498      1  7.135e-09 ***   # NEED MJ

# FINAL MODEL
dailyPRTmod_FINAL <- lmer(log10(totalPRT) ~ 
                            DaysFedPerWeek*SocialStatus + 
                            MeanMJperFeedingDay +
                            (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                          data=dailyPRT_substat, REML=F,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05))) 
a <- coef(summary(dailyPRTmod_FINAL))

### LR tests to get chisq to report

# signif of the interaction
dailyPRTmod_FINAL_nointer <- lmer(log10(totalPRT) ~ 
                                    DaysFedPerWeek + SocialStatus + 
                                    MeanMJperFeedingDay +
                                    (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                  data=dailyPRT_substat, REML=F,
                                  control=lmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05))) 
anova(dailyPRTmod_FINAL, dailyPRTmod_FINAL_nointer) # 6.6098,1, 0.01014

# check model fit
plot(fitted(dailyPRTmod_FINAL), resid(dailyPRTmod_FINAL)) # OK
lines(smooth.spline(fitted(dailyPRTmod_FINAL), resid(dailyPRTmod_FINAL)), col="red" )
hist(resid(dailyPRTmod_FINAL))

# Check VIF
vif.mer(dailyPRTmod_FINAL) # SHOULD BE <5. Can ignore high VIF for interaction terms I think, and if still significant then is OK.
# ALL GOOD when used centred variables. 
# For non-centered, status*DF is >5 but still significant so OK. -> decided to use non-centered as I find easier to interpret. 

# SAVE COEFFICIENTS
a <- coef(summary(dailyPRTmod_FINAL))




## Thesis model:  PRT ~ Status * Patch quality - CORE FOXES ONLY ####
# (to see if makes a difference to MJ*status interaction)

# Full model
dailyPRTmodCORE <- lmer(log10(totalPRT) ~ 
                          DaysFedPerWeek*SocialStatus + 
                          MeanMJperFeedingDay*SocialStatus +
                          (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                        data=subset(dailyPRT_substat, Core==1), REML=F,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=100000), 
                                            check.conv.grad=.makeCC("warning",0.05))) 
# No DF*status
dailyPRTmodCORE_noDFstatint <- lmer(log10(totalPRT) ~ 
                                      DaysFedPerWeek + 
                                      MeanMJperFeedingDay*SocialStatus +
                                      (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                    data=subset(dailyPRT_substat, Core==1), REML=F,
                                    control=lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=100000), 
                                                        check.conv.grad=.makeCC("warning",0.05))) 
anova(dailyPRTmodCORE, dailyPRTmodCORE_noDFstatint) # x2(1)=8.339, 0.00388 **

# No MJ*status
dailyPRTmodCORE_noMJstatint <- lmer(log10(totalPRT) ~ 
                                      DaysFedPerWeek*SocialStatus + 
                                      MeanMJperFeedingDay +
                                      (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                    data=subset(dailyPRT_substat, Core==1), REML=F,
                                    control=lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=100000), 
                                                        check.conv.grad=.makeCC("warning",0.05))) 
anova(dailyPRTmodCORE, dailyPRTmodCORE_noMJstatint) # x2(1)=5.1912, 1, 0.0227 * 

# Full model fitst best now
a <- coef(summary(dailyPRTmodCORE)) 

# CONCLUSION: when use subset of only core foxes, both DF AND MJ have sig interaction with status


### Predictions

# DF - all foxes
lsm_preds <- lsmeans::lsmeans(dailyPRTmod_FINAL, "DaysFedPerWeek", 
                              by = c("SocialStatus"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        SocialStatus=c("Dom","Sub")), 
                              type="response")
lsmDF <- data.frame(summary(lsm_preds))

# DF core only
lsm_preds <- lsmeans::lsmeans(dailyPRTmodCORE, "DaysFedPerWeek", 
                              by = c("SocialStatus"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        SocialStatus=c("Dom","Sub")), 
                              type="response")
lsmDFcore <- data.frame(summary(lsm_preds))

# MJ core only
lsm_preds <- lsmeans::lsmeans(dailyPRTmodCORE, "MeanMJperFeedingDay", 
                              by = c("SocialStatus"),
                              at = list(MeanMJperFeedingDay = seq(0,6,1), 
                                        SocialStatus=c("Dom","Sub")), 
                              type="response")
lsmMJ <- data.frame(summary(lsm_preds))


### Plots

library(ggplot2)

# DF - all foxes (at mean MJ per day)
ggp <- ggplot(lsmDF, aes(x=DaysFedPerWeek, y=response, colour=factor(SocialStatus))) 
# plot in colour
ggp + 
  xlab("Days fed per week") + ylab("Patch residence time (s)\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.y = element_text(size=NULL, face="bold")) +
  scale_colour_manual(name="Social\nstatus", values = c("royalblue3","firebrick1")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100,120,140,160)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_line(size=1, aes(group=SocialStatus), position=position_dodge(0.2)) 
# save above in high resolution for publications
# dev.print(jpeg, "RPlot_lsmean dailyPRT DF-status interaction ALL FOXES.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper 
ggp + 
  xlab("Days fed per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA)) +
  scale_colour_manual(name="Social status", values = c("gray30","gray65")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100,120,140,160)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_line(size=1.2, aes(group=SocialStatus), position=position_dodge(0.2)) 

# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmean dailyPRT DF-status interaction ALL FOXES_BW2.jpeg", res=700, height=6, width=7, units="in") 

### DF - core foxes only (at mean MJ per day)
ggp <- ggplot(lsmDFcore, aes(x=DaysFedPerWeek, y=response, colour=factor(SocialStatus))) 
# plot in colour
ggp + 
  xlab("Days fed per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100,120,140,160, 180, 200, 220)) +
  scale_colour_manual(name="Social status", values = c("royalblue3","firebrick1")) + #for colour
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_line(size=1.2, aes(group=SocialStatus), position=position_dodge(0.2)) 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmean dailyPRT DF-status interaction CORE ONLY.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper
ggp + 
  xlab("Days fed per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(breaks=c(0,20,40,60,80,100,120,140,160, 180, 200, 220)) +
  scale_colour_manual(name="Social status", values = c("gray30","gray65")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_line(size=1.2, aes(group=SocialStatus), position=position_dodge(0.2)) 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmean dailyPRT DF-status interaction CORE ONLY_B&W.jpeg", res=700, height=6, width=7, units="in") 


### MJ (at mean DF)
ggp <- ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response, colour=SocialStatus))
# plot in colour
ggp + 
  xlab("\nMJ per day") + ylab("Patch residence time (s)\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.y = element_text(size=NULL, face="bold")) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  scale_y_continuous(breaks = seq(0,1600,200)) +
  scale_colour_manual(name="Social\nstatus", values = c("royalblue3","firebrick1")) +
  geom_line(size=1, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2, position=position_dodge(0.2))
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmean dailyPRT MJ-status interaction CORE ONLY.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper
ggp + 
  xlab("MJ per feeding day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA)) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  scale_y_continuous(breaks = seq(0,1600,250)) +
  scale_colour_manual(name="Social status", values = c("gray30","gray65")) +
  geom_line(size=1.2, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_point(size=4, aes(group=SocialStatus), position=position_dodge(0.2)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2, position=position_dodge(0.2))
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmean dailyPRT MJ-status interaction CORE ONLY_BW2.jpeg", res=700, height=6, width=7, units="in") 




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STATS FOR BEHAVIORAL ECOLOGY PATCH PAPER ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combining PRT ~ Sex + season + Status + Food into same model: preparation ####
# used separate models in thesis, combined for paper.

# Take subset excluding unknown sex 
dailyPRT_subsexstat <- subset(dailyPRT, !is.na(Sex) & !is.na(SocialStatus))

# subset core residents only
PRTsexstatcore <- subset(dailyPRT_subsexstat, Core=="1")


# Check distribution
library(fitdistrplus)
require(MASS)
descdist(PRTsexstatcore$totalPRT, discrete = F, boot=1000) # = gamma/lognormal/beta/weibull 
# not beta (see http://stats.stackexchange.com/questions/58220/what-distribution-does-my-data-follow)
hist(PRTsexstatcore$totalPRT, breaks=500) # log10 transforming the daily data doesn't work very well

library(car)
qqp(PRTsexstatcore$totalPRT, "norm", main="normal") 
qqp(PRTsexstatcore$totalPRT, "lnorm", main="lognormal")
gamma <- fitdist(PRTsexstatcore$totalPRT, "gamma", meth="mme") # method "mme" is only one that works
qqp(PRTsexstatcore$totalPRT, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")
#gamma better than lnorm

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fit some lognormal models anyway, as produced less warnings (and way faster) than gamma models:
# KEEP THIS SECTION ON LNORM MODELS MINIMISED TO KEEP THE CODE FILE CLEAR:

# Have kept code (for model selection, contrasts, preds & plots) just in case,
# but conclusions are as follows:

# Predictions were far too low compared to the raw data, so went on to fit Gamma models instead
# as these gave more sensible predictions

# Also discovered that adding FeedingDay as a predictor improved model fit (see workings below), 
# but this still didn't make the lognormal model preds as realistic as the gamma ones, so I used a 
# gamma model in the paper


# CODE FOR LOGNORMAL MODELS: (didn't use) ####

## Combined lognormal model, without FeedingDay
dailyPRTmod_comb_FULL <- lmer(log10(totalPRT) ~ 
                                fSeason*Sex +
                                DaysFedPerWeek*SocialStatus + 
                                MeanMJperFeedingDay*SocialStatus +
                                (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                              data=PRTsexstatcore, 
                              REML=F,
                              control=lmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05)))
car::Anova(dailyPRTmod_comb_FULL)
MuMIn::r.squaredGLMM(dailyPRTmod_comb_FULL) # 11.7% (R2m=0.1165, R2c=0.3011)

#~~~~

# Model selection (chisq reported in Excel sheet - was originally going to use this model for paper, 
# until realised predictions were too low and changed to GAMMA model, that also included FeedingDay):

# sex+season instead of sex*season
dailyPRTmod_comb_noSSint <- update(dailyPRTmod_comb_FULL, .~. -Sex:fSeason)
anova(dailyPRTmod_comb_FULL, dailyPRTmod_comb_noSSint) # x2(3)=34.341, p=1.679e-07***
# Status+DF instead of Status*DF
dailyPRTmod_comb_noStatDFint <- update(dailyPRTmod_comb_FULL, .~. - DaysFedPerWeek:SocialStatus)
anova(dailyPRTmod_comb_FULL, dailyPRTmod_comb_noStatDFint) # x2(1)=9.3095, p=0.00228**
# Status+MJ instead of Status*MJ
dailyPRTmod_comb_noStatMJint <- update(dailyPRTmod_comb_FULL, .~. - MeanMJperFeedingDay:SocialStatus)
anova(dailyPRTmod_comb_FULL, dailyPRTmod_comb_noStatMJint) # x2(1)=6.681, p=0.009745**

#~~~~

# Checked PRT model without 16WD, which had an exceptionally high MJ ##
dailyPRT_subsexstat16wdcore <- subset(dailyPRT, !is.na(Sex) & !is.na(SocialStatus) & StationCode!="16WD" & Core=="1")
# full model
PRTcomb_no16wd_FULL <- lmer(log10(totalPRT) ~ 
                              fSeason*Sex +
                              DaysFedPerWeek*SocialStatus + 
                              MeanMJperFeedingDay*SocialStatus +
                              (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                            data=dailyPRT_subsexstat16wdcore, REML=F)
# sex+season instead of sex*season
PRTcomb_no16wd_noSS <- update(PRTcomb_no16wd_FULL, .~. - Sex:fSeason)
anova(PRTcomb_no16wd_FULL, PRTcomb_no16wd_noSS) # x2(3)=25.722, p=1.09e-05***   # need sex*season int.

# Status+DF instead of Status*DF
PRTcomb_no16wd_noStatDFint <- update(PRTcomb_no16wd_FULL, .~. - DaysFedPerWeek:SocialStatus)
anova(PRTcomb_no16wd_FULL, PRTcomb_no16wd_noStatDFint) # x2(1)=10.665, p=0.001092**   # need status*DF int.

# Status+MJ instead of Status*MJ
PRTcomb_no16wd_noStatMJint <- update(PRTcomb_no16wd_FULL, .~. - MeanMJperFeedingDay:SocialStatus)
anova(PRTcomb_no16wd_FULL, PRTcomb_no16wd_noStatMJint) # x2(1)=11.8, p=0.0005922***
# MJ still has significant effect, so keep 16WD in. 

#~~~~

### CONTRASTS from Lognormal model without FeedingDay 
# (these are reported in Excel sheet as was originally going to use in paper)

## Status*DF
# Dif between dom and sub at diff DF
lsm_contrastsSDF <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, pairwise~SocialStatus|DaysFedPerWeek,
                                     at = list(DaysFedPerWeek = seq(1,7,2),
                                               SocialStatus=c("Dom", "Sub"))) 
# Diff between DF for dom and sub
lsm_contrastsSDFvv <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, pairwise~DaysFedPerWeek|SocialStatus,
                                       at = list(DaysFedPerWeek = c(1,7),
                                                 SocialStatus=c("Dom", "Sub")))  
# combine all tests into same dataframe and correct P for multiple testing, to avoid false negs
a <-pairs(lsm_contrastsSDF)
b <-pairs(lsm_contrastsSDFvv)
lsmSDFcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")


## Status*MJ
# Dif between dom and sub at diff MJ
lsm_contrastsSMJ <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, pairwise~SocialStatus|MeanMJperFeedingDay,
                                     at = list(MeanMJperFeedingDay = c(0.1,1,2,3,4,5,6),
                                               SocialStatus=c("Dom", "Sub"))) 
# Diff between MJ for dom and sub
lsm_contrastsSMJvv <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, pairwise~MeanMJperFeedingDay|SocialStatus,
                                       at = list(MeanMJperFeedingDay = c(1,6),
                                                 SocialStatus=c("Dom", "Sub")))  
# combine all tests into same dataframe and correct P for multiple testing, to avoid false negs
a <-pairs(lsm_contrastsSMJ)
b <-pairs(lsm_contrastsSMJvv)
lsmSMJcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")


## Sex*season
# test
lsm_ss1 <- pairs(lsmeans::lsmeans(dailyPRTmod_comb_FULL, ~fSeason|Sex) )
lsm_ss2 <- pairs(lsmeans::lsmeans(dailyPRTmod_comb_FULL, ~Sex|fSeason) )
# combine all tests into same dataframe and correct P for multiple testing, to avoid false negs
lsm_tukey <- lsmeans::test(rbind(lsm_ss1, lsm_ss2), type="response", adjust="tukey")

#~~~~

### PREDICTIONS from Lognormal model without FeedingDay 
# (these are reported in Excel sheet as was originally going to use in paper)

# MJ*status (holding all other variables at the mean)
lsm_preds <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, "MeanMJperFeedingDay", 
                              by = c("SocialStatus"),
                              at = list(MeanMJperFeedingDay = seq(0,6,1), 
                                        SocialStatus=c("Dom","Sub")), 
                              type="response")
lsmMJ <- data.frame(summary(lsm_preds))

# DF*status (holding all other variables at the mean)
lsm_preds <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, "DaysFedPerWeek", 
                              by = c("SocialStatus"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        SocialStatus=c("Dom","Sub")), 
                              type="response")
lsmDF <- data.frame(summary(lsm_preds))

# Sex*Season
lsm_ss <- lsmeans::lsmeans(dailyPRTmod_comb_FULL, ~fSeason|Sex, type="response") 
lsmSS <- data.frame(summary(lsm_ss))
lsmSS$Sexord <- ordered(lsmSS$Sex, levels(lsmSS$Sex)[c(2,1)])
lsmSS$SexLong <- factor(lsmSS$Sexord, levels = c("M", "F"), labels=c("Male", "Female")) 


## CODE FOR LOGNORMAL MODEL including FeedingDay, to see if makes more sensible preds - didnt use ####
PRTmodlnorm_feedingday <- lmer(log10(totalPRT) ~ 
                                 fSeason*Sex +
                                 DaysFedPerWeek*SocialStatus + 
                                 MeanMJperFeedingDay*SocialStatus +
                                 fFeedingDay +
                                 (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                               data=PRTsexstatcore, REML=F) 
car::Anova(PRTmodlnorm_feedingday)
MuMIn::r.squaredGLMM(PRTmodlnorm_feedingday) # 17.1% (R2m=0.1714, R2c=0.3581)

# Dharma checks
simlnormFD <- simulateResiduals(PRTmodlnorm_feedingday)
plotSimulatedResiduals(simlnormFD, quantreg=F) # looks quite nice/more normal

# model simplification
car::Anova(PRTmodlnorm_feedingday)

PRTmodlnorm_FD_noMJStat <- update(PRTmodlnorm_feedingday, .~. - MeanMJperFeedingDay:SocialStatus)
anova(PRTmodlnorm_feedingday, PRTmodlnorm_FD_noMJStat) # x2(1)=7.4775, p=0.006247 **

PRTmodlnorm_FD_noDFStat <- update(PRTmodlnorm_feedingday, .~. - DaysFedPerWeek:SocialStatus)
anova(PRTmodlnorm_feedingday, PRTmodlnorm_FD_noDFStat) # x2(1)=10.114, p=0.001472 **

PRTmodlnorm_FD_noSS <- update(PRTmodlnorm_feedingday, .~. - fSeason:Sex)
anova(PRTmodlnorm_feedingday, PRTmodlnorm_FD_noSS) # x2(3)=37.984, p=2.849e-08 ***

PRTmodlnorm_noFD <- update(PRTmodlnorm_feedingday, .~. - fFeedingDay)
anova(PRTmodlnorm_feedingday, PRTmodlnorm_noFD) # x2(3)=1221.7, p<2.2e-16 ***

# preds
lsm_ss <- lsmeans::lsmeans(PRTmodlnorm_feedingday, ~fSeason|Sex, type="response") 
lsmSS <- data.frame(summary(lsm_ss))

# rename sex for clearer plot legend
lsmSS$SexLong <- factor(lsmSS$Sex, levels = c("M", "F"), labels=c("Male", "Female")) 

# plot preds
ggp <- ggplot(lsmSS, aes(x=fSeason, y=response, colour=SexLong)) 
ggp + 
  geom_boxplot(data=PRTpatchmeans_season, aes(x=fSeason, y=meanPRT, colour=SexLong), size=0.7, position=position_dodge(0.8)) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.89,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_colour_manual(values = c("gray30","gray65")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  # ylim(0,1500) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=1, width=0.15, position=position_dodge(0.8)) +
  geom_point(size=5, pch=15, aes(group=SexLong), position=position_dodge(0.8)) 

# preds are still way too low from the lognormal model, despite having better residuals!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of lnorm section ##### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## GAMMA MODELS  ####

# OUTCOME of fitting gamma models: 
# Large number of 5-second visits (PRTs) behaved like zero-inflation or over-dispersion; 
# not much can do about this. Residual - predictor plots were OK (no autocorrelation, correlation, 
# heterogeneity and predictions closely matched raw data. 
# So decided the fit was the best I was going to get and stuck with it. Large datasets rarely 
# approximate any particular distribution so just have to make the best of it. Made no diff 
# excluding high 'outliers' (1.96SD from the mean), so used model fitted to the full dataset.
# Some patches changed significance of MJ*status interaction, but too many to exclude (101AR had greatest influence).

# Gamma model wouldnt fit with default inverse link as predictions kept becoming negative, so
# gotta fit with log link (see https://github.com/lme4/lme4/issues/179)

# Original Gamma model (without FeedingDay, which added later on) ####
dailyPRTmod_comb_FULLgamma <- glmer(totalPRT ~ 
                                      fSeason + Sex + MeanMJperFeedingDay +
                                      DaysFedPerWeek + SocialStatus +
                                      fSeason:Sex +
                                      DaysFedPerWeek:SocialStatus + 
                                      MeanMJperFeedingDay:SocialStatus +
                                      (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                    data=PRTsexstatcore, 
                                    family=Gamma(link="log"),
                                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) 

car::Anova(dailyPRTmod_comb_FULLgamma) # MJ*status interaction Wald p=0.09829. Everything else significant.

# GAMMA RESIDUALS ARE NOT ASSUMED TO BE NORMAL
# So no point doing normal model checks, e.g. hist() and qqnorm()
# DHARMA checks residuals against the empirical distribution expected from the fitted model
# and provides standardised residuals based on the model family:

# Dharma model checking
library(DHARMa)

## Simulate residuals standardised between 0-1 (most DHARMA functions only work on standardised simulated resids)
simgam <- simulateResiduals(dailyPRTmod_comb_FULLgamma)

# Residual plots: 
# Website with examples of how 'perfect' Gamma model resids should look: https://github.com/florianhartig/DHARMa/blob/master/Code/Examples/GammaProblems.md
plotSimulatedResiduals(simgam, quantreg=T) # takes a while if quantreg=T
# QQ-plot should be straight line, RHS plot should be uniform in y direction (but 
# density of points depends on distrib of data so doesnt matter, e.g. i have lots of low values)

### Plot resids against the predictors in the model to detect possible misspecifications
# Should show no patterns or correlations (autocorrelation), as with a fitted-resid plot, 
plotResiduals(PRTsexstatcore$Sex, simgam$scaledResiduals)
plotResiduals(PRTsexstatcore$fSeason, simgam$scaledResiduals)
plotResiduals(PRTsexstatcore$SocialStatus, simgam$scaledResiduals)
plotResiduals(PRTsexstatcore$MeanMJperFeedingDay, simgam$scaledResiduals)
plotResiduals(PRTsexstatcore$DaysFedPerWeek, simgam$scaledResiduals)
# variance looks equal across categories and values of all predictors

plotResiduals(PRTsexstatcore$ShortCode, simgam$scaledResiduals)
plotResiduals(PRTsexstatcore$StationCode, simgam$scaledResiduals)
# variance looks approx equal across levels of random effects

plotResiduals(PRTsexstatcore$Territory, simgam$scaledResiduals) # no effect of territory
plotResiduals(PRTsexstatcore$fFeedingDay, simgam$scaledResiduals) # looks like there is an effect
# maybe FeedingDay needs to be in the model. 

#~~~

### Model simplification to plot/compare preds with gamma model with FeedingDay
PRTgammanoFD_noMJ <- update(dailyPRTmod_comb_FULLgamma, .~. - MeanMJperFeedingDay:SocialStatus)
anova(dailyPRTmod_comb_FULLgamma, PRTgammanoFD_noMJ) # x2(1)=2.7038, p=0.1001 # don't neef MJ*status
car::Anova(PRTgammanoFD_noMJ)

## PREDICTIONS:
# DF*status
lsm_preds <- lsmeans::lsmeans(PRTgammanoFD_noMJ, "DaysFedPerWeek", 
                              by = c("SocialStatus"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        SocialStatus=c("Dom","Sub")), type="response")
lsmDF <- data.frame(summary(lsm_preds))
lsmDF$StatusLong <- factor(lsmDF$SocialStatus, levels = c("Dom", "Sub"), labels=c("Dominant", "Subordinate")) 

# MJ
lsm_preds <- lsmeans::lsmeans(PRTgammanoFD_noMJ, "MeanMJperFeedingDay", 
                              at = list(MeanMJperFeedingDay = seq(0,6,1)), type="response")
lsmMJ <- data.frame(summary(lsm_preds))

# Sex*season 
lsm_ss <- lsmeans::lsmeans(PRTgammanoFD_noMJ, ~fSeason|Sex, type="response") 
lsmSS <- data.frame(summary(lsm_ss))

# (Then use code below to plot same graphs as Gamma model with FeedingDay variable)

#~~~

### As an aside: 

# Ran some gamma mods without outliers to see if improved fit/eliminated warnings: MINIMISE to save space/confusion ####

# Removed outliers 1.96, 3 or 5 standard deviations from the mean totalPRT, in attempt to improve model fit (didnt work so ignore)

## To mark and remove outliers from dataset: 
# (see outlierSummary function and below in 'Useful bits and bobs' file)

# Remove rows >3 SD from mean (more conservative than the default of 1.96 SD)
PRTsexstatcore$totalPRT_z3sd <- outliersZ(PRTsexstatcore$totalPRT, zCutOff=3.0)
PRTsexstatcore_below3SD <- subset(PRTsexstatcore, !is.na(totalPRT_z3sd))
nrow(PRTsexstatcore)-nrow(PRTsexstatcore_below3SD) # removed 359 rows
max(PRTsexstatcore_below3SD$totalPRT) # =2577 seconds, a.k.a. 43 mins

# Remove rows >1.96 SD from mean (default for this function)
PRTsexstatcore$totalPRT_z <- outliersZ(PRTsexstatcore$totalPRT)
PRTsexstatcore_below1.96SD <- subset(PRTsexstatcore, !is.na(totalPRT_z))
nrow(PRTsexstatcore)-nrow(PRTsexstatcore_below1.96SD) # removed 864 rows
max(PRTsexstatcore_below1.96SD$totalPRT) # =1845 seconds, a.k.a. 30 mins


## Run models

# 3SD GAMMA MODEL:
# excl. all rows with totalPRT> 3 standard deviations from the mean
dailyPRTmod_comb_FULLgamma3sd <- glmer(totalPRT ~ 
                                         fSeason + Sex + MeanMJperFeedingDay +
                                         DaysFedPerWeek + SocialStatus +
                                         fSeason:Sex +
                                         DaysFedPerWeek:SocialStatus + 
                                         MeanMJperFeedingDay:SocialStatus +
                                         (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                       data=PRTsexstatcore_below3SD, 
                                       family=Gamma(link="log"),
                                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) 

# DHARMA model checks for 3SD gamma model
sim3SD <- simulateResiduals(dailyPRTmod_comb_FULLgamma3sd)
plotSimulatedResiduals(sim3SD, quantreg=T) # takes a while if include quantreg


### 1.96SD GAMMA MODEL: 
# Excl. all rows with totalPRT> 3 standard deviations from the mean
dailyPRTmod_comb_FULLgamma1.96sd <- glmer(totalPRT ~ 
                                            fSeason + Sex + MeanMJperFeedingDay +
                                            DaysFedPerWeek + SocialStatus +
                                            fSeason:Sex +
                                            DaysFedPerWeek:SocialStatus + 
                                            MeanMJperFeedingDay:SocialStatus +
                                            (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                          data=PRTsexstatcore_below1.96SD, 
                                          family=Gamma(link="log"),
                                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))) 

# Dharma checks for 1.96SD gamma model
sim196SD <- simulateResiduals(dailyPRTmod_comb_FULLgamma1.96sd) 
plot(sim196SD, quantreg=T) # takes a while if include quantreg - 'horizontal' lines for 25th, 50th and 75th percentiles


# END OF GAMMA models run without potential outliers (KEEP MINIMISED) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#####


# GAMMA MODELS including FEEDING DAY variable - IN PAPER ####

# Rename FeedingDay = true/false as 0/1 like with the variable Core, WITHOUT ORDERING
library(plyr)
PRTsexstatcore$fFeedingDay <- revalue(PRTsexstatcore$FeedingDay, c("TRUE"="1", "FALSE"="0"))
# change reference level to 1 instead of 0
PRTsexstatcore$fFeedingDay <- relevel(PRTsexstatcore$fFeedingDay, ref="1")

# Standard syntax with default optimiser
PRTmodgamma_feedingday <- glmer(totalPRT ~ 
                                  fSeason*Sex +
                                  DaysFedPerWeek*SocialStatus + 
                                  MeanMJperFeedingDay*SocialStatus +
                                  fFeedingDay +
                                  (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                data=PRTsexstatcore, 
                                family=Gamma(link="log"))

# FeedingDay GAMMA model: got convergence warning, so tried fitting with diff optimisers... ####

# bobyqa
PRTmodgamma_feedingdayBOBY <- glmer(totalPRT ~ 
                                      fSeason*Sex +
                                      DaysFedPerWeek*SocialStatus + 
                                      MeanMJperFeedingDay*SocialStatus +
                                      FeedingDay +
                                      (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                    data=PRTsexstatcore, 
                                    family=Gamma(link="log"),
                                    control=glmerControl(optimizer="bobyqa", 
                                                         optCtrl=list(maxfun=2e5))) 
# nelder-mead
PRTmodgamma_feedingdayNM <- update(PRTmodgamma_feedingdayBOBY, 
                                   control=glmerControl(optimizer="Nelder_Mead"))

# Optimx
library(optimx)
PRTmodgamma_feedingdayOP <- update(PRTmodgamma_feedingdayBOBY, 
                                   control=glmerControl(optimizer="optimx",
                                                        optCtrl=list(method="nlminb")))

# LBFGSB
PRTmodgamma_feedingdayLBFGSB <- update(PRTmodgamma_feedingdayBOBY,
                                       control=glmerControl(optimizer="optimx",
                                                            optCtrl=list(method="L-BFGS-B")))
# nloptr
library(nloptr)
# run these codes first to make the functions
defaultControl <- list(algorithm="NLOPT_LN_BOBYQA",xtol_rel=1e-6,maxeval=1e5)
nloptwrap2 <- function(fn,par,lower,upper,control=list(),...) {
  for (n in names(defaultControl)) 
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
    res <- nloptr(x0=par,eval_f=fn,lb=lower,ub=upper,opts=control,...)
    with(res,list(par=solution,
                  fval=objective,
                  feval=iterations,
                  conv=if (status>0) 0 else status,
                  message=message))
}

# Then rerun model with the new optimiser nloptr version of bobyqa, "nloptwrap2":
PRTmodgamma_feedingdayBOBY2 <- update(PRTmodgamma_feedingdayBOBY, 
                                      control=glmerControl(optimizer=nloptwrap2))

# 6. Rerun model with the nloptr version of N-M optimiser: (FITTED WITHOUT WARNINGS!)
PRTmodgamma_feedingdayNM2 <- update(PRTmodgamma_feedingdayBOBY, 
                                    control=glmerControl(optimizer=nloptwrap2,
                                                         optCtrl=list(algorithm="NLOPT_LN_NELDERMEAD")))
# compare AIC for models with diff optimisers
bbmle::AICtab(normal = PRTmodgamma_feedingday,
              bobyqa=PRTmodgamma_feedingdayBOBY,
              NM=PRTmodgamma_feedingdayNM,
              nlminb=PRTmodgamma_feedingdayOP,
              LBFGSB=PRTmodgamma_feedingdayLBFGSB, 
              bobyqa2=PRTmodgamma_feedingdayBOBY2,
              NM2=PRTmodgamma_feedingdayNM2)
# no major difference in AIC between any of these optimisers

# compare log likelihoods for models with diff optimisers
modList <- list(normal = PRTmodgamma_feedingday,
                bobyqa=PRTmodgamma_feedingdayBOBY,
                NM=PRTmodgamma_feedingdayNM,
                nlminb=PRTmodgamma_feedingdayOP,
                LBFGSB=PRTmodgamma_feedingdayLBFGSB, 
                bobyqa2=PRTmodgamma_feedingdayBOBY2,
                NM2=PRTmodgamma_feedingdayNM2)
likList <- sort(sapply(modList,logLik))
round(log10(max(likList)-likList),2) # view log10 transformed to 2 decimal places - clearer differences, but not huge

# Plot to compare models with diff optimisers:
getpar <- function(x) c(getME(x,c("theta")),fixef(x))
ctab <- sapply(modList,getpar)
library(reshape)
mtab <- melt(ctab) # REQUIRES library(reshape2)

ggplot(mtab,aes(x=X1,y=value,colour=X2)) +
  geom_point(position=position_jitter(0.2))+facet_wrap(~X1, scale="free") + theme_bw()
# no major differences in model coefficients
# So just ignore the convergence warnings, as the standard model still seems to fit OK.
# End of FeedingDay GAMMA model optimiser section ####
#####

# Dharma checks of standard / normal FeedingDay GAMMA model:
library(DHARMa)
simgamFD <- simulateResiduals(PRTmodgamma_feedingday)
plotSimulatedResiduals(simgamFD, quantreg=F)

# check for multicollinearity (should be <5)
vif.mer(PRTmodgamma_feedingday) # social status interactions >5, but as status is not continuous it's OK

plotResiduals(PRTsexstatcore$Sex, simgamFD$scaledResiduals)
plotResiduals(PRTsexstatcore$fSeason, simgamFD$scaledResiduals)
plotResiduals(PRTsexstatcore$SocialStatus, simgamFD$scaledResiduals)
plotResiduals(PRTsexstatcore$MeanMJperFeedingDay, simgamFD$scaledResiduals)
plotResiduals(PRTsexstatcore$DaysFedPerWeek, simgamFD$scaledResiduals) # +ve correlation but OK as DF is included in model
plotResiduals(PRTsexstatcore$ShortCode, simgamFD$scaledResiduals)
plotResiduals(PRTsexstatcore$StationCode, simgamFD$scaledResiduals)
plotResiduals(PRTsexstatcore$Territory, simgamFD$scaledResiduals) 
# variance looks equal across categories and values of all predictors

plotResiduals(PRTsexstatcore$fFeedingDay, simgamFD$scaledResiduals) 
# variance is *more* equal between categories now FeedingDay is included in the model

# Influence.ME to check whether particular gardens influence model coefficients ####
library(influence.ME)
PRTmodgamma_feedingday_inf <- influence(model=PRTmodgamma_feedingday, count=TRUE, group="StationCode") # takes 4-5 mins per garden, so took between 2.5-3 hours!!

PRTmodgamma_feedingday_inf <- inf

dfbetas(PRTmodgamma_feedingday_inf)
# Altered.sig = significance of coeff in the new model without the patch (true=sig, false=not sig)

plot(PRTmodgamma_feedingday_inf,
     which="dfbetas", # what to plot
     parameters=c(2,3,4), # which model parameters to plot
     xlab="DFbetaS", ylab="StationCode",
     cutoff=2/sqrt(35)) # 2/vn where N=no. patches (recommended rule of thumb from Belsley et al 1980)

# How did removal of different patches affect each model estimate?
sigtest(PRTmodgamma_feedingday_inf, test=1.96) 
# Altered.sig = significance of coeff in the new model without the patch (true=sig, false=not sig)
# Changed.sig means 'did sig of model change when patch was excluded?'
# Removing patches only changed significance of Status*MJ interaction, but for too many patches to 
# remove them all (101AR, 16WD, 23NR, 34DCD, 57CBA, 58GB, 7DR, 7RHL and GL), so keep them all in.

# 101AR had the biggest influence:
cooks.distance(PRTmodgamma_feedingday_inf, parameter=c(1,2,3,4), sort=TRUE) # print values of Cook's distance in ascending order
plot(PRTmodgamma_feedingday_inf, 
     which="cook", # plot cooks distance
     cutoff=4/35, # 2/vn where N=no.grouping levels (patches) - (Van der Meer et al., 2010)
     sort=TRUE, # sort in order of most-least influential
     xlab="Cook´s Distance", ylab="StationCode") 

#~~~~~~~

# FeedingDay gamma model without 16WD to see if makes difference to effect of MJ ####
# (as it did in the N.visitors model)

# model excl. data from 16WD
PRTmodgamma_feedingdayno16WD <- glmer(totalPRT ~ 
                                        fSeason*Sex +
                                        DaysFedPerWeek*SocialStatus + 
                                        MeanMJperFeedingDay*SocialStatus +
                                        fFeedingDay +
                                        (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                      data=subset(PRTsexstatcore, StationCode!="16WD"), 
                                      family=Gamma(link="log")) # convergence warning but still fit so ignore

# compare with model containing 16WD
bbmle::AIC(PRTmodgamma_feedingday, PRTmodgamma_feedingdayno16WD) # 16WD reduces the AIC (lower/smaller is better)

# Dharma checks
simgamFD_16WD <- simulateResiduals(PRTmodgamma_feedingdayno16WD)
plotSimulatedResiduals(simgamFD_16WD, quantreg=T)
# QQ-plot slightly less overdispersed (straighter) but otherwise no difference in fit
plotResiduals(subset(PRTsexstatcore, StationCode!="16WD")$StationCode, simgamFD_16WD$scaledResiduals, cex.axis=0.5) # made text small to label all patches

# check significance and coefficients
car::Anova(PRTmodgamma_feedingdayno16WD)
summary(PRTmodgamma_feedingdayno16WD)
# when 16WD is excluded, the status*MJ interaction is more significant. 
# Prob the same if exclude 101AR, based on Influence.ME output - CHECK BELOW: 

# FeedingDay gamma model without 101AR to see if makes difference to effect of MJ ####
PRTmodgamma_feedingdayno101AR <- glmer(totalPRT ~ 
                                         fSeason*Sex +
                                         DaysFedPerWeek*SocialStatus + 
                                         MeanMJperFeedingDay*SocialStatus +
                                         fFeedingDay +
                                         (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                       data=subset(PRTsexstatcore, StationCode!="101AR"), 
                                       family=Gamma(link="log")) 
# Dharma checks
simgam_101AR <- simulateResiduals(PRTmodgamma_feedingdayno101AR)
plotSimulatedResiduals(simgam_101AR, quantreg=T)# looks quite nice/more normal
plotResiduals(subset(PRTsexstatcore, StationCode!="101AR")$StationCode, simgam_101AR$scaledResiduals, cex.axis=0.5) # made text small to label all patches

testUniformity(simgam_101AR) # still not uniform
testOverdispersion(simgam_101AR) # not much different from full gamma model with all patches.

# check significance and coefficients
car::Anova(PRTmodgamma_feedingdayno101AR) # 
summary(PRTmodgamma_feedingdayno101AR)
# as expected, status*MJ interaction is more significant when 101AR is excluded from model.
# Prob same with all those other patches identified by influence.ME function. Ignore and keep all patches in model.

# Gamma with all patches and Feeding Day is best fit I am going to get, without faffing about with Bayesian.
# Stick with PRTmodgamma_feedingday.

#~~~~~~~~~
  
# Model simplification for PRTmodgamma_feedingday model ####

car::Anova(PRTmodgamma_feedingday)

# no MJ*Status interaction
gammamod_noMJStatusInt <- update(PRTmodgamma_feedingday, . ~ . - MeanMJperFeedingDay:SocialStatus)
anova(PRTmodgamma_feedingday, gammamod_noMJStatusInt) # x2(1)=3.6397, p=0.05642. # MJ*Status borderline sig.
bbmle::AICtab(PRTmodgamma_feedingday, gammamod_noMJStatusInt) 
# dAIC = 1.6, not really enough to justify keeping it in

car::Anova(gammamod_noMJStatusInt)

# no DF*Status interaction
gammamod_noDFStatusInt <- update(gammamod_noMJStatusInt, . ~ . - DaysFedPerWeek:SocialStatus)
anova(gammamod_noMJStatusInt, gammamod_noDFStatusInt) # x2(1)=6.171, p=0.01299* # need DF*Status

# no Sex*Season interaction
gammamod_noSSInt <- update(gammamod_noMJStatusInt, . ~ . - Sex:fSeason)
anova(gammamod_noMJStatusInt, gammamod_noSSInt) # x2(3)= 63.081, p=1.29e-13***

# no FeedingDay
gammamod_noFD <- update(gammamod_noMJStatusInt, . ~ . - fFeedingDay)
anova(gammamod_noMJStatusInt, gammamod_noFD) # x2(1)= 1043.8, p<2.2e-16 ***

# no MJ
gammamod_noMJ <- update(gammamod_noMJStatusInt, . ~ . - MeanMJperFeedingDay)
anova(gammamod_noMJStatusInt, gammamod_noMJ) # x2(1)=27.381 , p=1.671e-07***

# Final model
a <- coef(summary(gammamod_noMJStatusInt))

#~~~

# Paper gamma model: preds & plotting ####

### PREDICTIONS

# FeedingDay
lsm_preds <- lsmeans::lsmeans(gammamod_noMJStatusInt, ~fFeedingDay, type="response")
lsmFD <- data.frame(summary(lsm_preds))
# rename for plot labels
lsmFD$FD <- ordered(lsmFD$fFeedingDay, levels = c("1", "0"), labels=c("Provisioning day", "Non-provisioning day")) 


# Sex*Season #
# averaged across FeedingDay
lsm_ss <- lsmeans::lsmeans(gammamod_noMJStatusInt, ~fSeason|Sex, type="response") 
lsmSS <- data.frame(summary(lsm_ss))
# rename sex for clearer plot legend
lsmSS$SexLong <- factor(lsmSS$Sex, levels = c("M", "F"), labels=c("Male", "Female")) 

# with FeedingDay
lsm_ss <- lsmeans::lsmeans(gammamod_noMJStatusInt, c("fSeason", "Sex", "fFeedingDay"), type="response") 
lsmSSF <- data.frame(summary(lsm_ss))
# rename sex for clearer plot legend
lsmSSF$SexLong <- factor(lsmSSF$Sex, levels = c("M", "F"), labels=c("Male", "Female")) 


# Status*DF #
# without FeedingDay
lsm_preds <- lsmeans::lsmeans(gammamod_noMJStatusInt, "DaysFedPerWeek", 
                               by = c("SocialStatus"),
                               at = list(DaysFedPerWeek = seq(1,7,1), 
                                         SocialStatus=c("Dom","Sub")), 
                               type="response")
lsmDF <- data.frame(summary(lsm_preds))
# rename status for clearer plot legend
lsmDF$StatusLong <- factor(lsmDF$SocialStatus, levels = c("Dom", "Sub"), labels=c("Dominant", "Subordinate")) 

# with FeedingDay
lsm_predsF <- lsmeans::lsmeans(gammamod_noMJStatusInt, c("DaysFedPerWeek", "fFeedingDay"), 
                              by = c("SocialStatus"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        SocialStatus=c("Dom","Sub")), 
                              type="response")
lsmDFF <- data.frame(summary(lsm_predsF))
# rename status for clearer plot legend
lsmDFF$StatusLong <- factor(lsmDFF$SocialStatus, levels = c("Dom", "Sub"), labels=c("Dominant", "Subordinate")) 
lsmDFF$FD <- ordered(lsmDFF$fFeedingDay, levels = c("1", "0"), labels=c("Provisioning day", "Non-provisioning day")) 

# MJ
# without FeedingDay
lsm_preds <- lsmeans::lsmeans(gammamod_noMJStatusInt, "MeanMJperFeedingDay", 
                              at = list(MeanMJperFeedingDay = seq(0,6,1)),
                              type="response")
lsmMJ <- data.frame(summary(lsm_preds))

# with FeedingDay
lsm_predsF <- lsmeans::lsmeans(gammamod_noMJStatusInt, c("MeanMJperFeedingDay", "fFeedingDay"), 
                              at = list(MeanMJperFeedingDay = seq(0,6,1)),
                              type="response")
lsmMJF <- data.frame(summary(lsm_predsF))

#~~

# Contrasts

# DF*Status:
# Do doms have different PRTs to subs at patches where fed either 1 day per week or 7 days per week?
lsm_contrastsSDF <- lsmeans::lsmeans(gammamod_noMJStatusInt, pairwise~SocialStatus|DaysFedPerWeek,
                                     at = list(DaysFedPerWeek = c(1,7),
                                               SocialStatus=c("Dom", "Sub")),
                                     type="response") 
# Do doms/ subs have higher or lower PRT where fed 1 vs 7 days per week?
lsm_contrastsSDFvv <- lsmeans::lsmeans(gammamod_noMJStatusInt, pairwise~DaysFedPerWeek|SocialStatus,
                                       at = list(DaysFedPerWeek = c(1,7),
                                                 SocialStatus=c("Dom", "Sub")))  
# combine all tests into same dataframe and correct P for multiple testing, to avoid false negs
a <-pairs(lsm_contrastsSDF)
b <-pairs(lsm_contrastsSDFvv)
lsmSDFcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")


# Use lstrends to compare the slopes (effects) of DF between doms and subs 
# (as not interested in differences between specific values of DF, e.g. once vs twice per week)
# estimate and compare slope of DF for each social status:
lst <- lsmeans::lstrends(gammamod_noMJStatusInt, pairwise~SocialStatus, 
                         var="DaysFedPerWeek", type="response") 
a <- data.frame(summary(lst$lsmeans)) # save for Excel
a <- data.frame(summary(lst$contrasts)) # save for Excel
lsmeans::cld(lst) # view groups: the slopes differ significantly



## Sex*season
# test
lsm_ss1 <- pairs(lsmeans::lsmeans(gammamod_noMJStatusInt, ~fSeason|Sex) )
lsm_ss2 <- pairs(lsmeans::lsmeans(gammamod_noMJStatusInt, ~Sex|fSeason) )
# combine all tests into same dataframe and correct P for multiple testing, to avoid false negs
lsm_tukey <- lsmeans::test(rbind(lsm_ss1, lsm_ss2), type="response", adjust="tukey")


#~~~

### PLOTTING
library(ggplot2)


## Sex * season

# Predictions only, as points joined by lines (clearer but no raw data)
ggplot(lsmSS, aes(x=fSeason, y=response, colour=SexLong)) + 
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.87,0.91),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_colour_manual(values = c("gray30","gray65")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2,
                position=position_dodge(0.25)) +
  geom_point(size=4, pch=15, aes(group=SexLong), position=position_dodge(0.25)) +
  geom_line(size=1.2, aes(group=SexLong), position=position_dodge(0.25))
# dev.print(jpeg, "RPlot_lsmPRT-sexXseason_CoreCombiGamMod_LinePlot_BW.jpeg", res=700, height=6, width=7, units="in") 


# Predictions only, as a bar plot - BEST (better than lines as seasons were not studied in order).
# plot averaged over FeedingDay
ggp1 <- ggplot(lsmSS, aes(x=fSeason, y=response, fill=SexLong)) 
# Plot FeedingDays only
ggp <- ggplot(subset(lsmSSF, fFeedingDay=="1"), aes(x=fSeason, y=response, fill=SexLong)) 

ggp1 + 
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") + 
  scale_fill_manual(name="SexLong", values = c("gray50","white")) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.89,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.6, width=0.2,
                position=position_dodge(0.9))
#dev.print(jpeg, "RPlot_lsmPRT-sexXseason_CoreCombiGamMod_Barplot_FDonly.jpeg", res=700, height=6, width=7, units="in") 



# Preds with raw data: 

# First calc mean PRT per day per fox per patch, PER SEASON, from raw data
PRTpatchmeans_season <- ddply(PRTsexstatcore, c("ShortCode", "StationCode", "Sex", "SocialStatus", "fSeason"),
                              summarise,
                              meanPRT = mean(totalPRT),
                              sdPRT = sd(totalPRT),
                              meanDF = mean(DaysFedPerWeek),
                              meanMJ = mean(MeanMJperFeedingDay))
# rename sex for clearer plot legend
PRTpatchmeans_season$SexLong <- factor(PRTpatchmeans_season$Sex, levels = c("M", "F"), labels=c("Male", "Female")) 


# Plot points for predictions & with box plot of raw means in background
# plot averaged over FeedingDay
ggp <- ggplot(lsmSS, aes(x=fSeason, y=response, colour=SexLong)) 
# Plot FeedingDays only
ggp <- ggplot(subset(lsmSSF, fFeedingDay=="1"), aes(x=fSeason, y=response, colour=SexLong)) 

ggp +
  geom_boxplot(data=PRTpatchmeans_season, 
               aes(x=fSeason, y=meanPRT, fill=SexLong, colour=SexLong), 
               size=0.7, alpha=0.3, position=position_dodge(0.8)) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.88,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_colour_manual(values = c("gray10","gray45")) +
  scale_fill_manual(values = c("gray30","gray75"), guide=FALSE) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  ylim(0,1500) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.9, width=0.15, position=position_dodge(0.8)) +
  geom_point(size=4, pch=15, aes(group=SexLong), position=position_dodge(0.8)) 
#dev.print(jpeg, "lsmPRT-sexXseason_boxplotismeanPRT.FoxPatchSeas_omit14rowsOver1500.jpeg", res=700, height=6, width=7, units="in") 


#~~~

### DF  (at mean of other things) 

# Preds with raw data:

# First calc mean PRT per day per fox per patch (across whole year), from raw data
PRTpatchmeans_year <- ddply(PRTsexstatcore, c("ShortCode", "StationCode", "Sex", "SocialStatus"),
                            summarise,
                            meanPRT = mean(totalPRT),
                            sdPRT = sd(totalPRT),
                            meanDF = mean(DaysFedPerWeek),
                            meanMJ = mean(MeanMJperFeedingDay))
# rename status for clearer plot legend
PRTpatchmeans_year$StatusLong <- factor(PRTpatchmeans_year$SocialStatus, levels = c("Dom", "Sub"), labels=c("Dominant", "Subordinate")) 

# Preds only, as dots and lines
ggplot(lsmDF, aes(x=DaysFedPerWeek, y=response, colour=StatusLong)) +
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        legend.position = c(0.83,0.91), 
        legend.background = element_rect(fill = NA), 
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_colour_manual(name="Social status", values = c("gray30","gray65")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2,
                position=position_dodge(0.25)) +
  geom_point(size=4, aes(group=StatusLong), position=position_dodge(0.25)) +
  geom_line(size=1.2, aes(group=StatusLong), position=position_dodge(0.25)) 
# dev.print(jpeg, "RPlot_lsmPRT-DFxStatus_CoreCombiGamMod_LinePlot.jpeg", res=700, height=6, width=7, units="in") 

# Preds as dot and line with points for raw means
ggplot(lsmDF, aes(x=DaysFedPerWeek, y=response, colour=StatusLong, shape=StatusLong)) +
  geom_point(data=PRTpatchmeans_year, aes(x=meanDF, y=meanPRT, shape=StatusLong),
             size=2.5, position=position_jitter()) +
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        legend.position = c(0.16,0.91), 
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  ylim(0,500) +
  scale_colour_manual(values = c("gray30","gray65")) +
  geom_line(size=1.2, position=position_dodge(0.25)) +
  geom_point(size=4, position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.2, position=position_dodge(0.25))
#dev.print(jpeg, "lsmPRT-DFxStatus_CoreCombiGamMod_LinePlot_Points4MeanDPRT.FoxPatchYr.Omits66rowsOver500s.jpeg", res=700, height=6, width=7, units="in") 


# Preds & raw means with coloured patches (no one patch looks overly influential)

# Preds as dot and line with points for raw means
ggplot(lsmDF, aes(x=DaysFedPerWeek, y=response, shape=StatusLong)) +
  geom_point(data=PRTpatchmeans_year, aes(x=meanDF, y=meanPRT, shape=StatusLong, colour=StationCode),
             size=3, position=position_jitter()) +
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        #legend.position = c(0.16,0.91), 
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  # ylim(0,500) +
  #scale_colour_manual(values = c("gray30","gray65")) +
  geom_line(size=1.2, position=position_dodge(0.25)) +
  geom_point(size=5, position=position_dodge(0.25)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2, position=position_dodge(0.25))
# dev.print(jpeg, "RPlot_lsmPRT-DFxStatus_CoreCombiGamMod_LinePlot_Points4MeanDPRT.FoxPatchYr_colourPatches.jpeg", res=700, height=6, width=7, units="in") 


# Model preds per feeding day, with raw mean daily PRT in background (lsmDFF)
ggplot(lsmDFF, aes(x=DaysFedPerWeek, y=response, colour=StatusLong)) +
  geom_boxplot(data=MEANdailyPRT_FD, aes(x=factor(round(DaysFedPerWeek)), y=meanPRT, 
              fill=StatusLong), size=0.5, position=position_dodge(0.85), colour="gray55") +
  geom_point(size=2.5, pch=15, position=position_dodge(0.85)) + 
  facet_wrap(~FD) +   
  ylim(0,1500) +
  geom_line(size=0.9, position=position_dodge(0.85)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.8, width=0.25, position=position_dodge(0.85)) +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        legend.position = c(0.12,0.91), 
        legend.background = element_rect(fill = NA),
        legend.title=element_blank(),
        axis.text = element_text(colour = "black"),
        strip.background = element_rect(color = "black", size = 1.2)) +
  scale_fill_manual(values = c("gray70","gray95"), name="Social status", guide=FALSE) +
  scale_colour_manual(values = c("gray20","gray45"), name="Social status") + 
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") 
#dev.print(jpeg, "lsmPRTDFXStatus_FacetFD_Core_BoxplotrawMeanPRT.FoxPatchDay_omits24Over1500s_sq.jpeg", res=700, height=6, width=9, units="in") 


# Model preds on feeding days only (lsmDFF)
ggplot(subset(lsmDFF, fFeedingDay=="1"), aes(x=DaysFedPerWeek, y=response, colour=StatusLong)) + 
  geom_boxplot(data=subset(MEANdailyPRT_FD, FeedingDay=="TRUE"), 
               aes(x=factor(round(DaysFedPerWeek)), y=meanPRT, fill=StatusLong), 
               size=0.6, position=position_dodge(0.8), alpha=0.3) +
    geom_point(size=3, pch=15, aes(group=StatusLong), position=position_dodge(0.8)) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.9, width=0.15, position=position_dodge(0.8)) +
    geom_line(size=1, position=position_dodge(0.8)) +
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.17,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
scale_fill_manual(values = c("gray35","gray90"), name="Social status", guide=FALSE) +
  scale_colour_manual(values = c("gray20","gray45"), name="Social status") + 
    ylim(0,1500)
#dev.print(jpeg, "lsmPRTDFXStatus_Core_Preds&rawBoxplotMeanPRT.FoxPatchDay_omits15Over1500s_FDonly.jpeg", res=700, height=6, width=7, units="in") 


# Model preds averaged across feeding days (lsmDF)
ggplot(lsmDF, aes(x=DaysFedPerWeek, y=response, colour=StatusLong)) + 
  geom_boxplot(data=MEANdailyPRT_FD, 
               aes(x=factor(round(DaysFedPerWeek)), y=meanPRT, fill=StatusLong), 
               size=0.6, position=position_dodge(0.8), alpha=0.3) +
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.17,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
scale_fill_manual(values = c("gray35","gray90"), name="Social status", guide=FALSE) +
  scale_colour_manual(values = c("gray20","gray45"), name="Social status") + 
    ylim(0,1500) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.15, position=position_dodge(0.8)) +
  geom_point(size=3, pch=15, aes(group=StatusLong), position=position_dodge(0.8)) +
  geom_line(size=1)
#dev.print(jpeg, "lsmPRTDFXStatus_Core_Preds&rawBoxplotMeanPRT.FoxPatchDay_omits24Over1500s.jpeg", res=700, height=6, width=7, units="in") 



#~~~~~

### MJ

# B&W preds only:
ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response)) + 
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.19,0.89), 
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black"),
        legend.title = element_blank()) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  scale_colour_manual(values = c("gray30","gray65")) +
  geom_line(size=1.2) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2)
#dev.print(jpeg, "RPlot_lsmPRT-MJ_CoreCombiGamMod_LinePlot.jpeg", res=700, height=6, width=7, units="in") 


# B&W preds with raw means:
# all
ggp <- ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response))
# FeedingDays only
ggp <- ggplot(subset(lsmMJF, fFeedingDay=="1"), aes(x=MeanMJperFeedingDay, y=response))
ggp +
  geom_point(data=PRTpatchmeans_year, aes(x=meanMJ, y=meanPRT), size=2, colour="gray65") +
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  ylim(0,2000) +
  scale_colour_manual(values = c("gray30")) +  
  geom_line(size=1.2) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2)
#dev.print(jpeg, "lsmPRT-MJ_CoreCombiGamMod_RawMeanDPRT.FoxPatchYr_omits3rowsOver1500.jpeg", res=700, height=6, width=7, units="in") 



# Preds with raw means - COLOURED PATCHES:

# ALL PATCHES
ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response)) +
  geom_point(data=PRTpatchmeans_year, 
             aes(x=meanMJ, y=meanPRT, colour=StationCode), size=2) +
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  geom_line(size=1.2) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2)
#dev.print(jpeg, "RPlot_lsmPRT-MJ_CoreCombiGamMod_LinePlot_ColouredPatches.jpeg", res=700, height=6, width=7, units="in") 


# WITHOUT 16WD
ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response)) +
  geom_point(data=subset(PRTpatchmeans_year, StationCode!="16WD"), 
             aes(x=meanMJ, y=meanPRT, colour=StationCode), size=2) +
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  geom_line(size=1.2) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2)
#dev.print(jpeg, "RPlot_lsmPRT-MJ_CoreCombiGamMod_LinePlot_ColouredPatches-16WD.jpeg", res=700, height=6, width=7, units="in") 


# WITHOUT 13DCV
ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response)) +
  geom_point(data=subset(PRTpatchmeans_year, StationCode!="13DCV"), 
             aes(x=meanMJ, y=meanPRT, colour=StationCode), size=2) +
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  geom_line(size=1.2) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2)
#dev.print(jpeg, "RPlot_lsmPRT-MJ_CoreCombiGamMod_LinePlot_ColouredPatches-13DCV.jpeg", res=700, height=6, width=7, units="in") 


# WITHOUT 6EL
ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response)) +
  geom_point(data=subset(PRTpatchmeans_year, StationCode!="6EL"), 
             aes(x=meanMJ, y=meanPRT, colour=StationCode), size=2) +
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  geom_line(size=1.2) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1.2, width=0.2)
#dev.print(jpeg, "RPlot_lsmPRT-MJ_CoreCombiGamMod_LinePlot_ColouredPatches-6EL.jpeg", res=700, height=6, width=7, units="in") 


## Feeding day

# just preds
ggplot(lsmFD, aes(x=FD, y=response)) + 
  geom_bar(stat="identity", colour="black", fill="gray80", width=0.7) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.17,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.15) 
#dev.print(jpeg, "lsmPRT_FeedingDay_barplot.jpeg", res=700, height=6, width=7, units="in") 


#  Raw data only - bar plot

# first calc mean PRT on feeding and non-feeding days (rough as doesn't control for repeated measures...)
rawPRT_FD <- ddply(MEANdailyPRT_FD, "FD", summarise,
                      meanPRTFD=mean(meanPRT),
                      sd=sd(meanPRT),
                      N=length(meanPRT),
                      se= sd / sqrt(N))

ggplot(rawPRT_FD, aes(x=FD, y=meanPRTFD)) + 
  geom_bar(stat="identity", colour="black", fill="gray80", width=0.7) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  geom_errorbar(aes(ymin=meanPRTFD-se, ymax=meanPRTFD+se), size=1, width=0.15)
#dev.print(jpeg, "Raw meanPRT+SE_FeedingDay_barplot.jpeg", res=700, height=6, width=7, units="in") 



# Preds & raw
ggplot(lsmFD, aes(x=FD, y=response)) + 
      geom_boxplot(data=MEANdailyPRT_FD, aes(x=FD, y=meanPRT), fill="grey80") +
    geom_point(size=3, pch=15) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.17,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=1, width=0.1) +
  ylim(0,1500)
#dev.print(jpeg, "lsmPRT_FeedingDay_PointPreds_boxplotRawMeans_omits24rowsOver1500s.jpeg", res=700, height=6, width=7, units="in") 




#### Arrange the best plots above in a 2X2 panel figure following the special requirements for BEHECO paper: ####

# (Fig 4a) PRT by Provisioning Day (Y/N)
ggp4a <- ggplot(lsmFD, aes(x=FD, y=response)) + 
  geom_bar(stat="identity", colour="black", fill="gray80", width=0.7) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.17,0.92),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=14)) +
  scale_x_discrete(breaks=c("Provisioning day","Non-provisioning day"), labels=c("Provisioning\nday", "Non-provisioning\nday")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.6, width=0.3) +
  ggtitle("(a)") 

# (Fig 4b) PRT by Sex X Season - averaged over Provisioning Day (Y/N)
ggp4b <- ggplot(lsmSS, aes(x=fSeason, y=response, fill=SexLong)) + 
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") + 
  scale_fill_manual(name="SexLong", values = c("gray50","white")) +
  xlab("") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.85,0.88),
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.text = element_text(colour = "black"),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=14)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.6, width=0.3,
                position=position_dodge(0.9)) +
  ggtitle("(b)") 

# (Fig 4c) PRT by X MJ - averaged over Provisioning Day (Y/N)
ggp4c <- ggplot(lsmMJ, aes(x=MeanMJperFeedingDay, y=response)) + 
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        strip.text.y = element_text(size=NULL, face="bold"),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=14)) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  scale_colour_manual(values = c("gray30","gray65")) +
  geom_line(size=1.1) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.6, width=0.3) +
  ggtitle("(c)") 

# (Fig 4d) PRT by DF X Status - averaged over Provisioning Day (Y/N)
ggp4d <- ggplot(lsmDF, aes(x=DaysFedPerWeek, y=response, colour=StatusLong)) +
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        legend.position = c(0.8,0.89), 
        legend.background = element_rect(fill = NA), 
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size=13),
        plot.title = element_text(size=14)) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_colour_manual(name="Social status", values = c("gray30","gray65")) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.6, width=0.3,
                position=position_dodge(0.25)) +
  geom_point(size=3, aes(group=StatusLong), position=position_dodge(0.25)) +
  geom_line(size=1.1, aes(group=StatusLong), position=position_dodge(0.25)) +
  ggtitle("(d)") 

# plot the 2 ggplots side by side on a panel (as can't use par() command for ggplots)
library(gridExtra)
grid.arrange(ggp4a, ggp4b, ggp4c, ggp4d, nrow=NULL, ncol=2) # where ggp is ggplot2 code for each plot

#dev.print(tiff, "Figure 4. 150 X 177mm_res1200.tif", res=1200, height=150, width=177, units="mm") 





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DO CORE GROUP MEMBERS STAY LONGER IN PATCHES WITH GREATER FOOD AVAILABILITY?  - in thesis, not in paper ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 'Core' causes collinearity problems.
# tried entering and changing core=1 to reference category (as contained more observations) 
# but didnt change model much so decided to keep as is and gnore the collinearity.

### FULL MODEL
COREPRTmod_full <- lmer(log10(totalPRT) ~ 
                          DaysFedPerWeek*Core +
                          MeanMJperFeedingDay*Core +
                          (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                        data=dailyPRT, REML=F,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=100000), 
                                            check.conv.grad=.makeCC("warning",0.05))) 
# NO DF*CORE
COREPRTmod_noDFint <- lmer(log10(totalPRT) ~ 
                             DaysFedPerWeek +
                             MeanMJperFeedingDay*Core +
                             (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                           data=dailyPRT, REML=F,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05))) 
anova(COREPRTmod_full, COREPRTmod_noDFint) # x2= 7.4596,1,0.00631**   # NEED DF*CORE

# NO MJ*CORE
COREPRTmod_noMJint <- lmer(log10(totalPRT) ~ 
                             DaysFedPerWeek*Core +
                             MeanMJperFeedingDay +
                             (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                           data=dailyPRT, REML=F,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05))) 
anova(COREPRTmod_full, COREPRTmod_noMJint) # x2=4.836,1,0.02787*  # NEED MJ*CORE

# FINAL MODEL = FULL MODEL
COREPRTmod_full

# check model fit
plot(fitted(COREPRTmod_full), resid(COREPRTmod_full)) # OK
lines(smooth.spline(fitted(COREPRTmod_full), resid(COREPRTmod_full)), col="red" )
hist(resid(COREPRTmod_full))

# Check VIF
vif.mer(COREPRTmod_full) # SHOULD BE <5. OK AS ONLY ONE >5 IS due to core + IS CATEGORICAL (but represented as 0 and 1 so R interprets this as a correlation)

a <- coef(summary(COREPRTmod_full))


### MAKE PREDICTIONS
library(lsmeans)

# ORIGINAL DATA: Rename CORE for ggplot2 facet labels
dailyPRT$CORElong <- ordered(dailyPRT$Core, levels = c(1,0), labels=c("Core", "Not core"))
MEANdailyPRT$CORElong <- ordered(MEANdailyPRT$Core, levels = c(1,0), labels=c("Core", "Not core"))


# Plot MJ*Core - in thesis

# for reporting
lsm_cMJ <- lsmeans::lsmeans(COREPRTmod_full, "MeanMJperFeedingDay", 
                            by = "Core",
                            at = list(MeanMJperFeedingDay = c(0.1,seq(0.5,6,1)),
                                      Core=c("0", "1")), type="response") 
lsm_cMJ <- lsmeans::lsmeans(COREPRTmod_full, ~MeanMJperFeedingDay|Core, type="response") 
# for plotting
lsm_cMJ <- lsmeans::lsmeans(COREPRTmod_full, "MeanMJperFeedingDay", 
                            by = "Core",
                            at = list(MeanMJperFeedingDay = seq(0,6,0.5),
                                      Core=c("0", "1")), type="response") 
lsmCMJ <- data.frame(summary(lsm_cMJ))

# rename CORE for ggplot2 facet labels
lsmCMJ$CORElong <- ordered(lsmCMJ$Core, levels = c(1,0), labels=c("Resident", "Non-resident")) 
MEANdailyPRT$CORElong <- ordered(MEANdailyPRT$Core, levels = c(1,0), labels=c("Resident", "Non-resident")) 

ggp <- ggplot(lsmCMJ, aes(x=MeanMJperFeedingDay, y=response, colour=CORElong))
# plot in colour (did not use in paper)
ggp + 
  xlab("\nMJ per provisioning day") + ylab("Patch residence time (s)\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.y = element_text(size=NULL, face="bold")) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  # scale_y_continuous(breaks=seq(0,2500,500)) + # for when overlay raw data
  scale_colour_manual(name="", values = c("royalblue3","firebrick1")) +
  geom_line(size=1, aes(group=CORElong)) +
  geom_point(size=3, aes(group=CORElong)) +
  geom_ribbon(data=lsmCMJ, mapping=aes(x=MeanMJperFeedingDay, ymin=asymp.LCL, ymax=asymp.UCL), 
              fill="grey40", lty=0, alpha=0.3) # + add plus to overlay raw data: 
  # overlay raw data
  geom_point(data=MEANdailyPRT, aes(MeanMJperFeedingDay, meanPRT, colour=CORElong), 
             position=position_jitter()) 

# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT with MJ-Core int_ribbon RESIDENT.jpeg", res=700, height=6, width=8, units="in") 
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT with MJ-Core int WITH raw means overlaid RESIDENT.jpeg", res=700, height=6, width=8, units="in") 

# plot in B&W 
ggp + 
  xlab("MJ per provisioning day") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
      strip.text.y = element_text(size=NULL, face="bold"),
      legend.position = c(0.16,0.96),
      legend.background = element_rect(fill = NA)) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(breaks=seq(0,700,100)) +
  # scale_y_continuous(breaks=seq(0,2500,500)) + # for when overlay raw data
  scale_colour_manual(name="", values = c("gray30","gray65")) +
  geom_line(size=1.2, aes(group=CORElong)) +
  geom_point(size=4, aes(group=CORElong)) +
  geom_ribbon(data=lsmCMJ, mapping=aes(x=MeanMJperFeedingDay, ymin=asymp.LCL, ymax=asymp.UCL), 
            fill="grey60", lty=0, alpha=0.3) 
# save
dev.print(jpeg, "RPlot_lsmean preds for daily PRT with MJ-Core int WITH raw means overlaid RESIDENT_BW.jpeg", res=700, height=6, width=7, units="in") 


# model preds for plotting
lsm_cDF <- lsmeans::lsmeans(COREPRTmod_full, "DaysFedPerWeek", 
                            by = "Core",
                            at = list(DaysFedPerWeek = seq(1,7,1),
                                      Core=c("0", "1")), type="response") 
lsmCDF <- data.frame(summary(lsm_cDF))
lsmCDF$CORElong <- ordered(lsmCDF$Core, levels = c(1,0), labels=c("Resident", "Non-resident")) 


# plot...
ggp <- ggplot(lsmCDF, aes(x=DaysFedPerWeek, y=response, colour=CORElong))

# ..in colour
ggp + 
  xlab("\nDays provisioned per week") + ylab("Patch residence time (s)\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.y = element_text(size=NULL, face="bold")) +
  scale_x_continuous(breaks = 0:7) +
  scale_y_continuous(breaks = seq(0,140,20)) +
  scale_colour_manual(name="", values = c("royalblue3","firebrick1")) +
  geom_line(size=1, aes(group=CORElong)) +
  geom_point(size=3, aes(group=CORElong)) +
  geom_ribbon(data=lsmCDF, mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), 
              fill="grey40", lty=0, alpha=0.3) 
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT with DF-Core int_ribbon RESIDENT.jpeg", res=700, height=6, width=8, units="in") 

# B&W for paper
ggp + 
  xlab("Days provisioned per week") + ylab("Patch residence time (s)") +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.16,0.96),
        legend.background = element_rect(fill = NA)) +
  scale_x_continuous(breaks = 0:7) +
  scale_y_continuous(breaks = seq(0,140,20)) +
  scale_colour_manual(name="", values = c("gray30","gray65")) +
  geom_line(size=1.2, aes(group=CORElong)) +
  geom_point(size=4, aes(group=CORElong)) +
  geom_ribbon(data=lsmCDF, mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), 
              fill="grey60", lty=0, alpha=0.3) 
#dev.print(jpeg, "RPlot_lsmean preds for daily PRT with DF-Core int_ribbon RESIDENT_BW2.jpeg", res=700, height=6, width=7, units="in") 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### POST_HOC TESTs (DONT THINK THIS WAS NECESSARY FOR CONTINUOUS*CATEGORICAL WITH ONLY 2 LEVELS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Core*MJ
# Diff between core and non-core at diff levels of Mean MJ per day
lsm_contrastsCMJ <- lsmeans::lsmeans(COREPRTmod_full, pairwise~Core|MeanMJperFeedingDay,
                                     at = list(MeanMJperFeedingDay = c(0.1,1,2,3,4,5,6),
                                               Core=c("0", "1")), 
                                     type="response", adjust="tukey")  
# Diff between food quality for core and non-core
lsm_contrastsCMJvv <- lsmeans::lsmeans(COREPRTmod_full, pairwise~MeanMJperFeedingDay|Core,
                                       at = list(MeanMJperFeedingDay = c(1,6),
                                                 Core=c("0", "1")), 
                                       type="response", adjust="tukey")  
# Combine all tests to adjust p-values and avoid false negs
a <-pairs(lsm_contrastsCMJ)
b <-pairs(lsm_contrastsCMJvv)
lsmCMJcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")

## Core*MJ
# Diff between core and non-core at diff levels of Mean MJ per day
lsm_contrastsCDF <- lsmeans::lsmeans(COREPRTmod_full, pairwise~Core|DaysFedPerWeek,
                                     at = list(DaysFedPerWeek = seq(1,7,1),
                                               Core=c("0", "1")), 
                                     type="response", adjust="tukey")  
# Diff between food quality for core and non-core
lsm_contrastsCDFvv <- lsmeans::lsmeans(COREPRTmod_full, pairwise~DaysFedPerWeek|Core,
                                       at = list(DaysFedPerWeek = c(1,7),
                                                 Core=c("0", "1")), 
                                       type="response", adjust="tukey")  
# Combine all tests to adjust p-values and avoid false negs
a <-pairs(lsm_contrastsCDF)
b <-pairs(lsm_contrastsCDFvv)
lsmCDFcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")


## Status*DF
# Dif between dom and sub at diff DF - ALL FOXES
lsm_contrastsSDF <- lsmeans::lsmeans(dailyPRTmod_FINAL, pairwise~SocialStatus|DaysFedPerWeek,
                                     at = list(DaysFedPerWeek = seq(1,7,2),
                                               SocialStatus=c("Dom", "Sub"))) 
# Diff between DF for dom and sub
lsm_contrastsSDFvv <- lsmeans::lsmeans(dailyPRTmod_FINAL, pairwise~DaysFedPerWeek|SocialStatus,
                                       at = list(DaysFedPerWeek = c(1,7),
                                                 SocialStatus=c("Dom", "Sub")))  
# Combine all tests to adjust p-values and avoid false negs
a <-pairs(lsm_contrastsSDF)
b <-pairs(lsm_contrastsSDFvv)
lsmSDFcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")



## Status*MJ - CORE FOXES ONLY
# Dif between dom and sub at diff MJ
lsm_contrastsSMJ <- lsmeans::lsmeans(dailyPRTmodCORE, pairwise~SocialStatus|MeanMJperFeedingDay,
                                     at = list(MeanMJperFeedingDay = c(0.1,1,2,3,4,5,6),
                                               SocialStatus=c("Dom", "Sub"))) 
# Diff between MJ for dom and sub
lsm_contrastsSMJvv <- lsmeans::lsmeans(dailyPRTmodCORE, pairwise~MeanMJperFeedingDay|SocialStatus,
                                       at = list(MeanMJperFeedingDay = c(1,6),
                                                 SocialStatus=c("Dom", "Sub")))  
# Combine all tests to adjust p-values and avoid false negs
a <-pairs(lsm_contrastsSMJ)
b <-pairs(lsm_contrastsSMJvv)
lsmSDFcombo <- lsmeans::test(rbind(a,b), type="response", adjust="tukey")

