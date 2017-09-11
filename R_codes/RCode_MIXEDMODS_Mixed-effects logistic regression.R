rm(list=ls()) #to clear the R workspace
dev.off()
setwd("~/Analysis/MLM Group stats practice")

#############################################################################################
              ### Mixed-effects logistic regression ###
#############################################################################################

# R's main command for fitting multilevel models for binary and other discrete
# response variables is the glmer command
library(lme4) # to run regression models
library(plyr) # to do calculations on data
library(lattice) # need for xyplot

############# IMPORT DATA, TAKE A SUBSET AND SET UP THE VARIABLES:
mydata <- read.csv(file = "MultilevelModels_PatchResTimes.csv", header = TRUE) #import dataset

# Add column for whether or not the visit contained only 1 photo (or photo-burst)
# (if yes, shortvisdur =1, otherwise =0)
foxdata<-mutate(mydata, shortvisdur=ifelse(VisDur.S<7, 1, 0))    

## Make new column with unique ID number for each patch in new dataset called foxdata
foxdata<-mutate(foxdata, PatchID=as.integer(foxdata$StationCode))   

#Take subset of data that excludes cubs, foxes of unknown sex/status
# (!= means not-equal-to, == means equal-to)
foxdatasubset <- subset(foxdata, SocialStatus != "" & Sex != "" & AgeClass != "Cub")

# change numeric/integer categorical variables to factors
foxdatasubset$AnimalID<-as.factor(foxdatasubset$AnimalID)
foxdatasubset$Territory<-as.factor(foxdatasubset$Territory)
foxdatasubset$PatchID<-as.factor(foxdatasubset$PatchID)
foxdatasubset$BeforeMidnight<-as.factor(foxdatasubset$BeforeMidnight)
foxdatasubset$OtherFoxesPresent<-as.factor(foxdatasubset$OtherFoxesPresent)
foxdatasubset$OtherSpeciesPresent<-as.factor(foxdatasubset$OtherSpeciesPresent)

# re-level Seasons, Sex & Status in logical order (and exclude "Unknown" from level lists for later plotting):
print(levels(foxdatasubset$Season)) #automatically listed in alphabetic order
foxdatasubset$Season <- factor(foxdatasubset$Season,levels(foxdatasubset$Season)[c(2,3,1,4)]) #change order
print(levels(foxdatasubset$Season)) #much better
foxdatasubset$Sex <- factor(foxdatasubset$Sex,levels(foxdatasubset$Sex)[c(3,2)]) #change order & remove unknown
foxdatasubset$SocialStatus <- factor(foxdatasubset$SocialStatus,levels(foxdatasubset$SocialStatus)[c(2,3)]) #remove unknown

# use rownames (auto row number) to add a new column for Visit ID number (as the Level 1 unit identifiers / individ random effects) 
foxdatasubset$Visit<-as.numeric(rownames(foxdatasubset))

############################################################
# Check distribution of VisDur.S
hist(foxdatasubset$VisDur.S)  #data is skewed (negative binomial distribution?)
hist(log(foxdatasubset$VisDur.S))  #log-transform visit duration in attempt to normalise

# Take subset of data that excludes visits with durations <=6secs or >6000secs
foxdatasubset.minvisdur <- subset(foxdatasubset, shortvisdur == "0")
levels(as.factor(foxdatasubset.minvisdur$shortvisdur)) #check subset does not include short visits (<=6 sec)

# Check distribution of VisDur.S for subset with only long visits
hist(foxdatasubset.minvisdur$VisDur.S)  # plot hist of subset with only visits >6 seconds long
hist(log(foxdatasubset.minvisdur$VisDur.S))  # log-transformed visits >6 secs long only - approx normal
############################################################

str(foxdatasubset)
hist(foxdatasubset$DaysSeen)
hist(log(foxdatasubset$DaysSeen))
hist(sqrt(foxdatasubset$DaysSeen))
hist(foxdatasubset$TotalVisits)
hist(log(foxdatasubset$TotalVisits))

# Take subset of data that excludes visits with durations <=6secs or >6000secs
foxdatasubset.minvisdur <- subset(foxdatasubset, shortvisdur == "0")

# calc mean visit duration for each fox
mvd <- ddply(foxdatasubset, c("Name", "AnimalID", "SocialStatus", "Sex", "DaysSeen", "TotalVisits"), 
                    summarise,
                    sumvd=sum(VisDur.S),
                    meanvd=mean(VisDur.S))

plot(mvd$meanvd,mvd$TotalVisits)





# fit <- glmer(antemed ~ (1 | comm), family = binomial("logit"), data = mydata) 
nullLR1 <- glmer(shortvisdur ~ (1|AnimalID) + (1|PatchID), family = binomial("logit"), data = foxdatasubset) 
summary(nullLR1)
