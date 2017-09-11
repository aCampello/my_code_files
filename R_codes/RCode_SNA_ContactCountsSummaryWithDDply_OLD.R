rm(list=ls()) #to clear the R workspace
setwd("~/Analysis/Contactcounts") # uni
setwd("G:/Statistics 2016/Contactcounts") # home



mydata <- read.table(file = "ContactCounts.txt", sep='\t', header = TRUE) #import dataset
str(mydata)

library(plyr)
#subsetting to remove unknowns
mydatasub<- subset(mydata, Sex!="Unknown" | SocialStatus!="Unknown")
#subsetting wont work for some reason so removed Unknowns in Excel
mydata <- read.table(file = "ContactCounts_NoUnknowns.txt", sep='\t', header = TRUE) #import dataset
str(mydata)

#find mean daily contact rates for each AnimalID
contactsummary.AnimalID <- ddply(mydata, c("SeasonID", "AnimalID", "Sex", "SocialStatus", "SexStatus"), summarise,
                              n.DaysObserved=length(TRANScDate),
                              m.TotVisits=mean(TotVisits),
                              m.TotSharedVisits=mean(TotSharedVisits),
                              m.TotEncountersAnyFox=mean(TotEncountersAnyFox),
                              m.TotEncountersPerStation=mean(TotEncountersPerStation),
                              med.TotEncountersPerStation=median(TotEncountersPerStation),
                              m.EncountersPerHour=mean(EncountersPerHour),
                              sum.TotVisDurat_NMins=sum(TotVisDurat_NMins),
                              m.TotVisDurat_NMins=mean(TotVisDurat_NMins),
                              m.Adult_encounters=mean(Ad_encounters),
                              m.Subadult_encounters=mean(Subad_encounters),
                              m.Cub_encounters=mean(Cub_encounters),
                              m.MaleDom=mean(AdultMaleDom),
                              m.MaleSubord=mean(AdultMaleSub),
                              m.MaleUnknownStatus=mean(AdultMaleUnknown),
                              m.FemaleDom=mean(AdultFemaleDom),
                              m.FemaleSubord=mean(AdultFemaleSub),
                              m.FemaleUnknownStatus=mean(AdultFemaleUnknown),
                              m.UnknownsexSubord=mean(AdultUnknownSub),
                              m.UnknownSexorStatus=mean(AdultUnknownUnknown))
                              
head(contactsummary.AnimalID) #view top few rows of the table                      

#find mean daily contact rates for individuals by attribute in each season, with St deviations
contactsummary.table <- ddply(contactsummary.AnimalID, c("SeasonID", "Sex", "SocialStatus", "SexStatus"), summarise,
                              n.Foxes=length(AnimalID),
                              mean.TotVisitsPerDay=mean(m.TotVisits),
                              sd.TotVisitsPerDay=sd(m.TotVisits),
                              se.TotVisitsPerDay=sd.TotVisitsPerDay/sqrt(n.Foxes),                              
                                  mean.TotSharedVisitsPerDay=mean(m.TotSharedVisits),
                                  sd.TotSharedVisitsPerDay=sd(m.TotSharedVisits),
                                  se.TotSharedVisitsPerDay= sd.TotSharedVisitsPerDay/sqrt(n.Foxes),                            
                              mean.TotEncountersAnyFoxPerDay=mean(m.TotEncountersAnyFox),
                              sd.TotEncountersAnyFoxPerDay=sd(m.TotEncountersAnyFox),
                              se.TotEncountersAnyFoxPerDay= sd.TotEncountersAnyFoxPerDay/sqrt(n.Foxes),                              
                                  mean.TotEncountersPerStationPerDay=mean(m.TotEncountersPerStation),
                                  sd.meanTotEncountersPerStationPerDay=sd(m.TotEncountersPerStation),
                                  se.meanTotEncountersPerStationPerDay= sd.meanTotEncountersPerStationPerDay/sqrt(n.Foxes),                              
                              median.TotEncountersPerStationPerDay=median(med.TotEncountersPerStation),
                              sd.medianTotEncountersPerStationPerDay=sd(med.TotEncountersPerStation),
                              se.medianTotEncountersPerStationPerDay= sd.medianTotEncountersPerStationPerDay/sqrt(n.Foxes),                              
                                  mean.EncountersPerHour=mean(m.EncountersPerHour),
                                  sd.EncountersPerHour=sd(m.EncountersPerHour),
                                  se.EncountersPerHour= sd.EncountersPerHour/sqrt(n.Foxes),                              
                              mean.TotVisDurat_NMins_PerDay=mean(m.TotVisDurat_NMins),
                              sd.TotVisDurat_NMins_PerDay=sd(m.TotVisDurat_NMins),
                              se.TotVisDurat_NMins_PerDay= sd.TotVisDurat_NMins_PerDay/sqrt(n.Foxes),                              
                                  mean.Adult_encounters_PerDay=mean(m.Adult_encounters),
                                  sd.Adult_encounters_PerDay=sd(m.Adult_encounters),
                                  se.Adult_encounters_PerDay= sd.Adult_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.Subadult_encounters_PerDay=mean(m.Subadult_encounters),
                              sd.Subadult_encounters_PerDay=sd(m.Subadult_encounters),
                              se.Subadult_encounters_PerDay= sd.Subadult_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.Cub_encounters_PerDay=mean(m.Cub_encounters),
                                  sd.Cub_encounters_PerDay=sd(m.Cub_encounters),
                                  se.Cub_encounters_PerDay= sd.Cub_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.MaleDominant_encounters_PerDay=mean(m.MaleDom),
                              sd.MaleDominant_encounters_PerDay=sd(m.MaleDom),
                              se.MaleDominant_encounters_PerDay= sd.MaleDominant_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.MaleSubordinate_encounters_PerDay=mean(m.MaleSubord),
                                  sd.MaleSubordinate_encounters_PerDay=sd(m.MaleSubord),
                                  se.MaleSubordinate_encounters_PerDay= sd.MaleSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.MaleUnknown_encounters_PerDay=mean(m.MaleUnknownStatus),
                              sd.MaleUnknown_encounters_PerDay=sd(m.MaleUnknownStatus),
                              se.MaleUnknown_encounters_PerDay= sd.MaleUnknown_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.FemaleDominant_encounters_PerDay=mean(m.FemaleDom),
                                  sd.FemaleDominant_encounters_PerDay=sd(m.FemaleDom),
                                  se.FemaleDominant_encounters_PerDay= sd.FemaleDominant_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.FemaleSubordinate_encounters_PerDay=mean(m.FemaleSubord),
                              sd.FemaleSubordinate_encounters_PerDay=sd(m.FemaleSubord),
                              se.FemaleSubordinate_encounters_PerDay= sd.FemaleSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.FemaleUnknown_encounters_PerDay=mean(m.FemaleUnknownStatus),
                                  sd.FemaleUnknown_encounters_PerDay=sd(m.FemaleUnknownStatus),
                                  se.FemaleUnknown_encounters_PerDay= sd.FemaleUnknown_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.UnknownsexSubordinate_encounters_PerDay=mean(m.UnknownsexSubord),
                              sd.UnknownsexSubordinate_encounters_PerDay=sd(m.UnknownsexSubord),
                              se.UnknownsexSubordinate_encounters_PerDay= sd.UnknownsexSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                      mean.UnknownSexorStatus_encounters_PerDay=mean(m.UnknownSexorStatus),
                      sd.UnknownSexorStatus_encounters_PerDay=sd(m.UnknownSexorStatus),
                      se.UnknownSexorStatus_encounters_PerDay= sd.UnknownSexorStatus_encounters_PerDay/sqrt(n.Foxes))



library(xlsx) #load package to export R objects to Excel
write.xlsx(contactsummary.table, "RContactMeans_NoUnknownFoxes230815.xlsx") #export to current working directory


#just for sex
contactsummary.table.sex <- ddply(contactsummary.AnimalID, c("SeasonID", "Sex"), summarise,
                              n.Foxes=length(AnimalID),
                              mean.TotVisitsPerDay=mean(m.TotVisits),
                              sd.TotVisitsPerDay=sd(m.TotVisits),
                              se.TotVisitsPerDay=sd.TotVisitsPerDay/sqrt(n.Foxes),                              
                              mean.TotSharedVisitsPerDay=mean(m.TotSharedVisits),
                              sd.TotSharedVisitsPerDay=sd(m.TotSharedVisits),
                              se.TotSharedVisitsPerDay= sd.TotSharedVisitsPerDay/sqrt(n.Foxes),                            
                              mean.TotEncountersAnyFoxPerDay=mean(m.TotEncountersAnyFox),
                              sd.TotEncountersAnyFoxPerDay=sd(m.TotEncountersAnyFox),
                              se.TotEncountersAnyFoxPerDay= sd.TotEncountersAnyFoxPerDay/sqrt(n.Foxes),                              
                              mean.TotEncountersPerStationPerDay=mean(m.TotEncountersPerStation),
                              sd.meanTotEncountersPerStationPerDay=sd(m.TotEncountersPerStation),
                              se.meanTotEncountersPerStationPerDay= sd.meanTotEncountersPerStationPerDay/sqrt(n.Foxes),                              
                              median.TotEncountersPerStationPerDay=median(med.TotEncountersPerStation),
                              sd.medianTotEncountersPerStationPerDay=sd(med.TotEncountersPerStation),
                              se.medianTotEncountersPerStationPerDay= sd.medianTotEncountersPerStationPerDay/sqrt(n.Foxes),                              
                              mean.EncountersPerHour=mean(m.EncountersPerHour),
                              sd.EncountersPerHour=sd(m.EncountersPerHour),
                              se.EncountersPerHour= sd.EncountersPerHour/sqrt(n.Foxes),                              
                              mean.TotVisDurat_NMins_PerDay=mean(m.TotVisDurat_NMins),
                              sd.TotVisDurat_NMins_PerDay=sd(m.TotVisDurat_NMins),
                              se.TotVisDurat_NMins_PerDay= sd.TotVisDurat_NMins_PerDay/sqrt(n.Foxes),                              
                              mean.Adult_encounters_PerDay=mean(m.Adult_encounters),
                              sd.Adult_encounters_PerDay=sd(m.Adult_encounters),
                              se.Adult_encounters_PerDay= sd.Adult_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.Subadult_encounters_PerDay=mean(m.Subadult_encounters),
                              sd.Subadult_encounters_PerDay=sd(m.Subadult_encounters),
                              se.Subadult_encounters_PerDay= sd.Subadult_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.Cub_encounters_PerDay=mean(m.Cub_encounters),
                              sd.Cub_encounters_PerDay=sd(m.Cub_encounters),
                              se.Cub_encounters_PerDay= sd.Cub_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.MaleDominant_encounters_PerDay=mean(m.MaleDom),
                              sd.MaleDominant_encounters_PerDay=sd(m.MaleDom),
                              se.MaleDominant_encounters_PerDay= sd.MaleDominant_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.MaleSubordinate_encounters_PerDay=mean(m.MaleSubord),
                              sd.MaleSubordinate_encounters_PerDay=sd(m.MaleSubord),
                              se.MaleSubordinate_encounters_PerDay= sd.MaleSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.MaleUnknown_encounters_PerDay=mean(m.MaleUnknownStatus),
                              sd.MaleUnknown_encounters_PerDay=sd(m.MaleUnknownStatus),
                              se.MaleUnknown_encounters_PerDay= sd.MaleUnknown_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.FemaleDominant_encounters_PerDay=mean(m.FemaleDom),
                              sd.FemaleDominant_encounters_PerDay=sd(m.FemaleDom),
                              se.FemaleDominant_encounters_PerDay= sd.FemaleDominant_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.FemaleSubordinate_encounters_PerDay=mean(m.FemaleSubord),
                              sd.FemaleSubordinate_encounters_PerDay=sd(m.FemaleSubord),
                              se.FemaleSubordinate_encounters_PerDay= sd.FemaleSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.FemaleUnknown_encounters_PerDay=mean(m.FemaleUnknownStatus),
                              sd.FemaleUnknown_encounters_PerDay=sd(m.FemaleUnknownStatus),
                              se.FemaleUnknown_encounters_PerDay= sd.FemaleUnknown_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.UnknownsexSubordinate_encounters_PerDay=mean(m.UnknownsexSubord),
                              sd.UnknownsexSubordinate_encounters_PerDay=sd(m.UnknownsexSubord),
                              se.UnknownsexSubordinate_encounters_PerDay= sd.UnknownsexSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                              mean.UnknownSexorStatus_encounters_PerDay=mean(m.UnknownSexorStatus),
                              sd.UnknownSexorStatus_encounters_PerDay=sd(m.UnknownSexorStatus),
                              se.UnknownSexorStatus_encounters_PerDay= sd.UnknownSexorStatus_encounters_PerDay/sqrt(n.Foxes))
#just for status
contactsummary.table.status <- ddply(contactsummary.AnimalID, c("SeasonID", "SocialStatus"), summarise,
                                  n.Foxes=length(AnimalID),
                                  mean.TotVisitsPerDay=mean(m.TotVisits),
                                  sd.TotVisitsPerDay=sd(m.TotVisits),
                                  se.TotVisitsPerDay=sd.TotVisitsPerDay/sqrt(n.Foxes),                              
                                  mean.TotSharedVisitsPerDay=mean(m.TotSharedVisits),
                                  sd.TotSharedVisitsPerDay=sd(m.TotSharedVisits),
                                  se.TotSharedVisitsPerDay= sd.TotSharedVisitsPerDay/sqrt(n.Foxes),                            
                                  mean.TotEncountersAnyFoxPerDay=mean(m.TotEncountersAnyFox),
                                  sd.TotEncountersAnyFoxPerDay=sd(m.TotEncountersAnyFox),
                                  se.TotEncountersAnyFoxPerDay= sd.TotEncountersAnyFoxPerDay/sqrt(n.Foxes),                              
                                  mean.TotEncountersPerStationPerDay=mean(m.TotEncountersPerStation),
                                  sd.meanTotEncountersPerStationPerDay=sd(m.TotEncountersPerStation),
                                  se.meanTotEncountersPerStationPerDay= sd.meanTotEncountersPerStationPerDay/sqrt(n.Foxes),                              
                                  median.TotEncountersPerStationPerDay=median(med.TotEncountersPerStation),
                                  sd.medianTotEncountersPerStationPerDay=sd(med.TotEncountersPerStation),
                                  se.medianTotEncountersPerStationPerDay= sd.medianTotEncountersPerStationPerDay/sqrt(n.Foxes),                              
                                  mean.EncountersPerHour=mean(m.EncountersPerHour),
                                  sd.EncountersPerHour=sd(m.EncountersPerHour),
                                  se.EncountersPerHour= sd.EncountersPerHour/sqrt(n.Foxes),                              
                                  mean.TotVisDurat_NMins_PerDay=mean(m.TotVisDurat_NMins),
                                  sd.TotVisDurat_NMins_PerDay=sd(m.TotVisDurat_NMins),
                                  se.TotVisDurat_NMins_PerDay= sd.TotVisDurat_NMins_PerDay/sqrt(n.Foxes),                              
                                  mean.Adult_encounters_PerDay=mean(m.Adult_encounters),
                                  sd.Adult_encounters_PerDay=sd(m.Adult_encounters),
                                  se.Adult_encounters_PerDay= sd.Adult_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.Subadult_encounters_PerDay=mean(m.Subadult_encounters),
                                  sd.Subadult_encounters_PerDay=sd(m.Subadult_encounters),
                                  se.Subadult_encounters_PerDay= sd.Subadult_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.Cub_encounters_PerDay=mean(m.Cub_encounters),
                                  sd.Cub_encounters_PerDay=sd(m.Cub_encounters),
                                  se.Cub_encounters_PerDay= sd.Cub_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.MaleDominant_encounters_PerDay=mean(m.MaleDom),
                                  sd.MaleDominant_encounters_PerDay=sd(m.MaleDom),
                                  se.MaleDominant_encounters_PerDay= sd.MaleDominant_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.MaleSubordinate_encounters_PerDay=mean(m.MaleSubord),
                                  sd.MaleSubordinate_encounters_PerDay=sd(m.MaleSubord),
                                  se.MaleSubordinate_encounters_PerDay= sd.MaleSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.MaleUnknown_encounters_PerDay=mean(m.MaleUnknownStatus),
                                  sd.MaleUnknown_encounters_PerDay=sd(m.MaleUnknownStatus),
                                  se.MaleUnknown_encounters_PerDay= sd.MaleUnknown_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.FemaleDominant_encounters_PerDay=mean(m.FemaleDom),
                                  sd.FemaleDominant_encounters_PerDay=sd(m.FemaleDom),
                                  se.FemaleDominant_encounters_PerDay= sd.FemaleDominant_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.FemaleSubordinate_encounters_PerDay=mean(m.FemaleSubord),
                                  sd.FemaleSubordinate_encounters_PerDay=sd(m.FemaleSubord),
                                  se.FemaleSubordinate_encounters_PerDay= sd.FemaleSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.FemaleUnknown_encounters_PerDay=mean(m.FemaleUnknownStatus),
                                  sd.FemaleUnknown_encounters_PerDay=sd(m.FemaleUnknownStatus),
                                  se.FemaleUnknown_encounters_PerDay= sd.FemaleUnknown_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.UnknownsexSubordinate_encounters_PerDay=mean(m.UnknownsexSubord),
                                  sd.UnknownsexSubordinate_encounters_PerDay=sd(m.UnknownsexSubord),
                                  se.UnknownsexSubordinate_encounters_PerDay= sd.UnknownsexSubordinate_encounters_PerDay/sqrt(n.Foxes),                              
                                  mean.UnknownSexorStatus_encounters_PerDay=mean(m.UnknownSexorStatus),
                                  sd.UnknownSexorStatus_encounters_PerDay=sd(m.UnknownSexorStatus),
                                  se.UnknownSexorStatus_encounters_PerDay= sd.UnknownSexorStatus_encounters_PerDay/sqrt(n.Foxes))


write.xlsx(contactsummary.table.sex, "ContactMeans.SEX230815.xlsx") #export to current working directory
write.xlsx(contactsummary.table.status, "ContactMeans.STATUS230815.xlsx") #export to current working directory


#Plots
contactdata<-contactsummary.table
str(contactdata)
library(ggplot2)

#reorder Sex as Male-Female instead of the default Female-Male
levels(contactdata$Sex) #check order of levels is appropriate
contactdata$Sex <- factor(contactdata$Sex, levels=c("Male", "Female", "Unknown"))
#cant use "levels(sexfac)" or ggplot wont recognise it as being part of the dataset (thinks its a separate object)

seasonfac <- factor(contactdata$SeasonID)
levels(contactdata$SocialStatus) #order is fine

contactspercamday<-contactdata$mean.TotEncountersPerStationPerDay
contactsperhour<-contactdata$mean.EncountersPerHour

#plot mean contacts per camera day (i.e. per patch per day)
boxplotcode<-ggplot(contactdata,aes(x=SeasonID,y=contactspercamday,colour=Sex,linetype=SocialStatus)) + 
  geom_point(size=3) + 
  geom_errorbar(data=contactdata, mapping=aes(x=SeasonID, ymin=contactspercamday-se.meanTotEncountersPerStationPerDay, 
                                              ymax=contactspercamday+se.meanTotEncountersPerStationPerDay), 
                                              width=0.2, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") + facet_wrap(~Sex) + geom_line(size=1)
#Then add labels to the boxplotcode:
boxplotcode + xlab(" ") + ylab("Mean encounters per patch per day\n") +
  scale_colour_manual(values = c("blue", "red")) + #set males to blue and females to red
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 
#had to change scale_x_discrete to scale_x_continous to make x axis the right length!



###Contacts per hour
boxplotcode<-ggplot(contactdata,aes(x=SeasonID,y=contactsperhour,colour=Sex,linetype=SocialStatus)) + 
  geom_point(size=3) + 
  geom_errorbar(data=contactdata, mapping=aes(x=SeasonID, ymin=contactsperhour-se.EncountersPerHour, 
                                              ymax=contactsperhour+se.EncountersPerHour), 
                width=0.2, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") + facet_wrap(~Sex) + geom_line(size=1)

#Then add labels to the boxplotcode:
boxplotcode + xlab(" ") + ylab("Mean encounters per hour\n") +
  scale_colour_manual(values = c("blue", "red")) + #set males to blue and females to red
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 
#had to change scale_x_discrete to scale_x_continous to make x axis the right length!

str(mydata)

###Contacts per patch per day for all foxes
totals <- ddply(mydata, c("SeasonID"), summarise,
                        n=length(TotEncountersPerStation),
                        mean.TotEncountersPerStation=mean(TotEncountersPerStation),
                        sd.TotEncountersPerStation=sd(TotEncountersPerStation),
                        se.TotEncountersPerStation= sd.TotEncountersPerStation/sqrt(n))
plot(mean.TotEncountersPerStation~SeasonID, data=totals)
ggplot(totals,aes(x=SeasonID,y=mean.TotEncountersPerStation,colour="",linetype="")) +
  geom_point(size=3) + 
  geom_errorbar(data=totals, mapping=aes(x=SeasonID, ymin=mean.TotEncountersPerStation-se.TotEncountersPerStation, 
                ymax=mean.TotEncountersPerStation+se.TotEncountersPerStation), 
                width=0.2, size=0.5, color="black") +
                theme_bw(base_size = 18, base_family = "")+ geom_line(size=1) +
          xlab(" ") + ylab("Mean encounters per patch per day") +
          scale_colour_manual(values = c("black")) + 
          scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 




########### Modelling ################
str(mydata)
str(contactsummary.AnimalID)
mydata<-contactsummary.AnimalID
hist(mydata$m.TotEncountersPerStation) #data are skewed
#Normalise ti.threshold.s (use an underscore to indicate original data has been transformed and stored as a new object)
EncountersPerStation_ <- with(mydata, (m.TotEncountersPerStation-min(m.TotEncountersPerStation))/(max(m.TotEncountersPerStation)-min(m.TotEncountersPerStation)))
hist(EncountersPerStation_) #data are still skewed but slightly less!
#try removing animals with zero values
mydata_subset <- subset(mydata,SocialStatus != "Unknown") #take subset not including unknown social statuses
mydata_subset <- subset(mydata,Sex != "Unknown") #take subset not including unknown sex
seasonfac<-as.factor(mydata$SeasonID) #convert season from an integer to factor
hist(mydata_subset$m.TotEncountersPerStation) #data are still skewed but slightly less

# Normalise ti.threshold.s between 0-1 (use an underscore to indicate original data has been transformed and stored as a new object)
EncountersPerStation_ <- with(mydata_subset, (m.TotEncountersPerStation-min(m.TotEncountersPerStation))/(max(m.TotEncountersPerStation)-min(m.TotEncountersPerStation)))
hist(EncountersPerStation_) #data are still skewed but slightly less!

model1<- lmer(EncountersPerStation_ ~ SeasonID + 
                Sex*SocialStatus +
                sum.TotVisDurat_NMins +
                (1|AnimalID), data = mydata_subset, na.action="na.pass", REML=FALSE)
summary(model1)
anova(model1)
model1_set <- dredge(model1) 
top_model1 <- get.models(model1_set, subset=delta<10)
model.sel(top_model1) # Show a summary of the best models
model.avg(top_model1)$importance 
get.models(model1_set, subset = 1)[[1]] # Show details of a certain model within the list of best models. Specify using
# the line number in subset= e.g. to specify model in line 5, type subset=5.

model1.vis<- get.models(model1_set, subset=1)[[1]]
model2.vis <- get.models(model1_set, subset = 2)[[1]] # Select a model to work with...

anova(model1.vis, model2.vis) #compare the top 2 models - no sig.diff between models 1 and 2
summary(model2.vis) #summary doesn't contain P values - need to do ANOVA
AICc(model2.vis)
