setwd("G:/Statistics 2016/Tail position")

mydata <- read.csv(file = "TailPos_061016.csv", header = TRUE) #import dataset
str(mydata)
library(plyr) # to do calculations on data

mydata$Sex <- factor(mydata$ShortSex,levels(mydata$ShortSex)[c(2,1)]) #change order & remove unknown
               
print(levels(tp$tailpos)) #automatically listed in alphabetic order
# combine tail pos score and name to interpret more easily
mydata$tpcombo <- factor(paste(mydata$TPscore, mydata$tailpos))

#### old code for tidying up data; not needed now as only exported true tail positions from CameraBase ####
# tp <- subset(mydata, tailpos != "PB" & tailpos != "Sp" & tailpos !=  "Un" & tailpos != "New") #remove junk categories
# tp$tailpos <- factor(tp$tailpos,levels(tp$tailpos)[c(1,2,3,4,5,6,7,8)]) #remove junk categories from level list for neater plots
#### 

tp <- mydata
tp$fTerritory <- factor(tp$Territory)

# convert Alone variable to factor to enable relevelling (to put Alone first as reference category):
tp$Alone <- as.factor(tp$Alone) 
levels(tp$Alone)
tp$fAlone <- factor(tp$Alone, levels(tp$Alone) [c(2,1)], labels=c("Alone", "Not alone"))
levels(tp$fAlone)

# rename seasons 1-4 as spring-winter
tp$fSeason <- ordered(tp$SeasonID, levels = c(1,2,3,4), 
                      labels=c("Spring", "Summer", "Autumn", "Winter"))
tp$fSeason_ <- factor(tp$SeasonID)
# alternative way to convert season to a factor in new column 'seasonfac' (without renaming)
# mydata<-mutate(mydata, seasonfac=as.factor(mydata$SeasonID)) 

# Plots
plot(tp$tpcombo, tp$NPhotos) 
boxplot(tp$NPhotos~tp$fAlone*tp$tpcombo) 
boxplot(tp$NPhotos~tp$tpcombo*tp$fAlone)

library(ggplot2)
# NEED TO PLOT PROPORTION OF PHOTOS OF EACH INDIVIDUAL THAT WERE EACH CATEG, NOT SIMPLY A COUNT
ggplot(tp,aes(x=tpcombo,y=NPhotos, colour=fAlone)) + geom_boxplot() +
  facet_grid(~ShortStatus) + labs(x = "\nTail position", y = "N photos\n") +
  scale_colour_manual(name="Alone",values=c('royalblue3','firebrick1'), guide=FALSE) +
  theme_bw() + # must put theme_bw FIRST or it won't work
  theme(panel.grid.minor = element_blank(), # to add light grey gridlines
  panel.grid.major = element_blank(),
  panel.border = element_rect(linetype = "solid", colour = "black"))

hist(tp$TPscore)

library(lme4)
library(lmerTest)
tpmod1<-lmer(TPscore ~ fSeason_*fAlone + ShortStatus + Sex + (1|AnimalID) + (1|fTerritory), 
             REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
summary(tpmod1)

# no interaction
tpmod2 <-lmer(TPscore ~ fSeason_ + fAlone + ShortStatus + Sex + (1|AnimalID) + (1|fTerritory), 
              REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
anova(tpmod1, tpmod2) # non-sig

# no Sex
tpmod3 <-lmer(TPscore ~ fSeason_ + fAlone + ShortStatus + (1|AnimalID) + (1|fTerritory), 
              REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
anova(tpmod2, tpmod3) # non-sig

# no Season
tpmod4 <-lmer(TPscore ~ fAlone + ShortStatus + (1|AnimalID) + (1|fTerritory), 
              REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
anova(tpmod3, tpmod4) # non-sig

# no Status
tpmod5 <-lmer(TPscore ~ fAlone + (1|AnimalID) + (1|fTerritory), 
              REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
anova(tpmod4, tpmod5) # SIG

# no Alone
tpmod6 <-lmer(TPscore ~ ShortStatus + (1|AnimalID) + (1|fTerritory), 
              REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
anova(tpmod4, tpmod6) # very SIG


### FINAL MODEL
tpmod_final <-lmer(TPscore ~ fAlone + ShortStatus + (1|AnimalID) + (1|fTerritory), 
              REML=F, data=subset(tp,TPscore>0), na.action="na.pass") # + Alone + seasonfac + Sex*SocialStatus
plot(resid(tpmod_final), fitted(tpmod_final))
lines(smooth.spline(resid(tpmod_final), fitted(tpmod_final)))


