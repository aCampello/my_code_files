
###########################################
#### Number of patches visited per day ####
###########################################
rm(list = ls())

setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/N patches visited per day")

# Load already-prepared data; if not available, start with mydata and prepare data manually below, otherwise,
# skip to "START HERE" (search)
patchesperdayCORE <- read.csv("patchesperdayCORE.csv") # resident foxes only

# make factors
patchesperdayCORE$fTerritory <- factor(patchesperdayCORE$Territory)
patchesperdayCORE$fSeason <- factor(patchesperdayCORE$SeasonID)
patchesperdayCORE$ShortCode <- factor(patchesperdayCORE$ShortCode)
patchesperdayCORE$SocialStatus <- factor(patchesperdayCORE$SocialStatus)



#~~~~~~~~~~~~~~~~~~~~~~~~

# To prepare dataset from mydata manually: ####
# mydata <- read.csv(file = "g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Contactcounts/SNA_R_noCubs_AllPatches_noduplicates.csv", header = T, stringsAsFactors=FALSE) 
# mydata <- read.csv(file = "G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/MultilevelModels_PatchResTimes_NoCubs28surveys.csv", header = T, stringsAsFactors=FALSE) 

mydata$DAY <- strptime(mydata$TRANScDate,format="%d/%m/%Y") 
mydata$daynum <- as.numeric(difftime(mydata$DAY,min(mydata$DAY),units="days")) + 1
mydata$DAY <- NULL # POSIXCT data type messes with plyr so now delete the date column



# Calc days seen and add to mydata ####
daysseendata <- ddply(mydata, c("AnimalID", "Territory", "SeasonID"), summarise, 
                      DaysSeen = length(unique(TRANScDate)))

daysseendata$ATS <- interaction(daysseendata$AnimalID, daysseendata$Territory, daysseendata$SeasonID)
mydata$ATS <- interaction(mydata$AnimalID, mydata$Territory, mydata$SeasonID)
mydata$DaysSeen <- daysseendata[match(mydata$ATS, daysseendata$ATS),4] 

### Mark number of patches available in the territory on each day ####
dayspatchactive <- read.csv(file = "g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/DatesCamerasActive.csv", header = T, stringsAsFactors=FALSE) # stringsasfactors stores text as text rather than factors
availablePatches <- ddply(dayspatchactive, c("Territory", "SeasonID", "TRANScDate"), 
                          summarise,
                          pavail = length(unique(StationID)))

mydata$TD <- interaction(factor(mydata$Territory), factor(mydata$TRANScDate))
availablePatches$TD <- interaction(factor(availablePatches$Territory), factor(availablePatches$TRANScDate))
# match
mydata$pAvail <- availablePatches[match(mydata$TD, availablePatches$TD),4] 

#~~~~~~~~~~~~~~~~~~~~~~~~

# Calc number of patches visited per day ####
library(plyr)
patchesperday <- ddply(mydata, c("Territory", "SeasonID", "AnimalID", "ShortCode", "Sex", 
                                 "SocialStatus", "Core", "DaysSeen", "daynum", "pAvail"), 
                       summarise,
                       npatches = length(unique(StationCode)))

# make factors
patchesperday$fTerritory <- factor(patchesperday$Territory)
patchesperday$fSeason <- factor(patchesperday$SeasonID)
patchesperday$ShortCode <- factor(patchesperday$ShortCode)

patchesperdayCORE <- subset(patchesperday,Core=="1")
mean(patchesperdayCORE$npatches) 
sd(patchesperdayCORE$npatches) 

# Calc proportion of available patches visited per day
patchesperdayCORE$propPatches<-(patchesperdayCORE$npatches/patchesperdayCORE$pAvail)

# save as csv file to make data publicly available:
# write.csv(patchesperdayCORE, file="patchesperdayCORE.csv", row.names = FALSE) # saves as csv file in current working directory
# import again - if not already
# patchesperdayCORE <- read.csv("patchesperdayCORE.csv") 


#~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~ "START HERE" ~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~#


#### MODELS ####


# Q: do dominants visit more patches on territory than subordinates? 

#### 1. STATUS ONLY MODELS ####
### DIDNT USE THESE IN THESIS AS DECIDED TO ALSO INCLUDE SEX: SEE BELOW FOR MODELS USED IN THESIS

# Fit Poisson model with offset for Days seen
library(lme4)
npatchesmod_FULL <- glmer(npatches ~
                            SocialStatus*fSeason +
                            (1|ShortCode) + (1|fTerritory),
                          offset=log(pAvail), 
                          data=patchesperdayCORE,
                          family=poisson(link="log"),
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05))) 
# Check for overdispersion first
deviance(npatchesmod_FULL)/df.residual(npatchesmod_FULL) # 0.28 # should be ~1 # =underdispersed
library("blmeco") 
dispersion_glmer(npatchesmod_FULL) # 0.53 (should be between 0.75 and 1.4)
# Tried fitting negative binomials in glmmADMB but they didn't seem to fit better than poisson and were all not overdispersed according to the function.
# (plus very slow to fit)

# model checking
plot(fitted(npatchesmod_FULL), resid(npatchesmod_FULL))
lines(lowess(fitted(npatchesmod_FULL), resid(npatchesmod_FULL))) # good!

# stepwise model refinement

# additive
npatchesmod_nostatusINT <- glmer(npatches ~ SocialStatus + fSeason +
                                   (1|ShortCode) + (1|fTerritory),
                                 offset=log(pAvail), 
                                 data=patchesperdayCORE,
                                 family=poisson(link="log"),
                                 control=glmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05))) 
anova(npatchesmod_FULL, npatchesmod_nostatusINT) # x2= 3.7508,3,0.2897 # don't need status*season

# no season
npatchesmod_noseas <- glmer(npatches ~ SocialStatus +
                                   (1|ShortCode) + (1|fTerritory),
                                 offset=log(pAvail), 
                                 data=patchesperdayCORE,
                                 family=poisson(link="log"),
                                 control=glmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05))) 
anova(npatchesmod_nostatusINT, npatchesmod_noseas) # x2=20.663,3,0.0001236 *** # need season

# no status
npatchesmod_nostatus <- glmer(npatches ~  fSeason +
                                   (1|ShortCode) + (1|fTerritory),
                                 offset=log(pAvail), 
                                 data=patchesperdayCORE,
                                 family=poisson(link="log"),
                                 control=glmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05))) 
anova(npatchesmod_nostatusINT, npatchesmod_nostatus) # x2=3.823,1,0.05055. # need status JUST


# FINAL MODEL
npatchesmod_FINAL <- glmer(npatches ~ 
                             SocialStatus +
                             fSeason +
                             (1|ShortCode) + (1|fTerritory),
                                 offset=log(pAvail), 
                                 data=patchesperdayCORE,
                                 family=poisson(link="log"),
                                 control=glmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=100000), 
                                                      check.conv.grad=.makeCC("warning",0.05))) 
# save coefficients
a <- coef(summary(npatchesmod_FINAL))

### POST HOC TEST OF WHICH SEASONS WERE SIG DIFF 
library(lsmeans)
lsm1 <- lsmeans::lsmeans(npatchesmod_FINAL, pairwise~fSeason, type="response")
lsmcontrasts <- data.frame(summary(lsm1)$contrasts) 

lsm1 <- lsmeans::lsmeans(npatchesmod_FINAL, pairwise~SocialStatus, type="response")

### PLOT
lsm1 <- lsmeans::lsmeans(npatchesmod_FINAL, ~fSeason|SocialStatus, type="response")
lsm <- data.frame(summary(lsm1)) 

library(ggplot2)
ggplot(lsm, aes(x=fSeason,y=rate, colour=SocialStatus)) + 
 ylim(0,1.05) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_colour_manual(name="Social\nstatus", values = c("royalblue3","firebrick1")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Proportion of patches visited\n") +
  # add raw data
  geom_point(data=patchesperdayCORE, aes(fSeason, propPatches, colour=SocialStatus), 
             position=position_jitter(0.2), size=0.1)  +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1.2, width=0.3, 
                position=position_dodge(0.2)) +
  geom_point(size=5, position=position_dodge(0.2)) 

# save above in high resolution for publications
setwd("G:/Statistics 2016/Patch visitation/N patches visited per day")
dev.print(jpeg, "RPlot_lsmeans proportion of available patches visited per day by status and season_with raw data.jpeg", 
          res=700, height=14, width=18, units="cm") 


#######################################################################################################

# TESTED NEGATIVE BINOMIAL MODELS IN GLMMADMB AS RESIDS WERE OVERDISPERSED, BUT DIDNT HELP SO JUST
# USED POISSON IN LME4

library(glmmADMB)
patchesperdayCORE$pAvailLOG <-log(patchesperdayCORE$pAvail) # have to log offset term first for glmmADMB

#11:50-12:00
npatchesmod_FULL_poiss <- glmmadmb(npatches ~
                                     SocialStatus + 
                                     offset(pAvailLOG) +
                                     (1|ShortCode) + (1|fTerritory),
                                   data=patchesperdayCORE,
                                   family="poisson", zeroInflation=F, debug=F, verbose=T)
#11.42-11:49
npatchesmod_FULL_nb1 <- glmmadmb(npatches ~
                                   SocialStatus + 
                                   offset(pAvailLOG) +
                                   (1|ShortCode) + (1|fTerritory),
                                 data=patchesperdayCORE,
                                 family="nbinom1", zeroInflation=F, debug=F, verbose=T)
# 12.00-12:19 or before
npatchesmod_FULL_nb2 <- glmmadmb(npatches ~
                                   SocialStatus + 
                                   offset(pAvailLOG) +
                                   (1|ShortCode) + (1|fTerritory),
                                 data=patchesperdayCORE,
                                 family="nbinom", zeroInflation=F, debug=F, verbose=T)

# test for overdispersion for glmmADMB models: if p<0.05 data are overdispersed.
overdisp_fun <- function(model) {
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(npatchesmod_FULL_poiss) #p=1
overdisp_fun(npatchesmod_FULL_nb1) #p=1
overdisp_fun(npatchesmod_FULL_nb2) #p=1

# compare negbinom type 1 and 2 and poisson
bbmle::ICtab(npatchesmod_FULL_nb2, npatchesmod_FULL_nb1, npatchesmod_FULL_poiss) # poisson is best

######################################################################################################

#### N PATCHES PER DAY MODEL WITH sex and season AS ADDITIONAL PREDICTORS - USED IN THESIS ####

### Q: do foxes visit more patches on territory at certain times of year AND does this depend on 
# social status, sex or their interaction?

# Fit Poisson model with offset for Days seen
library(lme4)
npatchesmod_FULL <- glmer(npatches ~
                            SocialStatus*Sex*fSeason +
                            (1|ShortCode) + (1|fTerritory),
                          offset=log(pAvail), 
                          data=patchesperdayCORE,
                          family=poisson(link="log"),
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05))) 
# Check for overdispersion first
deviance(npatchesmod_FULL)/df.residual(npatchesmod_FULL) # 0.27 # should be ~1 # =underdispersed
library("blmeco") 
dispersion_glmer(npatchesmod_FULL) # 0.53 (should be between 0.75 and 1.4)
# Tried fitting negative binomials in glmmADMB but they didn't seem to fit better than poisson and were all not overdispersed according to the function.
# (plus very slow to fit)

### model checking
plot(fitted(npatchesmod_FULL), resid(npatchesmod_FULL))
lines(lowess(fitted(npatchesmod_FULL), resid(npatchesmod_FULL))) # good!

### stepwise model refinement

# two way int
npatchesmod_noSSint <- glmer(npatches ~ SocialStatus*fSeason + Sex*fSeason +
                               (1|ShortCode) + (1|fTerritory),
                             offset=log(pAvail), 
                             data=patchesperdayCORE,
                             family=poisson(link="log"),
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05))) 
anova(npatchesmod_FULL, npatchesmod_noSSint) # x2= 3.9371      4     0.4146 # dont need sex*status


npatchesmod_noSexSeasINT <- glmer(npatches ~ SocialStatus*fSeason + Sex +
                                    (1|ShortCode) + (1|fTerritory),
                                  offset=log(pAvail), 
                                  data=patchesperdayCORE,
                                  family=poisson(link="log"),
                                  control=glmerControl(optimizer="bobyqa",
                                                       optCtrl = list(maxfun=100000), 
                                                       check.conv.grad=.makeCC("warning",0.05))) 
anova(npatchesmod_noSSint, npatchesmod_noSexSeasINT) # 11.14      3    0.01099 * # need sex*season


npatchesmod_noStatSeasint <- glmer(npatches ~ SocialStatus + Sex*fSeason +
                                     (1|ShortCode) + (1|fTerritory),
                                   offset=log(pAvail), 
                                   data=patchesperdayCORE,
                                   family=poisson(link="log"),
                                   control=glmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=100000), 
                                                        check.conv.grad=.makeCC("warning",0.05)))
anova(npatchesmod_noSSint,npatchesmod_noStatSeasint) # 4.7036      3     0.1948 # dont need status*season

### FINAL MODEL
npatchesmod_FINAL <- glmer(npatches ~ SocialStatus + Sex*fSeason +
                             (1|ShortCode) + (1|fTerritory),
                           offset=log(pAvail), 
                           data=patchesperdayCORE,
                           family=poisson(link="log"),
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))

### model checking
deviance(npatchesmod_FINAL)/df.residual(npatchesmod_FINAL) # 0.27
plot(fitted(npatchesmod_FINAL), resid(npatchesmod_FINAL))
lines(lowess(fitted(npatchesmod_FINAL), resid(npatchesmod_FINAL))) # good!

### Save coefficients
a <- coef(summary(npatchesmod_FINAL))

### LR tests to report chisq values

# without sex*seas interaction
npatchesmod_add <- glmer(npatches ~ SocialStatus + Sex + fSeason +
                           (1|ShortCode) + (1|fTerritory),
                         offset=log(pAvail), 
                         data=patchesperdayCORE,
                         family=poisson(link="log"),
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05)))
anova(npatchesmod_FINAL, npatchesmod_add) # 10.188, 3, 0.01703*


# without status
npatchesmod_noStatus <- glmer(npatches ~ Sex*fSeason +
                                (1|ShortCode) + (1|fTerritory),
                              offset=log(pAvail), 
                              data=patchesperdayCORE,
                              family=poisson(link="log"),
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=100000), 
                                                   check.conv.grad=.makeCC("warning",0.05)))
anova(npatchesmod_FINAL,npatchesmod_noStatus) # 4.024,1,0.04486*

# with status*season
npatchesmod_withSS <- glmer(npatches ~ Sex*fSeason + SocialStatus*fSeason +
                              (1|ShortCode) + (1|fTerritory),
                            offset=log(pAvail), 
                            data=patchesperdayCORE,
                            family=poisson(link="log"),
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000), 
                                                 check.conv.grad=.makeCC("warning",0.05)))
anova(npatchesmod_FINAL,npatchesmod_withSS) # x2=4.7036,3,0.1948

# with sex*status
npatchesmod_3wint <- glmer(npatches ~ SocialStatus*Sex*fSeason +
                           (1|ShortCode) + (1|fTerritory),
                         offset=log(pAvail), 
                         data=patchesperdayCORE,
                         family=poisson(link="log"),
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05)))
anova(npatchesmod_FINAL,npatchesmod_3wint) # x2=8.6407,7,0.2795


### POST HOC TEST OF WHICH SEASONS WERE SIG DIFF 
library(lsmeans)
lsm1 <- pairs(lsmeans::lsmeans(npatchesmod_FINAL, ~fSeason|Sex, type="response"))
lsm2 <- pairs(lsmeans::lsmeans(npatchesmod_FINAL, ~Sex|fSeason, type="response"))
lsmcontrasts <- test(rbind(lsm1, lsm2))

lsm1 <- lsmeans::lsmeans(npatchesmod_FINAL, ~fSeason|c(Sex*SocialStatus), type="response")
lsm <- data.frame(summary(lsm1))

### PLOT
#lsm1 <- lsmeans::lsmeans(npatchesmod_FINAL, ~fSeason|SocialStatus, type="response")
#lsm <- data.frame(summary(lsm1)) 

lsm$StatusLONG <- ordered(lsm$SocialStatus, levels(lsm$SocialStatus)[c(1,2)], labels=c("Dominant","Subordinate"))
lsm$Sexord <- ordered(lsm$Sex, levels(lsm$Sex)[c(1,2)])

patchesperdayCORE$Sex <- factor(patchesperdayCORE$Sex)
patchesperdayCORE$Sexord <- ordered(patchesperdayCORE$Sex, levels(patchesperdayCORE$Sex)[c(2,1)])

# plot with raw data
ggplot(lsm, aes(x=fSeason,y=rate, group=Sexord, colour=Sexord)) + 
  geom_point(data=patchesperdayCORE, aes(fSeason, propPatches, colour=Sexord), 
             position=position_jitter(0.4), size=0.05)  +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1.2, width=0.3, 
                position=position_dodge(0.5)) +
  geom_point(size=4.5, position=position_dodge(0.5)) +
  geom_line(size=1.2, position=position_dodge(0.5)) +
   facet_wrap(~StatusLONG) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.x = element_text(size = 12, face=NULL)) +
  scale_colour_manual(name="Sex", values = c("royalblue3","firebrick1")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Proportion of patches visited\n") 
  
# save above in high resolution for publications
#setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/N patches visited per day")
#dev.print(jpeg, "RPlot_lsmeans patches visited per day by sex, status & season_with raw data_lines.jpeg", res=700, height=15, width=21, units="cm") 

xxxx
# Plot with bar plot ####
# Think bar plot is best for this - looks too messy with raw data on it 

ggplot(lsm, aes(x=fSeason,y=rate, group=Sexord, fill=Sexord)) + 
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") +
  scale_fill_manual(name="Sexord", values = c("gray50","white")) +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=0.6, width=0.3, 
                position=position_dodge(0.9)) +
  facet_wrap(~StatusLONG) +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size = 1),
        strip.text.x = element_text(face=NULL),
        axis.text = element_text(colour = "black"),
        strip.background = element_rect(color = "black", size = 1),
        legend.title = element_blank(),
        legend.position = c(0.92,0.93), legend.direction=c("horizontal")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
    xlab(" ") + ylab("Proportion of patches visited") 

# dev.print(jpeg, "RPlot_lsmeans patches visited per day by sex, status & season_barplot.jpeg", res=700, height=15, width=24, units="cm") 

# As above but using special requirements for BEHECO paper (130617):

# rename sex for clearer plot legend
lsm$SexLong <- factor(lsm$Sexord, levels = c("M", "F"), labels=c("Male", "Female")) 

ggplot(lsm, aes(x=fSeason,y=rate, group=SexLong, fill=SexLong)) + 
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") +
  scale_fill_manual(name="SexLong", values = c("gray50","white")) +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=0.6, width=0.3, 
                position=position_dodge(0.9)) +
  facet_wrap(~StatusLONG) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size = 1),
        strip.text.x = element_text(face=NULL),
        axis.text = element_text(colour = "black"),
        strip.background = element_rect(color = "black", size = 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
       # legend.position = c(0.85,0.93), legend.direction=c("horizontal")) +
        legend.position = c(0.91,0.91)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Proportion of patches visited") 

#dev.print(tiff, "Figure 2. 120 x 177mm_res300.tif", res=300, height=120, width=177, units="mm") 



