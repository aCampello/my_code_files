
#===========================================================================================#
#### PATCH FOOD PROVISIONING: MEAN DAYS FED AND MODEL TO TEST SEASONAL DIFFS IN DAYS FED ####
#===========================================================================================#

# in daily kJ data I had to change a few numbers for spring in 17HP & 2CG and summer in 2CG as
# records said they fed exact same amount per day, so had zero variance - made model-fitting troublesome.

rm(list=ls()) 

setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding")
patchdata <- read.csv("Patch feeding freq_28surveys.csv")

# patchdata has one row per patch per survey - so already calculated MEAN days fed

# Get overall feeding frequency
mean(patchdata$NDaysFed) # 30.74194
sd(patchdata$NDaysFed) # 9.521794

mean(patchdata$DaysFedPerWeek) # 5.379839
sd(patchdata$DaysFedPerWeek) # 1.666314
median(patchdata$DaysFedPerWeek) # 5.95
mad(patchdata$DaysFedPerWeek) # Median Absolute Deviation = 1.55673
IQR(patchdata$DaysFedPerWeek) # IQR = 2.8
library(asbio)
ci.median(patchdata$DaysFedPerWeek) # [5.425-6.475]

mean(patchdata$MeankJperDay) # 1098.433 # DONT USE THIS USE THE CODE AT BOTTOM BASED ON DDPLY DATAFRAME
sd(patchdata$MeankJperDay) # 1147.597

mean(patchdata$MeanMJperFeedingDay) # 1.300506
sd(patchdata$MeanMJperFeedingDay) # 1.089562
median(patchdata$MeanMJperFeedingDay) # 0.9307418
mad(patchdata$MeanMJperFeedingDay) # 0.5808515
IQR(patchdata$MeanMJperFeedingDay) # 0.8815348



# Get day-to-day veriance for each patch = DONT THINK THIS IS USEFUL AS RELIES ON ACCURACY OF FEEDING RECS...
varMJperday <- ddply(dailykjdata, "StationCode", summarise, 
                  var=var(sumMJtoday))


### NUMBER OF DAYS FED ###

# Get mean days fed in each season
library(plyr)
SeasFeedPatterns <- ddply(patchdata, "SeasonID", summarise, 
                          meanDF_survey=mean(NDaysFed),
                          sdDF_survey=sd(NDaysFed),
                          meanDF_week=mean(DaysFedPerWeek),
                          sdDF_week=mean(DaysFedPerWeek),
                          medianDF_week=median(DaysFedPerWeek))





### SEASONAL VARIATION IN FOOD PREDICTABILITY (mean days fed per week) ###
#--------------------------------------------------------------------

# check distribution
library(fitdistrplus)
descdist(patchdata$DaysFedPerWeek, discrete = F, boot=5000) # uniform or normal
# Plot data against normal distribution
library(car)
fit.norm <- fitdist(patchdata$DaysFedPerWeek, "norm")
plot(fit.norm) # not quite normal
# check with hist
hist(patchdata$DaysFedPerWeek) # mostly uniform apart from large peak at 40 days from patches that fed daily
# in LEMMA course notes they have a similar peak at one end for school exam scores and ignore it
# SO i can do same as long as model resids are OK.

# Fit linear model with patch ID as ranef to account for repeated measures:
fSeason <- factor(patchdata$SeasonID)

library(lme4)
# random intercept
DFseas_mod <- lmer(DaysFedPerWeek ~ fSeason + (1|StationCode), data=patchdata, REML=F)

# check residuals
hist(resid(DFseas_mod))
plot(fitted(DFseas_mod), resid(DFseas_mod))
lines(smooth.spline(fitted(DFseas_mod), resid(DFseas_mod))) 
# look OK - slight deviation cos of large number of patches that fed on 40/40 days

# Check for overdispersion
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
} # expand to view
overdisp_fun(DFseas_mod) # p=1 so not overdispersed

# Compare full model to null model using LR test
DFseas_mod_null <- lmer(DaysFedPerWeek ~ 1 + (1|StationCode), data=patchdata, REML=F)
anova(DFseas_mod, DFseas_mod_null) # no seasonal variation in n days fed.

# Save coefficients
a <- coef(summary(DFseas_mod)) # based on the dataset called 'patchdata' - one row per survey-patch combo.




#================================================================#
#### SEASONAL VARIATION IN MEAN AMOUNT (KJ or MJ) FED PER DAY ####
#================================================================#

# data with kJ and MJ per day (including non-feeding days, marked as FeedingDay=Yes or =No)
dailykjdata <- read.csv("DailykJ_28surveys.csv")

# Replace NAs for non-feeding days with zeros
dailykjdata$sumMJtoday[is.na(dailykjdata$sumMJtoday)] <- 0 # MJ

# subset for only feeding days
dailykjdataFD <- subset(dailykjdata, FedToday=="Yes")



# MEAN MJ FED PER SURVEY DAY (OUT OF 40 so including any non-feeding days)
#------------------------------------------------------------------------

### TOTAL OVERALL MEAN - MJ per day per patch - including non-feeding days
mean(dailykjdata$sumkJtoday)
sd(dailykjdata$sumkJtoday)


### SEASONAL VARIATION IN MEAN AMOUNT (MJ) FED PER FEEDING DAY
#---------------------------------------------------------------
library(plyr)

## CALC OVERALL MEAN MJ PER PATCH per season:
# SD represents daily variation without accounting for between-patch differences in what a normal amount of food is.

# ALL SURVEY DAYS
MJperday <- ddply(dailykjdata, "SeasonID", summarise, 
                  mean=mean(sumMJtoday),
                  sd=sd(sumMJtoday))

# FEEDING DAYS ONLY (without first grouping by patch - each row is one day)
MJperdayFD <- ddply(dailykjdataFD, "SeasonID", summarise, 
                    mean=mean(sumMJtoday),
                    sd=sd(sumMJtoday),
                    median=median(sumMJtoday))

# FEEDING DAYS ONLY (after grouping by patch - each row is one patch/season)
MJperdayFD. <- ddply(patchdata, "SeasonID", summarise, 
                    mean=mean(MeanMJperFeedingDay),
                    sd=sd(MeanMJperFeedingDay),
                    median=median(MeanMJperFeedingDay))

# get confidence intervals for median
library(asbio)
ci.median(subset(dailykjdataFD, SeasonID=="1")$sumMJtoday)
ci.median(subset(dailykjdataFD, SeasonID=="2")$sumMJtoday)
ci.median(subset(dailykjdataFD, SeasonID=="3")$sumMJtoday)
ci.median(subset(dailykjdataFD, SeasonID=="4")$sumMJtoday)

# Median Absolute Deviation - similar to SD but more robust for detecting outliers
mad(dailykjdataFD$sumMJtoday)



## 2-STAGE CALCULATION OF mean MJ per patch per season - BEST

#~~~ ALL SURVEY DAYS ~~~#

# 1. Calc mean, sd and day-to-day variance for each patch (or can just use means in 'patchdata' if don't need SD)
MJperday_40days <- ddply(dailykjdata, c("SeasonID", "StationCode"), summarise, 
                         N=length(FedToday), # Some patches don't have 40 records - maybe because was not sure whether fed or not, or camera covered/not working: best to calc mean excluding these 'unclear' days - so this mean calc with ddply is fine. 
                         meanMJ=mean(sumMJtoday), 
                         sdMJ=sd(sumMJtoday),
                         varMJ=var(sumMJtoday)) # variance in energetic content of food per day (all survey days) as measure of predictability.

# 2. Calc mean per season, SD represents between-patch variation
meanMJperday_40days <- ddply(MJperday_40days, "SeasonID", summarise, 
                             mean=mean(meanMJ),
                             sd=sd(meanMJ))
# same as above, but using 'patchdata' :-
meanMJperday_40days. <- ddply(patchdata, "SeasonID", summarise, 
                             mean=mean(MeanMJperDay),
                             sd=sd(MeanMJperDay))

# Calc overall mean energetic value per day per patch - BEST as accounts for between-patch variation
mean(MJperday_40days$mean)
sd(MJperday_40days$mean)




#~~~ FEEDING DAYS ONLY ~~~#

# 1. Calc mean per patch
MJperday_40daysFD <- ddply(dailykjdataFD, c("SeasonID", "StationCode"), summarise, 
                           N=length(FedToday), # Some patches don't have 40 records - maybe because was not sure whether fed or not, or camera covered/not working: best to calc mean excluding these 'unclear' days - so this mean calc with ddply is fine. 
                           meanMJ=mean(sumMJtoday), 
                           sdMJ=sd(sumMJtoday)) 
# save as csv file for making data publicly available
write.csv(MJperday_40daysFD, file="MJperday_40daysFD.csv", row.names = FALSE) # saves as csv file in current working directory



# 2. Calc mean per season, SD represents between-patch variation
meanMJperday_40daysFD <- ddply(MJperday_40daysFD, "SeasonID", summarise, 
                               mean=mean(meanMJ),
                               sd=sd(meanMJ),
                               median=median(meanMJ))

meanMJperday_40daysFD. <- ddply(patchdata, "SeasonID", summarise, 
                               mean=mean(MeanMJperFeedingDay),
                               sd=sd(MeanMJperFeedingDay),
                               median=median(MeanMJperFeedingDay),
                               mad=mad(MeanMJperFeedingDay),
                               iqr = IQR(MeanMJperFeedingDay))


# Calc overall mean energetic value per day per patch - BEST as accounts for between-patch variation
mean(MJperday_40daysFD$mean)
sd(MJperday_40daysFD$mean)



### MODELS FOR SEASONAL DIFF IN MEANS (can't use raw (non-mean) values as leaves zeros)
#--------------------------------------

## Make factors
MJperday_40days$fSeason <- factor(MJperday_40days$SeasonID) # ALL SURVEY DAYS
MJperday_40daysFD$fSeason <- factor(MJperday_40daysFD$SeasonID) # FEEDING DAYS ONLY

## Check distribution
library(fitdistrplus)
library(car)

# ALL SURVEY DAYS
#meanMJ
descdist(MJperday_40days$meanMJ, discrete = F, boot=1000) # gamma or lognormal
qqp(MJperday_40days$meanMJ, "lnorm", main="lognormal") 
gamma <- fitdistr(MJperday_40days$meanMJ, "gamma", start=NULL)
qqp(MJperday_40days$meanMJ, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")

# FEEDING DAYS ONLY
descdist(MJperday_40daysFD$meanMJ, discrete = F, boot=1000) # gamma or lognormal
qqp(MJperday_40daysFD$meanMJ, "lnorm", main="lognormal") 
gamma <- fitdistr(MJperday_40daysFD$meanMJ, "gamma", start=NULL)
qqp(MJperday_40daysFD$meanMJ, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")

# Lognormal is best for all


## Fit lognormal modelS
MJseas_mod <-lmer(log10(meanMJ) ~ fSeason + (1|StationCode), data=MJperday_40days, REML=F)
MJseas_modFD <-lmer(log10(meanMJ) ~ fSeason + (1|StationCode), data=MJperday_40daysFD, REML=F)


## Check resids

# ALL SURVEY DAYS
#mean
hist(resid(MJseas_mod))
plot(fitted(MJseas_mod), resid(MJseas_mod))
lines(smooth.spline(fitted(MJseas_mod), resid(MJseas_mod))) # looks good

# FEEDING DAYS
hist(resid(MJseas_modFD))
plot(fitted(MJseas_modFD), resid(MJseas_modFD))
lines(smooth.spline(fitted(MJseas_modFD), resid(MJseas_modFD))) # looks good


## Fit null model to compare

# ALL SURVEY DAYS
MJseas_mod_null <-lmer(log10(meanMJ) ~ 1 + (1|StationCode), data=MJperday_40days, REML=F)
anova(MJseas_mod, MJseas_mod_null) # X2=8.1254,3,0.04349
a <-coef(summary(MJseas_mod))

# FEEDING DAYS
MJseas_mod_nullFD <-lmer(log10(meanMJ) ~ 1 + (1|StationCode), data=MJperday_40daysFD, REML=F)
anova(MJseas_modFD, MJseas_mod_nullFD) # X2=12.849,3,0.004976
a <-coef(summary(MJseas_modFD))


# CONCLUSIONS: 
# FOOD QUANTITY VARIES BETWEEN SEASONS REGARDLESS OF WHETHER CALC PER FEEDING DAY OR PER SURVEY DAY.
# FEEDING FREQUENCY (PREDICTABILITY or MEAN DAYS FED PER WEEK) DOESNT VARY WITH SEASON



### POST-HOC TEST TO SEE WHICH SEASONS DIFFERED
library(lsmeans)

lsm_MJ <- lsmeans::lsmeans(MJseas_modFD, pairwise~fSeason, type="response")
lsmcontrasts_MJ <- data.frame(summary(lsm_MJ)$contrasts)


### PLOTTING - not really needed.
library(ggplot2)

# MJ means
lsmpreds_MJ <- data.frame(summary(lsm_MJ)$lsmeans)
lsmpreds_MJ$group <- "1" # fake variable so can connect points with a line in ggplot

# also get means from days fed (DF) model
lsm_DF <- lsmeans::lsmeans(DFseas_mod, pairwise~fSeason, type="response")
lsmpreds_DF <- data.frame(summary(lsm_DF)$lsmeans)
lsmpreds_DF$group <- "1" # fake variable so can connect points with a line in ggplot
#----

# raw data for DF
patchdata$DaysFedPerWeek
patchdata$group <- "1" # fake variable for ggplot

# raw data for MJ
MJperday_40daysFD$meanMJ
MJperday_40daysFD$group <- "1" # fake variable for ggplot
MJperday_40daysFD$fSeason <- MJperday_40daysFD$SeasonID

# PLOT FOR DAYS FED PER WEEK
ggpDF <- ggplot(lsmpreds_DF, aes(x=fSeason, y=lsmean, group=factor(group))) +
  xlab("") + ylab("Mean days fed per week\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  geom_line(size=1)  +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=1, width=0.2,
                position=position_dodge(0.2)) +
  # overlay raw data to please Innes
  geom_point(data=patchdata, aes(fSeason, DaysFedPerWeek), colour="grey70", 
             position=position_jitter()) 

# PLOT FOR MJ PER DAY
ggpMJ <- ggplot(lsmpreds_MJ, aes(x=fSeason, y=response, group=factor(group))) +
  xlab("") + ylab("Mean MJ per feeding day\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  ylim(0,2.5) +
  geom_line(size=1)  + geom_point(size=4) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=1, width=0.2) +
  # overlay raw data to please Innes - colours for separate patches
  geom_point(data=MJperday_40daysFD, aes(fSeason, meanMJ, colour=StationCode), 
             size=2, position=position_jitter(0.2)) +
  scale_colour_discrete(guide=FALSE) # remove legend

scale_y_continuous(breaks=c(1,2,3,4,5,6)) +# to plot without ylimits

# save above in high resolution for publications
dev.print(jpeg, "RPlot_lsmean MJ per day across seasons - excl 16WD.jpeg", res=700, 
          height=8, width=6, units="in") 


### BASIC BOXPLOTS ARE CLEARER

boxplot(MJperday_40daysFD$meanMJ~MJperday_40daysFD$fSeason, xaxt='n', ylab="Mean MJ per provisioning day",xlab="", cex.axis=1.3,cex.lab=1.5)  # plot without axes intially
xtick<-seq(1, 4, by=1)                                      # specify where to put tick marks
seasonlabels<-c("Spring", "Summer", "Autumn", "Winter")   
axis(side=1, at=xtick, labels = seasonlabels, cex.axis=1.3,cex.lab=1.5) 

dev.print(jpeg, "RPlot_boxplot MJ per day across seasons.jpeg", res=700, height=20, width=15, units="cm") 


boxplot(patchdata$DaysFedPerWeek~patchdata$SeasonID, xaxt='n', ylab="Mean days provisioned per week",xlab="", cex.axis=1.3,cex.lab=1.5)  # plot without axes intially
xtick<-seq(1, 4, by=1)                                      # specify where to put tick marks
seasonlabels<-c("Spring", "Summer", "Autumn", "Winter")   
axis(side=1, at=xtick, labels = seasonlabels, cex.axis=1.3,cex.lab=1.5) 

dev.print(jpeg, "RPlot_boxplot days fed per week across seasons.jpeg", res=700, height=20, width=15, units="cm") 
