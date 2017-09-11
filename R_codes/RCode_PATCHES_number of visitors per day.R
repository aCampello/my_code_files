rm(list=ls()) 
save.image("Nvisitors.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WHO VISITS PATCHES? 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Activity patterns")

#mydata <- read.csv("Activity_NewVisitStarts_Adults28SurveyDates.CSV", header=T)
mydata <- read.csv("Activity_NewVisitStarts_Adults28SurveyDates_ResidentsMarked4Paper300317.CSV", header=T) # with resident/nonresident variable
str(mydata)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### GET NUMBER OF VISITORS TO PATCHES (includes residents and nonresidents) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(plyr)
visitors <- ddply(mydata, c("Territory", "SeasonID", "StationCode", "ShortSex", "ShortStatus"),
                  summarise,
                  nvisitors=length(unique(ShortCode)))

# Change rows to columns for reporting
library(tidyr)
# syntax is: newdata <- tidyr::spread(mydata, predictor, response) 

tidyvisitors <- tidyr::spread(visitors, ShortSex, nvisitors)

# Subset by status and then merge to create columns for each status and sex (instead of rows) - saves copying and pasting in Excel
domvisitors <- subset(tidyvisitors, ShortStatus=="Dom")
subvisitors <- subset(tidyvisitors, ShortStatus=="Sub")
ustvisitors <- subset(tidyvisitors, ShortStatus=="Ust")

domsubvisitors <- merge(x=domvisitors, y=subvisitors, by = c("Territory", "SeasonID", "StationCode"), all.x=T, all.y=T)
allvisitors <- merge(x=domsubvisitors, y=ustvisitors, by = c("Territory", "SeasonID", "StationCode"), all.x=T, all.y=T)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### GET MEAN VISITORS PER DAY TO EACH PATCH WITH ST DEV (to add to table above) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert datetimes from character to POSIXct
mydata$TRANScDateTime <-as.POSIXct(paste(mydata$TRANScDateTime),format="%d/%m/%Y %H:%M:%S", tz="UTC") 
# Extract date only from datetime
mydata$DAY <- as.Date(mydata$TRANScDateTime, format="%d/%m/%Y") 

# Step 1: count TOTAL visitors per day to each PATCH (includes residents & non-residents)
visitorsperday <- ddply(mydata, c("Territory", "SeasonID", "StationCode", "DAY"),
                        summarise,
                        nvisitors =length(unique(ShortCode)))

# Step 2: Calculate MEAN and SD visitors per day to each PATCH (includes residents & non-residents)
mean_visitorsperday <- ddply(visitorsperday, c("Territory", "SeasonID", "StationCode"),
                             summarise,
                             mean.day = mean(nvisitors),
                             sd.day = sd(nvisitors))


### GET MEAN VISITORS PER DAY across all patches
mean(visitorsperday$nvisitors)
sd(visitorsperday$nvisitors)

# Mean of means to account for between-patch diffs (BEST):
mean(mean_visitorsperday$mean.day)
sd(mean_visitorsperday$mean.day)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### DOES FOOD AVAILABILITY AFFECT NUMBER OF VISITORS? ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use meankJperDay (quantity) and Days Fed per week (predictability) as foxes will go to see what's there: if use 
# exact kJ given on a particular day this won't affect whether or not they visit, as they're already there!

# Merge NDaysFed from patch data to Nvisitors data
setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding")
patchdata <- read.csv("Patch feeding freq_28surveys.csv")
str(patchdata)
str(visitorsperday)

nvis_kj_data <- merge(x=visitorsperday, y=patchdata, by = c("Territory", "SeasonID", "StationCode"))
str(nvis_kj_data)

# delete irrelevant columns
nvis_kj_data$TotalkJFed <- NULL
nvis_kj_data$Survey.Name <- NULL
nvis_kj_data$SurveyID <- NULL
nvis_kj_data$NDaysFed <- NULL
nvis_kj_data$TotalkJFed <- NULL

# Convert season to factor
nvis_kj_data$fSeason <- factor(nvis_kj_data$SeasonID)

# save as csv file for making data publicly available
# write.csv(nvis_kj_data, file="nvis_kj_data.csv", row.names = FALSE) # saves as csv file in current working directory


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### TEST CORRELATIONS BETWEEN DAYS FED, MJ PER FEEDING DAY AND DAY-TO-DAY VARIANCE IN MJ ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check distributions
hist(nvis_kj_data$DaysFedPerWeek)
hist(nvis_kj_data$MeanMJperFeedingDay)
hist(nvis_kj_data$varMJperDay)
# All not normal so use Spearman:


### SPEARMAN RANK CORRELATION TESTS: DF not correlated with MJ or var, MJ and var not correlated.

## Is DF correlated with MJ/feeding day?
cor.test(nvis_kj_data$DaysFedPerWeek, nvis_kj_data$MeanMJperFeedingDay, method = "spearm") 
# Not strongly: rho=0.4481387, p-value < 2.2e-16, df=4862 # = weak positive that is not due to chance
# to calc df:
min(length(nvis_kj_data$DaysFedPerWeek), length(nvis_kj_data$MeanMJperFeedingDay))-2 # -2 for two-tailed test

## Is DF correlated with MJ/survey day?
cor.test(nvis_kj_data$DaysFedPerWeek, nvis_kj_data$MeanMJperDay, method = "spearm") 
# Yes: rho=0.7767401, p-value < 2.2e-16, df= 4862 # fairly strong positive correlation

## Is DF correlated with day-to-day variance in MJ?
cor.test(nvis_kj_data$DaysFedPerWeek, nvis_kj_data$varMJperDay, method = "spearm") 
# No: rho= -0.2208237, p-value < 2.2e-16, df=4862 # = weak negative correlation that is not due to chance


## Is MJ/feeding day correlated with day-to-day variance in MJ?
cor.test(nvis_kj_data$MeanMJperFeedingDay, nvis_kj_data$varMJperDay, method = "spearm") 
# Moderately: rho=0.5743315, p-value < 2.2e-16, df=4862 # = moderate correlation that is not due to chance



## Check distribution
library(fitdistrplus)
descdist(nvis_kj_data$nvisitors, discrete = T, boot=1000) # Poisson should be fine.

# check for influential data points
plot(nvis_kj_data$nvisitors~ nvis_kj_data$MeanMJperFeedingDay) 
# no extreme y values or single extreme x values so is prob OK - all 
# extreme x-values are from the same patches so having these as ranef may 
# help reduce their influence - also log link in model may reduce variance in x


# !! SEE AFTER MODEL FITTING FOR CHECKING FOR INFLUENTIAL VALUES!!


## Fit poisson model as nvisitors are counts
library(lme4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First test food variables separately ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nvis_mod_NULL <- glmer(nvisitors ~ 
                         (1|StationCode) + 
                         (1|fSeason), 
                       data=nvis_kj_data, 
                       family=poisson(link="log"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=100000), 
                                            check.conv.grad=.makeCC("warning",0.05))) 
# NOTE - DECIDED NOT TO USE VARIANCE IN THE END
nvis_mod_var <- glmer(nvisitors ~ 
                        varMJperDay +
                        (1|StationCode) + 
                        (1|fSeason), 
                      data=nvis_kj_data, 
                      family=poisson(link="log"),
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=100000), 
                                           check.conv.grad=.makeCC("warning",0.05))) 
anova(nvis_mod_var, nvis_mod_NULL) #10.077      1   0.001501 **

nvis_mod_DF <- glmer(nvisitors ~ 
                       DaysFedPerWeek +
                       (1|StationCode) + 
                       (1|fSeason), 
                     data=nvis_kj_data, 
                     family=poisson(link="log"),
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000), 
                                          check.conv.grad=.makeCC("warning",0.05)))
anova(nvis_mod_DF, nvis_mod_NULL) #27.85      1  1.311e-07 *** 

# MJ PER FEEDING DAY
nvis_mod_MJ <- glmer(nvisitors ~ 
                       MeanMJperFeedingDay + 
                       (1|StationCode) + 
                       (1|fSeason), 
                     data=nvis_kj_data, 
                     family=poisson(link="log"),
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000), 
                                          check.conv.grad=.makeCC("warning",0.05)))
anova(nvis_mod_MJ, nvis_mod_NULL) # 4.7093      1       0.03 *

# MJ PER SURVEY DAY - as combo od DF and MJ
nvis_mod_MJday <- glmer(nvisitors ~ 
                          MeanMJperDay + 
                          (1|StationCode) + 
                          (1|fSeason), 
                        data=nvis_kj_data, 
                        family=poisson(link="log"),
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05)))
anova(nvis_mod_MJday, nvis_mod_NULL) # 20.662      1  5.479e-06 ***
# MJ PER FEEDING DAY HAS STRONGER EFFECT THAN MJ PER DAY BUT WEAKER THAN DAYS FED



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PRINCIPAL COMPONENTS ANALYSIS TO COMBINE DF AND MJ WITH UNEQUAL WEIGHTING #### - didnt end up using this
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# OK to do PCA as DF and MJ are moderately correlated (r=0.45)
# output PC score represents 'mean daily food availability'

# Combine DF & MJ into 1 variable for first patch selection model
pr1 <- prcomp(~DaysFedPerWeek + MeankJperFeedingDay + varMJperDay, data=patchdata, scale=T) 
# scale=T rescales each variable to have mean=0 and variance=1, in case the variables have v. dissimilar variances
print(pr1)
# view % total variance captured by each component
summary(pr1) # report this (I think)

# plot the PCs
plot(pr1, type="lines") # PC1 explains more of the variance

# view eigenvalues of each PC - if eig<1 then PC is pretty useless 
eigs <- pr1$sdev^2
# calc % variance explained by each component
eigs/sum(eigs)

# plot PC1 vs PC2 scores for each observation (takes a while...)
biplot(pr1, cex=0.5)

# principal component loading vectors
pr1$rotation # days fed and MJ have equal loading in both PCs

# view PC scores for each principal component
pr1$x 
# Save PC1 scores as a new variable
patchdata$PC1 <- predict(pr1)[,1] 
summary(patchdata$PC1)
str(patchdata)
nvis_kj_data$PC1 <- patchdata[match(nvis_kj_data$StationID, patchdata$StationID),15]
summary(nvis_kj_data$PC1)

# plots to compare correlations with original variables
# to see whether PC1 is really better than using mean MJ per day (MJ/feeding day divided by days fed)
par(mfrow=c(2,2))
plot(-nvis_kj_data$PC1~nvis_kj_data$DaysFedPerWeek)
plot(nvis_kj_data$MeanMJperDay~nvis_kj_data$DaysFedPerWeek)
plot(-nvis_kj_data$PC1~ nvis_kj_data$MeanMJperFeedingDay)
plot(nvis_kj_data$MeanMJperDay~ nvis_kj_data$MeanMJperFeedingDay)
# CONCLUSION: PC1 SEEMS LESS BIASED BY (CORRELATED WITH) MJ PER FEEDING DAY THAN
# MJ PER DAY: THERE IS A WIDER SPREAD OF VALUES AT LOW MJ PER FEEDING DAY


# MODEL WITH PC1
nvis_mod_PC1 <- glmer(nvisitors ~ 
                        PC1 + 
                        (1|StationCode) + 
                        (1|fSeason), 
                      data=nvis_kj_data, 
                      family=poisson(link="log"),
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=100000), 
                                           check.conv.grad=.makeCC("warning",0.05)))
anova(nvis_mod_PC1, nvis_mod_NULL) # 28.643,1,8.705e-08 *** STRONGER EFFECT THAN MJ PER DAY & DF
a <- coef(summary(nvis_mod_PC1)) # saved but didn't use in thesis as didnt make sense to report both models.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MODELS USED IN THESIS - without variance & using MJ per feeding day (not survey day) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FULL MODEL - MJ PER FEEDING DAY
nvis_mod <- glmer(nvisitors ~ 
                    DaysFedPerWeek + 
                    MeanMJperFeedingDay +
                    (1|StationCode) + 
                    (1|fSeason), 
                  data=nvis_kj_data, 
                  family=poisson(link="log"),
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl = list(maxfun=100000), 
                                       check.conv.grad=.makeCC("warning",0.05))) 
# NO DAYS FED
nvis_mod_nodaysfed <- glmer(nvisitors ~ 
                              MeanMJperFeedingDay +
                              (1|StationCode) + 
                              (1|fSeason), 
                            data=nvis_kj_data, 
                            family=poisson(link="log"),
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000), 
                                                 check.conv.grad=.makeCC("warning",0.05))) 

anova(nvis_mod, nvis_mod_nodaysfed) # x2=25.484      1  4.461e-07 # NEED DAYS FED

nvis_mod_noMJ <- glmer(nvisitors ~ 
                         DaysFedPerWeek + 
                         (1|StationCode) + 
                         (1|fSeason), 
                       data=nvis_kj_data, 
                       family=poisson(link="log"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=100000), 
                                            check.conv.grad=.makeCC("warning",0.05))) 
anova(nvis_mod, nvis_mod_noMJ) # 2.3436      1     0.1258 # DONT NEED MJ

nvis_mod_null <- glmer(nvisitors ~ 
                         (1|StationCode) + 
                         (1|fSeason), 
                       data=nvis_kj_data, 
                       family=poisson(link="log"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=100000), 
                                            check.conv.grad=.makeCC("warning",0.05))) 

bbmle::ICtab(nvis_mod, nvis_mod_nodaysfed, nvis_mod_noMJ, nvis_mod_null) # def dont need MJ

# FINAL MODEL
nvis_mod_FINAL <- glmer(nvisitors ~ 
                          DaysFedPerWeek + 
                          (1|StationCode) + 
                          (1|fSeason), 
                        data=nvis_kj_data, 
                        family=poisson(link="log"),
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05))) 
# Check residuals
plot(fitted(nvis_mod_FINAL), resid(nvis_mod_FINAL))
lines(smooth.spline(fitted(nvis_mod_FINAL), resid(nvis_mod_FINAL)))
#plot cullen & frey graph
library(fitdistrplus)
descdist(residuals(nvis_mod_FINAL), boot = 1000) # to indicate influential datapoints that may be outliers
# resids are normal

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
overdisp_fun(nvis_mod_FINAL) # p=1 so not overdispersed

# Assess leverage of full mod (outliers with extreme x values likely to influence model estimates)
library(influence.ME)
inf <- influence(model=nvis_mod, count=TRUE, group="StationCode")
sigtest(inf, test=-1.96) # no patches are overly influential

### Variance inflation factor (VIF) to test for multicollineatiry 
vif.mer(nvis_mod) # both ~1 so OK (>2 becomes problematic - Graham 2003 paper)

### Save model coefficients
a <- coef(summary(nvis_mod_FINAL))

### Lsmeans predictions to discuss in thesis results section
library(lsmeans)

# Mean n visitors at increasing quantity per feeding day with mean predictability
lsm <- lsmeans::lsmeans(nvis_mod_FINAL, "DaysFedPerWeek",
                           at = list(DaysFedPerWeek = c(1:7)), 
                           type="response")
lsm_df <- data.frame(summary(lsm))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PLOTTING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(ggplot2)

# set wd to save plots in right place
setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/N visitors")

#= Plot preds for difflevels of food predictability (DaysFedPerWeek)
ggp <- ggplot(lsm_df, aes(x=DaysFedPerWeek, y=rate)) 
ggp + 
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80") +
  geom_line(size=1) + geom_point(size=3) +
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = 1:7) +
  xlab("\nDays fed per week") + ylab("Number of visitors per day\n") 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_Poisson GLMM preds & 95CIs nvisitors_DaysFedPerWeek.jpeg", res=700, height=5, width=6, units="in") 


# WITH RAW DATA MEANAS as points: 
# mean number of resident AND NON-RESIDENT visitors per patch per day
patchmeans_season_all <- ddply(nvis_kj_data, c("StationCode", "SeasonID"),
                           summarise,
                           meanvisitors = mean(nvisitors),
                           sdvisitors = sd(nvisitors),
                           meanDF = mean(DaysFedPerWeek),
                           sdDF = sd(DaysFedPerWeek),
                           meanMJ = mean(MeanMJperFeedingDay),
                           sdMJ = sd(MeanMJperFeedingDay))

ggp <- ggplot(lsm_df, aes(x=DaysFedPerWeek, y=rate)) 
ggp + 
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), 
              fill="gray80", alpha=0.5) +
  geom_point(data=patchmeans_season_all, aes(x=meanDF, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_line(size=1) + 
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.2),
        axis.text = element_text(colour = "black")) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day") +
  scale_x_continuous(breaks = c(1:7)) 
# dev.print(jpeg, "RPlot_lsmean N visitors X DF_for ALL FOXES_points are patch means per season_ModWithoutMJ.jpeg", res=700, height=6, width=7, units="in") 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### OLD CODE as realised it was not sensible to fit models with correlated predictors #####

# PREDICTIONS TO PLOT DAYS FED & MJ/FEEDING DAY ON SAME AXES 
# (Could have used rbind to combine two sets of lsmeans but I wanted to keep this code)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Specify levels of variables to predict response for
pred.data <- expand.grid(DaysFedPerWeek=seq(1, 7, 2),
                         MeanMJperDay=seq(0,6,0.2),
                         StationCode=unique(nvis_kj_data$StationCode),
                         fSeason=unique(nvis_kj_data$fSeason)) 

# 2. Get model matrix: write fixed effects in same way as model formula. Have to include all variables in pred.data
modelmatrix <-model.matrix(~ DaysFedPerWeek + MeanMJperDay, pred.data) 

# 3. Predictions on link scale
y <-modelmatrix%*%fixef(nvis_mod_FINAL) 

# 4. Save variance due to fixed effects (i think)
pvar1 <- diag(modelmatrix %*% tcrossprod(vcov(nvis_mod_FINAL),modelmatrix))

# 5. Add variance due to random effects
tvar1 <- pvar1 + VarCorr(nvis_mod_FINAL)$StationCode[1] + VarCorr(nvis_mod_FINAL)$fSeason[1] # Specifies to include intercepts (variance) from each random effect. [1] specifies to use the intercept value

# 6. Calc confidence intervals, backtransform and combine all in a data frame:
newdata <-data.frame(
  DaysFedPerWeek=pred.data$DaysFedPerWeek,
  MeanMJperDay=pred.data$MeanMJperDay,
  y=exp(y),
  ciLWR = exp(y-1.96*sqrt(pvar1))  # LWR 95% CI
  , ciUPR = exp(y+1.96*sqrt(pvar1)))  # UPR 95% CI
head(newdata)
preds_MJandDaysFed1357 <- newdata # save as new df incase overwrite

# PLOT
ggp <- ggplot(preds_MJandDaysFed1357, aes(x=MeanMJperDay, y=y, colour=factor(DaysFedPerWeek))) # need to specify it's a factor to get separate colours
ggp + 
  geom_ribbon(mapping=aes(x=MeanMJperDay, ymin=ciLWR, ymax=ciUPR, fill=factor(DaysFedPerWeek)),
              linetype=0, alpha = 0.2, show.legend=F) +
  geom_line(aes(group=DaysFedPerWeek), size=1) + 
  theme_bw(base_size = 14, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = 0:6) +
  scale_colour_discrete(name="Mean days fed\nper week") +
  xlab("\nMean MJ fed per day") + ylab("Number of visitors per day\n") 

# save above in high resolution for publications
#dev.print(jpeg, "RPlot_Poisson GLMM preds & 95CIs nvisitors_MJ & 1357daysfedPW.jpeg", res=700, height=5, width=7, units="in") 

#
#
#
#
#
#
#
#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### COMPARING EFFECT OF DF & MJ ON NUMBERS OF RESIDENT AND NON-RESIDENT VISITORS - FOR PAPER ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Step 1: count TOTAL visitors per day to each PATCH
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Resident visitors only
RESvisitorsperday <- ddply(subset(mydata, Resident==1), c("Territory", "SeasonID", "StationCode", "DAY"),
                           summarise,
                           nvisitors =length(unique(ShortCode)))

# Nonresident visitors only
NONRESvisitorsperday <- ddply(subset(mydata, Resident==0), c("Territory", "SeasonID", "StationCode", "DAY"),
                              summarise,
                              nvisitors =length(unique(ShortCode)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Step 2: Calculate MEAN and SD visitors per day to each PATCH (includes residents & non-residents)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Residents only
mean_RESvisitorsperday <- ddply(RESvisitorsperday, c("Territory", "SeasonID", "StationCode"),
                                summarise,
                                mean.day = mean(nvisitors),
                                sd.day = sd(nvisitors))

## Nonresidents only
mean_NONRESvisitorsperday <- ddply(NONRESvisitorsperday, c("Territory", "SeasonID", "StationCode"),
                                   summarise,
                                   mean.day = mean(nvisitors),
                                   sd.day = sd(nvisitors))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CREATE DATA FRAMES FOR MODELS:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load patch data
setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding")
patchdata <- read.csv("Patch feeding freq_28surveys.csv")

# Residents only
RESnvis_kj_data <- merge(x=RESvisitorsperday, y=patchdata, by = c("Territory", "SeasonID", "StationCode"))
RESnvis_kj_data$fSeason <- factor(RESnvis_kj_data$SeasonID) 

# Nonresidents only
NONRESnvis_kj_data <- merge(x=NONRESvisitorsperday, y=patchdata, by = c("Territory", "SeasonID", "StationCode"))
NONRESnvis_kj_data$fSeason <- factor(NONRESnvis_kj_data$SeasonID) 

## Check distribution
library(fitdistrplus)
descdist(RESnvis_kj_data$nvisitors, discrete = T, boot=1000)  .
descdist(NONRESnvis_kj_data$nvisitors, discrete = T, boot=1000) 
# Poisson should be fine for both residents and nonresidents

# check for influential data points
plot(RESnvis_kj_data$nvisitors~ RESnvis_kj_data$MeanMJperFeedingDay) 
# no extreme y values or single extreme x values so is prob OK - all 
plot(NONRESnvis_kj_data$nvisitors~ NONRESnvis_kj_data$MeanMJperFeedingDay) # two potentially problematic values at high MJ



#~~~~~~~~~~~~~~~
#### MODELS ####
#~~~~~~~~~~~~~~~
library(lme4)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Non-residents only ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Full model:
NONRESnvis_mod <- glmer(nvisitors ~ 
                          DaysFedPerWeek + 
                          MeanMJperFeedingDay +
                          (1|StationCode) + (1|fSeason), 
                        data=NONRESnvis_kj_data, 
                        family=poisson(link="log"),
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05))) 
car::Anova(NONRESnvis_mod) # only DF is sig if only nonresidents are tested


### Model simplification:

# no DF
NONRESnvis_mod_noDF <- glmer(nvisitors ~ 
                               MeanMJperFeedingDay +
                               (1|StationCode) + 
                               (1|fSeason), 
                             data=NONRESnvis_kj_data, 
                             family=poisson(link="log"),
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05))) 
anova(NONRESnvis_mod, NONRESnvis_mod_noDF) # x2(1)=17.87, p=2.365e-05***   # Need DF

# no MJ
NONRESnvis_mod_noMJ <- glmer(nvisitors ~ 
                               DaysFedPerWeek + 
                               (1|StationCode) + (1|fSeason), 
                             data=NONRESnvis_kj_data, 
                             family=poisson(link="log"),
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05))) 
anova(NONRESnvis_mod, NONRESnvis_mod_noMJ) # x2(1)=0.3895, p=0.5326  # dont need MJ


### Final model:
NONRESnvis_FINALMOD <- glmer(nvisitors ~ 
                               DaysFedPerWeek + 
                               (1|StationCode) + (1|fSeason), 
                             data=NONRESnvis_kj_data, 
                             family=poisson(link="log"),
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05))) 
a<-coef(summary(NONRESnvis_FINALMOD))

# chisq for reporting
NONRESnvis_NULL <- glmer(nvisitors ~ 
                           (1|StationCode) + (1|fSeason), 
                         data=NONRESnvis_kj_data, 
                         family=poisson(link="log"),
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05))) 
anova(NONRESnvis_FINALMOD, NONRESnvis_NULL) # x2(1)=17.535, p=2.821e-05***
anova(NONRESnvis_FINALMOD, NONRESnvis_mod) # x2(1)=0.3895, p=0.5326


### Model checking:
plot(fitted(NONRESnvis_FINALMOD), resid(NONRESnvis_FINALMOD))
lines(smooth.spline(fitted(NONRESnvis_FINALMOD), resid(NONRESnvis_FINALMOD)))
descdist(residuals(NONRESnvis_FINALMOD), boot = 1000) 
# resids are NOT NORMAL...
dispersion_glmer(NONRESnvis_FINALMOD) #0.5633025
deviance(NONRESnvis_FINALMOD)/df.residual(NONRESnvis_FINALMOD) # underdispersed
# check for overly influential patches:
inf2 <- influence(model=NONRESnvis_FINALMOD, count=TRUE, group="StationCode")
sigtest(inf2, test=-1.96) # no patches are overly influential on DF, some change intercept but not important
plot(inf2)
# Try refitting without 17BC...


# ...Nonresident models without 17BC:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Full model:
NONRESnvis_mod_no17BC <- glmer(nvisitors ~ 
                                 DaysFedPerWeek + 
                                 MeanMJperFeedingDay +
                                 (1|StationCode) + (1|fSeason), 
                               data=subset(NONRESnvis_kj_data, StationCode!="17BC"), 
                               family=poisson(link="log"),
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05))) 
summary(NONRESnvis_mod_no17BC) # nothing is significant now

### Model simplification:
# no DF
NONRESnvis_mod_noDF_no17BC <- glmer(nvisitors ~ 
                                      MeanMJperFeedingDay +
                                      (1|StationCode) + 
                                      (1|fSeason), 
                                    data=subset(NONRESnvis_kj_data, StationCode!="17BC"), 
                                    family=poisson(link="log"),
                                    control=glmerControl(optimizer="bobyqa",
                                                         optCtrl = list(maxfun=100000), 
                                                         check.conv.grad=.makeCC("warning",0.05))) 
anova(NONRESnvis_mod_no17BC, NONRESnvis_mod_noDF_no17BC) # x2(1)=2.0294, p=0.1543  

# no MJ
NONRESnvis_mod_noMJ_no17BC <- glmer(nvisitors ~ 
                                      DaysFedPerWeek + 
                                      (1|StationCode) + (1|fSeason), 
                                    data=subset(NONRESnvis_kj_data, StationCode!="17BC"), 
                                    family=poisson(link="log"),
                                    control=glmerControl(optimizer="bobyqa",
                                                         optCtrl = list(maxfun=100000), 
                                                         check.conv.grad=.makeCC("warning",0.05))) 
anova(NONRESnvis_mod_no17BC, NONRESnvis_mod_noMJ_no17BC) # x2(1)=0.4997, p=0.4796

# CONCLUSION: DF and MJ has no sig effect on the number of nonresidents seen in gardens.
# So just present data on residents.

### Model checking:
plot(fitted(NONRESnvis_mod_no17BC), resid(NONRESnvis_mod_no17BC))
lines(smooth.spline(fitted(NONRESnvis_mod_no17BC), resid(NONRESnvis_mod_no17BC)))
descdist(residuals(NONRESnvis_mod_no17BC), boot = 1000) 
# resids are NOT NORMAL...
dispersion_glmer(NONRESnvis_mod_no17BC) #0.517
deviance(NONRESnvis_mod_no17BC)/df.residual(NONRESnvis_mod_no17BC) # 0.25; underdispersed
# check for overly influential patches:
inf2 <- influence(model=NONRESnvis_mod_no17BC, count=TRUE, group="StationCode")
sigtest(inf2, test=-1.96) # no patches are overly influential on DF, some change intercept but not important
plot(inf2)




#~~~~~~~~~~~~~~~~~~~~~~~
#### Residents only ####
#~~~~~~~~~~~~~~~~~~~~~~~

### Full model:
RESnvis_mod <- glmer(nvisitors ~ 
                       DaysFedPerWeek + 
                       MeanMJperFeedingDay +
                       (1|StationCode) + (1|fSeason), 
                     data=RESnvis_kj_data, 
                     family=poisson(link="log"),
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000), 
                                          check.conv.grad=.makeCC("warning",0.05))) 
car::Anova(RESnvis_mod) # now MJ is also sig if only residents are tested


### Model simplification:

# no DF
RESnvis_mod_noDF <- glmer(nvisitors ~ 
                            MeanMJperFeedingDay +
                            (1|StationCode) + 
                            (1|fSeason), 
                          data=RESnvis_kj_data, 
                          family=poisson(link="log"),
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05))) 
anova(RESnvis_mod, RESnvis_mod_noDF) # x2=14.375, p=0.0001498***   # Need DF

# no MJ
RESnvis_mod_noMJ <- glmer(nvisitors ~ 
                            DaysFedPerWeek + 
                            (1|StationCode) + (1|fSeason), 
                          data=RESnvis_kj_data, 
                          family=poisson(link="log"),
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05))) 
anova(RESnvis_mod, RESnvis_mod_noMJ) # x2(1)=3.7077, p=0.05416 .   # Need MJ (just!)


### Final model:
RESnvis_FINALMOD <- glmer(nvisitors ~ 
                            DaysFedPerWeek + 
                            MeanMJperFeedingDay +
                            (1|StationCode) + (1|fSeason), 
                          data=RESnvis_kj_data, 
                          family=poisson(link="log"),
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05)))
a<-coef(summary(RESnvis_FINALMOD))

# chisq for reporting
anova(RESnvis_FINALMOD, RESnvis_mod_noDF) # x2(1)=14.375, p=0.0001498***
anova(RESnvis_FINALMOD, RESnvis_mod_noMJ) # x2(1)=3.7077, p=0.05416 .


### Model checking:
plot(fitted(RESnvis_FINALMOD), resid(RESnvis_FINALMOD))
lines(smooth.spline(fitted(RESnvis_FINALMOD), resid(RESnvis_FINALMOD)))
descdist(residuals(RESnvis_FINALMOD), boot = 1000) 
# resids are normal
vif.mer(RESnvis_FINALMOD) # both VIF<2 so OK
dispersion_glmer(RESnvis_FINALMOD) #0.625 - underdispersed
# check for overly influential patches:
inf <- influence(model=RESnvis_FINALMOD, count=TRUE, group="StationCode")
sigtest(inf, test=-1.96) # no patches are overly influential
plot(inf) # no overly influential patches (easier to tell from the plot than the test)

# Try refitting without 16WD as has really high mean n.visitors and MJ:
RESnvismod_No16WD <- glmer(nvisitors ~ 
                             DaysFedPerWeek + 
                             MeanMJperFeedingDay +
                             (1|StationCode) + (1|fSeason), 
                           data=subset(RESnvis_kj_data, StationCode!="16WD"), 
                           family=poisson(link="log"),
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))

a<-coef(summary(RESnvis_FINALMOD)) # Now MJ is not significant - was borderline anyway, so can remove from model...

# get chisq for reporting (in case):
# Remove DF:
RESnvismod_No16WD_noDF = update(RESnvismod_No16WD, . ~ . - DaysFedPerWeek)
anova(RESnvismod_No16WD, RESnvismod_No16WD_noDF) # x2(1)=16.056, p=6.148e-05*** # still need DF

# Remove MJ:
RESnvismod_No16WD_noMJ = update(RESnvismod_No16WD, . ~ . - MeanMJperFeedingDay)
anova(RESnvismod_No16WD, RESnvismod_No16WD_noMJ) # x2(1)=0.7474, P=0.3873 # MJ is now v.non-sig.


# Save model fit to full dataset without MJ, to compare plots:
a<-coef(summary(RESnvis_mod_noMJ))

RESnvis_mod_noMJ_noDF = update(RESnvis_mod_noMJ, . ~ . - DaysFedPerWeek)
anova(RESnvis_mod_noMJ, RESnvis_mod_noMJ_noDF) # x2(1)=16.794, p=4.168e-05***

RESnvis_mod_noMJ_withMJ = update(RESnvis_mod_noMJ, . ~ . + MeanMJperFeedingDay)
anova(RESnvis_mod_noMJ, RESnvis_mod_noMJ_withMJ) # x2(1)=3.7077, p=0.05416


#~~~~~~~~~~~~~~~~~~~~~~~~~
### Model predictions #### - Residents only
#~~~~~~~~~~~~~~~~~~~~~~~~~
library(lsmeans)

# Mean n visitors at increasing quantity per feeding day with mean predictability
lsmdf <- lsmeans::lsmeans(RESnvis_FINALMOD, "DaysFedPerWeek",
                          at = list(DaysFedPerWeek = c(1:7)), 
                          type="response")
lsm_DF <- data.frame(summary(lsmdf))


lsmmj<- lsmeans::lsmeans(RESnvis_FINALMOD, "MeanMJperFeedingDay",
                         at = list(MeanMJperFeedingDay = seq(0,6,1)), 
                         type="response")

lsm_MJ <- data.frame(summary(lsmmj))


# preds from alternative model without MJ, as is only borderline significant:
lsmdf_alt <- lsmeans::lsmeans(RESnvis_mod_noMJ, "DaysFedPerWeek",
                              at = list(DaysFedPerWeek = c(1:7)), 
                              type="response")
lsm_DF_alt <- data.frame(summary(lsmdf_alt))





### Plotting (in B&W for paper):
library(ggplot2)

### Plotting DF ####

# just plot predictions:
ggp <- ggplot(lsm_DF, aes(x=DaysFedPerWeek, y=rate)) 
ggp + 
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.7) +
  geom_line(size=1.2) + geom_point(size=4) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = 1:7) +
  ylim(1.5,4.5) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day")  
#dev.print(jpeg, "RPlot_lsmean N visitors X DF for RESIDENTS.jpeg", res=700, height=6, width=7, units="in") 


# plot preds With raw data:
ggp <- ggplot(lsm_DF, aes(x=DaysFedPerWeek, y=rate)) 
ggp + 
  geom_point(data=RESnvis_kj_data, aes(x=DaysFedPerWeek, y=nvisitors), 
             col="gray40", size=0.85, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.7) +
  geom_line(size=1.2) + geom_point(size=4) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks = 1:7) +
 # ylim(1,7) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day")  
#dev.print(jpeg, "RPlot_lsmean N visitors X DF for RESIDENTS_points are raw data.jpeg", res=700, height=6, width=7, units="in") 



### plot preds with mean n visitors per patch as points rather than all days (clearer):

# Calc mean visitors per day & mean MJ and DF for each patch (across all seasons, residents only):
patchmeans_year <- ddply(RESnvis_kj_data, c("StationCode"),
                         summarise,
                         meanvisitors = mean(nvisitors),
                         sdvisitors = sd(nvisitors),
                         meanDF = mean(DaysFedPerWeek),
                         sdDF = sd(DaysFedPerWeek),
                         meanMJ = mean(MeanMJperFeedingDay),
                         sdMJ = sd(MeanMJperFeedingDay))
# mean number of RESIDENT visitors per patch per day
patchmeans_season <- ddply(RESnvis_kj_data, c("StationCode", "SeasonID"),
                         summarise,
                         meanvisitors = mean(nvisitors),
                         sdvisitors = sd(nvisitors),
                         meanDF = mean(DaysFedPerWeek),
                         sdDF = sd(DaysFedPerWeek),
                         meanMJ = mean(MeanMJperFeedingDay),
                         sdMJ = sd(MeanMJperFeedingDay))


# now can plot the preds with raw data means:
# year
ggp <- ggplot(lsm_DF, aes(x=DaysFedPerWeek, y=rate)) 
ggp +
  geom_point(data=patchmeans_year, aes(x=meanDF, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.5) +
  geom_line(size=1.1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day")  
#dev.print(jpeg, "RPlot_lsmean N visitors X DF for RESIDENTS_points are patch means across all seasons.jpeg", res=700, height=6, width=7, units="in") 

# season
ggp <- ggplot(lsm_DF, aes(x=DaysFedPerWeek, y=rate)) 
ggp +
  geom_point(data=patchmeans_season, aes(x=meanDF, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.5) +
  geom_line(size=1.1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day")  
#dev.print(jpeg, "RPlot_lsmean N visitors X DF for RESIDENTS_points are patch means per season.jpeg", res=700, height=6, width=7, units="in") 



# As above but from model without MJ:
ggp <- ggplot(lsm_DF_alt, aes(x=DaysFedPerWeek, y=rate)) 

ggp + 
  geom_point(data=patchmeans_year, aes(x=meanDF, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.5) +
  geom_line(size=1.1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day")  
#dev.print(jpeg, "RPlot_lsmean N visitors X DF for RESIDENTS_points are patch means across all seasons_ModWithoutMJ.jpeg", res=700, height=6, width=7, units="in") 


# season
ggp <- ggplot(lsm_DF_alt, aes(x=DaysFedPerWeek, y=rate)) 
ggp +
  geom_point(data=patchmeans_season, aes(x=meanDF, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.5) +
  geom_line(size=1.1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  xlab("Days provisioned per week") + ylab("Number of visitors per day")  
#dev.print(jpeg, "RPlot_lsmean N visitors X DF for RESIDENTS_points are patch means per season_ModWithoutMJ.jpeg", res=700, height=6, width=7, units="in") 




### Plotting MJ ####

# Just plot model preds:
ggp <- ggplot(lsm_MJ, aes(x=MeanMJperFeedingDay, y=rate)) 
ggp + 
  geom_ribbon(mapping=aes(x=MeanMJperFeedingDay, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80") +
  geom_line(size=1.2) + geom_point(size=4) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(0:6)) +
  ylim(2,6) +
  xlab("MJ per provisioning day") + ylab("Number of visitors per day") 
#dev.print(jpeg, "RPlot_lsmean N visitors X MJ for RESIDENTS.jpeg", res=700, height=6, width=7, units="in") 


# plot preds with raw data:
ggp <- ggplot(lsm_MJ, aes(x=MeanMJperFeedingDay, y=rate)) 
ggp + 
  geom_point(data=RESnvis_kj_data, aes(x=MeanMJperFeedingDay, y=nvisitors), 
             col="gray40", size=0.85, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=MeanMJperFeedingDay, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.7) +
  geom_line(size=1.2) + geom_point(size=4) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  # scale_x_continuous(breaks=c(0:6)) +
  # ylim(2,6) +
  xlab("MJ per provisioning day") + ylab("Number of visitors per day") 
#dev.print(jpeg, "RPlot_lsmean N visitors X MJ for RESIDENTS_points are raw data.jpeg", res=700, height=6, width=7, units="in") 


# plot preds with raw data means:
# year
ggp <- ggplot(lsm_MJ, aes(x=MeanMJperFeedingDay, y=rate)) 
ggp + 
  geom_point(data=patchmeans_year, aes(x=meanMJ, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=MeanMJperFeedingDay, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.5) +
  geom_line(size=1.1) + 
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  # scale_x_continuous(breaks=c(0:6)) +
  #ylim(2,5.6) + # to exclude the outlier - but CI still remains...
  xlab("MJ per provisioning day") + ylab("Number of visitors per day") 

#dev.print(jpeg, "RPlot_lsmean N visitors X MJ for RESIDENTS_points are patch means across all seasons.jpeg", res=700, height=6, width=7, units="in") 
# 16WD is massive outlier...

# year
ggp <- ggplot(lsm_MJ, aes(x=MeanMJperFeedingDay, y=rate)) 
ggp + 
  geom_point(data=patchmeans_season, aes(x=meanMJ, y=meanvisitors), 
             shape=1, size=3, position=position_jitter()) +
  geom_ribbon(mapping=aes(x=MeanMJperFeedingDay, ymin=asymp.LCL, ymax=asymp.UCL), fill="gray80", alpha=0.5) +
  geom_line(size=1.1) + 
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  # scale_x_continuous(breaks=c(0:6)) +
  #ylim(2,5.6) +
  xlab("MJ per provisioning day") + ylab("Number of visitors per day") 
dev.print(jpeg, "RPlot_lsmean N visitors X MJ for RESIDENTS_points are patch means per season.jpeg", res=700, height=6, width=7, units="in") 
# all those 4 means at 6MJ must be from 16WD. 


