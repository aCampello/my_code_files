
# This code is for the contact rate analysis in thesis chapter 5

# seagate
load("e:/Statistics 2016/SNA/SNA in R/SNA models & randomisations/SNA models & randomisations.RData")

#================================================#
# Calculating daily contact rates and durations
#================================================#
# NOTES 180616:
# GLMER ended up being the best fit. Data were slightly overdispersed (0.5) but model residuals were not
# Could not fit a glmer poisson with obs-level ranef (wouldn't converge)
# No zero inflation (tested by fitting single-level ZI glms in library(pscl))
# Could not fit negbin models in glmmADMB - took ages and then got the 'Hessian was not positive definite' error - suspect due to the DaysSeen variable
# sequentially removed parameters from model to try and reduce complexity, but all terms were signif (if removing a term reduced model fit I put it back in before removing the next term)
# Compared models using AIC as model residuals were not overdispersed (otherwise would've used QAIC) # LR testing using anova based on AIC
# Full mod contained three-way interaction, days seen and days fed (days fed made a small contribution to model fit (dAIC=1.4 with vs without) so was kept in)

setwd("G:/Statistics 2016/SNA_synced with uni 180416noon/Trying sna in R 20-21.04.16") # SNA folder
setwd("G:/Statistics 2016/Contactcounts") # ContactCounts folder whether old version of contact rate work saved
setwd("//ads.bris.ac.uk/filestore/MyFiles/StudentPG1/jd12006/Documents/Analysis/WORKING ANALYSIS FOLDER - copy to seagate/") # uni


#===========#
# LOAD DATA 
#===========#

# Load associations
#----------------------- old file with duplicates
OLDdata <- read.csv(file = "G:/Statistics 2016/Contactcounts/SNA_R_noCubs_AllPatches_withduplicates.csv", header = T, stringsAsFactors=FALSE) # stringsasfactors stores text as text rather than factors that 'can cause problems' acc. to D. Farine
#----------------------- 
# new file after ran 'remove duplicates' in excel to remove duplicate recs of same dyad/place/time but diff GROUP & gbi_id (EncountersforR CamBase table contains 2 rows per fox in each dyad (so 4 rows per encounter) so could get both foxes in dyad to have the same GROUP ID)
mydata <- read.csv(file = "E:/Statistics 2016/Contactcounts/SNA_R_noCubs_AllPatches_noduplicates.csv", header = T, stringsAsFactors=FALSE) # stringsasfactors stores text as text rather than factors that 'can cause problems' acc. to D. Farine

mydata$Date <- strptime(mydata$DAY,format="%d/%m/%Y") 
mydata$daynum <- as.numeric(difftime(mydata$Date,min(mydata$Date),units="days")) + 1
mydata$Date <- NULL # POSIXCT data type messes with plyr so now delete the date column

# Self encounters are marked zero and true encounters = 1...
# To re-label true encounters as 1 to enable calculation of sums:
mydata$TrueAssoc <- ifelse(mydata$Selfencounter==0, 1, 0)

# Convert encounter durations to number of seconds     
library(lubridate)
mydata$duration <- hms(mydata$EncounterDuration)  # format character to 'hours:minutes:seconds'
mydata$duration.s <- hour(mydata$duration)*3600 + minute(mydata$duration)*60 + second(mydata$duration) # extract components of the datetime and convert to N seconds
detach(package:lubridate)


#===========================================#
# CALC SUMMARY STATS: N ENCOUNTERS PER DAY
#===========================================#
library(plyr)

# Calculate number of sightings and true associations per day
EncounterRate_sums <- ddply(mydata, c("Territory", "SeasonID", "Place", "DAY", "id"), 
                            summarise,
                            Nsightings = length(TrueAssoc),
                            NTrueAssocs = sum(TrueAssoc))

EncounterRate_sums # from data without duplicates - THEY ARE DIFFERENT...

# Calculate number of days seen PER PATCH
EncounterRate_DS <- ddply(EncounterRate_sums, c("Territory", "SeasonID", "Place", "id"), 
                          summarise,
                          DaysSeen = length(DAY))

# Add DaysSeen to EncounterRate data frame
EncounterRate_days <- merge(EncounterRate_sums, EncounterRate_DS, by=c("id", "SeasonID", "Territory", "Place"), all.x=TRUE)

# Add NDaysFedPerWeek (can't seem to add it from mydata so needed to import new table containing patch details)
patchdata <- read.csv("E:/Statistics 2016/Patch visitation/Feeding/Patch feeding freq_28surveys.csv")
patchdata$Place <- patchdata$StationCode
str(patchdata)

# merge
EncounterRate <- merge(x=EncounterRate_days, y=patchdata[ , c("SeasonID", "Place", "DaysFedPerWeek", "MeanMJperFeedingDay")], by.x=c("SeasonID", "Place"), all.x=T, all.y=F)

# Round days fed per week 
EncounterRate$DaysFedPW_rounded <- round(EncounterRate$DaysFedPerWeek)

# Add attributes: fox name, sex, status
attribs <-read.csv(file="E:/Statistics 2016/SNA/SNA in R/Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T, stringsAsFactors=FALSE) # Load attributes data
EncounterRate$ShortCode<-attribs[match(EncounterRate$id, attribs$id),4]
EncounterRate$Sex <-attribs[match(EncounterRate$id, attribs$id),5]
EncounterRate$SocialStatus <-attribs[match(EncounterRate$id, attribs$id),6]

# save factors and integers
EncounterRate$SocialStatus <- factor(EncounterRate$SocialStatus)
EncounterRate$Sex <- factor(EncounterRate$Sex)
EncounterRate$Place <- factor(EncounterRate$Place)
EncounterRate$ShortCode <- factor(EncounterRate$ShortCode)
EncounterRate$fSeason <- factor(EncounterRate$SeasonID)
EncounterRate$fTerritory <- factor(EncounterRate$Territory)
EncounterRate$NTrueAssocs <- as.integer(EncounterRate$NTrueAssocs)

# make combination factor for sex and status # can only make combo fields from factors
EncounterRate$SexStatus <- EncounterRate$Sex:EncounterRate$SocialStatus
EncounterRate$SexStatus <- factor(EncounterRate$SexStatus)

# remove unknowns
EncounterRate_noNAs <- subset(EncounterRate, !is.na(Sex) & !is.na(SocialStatus))



#====================================#
# PLOTTING - to view data structure
#====================================#
library(plyr)
# Calculate mean contact rate per day per patch
DailyPatchEncounterRate <- ddply(EncounterRate, c("Territory", "SeasonID", "Place", "id", "ShortCode", "Sex", "SocialStatus", "DaysSeen"), 
                                 summarise,
                                 SumSightings=sum(Nsightings),
                                 SumTrueAssocs=sum(NTrueAssocs),
                                 Erate = mean(NTrueAssocs))
# Make factors
DailyPatchEncounterRate$fSeason <- factor(DailyPatchEncounterRate$SeasonID)
DailyPatchEncounterRate$fTerritory <- factor(DailyPatchEncounterRate$Territory)
DailyPatchEncounterRate$ShortCode <- factor(DailyPatchEncounterRate$ShortCode)

# Re-add days fed per patch using merge
DailyERate <- merge(x=DailyPatchEncounterRate, y=patchdata[ , c("SeasonID", "Place", "DaysFedPerWeek", "MeanMJperFeedingDay")], by.x=c("SeasonID", "Place"), all.x=T, all.y=F)

# remove unknowns
DailyEncounterRate_noNAs <- subset(DailyPatchEncounterRate, !is.na(Sex) & !is.na(SocialStatus))
# put males first
DailyEncounterRate_noNAs$Sex <- factor(DailyEncounterRate_noNAs$Sex,levels(DailyEncounterRate_noNAs$Sex)[c(2,1)]) 

library(ggplot2)
ggplot(DailyEncounterRate_noNAs, aes(x=fSeason,y=Erate,fill=Sex)) + 
  geom_boxplot()  + facet_grid(SocialStatus~Sex) + 
  scale_y_continuous(limits=(c(0,1.2))) +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_fill_manual(values = c("royalblue3","firebrick1"), guide=FALSE) + #set males to blue and females to red
  xlab(" ") + ylab("Daily contact rate per patch\n") 

# save above in high resolution for publications
dev.print(jpeg, "RPlot_Observed daily patch contact rates BOXPLOT raw data_ylimits.jpeg", res=700, height=9, width=8, units="in") 

# Calc mean rate per day over all individuals - again just for plotting
Erate_means <- ddply(DailyEncounterRate_noNAs, c("fSeason", "Sex", "SocialStatus"), summarise, 
                     n.Foxes=length(ShortCode),
                     mean_Erate=mean(Erate),
                     sd.mean_Erate=sd(Erate),
                     se.mean_Erate=sd.mean_Erate/sqrt(n.Foxes))  

# Plot mean contacts per camera day (i.e. per patch per day)
Erate_means$iSeason <- as.integer(Erate_means$fSeason) # season must be integer to plot properly
Erate_means$Sex <- factor(Erate_means$Sex, levels=c("M", "F")) # reorder Males before females

library(ggplot2)
ggp <- ggplot(Erate_means,aes(x=iSeason,y=mean_Erate,colour=Sex,linetype=SocialStatus)) + 
  geom_point(size=3) + 
  theme_bw(base_size = 18, base_family = "") + facet_wrap(~Sex) + geom_line(size=1)
#Then add labels:
ggp + xlab(" ") + ylab("Daily contact rate per patch\n") +
  scale_colour_manual(values = c("blue", "red")) + #set males to blue and females to red
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 
#had to change scale_x_discrete to scale_x_continous to make x axis the right length!
# This is biased by non-resident visitors in winter that had a low contact rate... 
# Need model predictions:



#===================================#
# MODEL DAILY PATCH ENCOUNTER RATE
#===================================#

# 1. Choose model parameters:
#---------------------------
# Sex*Status*Season
# DaysSeen
# DaysFedPerWeek
# MJ per day
# RANEF: ID + PATCH crossed as foxes (level 1) did not visit all patches (level 2) (ONLY NESTED IF FOX IS MEANINGLESS WITHOUT PATCH)



#------------------------------------------------------------------------------------------------------
# 2. TEST FOR CORRELATIONS when choosing fixed effects: Spearman rank test for non-normal paired data
#------------------------------------------------------------------------------------------------------
# ERate ~ DaysSeen
plot(DailyERate$Erate, DailyERate$DaysSeen) # looks correlated
cor.test(as.integer(DailyERate$DaysSeen), DailyERate$Erate,
         alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # yes, so need in model
# alternative="two.tailed" means null hypothesis is that DaysSeen is correlated (either positively or negatively) with NTrueAssocs.

# Erate ~ DaysFedPerWeek 
plot(DailyERate$Erate~ DailyERate$DaysFedPerWeek) # looks correlated
cor.test(as.integer(DailyERate$DaysFedPerWeek), DailyERate$Erate,
         alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # yes, so need in model

# Erate ~ kJ fed per day
plot(DailyERate$Erate~ DailyERate$MeanMJperFeedingDay) 
cor.test(as.integer(DailyERate$MeanMJperFeedingDay), DailyERate$Erate,
         alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # yes, so need in model

#-------------------------------------------------------------------
# 3. CHECK DISTRIBUTION OF RAW DATA TO DETERMINE MODEL TYPE REQUIRED
#-------------------------------------------------------------------
mean(EncounterRate_noNAs$NTrueAssocs)/var(EncounterRate_noNAs$NTrueAssocs) # check for overdispersion: var should = mean for poisson model. Here it is 0.5 - suggests underdispersed... normal POisson model might be OK
hist(EncounterRate_noNAs$NTrueAssocs)         # right skew (long tail on right side) 
# Right skew indicates overdispersion so need to account for this.
# Options for overdispersed count data include:
# quasipoisson (overdispersed poisson) model
# neg binom model
# Normal poisson with an observation-level random effect

library(fitdistrplus)
require(MASS)
descdist(EncounterRate_noNAs$NTrueAssocs, discrete = T, boot=1000) # negative binomial



#----------------------------
#== 4. Try Poisson with OLR
#----------------------------
library(lme4)

EncounterRate_noNAs$OLR <- rownames(EncounterRate_noNAs) # use row name as observation-level ranef (OLR)

# 17:42-19:25 - FAILED!!! Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev, Downdated VtV is not positive definite
ERate_mod_FULL_norm_nodup <- glmer(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen +
                                     (1|ShortCode/Place) + (1|OLR),
                                   data=EncounterRate_noNAs, family=poisson, verbose=T, # verbose=T shows progress
                                   control=glmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=1000000),
                                                        check.conv.grad=.makeCC("warning",0.05)))
# won't fit
######### MODEL NOW REDUNDANT ANYWAY AS RANDOM EFFECTS WERE NESTED ###########



#-----------------------------------------------------------------------------------------------
#== 5.  CHECK FOR ZERO INFLATION - by comparing GLMs glm() with zero-inflated GLMs zeroinfl()
#----------------------------------------------------------------------------------------------
library(pscl) # for ZI single-level models: can't use ranefs in this package but can compare a fixef-only model with/without ZI to confirm if data are ZI
fm_nb     <- glm.nb(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen, data = EncounterRate_noNAs) # requires MASS package
fm_pois   <- glm(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen, data = EncounterRate_noNAs, family = "poisson")
fm_zinb   <- zeroinfl(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen|1, data = EncounterRate_noNAs, dist = "negbin")
fm_zipois <- zeroinfl(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen|1, data = EncounterRate_noNAs, dist = "poisson")

library(bbmle)
bbmle::ICtab(fm_nb, fm_zinb, fm_pois, fm_zipois, type="AIC") 
# NEG BINOM fits best either with or without ZERO INFLATION (dAIC=1)
# Though neg binom also fit better than poisson...



#------------------------------------------------------------------------
#== 6. Try negbinom without ZI (and with ZI, just in case) in glmmADMB
#------------------------------------------------------------------------
# wouldn't fit
# Tried reducing number of random effects but didn't help (just went on for about 90 minutes and then threw up a load of convergence warnings)
# Random effects are not the problem, it's DaysSeen and the general data structure that glmmADMB doesnt seem to like.. the same model fit in 5 minutes in lme4!

# Might be that sample size is too low for the complexity of the model


#=========================================
#== 7. GLMER MODELS - final ones used 
#=========================================
# Would not fit without an optimiser
# Fit separate models for the 3-way interacton and for effects of food availability. 


### THREE WAY INTERACTION BETWEEN SEX*STATUS*SEASON: DO CONTACT RATES VARY BETWEEN INDIVIDUALS AND SEAS?
#--------------------------------------------------------------------------------------------------------

#== FULL MODEL # ranef structure models patch-specific, fox-specific and patch-by-fox specific effects (see 'useful bits and bobs' R code file)
# 11:48-12:08
erateMOD_full <- glmer(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen +
                         (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                       data=EncounterRate_noNAs, family=poisson(link="log"),
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=100000), 
                                            check.conv.grad=.makeCC("warning",0.05)))
deviance(erateMOD_full)/df.residual(erateMOD_full) # =0.82, so no overdispersion

# Stepwise model simplification to get chisq values for each fixed effect
#-------------------------------------------------------------------------
car::Anova(erateMOD_full)

## No days seen # 12:09-12:14
erateMOD_noDaysSeen <- glmer(NTrueAssocs ~ Sex*SocialStatus*fSeason +
                               (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                             data=EncounterRate_noNAs, family=poisson(link="log"),
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05)))
anova(erateMOD_full, erateMOD_noDaysSeen) # x2(1)=594.05, < 2.2e-16 # NEED DAYS SEEN

# additive # 12:30-12:33
erateMOD_add <- glmer(NTrueAssocs ~ Sex + SocialStatus + fSeason +
                        DaysSeen +
                        (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                      data=EncounterRate_noNAs, family=poisson(link="log"),
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=100000), 
                                           check.conv.grad=.makeCC("warning",0.05)))
anova(erateMOD_full,erateMOD_add) # x2=75.284, 10, 4.189e-12 # NEED INTERACTION

## No sex*status interaction # 12:14-12:19
erateMOD_noSexStatint <- glmer(NTrueAssocs ~ 
                                 SocialStatus*fSeason +
                                 Sex*fSeason + 
                                 DaysSeen +
                                 (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                               data=EncounterRate_noNAs, family=poisson(link="log"),
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05)))
anova(erateMOD_full, erateMOD_noSexStatint) # 50.578,4,2.735e-10 # NEED SEX*STATUS

# No sex*fSeason interaction # 12:24-12:30
erateMOD_noSexSeasint <- glmer(NTrueAssocs ~ 
                                 SocialStatus*Sex + 
                                 SocialStatus*fSeason +
                                 DaysSeen +
                                 (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                               data=EncounterRate_noNAs, family=poisson(link="log"),
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05)))
anova(erateMOD_full, erateMOD_noSexSeasint) # x2=73.784,6,6.828e-14  # NEED SEX*SEASON

# no status*season interaction # 12:36-44
erateMOD_noStatusSeasint <- glmer(NTrueAssocs ~ 
                                    Sex*SocialStatus + 
                                    Sex*fSeason +
                                    DaysSeen +
                                    (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                                  data=EncounterRate_noNAs, family=poisson(link="log"),
                                  control=glmerControl(optimizer="bobyqa",
                                                       optCtrl = list(maxfun=100000), 
                                                       check.conv.grad=.makeCC("warning",0.05)))
anova(erateMOD_full, erateMOD_noStatusSeasint) # X2=51.4, 6, 2.462e-09 ***


#---------------------------------------------------
#== 8. FINAL MODEL
#---------------------------------------------------

erateMOD_FINAL <- erateMOD_full # AS TAKES TOO LONG TO RUN AGAIN JUST TO CHANGE THE NAME.


#--------------------------------------------------------------------------------------------------
#== 9a. MODEL CHECKING: Test for overdispersion - using different tests to get a general consensus
#--------------------------------------------------------------------------------------------------
deviance(erateMOD_FINAL)/df.residual(erateMOD_FINAL) # 0.8279799

# code for overdispersion function from Harrison 2014 PeerJ paper
od.point<-function(modelobject){ 
  x<-sum(resid(modelobject,type="pearson")^2)
  rdf<-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(erateMOD_FINAL) # slight overdispersion = 1.172894 

library("blmeco") 
dispersion_glmer(erateMOD_FINAL) # 0.9148488 (should be between 0.75-1.4)



#---------------------------------------
#== 9b. MODEL CHECKING: plot residuals
#---------------------------------------
# Plot fitted-resid in response scale
plot(fitted(erateMOD_FINAL), resid(erateMOD_FINAL, type="response"), main="Response residuals")
lines(smooth.spline(fitted(erateMOD_FINAL), residuals(erateMOD_FINAL, type="response"))) # residuals look OK

# For a Poisson model, variance increases as mean increases so ordinary residuals should increase in variance as values of the response (fitted) increases
# plot normal resids
plot(fitted(erateMOD_FINAL), resid(erateMOD_FINAL), main="Standard (poisson in this case) residuals")
lines(smooth.spline(fitted(erateMOD_FINAL), residuals(erateMOD_FINAL))) # residuals look OK

# Plot Pearson residuals: standardised resids that should have constant spread (variance) as fitted values increase


# Just using plot(model) also displays the pearson residuals against fitted:


# Plot the Pearson residuals against the independent variable(s) 
# Should have constant spread (variance) as fitted values increase
plot(erateMOD_FINAL, col=EncounterRate_noNAs$fSeason) # colours for season to aid interpretation

# Alternatively plot Pearson residuals against the real data (independent variables)
sresid <- resid(erateMOD_FINAL, type = "pearson")  # get Pearsons
plot(sresid ~ EncounterRate_noNAs$DaysSeen) # variance of resids is similar across the whole range of DaysSeen
plot(sresid ~ EncounterRate_noNAs$MeanMJperDay) # variance decreases as MJ increases... poss just due to fewer values?

# plots contain a confidence band, prediction line, and partial residuals
visreg::visreg(erateMOD_FINAL) 



#----------------------------------------------
#== 10. SAVE MODEL COEFFICIENTS FROM TOP MODEL
#----------------------------------------------
erateMOD_coeffs <- coef(summary(erateMOD_FINAL)) # save as DF

summary(erateMOD_FINAL) # get variance and SD for random effects

#--------------------------------------------------
#== 11. Calculate lsmeans & CIs - backtransformed
#---------------------------------------------------
library(lsmeans) # for effect sizes in units of measurement (predicted contact rates)
# (at mean days seen)
lsm <- data.frame(summary(lsmeans::lsmeans(erateMOD_FINAL, ~ c(Sex,SocialStatus)|fSeason), type="response")) # save lsmeans in a data frame

# lsmeans wont work for continuous predictors (DaysSeen)
# Package 'effects' does
# Same output as lsmeans but SE is not reliable for GLMMs (as is predicted from normal distrib)
library(effects) 
eff <-  Effect(c("DaysSeen", "MeanMJperDay", "Sex", "SocialStatus", "fSeason"), erateMOD_FINAL)
as.data.frame(eff) # SAME AS PREDICTED VALUES BELOW, that were calc from predict() function
# since they're the same, means I can reliably quote the mean and CIs from Effects for just DaysSeen without other predictors:
eff <-  Effect(c("DaysSeen"), erateMOD_FINAL)
effects_pred <- as.data.frame(eff) # SAME AS PREDICTED VALUES BELOW, that were calc from predict() function 
### used these in thesis to quote mean contact rates across all animals and seasons for 10 and 40 days seen.###


#----------------------------
#== 12. POST-HOC comparisons
#----------------------------
library(lsmeans)
# Need to perform multiple pairwise comparisons in ONE test so p-values are adjusted properly:
# First create objects for each set of pairwise comparisons (season*sexstat and sexstat*season) and combine into a table to run all comparisons at once
a <- pairs(lsmeans::lsmeans(erateMOD_FINAL, ~ fSeason|c(Sex,SocialStatus))) # compare rates between seasons for each sexstat
b <- pairs(lsmeans::lsmeans(erateMOD_FINAL, ~ c(Sex,SocialStatus)|fSeason)) # compare rates between sexstat for each season
lsm_tukey <- test(rbind(a,b), type="response", adjust="tukey")
# type="response" to get results on response scale (though tests done on log scale)
# test using tukey adjustment instead of default mvt ('multi-variate-testing' - bit generic...) adjustment - tukey us similar but slightly more conservative than mvt
# save individually as each command tests a diff hypothsesis:
lsm_a <-test(a, type="response", adjust="tukey") # Contact rate varies between seasons (within sex-status classes)
lsm_b <-test(b, type="response", adjust="tukey") # contact rate varies between sex-status classes (within seasons)
lsm_tukeya <- data.frame(lsm_a)
lsm_tukeyb <- data.frame(lsm_b)

#----------------
#== 13. PLOTTING
#----------------

library(lsmeans)
lsm_contrasts <- lsmeans::lsmeans(erateMOD_FINAL, pairwise ~ fSeason|c(Sex*SocialStatus), type="response") # 'rate' means backtransformed response
lsm <- confint(lsm_contrasts$lsmeans, type="response") # save lsmeans and confidence intervals as a sep object
lsm.ci <- data.frame(lsm)
lsm.ci$Sex <- factor(lsm.ci$Sex,levels(lsm.ci$Sex)[c(2,1)]) # reorder sex so males plotted first
lsm.ci$iSeason <-as.numeric(lsm.ci$fSeason) # seasons are better spaced along x-axis if season is numeric rather than a factor

library(ggplot2)
# BAR PLOT - mean contact rates across all patches and all days seen
lsmggp.ci <- ggplot(lsm.ci, aes(x = iSeason, y = rate, fill=factor(Sex))) +
  geom_bar(mapping=aes(iSeason, rate), stat="identity", position=position_dodge(0.9)) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_fill_manual(name="Sex", values=c('royalblue3','firebrick1')) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~SocialStatus)
# plot with CI error bars (as SEs can't be backtransformed)
lsmggp.ci + geom_errorbar(aes(ymax = asymp.UCL, ymin=asymp.LCL), size=1, width=0.2, 
                          color="black", stat="identity", position=position_dodge(0.9)) +
  xlab("") + ylab("Daily patch contact rate\n") 

# save above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM sexstat-season ggplot interaction_barplot.jpeg", res=900, height=6, width=10, units="in") 



# LINE PLOT - mean contact rates across all patches and all days seen  - USED IN THESIS
lsmggp.ci <- ggplot(lsm.ci, aes(x = iSeason, y = rate, colour=factor(Sex))) +
  geom_line(aes(group=Sex),size=1,position=position_dodge(0.2)) + 
  geom_point(size=4,position=position_dodge(0.2)) + 
  facet_wrap(~SocialStatus) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_colour_manual(name="Sex", values=c('royalblue3','firebrick1')) +
  theme_bw(base_size = 14, base_family = "") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=13, color = "black"),
        axis.text.y = element_text(size=13,color = "black"),
        axis.title.y = element_text(size=14),
        legend.title=element_text(size=14),
        strip.text.x = element_text(size = 13, face="bold"),
        strip.text.y = element_text(size = 13, face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) 
# plot with CI error bars (as SEs can't be backtransformed)
lsmggp.ci + geom_errorbar(aes(ymax = asymp.UCL, ymin=asymp.LCL), size=1, width=0.2, 
                          position=position_dodge(0.2)) + xlab("") + ylab("Daily patch contact rate\n") 

# save above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM sexstat-season ggplot interaction_lineplot.jpeg", res=900, height=6, width=10, units="in") 


#--------------------------------------------------------------------------------
#== MAKE PREDICTIONS - for more meaningful plots - standard predict() method
#--------------------------------------------------------------------------------
# Predict contact rate for foxes for DaysSeen=10 (non-residents) and DaysSeen=30 (residents)
# expand.grid makes data frame with all combinations of the listed values: 
# (has to have all model coefficients in, with same names, e.g. 'DaysFedPW_rounded', not shortened to 'DaysFed')
pred.data <- expand.grid(DaysSeen=c(10,20,30,40),
                         Sex=unique(EncounterRate_noNAs$Sex),
                         SocialStatus=unique(EncounterRate_noNAs$SocialStatus),
                         fSeason=unique(EncounterRate_noNAs$fSeason),
                         Place=unique(EncounterRate_noNAs$Place),
                         ShortCode=unique(EncounterRate_noNAs$ShortCode)) # can't also include interaction-random-effects ('Place:ShortCode') as it makes an object too large!

# pred.data is equivalent to the popularly named 'newdata' or 'newdat'

pred <- predict(erateMOD_FINAL, newdata=pred.data, type="response", re.form=~(1|ShortCode)) # re.form=NULL (default) means include all random effects. re.form=NA ignores random effects so predicts at the population level
# Have to include some random effects in pred.data to get a range of values to plot boxplots 
# (otherwise get a single value for each combination of fixed effects so can't draw a box - will
# just get a single horizontal line!)

predframe <- data.frame(pred.data, pred) # save in dframe

predframe$sexstat <- predframe$Sex:predframe$SocialStatus # make combo factor and re-order
levels(predframe$sexstat)
predframe$sexstat <- factor(predframe$sexstat,levels(predframe$sexstat)[c(3,4,1,2)])
predframe$Season <- predframe$fSeason # make season in predframe have same name as in raw data table so can plot predictions and raw values together
# Reorder and label seasons for clearer axes in plots
predframe$Season <- ordered(predframe$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
EncounterRate_noNAs$Season <- ordered(EncounterRate_noNAs$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

#-------------

# BOXPLOTS OF PREDICTED CONTACT RATES for different foxes at an 'average' patch i.e. incorporating diff levels of (1|ShortCode)
# and ignoring the (1|Place) random effect 
ggp <- ggplot(predframe, aes(x=factor(Season), y=pred, fill=factor(sexstat))) 
ggp + geom_boxplot() +  facet_wrap(~DaysSeen) +
  scale_fill_discrete(name="Sex and\nsocial status") + # rename legend
  scale_y_continuous(limits=c(0,1)) +
  ggtitle("") +
  xlab("") + ylab("Daily patch contact rate\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face=NULL, color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"),
        strip.text.x = element_text(size = 12, face="bold"))

# save above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM predicted daily patch contact rates BOXPLOT_with ShortCode ranef.jpeg", res=900, height=9, width=12, units="in") 

# DID NOT USE

#-------------

# Plot from predictions that ignore all random effects - to get a single prediction
# per combo for plotting points with error bars
pred <- predict(erateMOD_FINAL, newdata=pred.data, type="response", re.form=~0)
predframe <- data.frame(pred.data, pred)
predframe$Sex <- factor(predframe$Sex,levels(predframe$Sex)[c(2,1)]) # re-order sex
predframe$sexstat <- predframe$Sex:predframe$SocialStatus 
levels(predframe$sexstat)
predframe$sexstat <- factor(predframe$sexstat,levels(predframe$sexstat)[c(3,4,1,2)])
predframe$Season <- predframe$fSeason
predframe$Season <- ordered(predframe$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

# facetted by days seen
ggp <- ggplot(predframe, aes(x=Season, y=pred, colour=factor(sexstat))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=sexstat), size=1) + 
  geom_point(size=3) + 
  facet_grid(~DaysSeen) +
  scale_color_discrete(name="Sex and\nsocial status") + # rename legend
  xlab("") + ylab("Daily patch contact rate\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, face=NULL, color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        strip.text.x = element_text(size = 12, face="bold"),
        strip.text.y = element_text(size = 12, face="bold"),
        axis.line = element_line(colour = "black"))


# facetted by sex & status
ggp <- ggplot(predframe, aes(x=Season, y=pred, colour=factor(DaysSeen))) 
ggp + geom_line(aes(group=DaysSeen),size=1) + 
  geom_point(size=3) + 
  facet_grid(SocialStatus~Sex) +
  scale_colour_discrete(name="Days seen") + # rename legend
  xlab("") + ylab("Daily patch contact rate\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, face=NULL, color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        strip.text.x = element_text(size = 12, face="bold"),
        strip.text.y = element_text(size = 12, face="bold"),
        axis.line = element_line(colour = "black"))




# NEED TO GET CONFIDENCE INTERVALS OR SEs AND ADD TO PLOTS OF PREDICTIONS:


#----------------------------------------
# PREDICTIONS WITH CONFIDENCE INTERVALS
#----------------------------------------
# Code taken from:
# http://glmm.wikidot.com/faq
# http://www.r-bloggers.com/confidence-intervals-for-prediction-in-glmms/

# To calculate SEs from GLMMs have to backtransform predictions after adding/removing the SE
# So make predictions on the link scale initially:
# Can't use predict() to get CIs, need to make model matrix manually:

#== CALCULATE CIs & PREDICTION INTERVALS USING A PREDICT()-LIKE METHOD using code from: http://glmm.wikidot.com/faq
#--------------------------------------------------------------------------------------------------------------------
# 1. Make data frame to calc predictions from: include levels of all FIXED EFFECTS included in the model (NO RANDOM EFFECTS)
pred.data <- expand.grid(DaysSeen=c(10,20,30,40),
                         Sex=unique(EncounterRate_noNAs$Sex),
                         SocialStatus=unique(EncounterRate_noNAs$SocialStatus),
                         fSeason=unique(EncounterRate_noNAs$fSeason))

# 2. Get model matrix: write fixed effects in same way as model formula. Have to include all variables in pred.data
modelmatrix <-model.matrix(~Sex*SocialStatus*fSeason+DaysSeen, pred.data) 
head(modelmatrix) # check all columns contain data

# 3. Calculate predictions (on link scale, so same values as get via predict(reateMOD_full, newdata=pred.data, re.form=NA, type="link")
y <-modelmatrix%*%fixef(erateMOD_FINAL) 

# 4. Save variance due to fixed effects (i think)
pvar1 <- diag(modelmatrix %*% tcrossprod(vcov(erateMOD_FINAL),modelmatrix))

# 5. Add variance due to random effects
tvar1 <- pvar1 + VarCorr(erateMOD_FINAL)$ShortCode[1] + VarCorr(erateMOD_FINAL)$Place[1] + VarCorr(erateMOD_FINAL)$`Place:ShortCode`[1] # Specifies to include intercepts (variance) from each random effect. [1] specifies to use the intercept value

# 6. Calc confidence intervals, backtransform and combine all in a data frame:
newdata <-data.frame(
  DaysSeen=pred.data$DaysSeen,
  Sex=pred.data$Sex,
  SocialStatus=pred.data$SocialStatus,
  fSeason=pred.data$fSeason,
  y=exp(y), ciLWR = exp(y-1.96*sqrt(pvar1)), ciUPR = exp(y+1.96*sqrt(pvar1)))
head(newdata)
predsWithCIs <- newdata # save as new df incase overwrite

#---------------------------
#====== PLOT preds with CIs
#---------------------------
# Adjust names and object types
predsWithCIs$Sex <- factor(predsWithCIs$Sex,levels(predsWithCIs$Sex)[c(2,1)]) # re-order sex
predsWithCIs$sexstat <- predsWithCIs$Sex:predsWithCIs$SocialStatus 
predsWithCIs$sexstat <- factor(predsWithCIs$sexstat,levels(predsWithCIs$sexstat)[c(3,4,1,2)])
predsWithCIs$iSeason <- predsWithCIs$fSeason
predsWithCIs$Season <- ordered(predsWithCIs$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

# facetted by days seen
ggp <- ggplot(predsWithCIs, aes(x=Season, y=y, group=sexstat, colour=factor(sexstat))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=sexstat), size=1, position=position_dodge(0.2)) + 
  geom_point(size=3, position=position_dodge(0.2)) + 
  facet_wrap(~DaysSeen) +
  scale_colour_discrete(name="Sex and\nsocial status") + # rename legend
  scale_y_continuous(limits=NULL) +
  xlab("") + ylab("Daily patch contact rate\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, face=NULL, color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        strip.text.x = element_text(size = 12, face="bold"),
        strip.text.y = element_text(size = 12, face="bold"),
        axis.line = element_line(colour = "black")) +
  geom_errorbar(data=predsWithCIs, 
                mapping=aes(x=Season, ymin=ciLWR, ymax=ciUPR),
                width=0.1, size=0.6, position=position_dodge(0.2)) 



# facetted by sex & status - USED IN THESIS
ggp <- ggplot(predsWithCIs, aes(x=Season, y=y, colour=factor(DaysSeen))) 
ggp + geom_line(aes(group=DaysSeen),size=1) + 
  geom_point(size=3) + 
  facet_grid(SocialStatus~Sex) +
  scale_colour_discrete(name="Days seen") + # rename legend
  xlab("") + ylab("Daily patch contact rate\n") +
  theme_bw(base_size = 14, base_family = "") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=13, color = "black"),
        axis.text.y = element_text(size=13,color = "black"),
        axis.title.y = element_text(size=14),
        legend.title=element_text(size=14),
        strip.text.x = element_text(size = 13, face="bold"),
        strip.text.y = element_text(size = 13, face="bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_errorbar(data=predsWithCIs, 
                mapping=aes(x=Season, ymin=ciLWR, ymax=ciUPR), 
                width=0.1, size=0.6) 

# save above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM predictions for contact rates facet sexstatus_nogridlines.jpeg", res=900, height=8, width=10, units="in") 

#=================================================================================================================================

#======================================================
### Effect of food availability on contact rates
#======================================================


# FULL MODEL - FIT IN 1 MIN
erateFOODmod_full <- glmer(NTrueAssocs ~ DaysFedPerWeek + MeanMJperFeedingDay +
                             (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                           data=EncounterRate_noNAs, family=poisson(link="log"),
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))
# NO MJ
erateFOODmod_noMJ <- glmer(NTrueAssocs ~ DaysFedPerWeek +
                             (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                           data=EncounterRate_noNAs, family=poisson(link="log"),
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))
anova(erateFOODmod_full, erateFOODmod_noMJ) # x2(1) = 62.912, p=2.162e-15 ***

# NO DF
erateFOODmod_noDF <- glmer(NTrueAssocs ~ MeanMJperFeedingDay +
                             (1|Place) + (1|ShortCode) + (1|Place:ShortCode),
                           data=EncounterRate_noNAs, family=poisson(link="log"),
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))
anova(erateFOODmod_full, erateFOODmod_noDF) # x2(1) =21.857,  p=2.938e-06***

# FULL MODEL IS BEST
a<- coef(summary(erateFOODmod_full))

# MODEL CHECKING
# check for overdispersion
deviance(erateFOODmod_full)/df.residual(erateFOODmod_full) # 0.8681122
# check VIF for multicollinearity - see "RCode_GENERAL_Functions for diagnosing collinearity"
vif.mer(erateFOODmod_full) # both =1.017 so OK (should be <5)

# code for overdispersion function from Harrison 2014 PeerJ paper
od.point<-function(modelobject){ 
  x<-sum(resid(modelobject,type="pearson")^2)
  rdf<-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(erateFOODmod_full) # slight overdispersion = 1.152482 

library("blmeco") 
dispersion_glmer(erateFOODmod_full) # 0.9380761 (should be between 0.75-1.4)

# Plot fitted-resid in response scale
plot(fitted(erateFOODmod_full), resid(erateFOODmod_full, type="response"), main="Response residuals")
lines(smooth.spline(fitted(erateFOODmod_full), residuals(erateFOODmod_full, type="response")), col="red") # residuals look OK

# Assess leverage of full mod (outliers with extreme x values likely to influence model estimates)
library(influence.ME)
inf <- influence(model=erateFOODmod_full, count=TRUE, group="Place")
sigtest(inf, test=-1.96) # no patches are overly influential


# PLOTTING
lsm_preds <- lsmeans::lsmeans(erateFOODmod_full, "MeanMJperFeedingDay", 
                              at = list(MeanMJperFeedingDay = seq(0,6,0.5)),
                              type="response")
lsmpreds <- data.frame(summary(lsm_preds))

ggp <- ggplot(lsmpreds, aes(x=MeanMJperFeedingDay, y=rate)) 
ggp +     geom_point(size=3) + geom_line(size=1) +
  geom_errorbar(aes(ymax = asymp.UCL, ymin=asymp.LCL), size=1, width=0.2, 
                position=position_dodge(0.2)) + xlab("") + ylab("Daily patch contact rate\n")  +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(0,6,1)) +
  xlab("\nMJ per feeding day") + ylab("Daily contact rate per patch\n")

# save above in high resolution for publications
dev.print(jpeg, "RPlot_Poisson GLMM preds & 95CIs nvisitors_MJperfeedingday.jpeg", res=700, height=5, width=6, units="in") 

###

lsm_preds <- lsmeans::lsmeans(erateFOODmod_full, "DaysFedPerWeek", 
                              at = list(DaysFedPerWeek = seq(0,7,1)),
                              type="response")
lsmpreds <- data.frame(summary(lsm_preds))

ggp <- ggplot(lsmpreds, aes(x=DaysFedPerWeek, y=rate)) 
ggp +     geom_point(size=4) + geom_line(size=1) +
  geom_errorbar(aes(ymax = asymp.UCL, ymin=asymp.LCL), size=1, width=0.2, 
                position=position_dodge(0.2)) + xlab("") + ylab("Daily patch contact rate\n")  +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_x_continuous(breaks = seq(0,7,1)) +
  xlab("\nDays fed per week") + ylab("Daily contact rate per patch\n")

# save above in high resolution for publications
dev.print(jpeg, "RPlot_Poisson GLMM preds & 95CIs nvisitors_daysfedpw.jpeg", res=700, height=5, width=6, units="in") 


#=================================================================================================================================
#=================================================================================================================================
#=================================================================================================================================


###########################################
# MEAN CONTACT DURATION (secs) - NOT USED IN THESIS ####
###########################################

# Q: What is the effect of pair type on encounter duration and does this vary with season and food availability?

# NOTES

# GAMMA MODELS ARE GOOD FOR DURATION MODELS WHERE Y CANNOT BE ZERO

# DATA: 2 sets, one with duplicate concatIDs removed, the other same as 'mydata' used for contact rate models above
reduced_duration_data <- duration_data # with duplicate concatIDs removed, so can only model pairtype, not focal ani attributes
reduced_duration_data_sub <- duration_data_sub # with duplicate concatIDs removed, so can only model pairtype, not focal ani attributes

# Random effects = dyad (pair IDs) and place (patch ID) OR dyad + ID + place + ID:place

#== REDUCED DATA:
# 1. Fit model with PairTypeSex*Season and PairTypeStatus*Season, with BeforeMidnight & DaysFedPerWeek -> neither interaction significant
# 2. Fit model without interactions -> neither pair types were significant
# 3. Fit model without pair types -> Season, BeforeMidnight and DaysFedPerWeek were all significant
# Negative binomial model fit best - managed to get it to run in lme4 (glmer.nb) with an optimiser

#== FULL DATA:
# 1. 
# 2. 
# 3. 





###########################################
# MODELS FIT TO ==FULL== DURATION_DATA FILE
###########################################
# Take subset of mydata to work on, that includes only true associations
duration_data <- subset(mydata, Selfencounter==0)

#== Remove unknown sex and status to make models easier to interpret:
# 1. Add attributes (use for subsetting only, as not one row per fox)
attribs <-read.csv(file="G:/Statistics 2016/SNA_synced with uni 180416noon/Trying sna in R 20-21.04.16/Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T, stringsAsFactors=FALSE) # Load attributes data
duration_data$FocalAniShortCode <-attribs[match(duration_data$id, attribs$id),4]
duration_data$FocalAniSex <-attribs[match(duration_data$id, attribs$id),5]
duration_data$FocalAniStatus <-attribs[match(duration_data$id, attribs$id),6]

# 2. save factors
duration_data$FocalAniSex <- factor(duration_data$FocalAniSex)
duration_data$FocalAniStatus <- factor(duration_data$FocalAniStatus)
duration_data$Dyad <- factor(duration_data$ConcatAnimalIDs) 
duration_data$fSeason <- factor(duration_data$SeasonID) 
duration_data$fBeforeMidnight <- factor(duration_data$BeforeMidnight) 

# 3. Mark unknown pair types as 1, leave others blank (in preparation for subsetting)
duration_data$PairTypeUnknown <- ifelse(duration_data$PairTypeSex=="MU", 1, NA)
duration_data$PairTypeUnknown <- ifelse(duration_data$PairTypeSex=="FU", 1, duration_data$PairTypeUnknown)
duration_data$PairTypeUnknown <- ifelse(duration_data$PairTypeStatus=="DU", 1, duration_data$PairTypeUnknown)
duration_data$PairTypeUnknown <- ifelse(duration_data$PairTypeStatus=="SU", 1, duration_data$PairTypeUnknown)

# 4. Take subset: ONLY foxes seen min 20 days & true associations & pairtypes of known sex and status combination
duration_data_sub <- subset(duration_data, Selfencounter==0 & is.na(PairTypeUnknown) & DaysSeen_focalfox>19 & DaysSeen_sharerfox>19)

#== check distrib of data
require(MASS)
require(car)
qqp(duration_data_sub$duration.s, "norm", main="normal") 
qqp(sqrt(duration_data_sub$duration.s), "norm", main="normal") # square-root transformation improves normality but still not normal
hist(duration_data_sub$duration.s, breaks=50) # the short visits create large bin at 5-10 seconds

qqp(duration_data_sub$duration.s, "lnorm", main="lognormal") 

nbinom <- fitdistr(duration_data_sub$duration.s, "Negative Binomial")
qqp(duration_data_sub$duration.s, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]], main="negative binomial")

gamma <- fitdistr(duration_data_sub$duration.s, "gamma", start=NULL)
qqp(duration_data_sub$duration.s, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")

# negbin or gamma fit best but not perfect...
# GAMMA MODELS ARE GOOD FOR DURATION MODELS WHERE Y CANNOT BE ZERO

#=============FIT MODELS including focal animal attributes
library(lme4)

# Gamma GLMM with log-link - took 30 secs
ind_durmod_gamma <- glmer(duration.s ~ FocalAniSex*FocalAniStatus*fSeason + 
                            PairTypeSex*fSeason + 
                            PairTypeStatus*fSeason + 
                            fBeforeMidnight +
                            DaysFedPerWeek + 
                            (1|Dyad) + (1|Place) + (1|FocalAniShortCode) + (1|FocalAniShortCode:Place), 
                          data=duration_data_sub, 
                          family=Gamma(link = "log"), verbose=T, 
                          control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))

plot(fitted(ind_durmod_gamma),resid(ind_durmod_gamma, type="response")) 
lines(smooth.spline(fitted(ind_durmod_gamma), residuals(ind_durmod_gamma, type="response"))) 
# fit but residuals not good
car::Anova(ind_durmod_gamma)
summary(ind_durmod_gamma)
# withut interaction
ind_durmod_gamma_noint <- glmer(duration.s ~ FocalAniSex+FocalAniStatus+fSeason + 
                                  PairTypeSex + 
                                  PairTypeStatus + 
                                  fBeforeMidnight +
                                  DaysFedPerWeek + 
                                  (1|Dyad) + (1|Place), 
                                data=duration_data_sub, 
                                family=Gamma(link = "log"), verbose=T, 
                                control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
plot(fitted(ind_durmod_gamma_noint),resid(ind_durmod_gamma_noint, type="response")) 
lines(smooth.spline(fitted(ind_durmod_gamma_noint), residuals(ind_durmod_gamma_noint, type="response"))) 
car::Anova(ind_durmod_gamma_noint)

#without pair type
ind_durmod_gamma_noPT <- glmer(duration.s ~ FocalAniSex+FocalAniStatus+fSeason + 
                                 fBeforeMidnight +
                                 DaysFedPerWeek + 
                                 (1|Dyad) + (1|Place), 
                               data=duration_data_sub, 
                               family=Gamma(link = "log"), verbose=T, 
                               control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
plot(fitted(ind_durmod_gamma_noint),resid(ind_durmod_gamma_noint, type="response")) 
lines(smooth.spline(fitted(ind_durmod_gamma_noPT), residuals(ind_durmod_gamma_noPT, type="response"))) 
car::Anova(ind_durmod_gamma_noPT)

#without focal ani sex and status <- FINAL MODEL
ind_durmod_gamma_nofocalattr <- glmer(duration.s ~ fSeason + 
                                        fBeforeMidnight +
                                        DaysFedPerWeek + 
                                        (1|Dyad) + (1|Place), 
                                      data=duration_data_sub, 
                                      family=Gamma(link = "log"), verbose=T, 
                                      control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
plot(fitted(ind_durmod_gamma_nofocalattr),resid(ind_durmod_gamma_nofocalattr)) 
lines(smooth.spline(fitted(ind_durmod_gamma_nofocalattr), residuals(ind_durmod_gamma_nofocalattr))) 
car::Anova(ind_durmod_gamma_nofocalattr)



#== Negative Binomial GLMM in lme4  # 21:24-22:49 or before
ind_durmod_glmernb <- glmer.nb(duration.s ~ FocalAniSex*FocalAniStatus*fSeason + 
                                 PairTypeSex + 
                                 PairTypeStatus + 
                                 fBeforeMidnight +
                                 DaysFedPerWeek + 
                                 (1|Dyad) + (1|Place), 
                               data=duration_data_sub, verbose=T,
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=1000000),
                                                    check.conv.grad=.makeCC("warning",0.05)))
# Model is nearly unidentifiable: very large eigenvalue- Rescale variables?
plot(fitted(ind_durmod_glmernb),resid(ind_durmod_glmernb, type="response"))  
lines(smooth.spline(fitted(ind_durmod_glmernb), residuals(ind_durmod_glmernb, type="response"))) 


# Compare all models
bbmle::ICtab(ind_durmod_glmernb, ind_durmod_gamma, type="AIC") 

car::Anova(ind_durmod_glmernb) # 3-way interactions take AGES: best not to use if poss!!


###############################################
# MODELS FIT TO ==REDUCED== DURATION_DATA FILE
###############################################
reduced_duration_data <-read.csv(file="G:/Statistics 2016/ContactCounts/SNA_R_noCubs_AllPatches_noduplicates_forEncounterDuration.csv", header=T, stringsAsFactors=T)

# Convert encounter durations to number of seconds     
library(lubridate)
reduced_duration_data$duration <- hms(reduced_duration_data$EncounterDuration)  # format character to 'hours:minutes:seconds'
reduced_duration_data$duration.s <- hour(reduced_duration_data$duration)*3600 + minute(reduced_duration_data$duration)*60 + second(reduced_duration_data$duration) # extract components of the datetime and convert to N seconds
detach(package:lubridate)

#== Remove unknown sex and status to make models easier to interpret:
# 1. Add attributes (use for subsetting only, as not one row per fox)
attribs <-read.csv(file="G:/Statistics 2016/SNA_synced with uni 180416noon/Trying sna in R 20-21.04.16/Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T, stringsAsFactors=FALSE) # Load attributes data
reduced_duration_data$FocalAniShortCode<-attribs[match(reduced_duration_data$id, attribs$id),4]
reduced_duration_data$FocalAniSex <-attribs[match(reduced_duration_data$id, attribs$id),5]
reduced_duration_data$FocalAniStatus <-attribs[match(reduced_duration_data$id, attribs$id),6]

# 2. save factors
reduced_duration_data$FocalAniSex <- factor(reduced_duration_data$FocalAniSex)
reduced_duration_data$FocalAniStatus <- factor(reduced_duration_data$FocalAniStatus)
reduced_duration_data$Dyad <- factor(reduced_duration_data$ConcatAnimalIDs) 
reduced_duration_data$fSeason <- factor(reduced_duration_data$SeasonID) 
reduced_duration_data$fBeforeMidnight <- factor(reduced_duration_data$BeforeMidnight) 

# 3. Mark unknown pair types as 1, leave others blank (in preparation for subsetting)
reduced_duration_data$PairTypeUnknown <- ifelse(reduced_duration_data$PairTypeSex=="MU", 1, NA)
reduced_duration_data$PairTypeUnknown <- ifelse(reduced_duration_data$PairTypeSex=="FU", 1, reduced_duration_data$PairTypeUnknown)
reduced_duration_data$PairTypeUnknown <- ifelse(reduced_duration_data$PairTypeStatus=="DU", 1, reduced_duration_data$PairTypeUnknown)
reduced_duration_data$PairTypeUnknown <- ifelse(reduced_duration_data$PairTypeStatus=="SU", 1, reduced_duration_data$PairTypeUnknown)

# 4. Take subset: ONLY foxes seen min 20 days & true associations & pairtypes of known sex and status combination
reduced_duration_data_sub <- subset(reduced_duration_data, Selfencounter==0 & is.na(PairTypeUnknown) & DaysSeen_focalfox>19 & DaysSeen_sharerfox>19)

#== check distrib of data
require(MASS)
require(car)
qqp(reduced_duration_data_sub$duration.s, "norm", main="normal") # def not normal
qqp(reduced_duration_data_sub$duration.s, "lnorm", main="lognormal") # def not lognormal

nbinom <- fitdistr(reduced_duration_data_sub$duration.s, "Negative Binomial")
qqp(reduced_duration_data_sub$duration.s, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]], main="negative binomial")

gamma <- fitdistr(reduced_duration_data_sub$duration.s, "gamma", start=NULL)
qqp(reduced_duration_data_sub$duration.s, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")
# negbinom and gamma fit best (equally well)

# GAMMA MODELS ARE GOOD FOR DURATION MODELS WHERE Y CANNOT BE ZERO



#=============FIT MODELS:

#== Try Gamma glmer in lme4 
library(lme4)

#= Gamma with log link function ### This was fit on reduced dataset (removed duplicate concat IDs)
durat_mod_gammaLL <- glmer(duration.s ~ PairTypeSex + PairTypeStatus + 
                             fSeason + fBeforeMidnight + DaysFedPerWeek +
                             (1|Dyad) + (1|Place), data=reduced_duration_data_sub, verbose=T, family=Gamma(link = "log"),
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=1000000),
                                                check.conv.grad=.makeCC("warning",0.05)))
# wouldn't fit with PairTypeSex + PairTypeStatus included
# when fit without the PairTypes the residuals were heteroscedastic
summary(durat_mod_final_glmer_LL)
plot(fitted(durat_mod_final_glmer_LL),resid(durat_mod_final_glmer_LL, type="response")) 
lines(smooth.spline(fitted(durat_mod_final_glmer_LL), residuals(durat_mod_final_glmer_LL, type="response"))) 
# residuals look v.weird


#=== Fit a lognormal GLMM in lme4 
# By log-transforming the response and then fitting a normal LMM using lmer
# (though data did not fit a lognormal distribution, but logging them might help somewhat...)
### This was fit on reduced dataset (removed duplicate concat IDs)
reduced_duration_data_sub$logDur <- log(reduced_duration_data_sub$duration.s)
durat_mod_final_glmer_logdur <- lmer(logDur ~ PairTypeSex + PairTypeStatus + 
                                       fSeason +  fBeforeMidnight +   DaysFedPerWeek +
                                       (1|Dyad) + (1|Place), data=reduced_duration_data_sub, verbose=T)
# this fit without error
plot(fitted(durat_mod_final_glmer_logdur),resid(durat_mod_final_glmer_logdur, type="response")) 
lines(smooth.spline(fitted(durat_mod_final_glmer_logdur), residuals(durat_mod_final_glmer_logdur, type="response"))) 
# resids increase as fitted increases and the 5-sec durations created a weird pattern.


#== Try a glmer.nb  # 19:45-19:54 ### This was fit on reduced dataset (removed duplicate concat IDs)
durat_mod_final_glmernb <- glmer.nb(duration.s ~ PairTypeSex + PairTypeStatus + 
                                      fSeason +  fBeforeMidnight +  DaysFedPerWeek +
                                      (1|Dyad) + (1|Place), 
                                    data=reduced_duration_data_sub, verbose=T,
                                    control=glmerControl(optimizer="bobyqa",
                                                         optCtrl = list(maxfun=1000000),
                                                         check.conv.grad=.makeCC("warning",0.05)))

plot(fitted(durat_mod_final_glmernb),resid(durat_mod_final_glmernb, type="response"))  
lines(smooth.spline(fitted(durat_mod_final_glmernb), residuals(durat_mod_final_glmernb, type="response"))) 


# Compare all models # CAN'T USE AIC TO COMPARE MODELS WITH DIFF RESPONSES, i.e. logged with not-logged
bbmle::ICtab(durat_mod_gammaLL, durat_mod_final_glmernb, durat_mod_NB2, durat_mod_NB1, type="AIC") 



#=== Try negative binomial in glmmADMB

library(glmmADMB) 
# 20.30- ### This was fit on reduced dataset (removed duplicate concat IDs)
durat_mod_NB1  <- glmmadmb(duration.s ~ PairTypeSex + PairTypeStatus + fSeason + fBeforeMidnight + 
                             DaysFedPerWeek + (1|Dyad) + (1|Place), data=reduced_duration_data_sub, 
                           family="nbinom1", zeroInflation=F, debug=F, verbose=T)

# 19:57-20.30 or earlier ### This was fit on reduced dataset (removed duplicate concat IDs)
durat_mod_NB2  <- glmmadmb(duration.s ~ PairTypeSex + PairTypeStatus + fSeason + fBeforeMidnight + 
                             DaysFedPerWeek + (1|Dyad) + (1|Place), data=reduced_duration_data_sub, 
                           family="nbinom", zeroInflation=F, debug=F, verbose=T)

# Model checking
plot(fitted(durat_mod_NB1),resid(durat_mod_NB1, type="response"))
lines(smooth.spline(fitted(durat_mod_NB1), residuals(durat_mod_NB1, type="response"))) # resids increase as fitted increases
plot(fitted(durat_mod_NB2),resid(durat_mod_NB2, type="response"))
lines(smooth.spline(fitted(durat_mod_NB2), residuals(durat_mod_NB2, type="response"))) # resids approx flat apart from 2 outliers at 700
# Compare NB1 and NB2
bbmle::ICtab(durat_mod_NB1, durat_mod_NB2, type="AIC") # nb2 is better fit than nb1
