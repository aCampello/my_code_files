# home
load("~/PhD/Contactcounts/ContactCounts_170616.RData")
# uni
load("//ads.bris.ac.uk/filestore/MyFiles/StudentPG1/jd12006/Documents/Analysis/Contactcounts/ContactCounts_170616.RData")

############ FITTING MODELS TO DATASET WITHOUT DUPLICATES

library(glmmADMB)
# #= 1. MAXIMAL MODEL (zi-NB1)     # <- FINAL MODEL
# 12:23-13:55 -> failed... removed DaysFed (but think problem is Days Seen as in error it has a colon where DaysSeen appears in the model code)
# 13:59-15:11 (put DaysSeen first in model code in hope that it fits parameters in order so error will end fitting sooner...)
# 15:19 without Days seen - failed

# Model based on data without duplicates won't converge when DaysSeen is included
# Instead mark foxes as being resident/nonresident using the 50% survey days threshold
EncounterRate_noNAs$DaysSeen20 <- ifelse(EncounterRate_noNAs$DaysSeen>19, "Y", "N")
testdata <- subset(EncounterRate_noNAs,Territory==1)
# worked for test data - took 10-15min
# all data: 18:30-20:06 -> failed. "Hessian does not appear to be positive definite", "Parameters were estimated, but standard errors were not: the most likely problem is that the curvature at MLE was zero or negative"
crossmod_ziNB1_ND_DaysSeen20  <- glmmadmb(NTrueAssocs ~  DaysSeen20 + Sex*SocialStatus*fSeason + DaysFedPW_rounded +
                                            (1|Place) + (1|ShortCode),  
                                          data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, 
                                          debug=F, verbose=T,   admb.opts=admbControl(shess=F,noinit=F))

# TRY WITHOUT ZERO-INFLATION AND NORMAL DAYS SEEN (WHICH MAKES MORE SENSE + FITTED BETTER THAN DAYSSEEN20 IN GLMER)
# 20:19-
crossmod_ziNB1_ND  <- glmmadmb(NTrueAssocs ~  DaysSeen + Sex*SocialStatus*fSeason + DaysFedPW_rounded +
                                 (1|Place) + (1|ShortCode),  
                               data=EncounterRate_noNAs, family="nbinom1", zeroInflation=F, 
                               debug=F, verbose=T,   admb.opts=admbControl(shess=F,noinit=F))

summary(crossmod_ziNB1_ND_daysseen20)
dropterms <- drop1(model, test="Chisq")
crossmod_ziNB1_ND_daysseen20_testdata # started 

# check ShortCode variance - maybe it's too close to zero to include in model
boxplot(EncounterRate_noNAs$NTrueAssoc~EncounterRate_noNAs$ShortCode)
boxplot(EncounterRate_noNAs$NTrueAssocs~EncounterRate_noNAs$Place) # can use this to show why needed to include Place as random effect
plot(EncounterRate_noNAs$NTrueAssocs,EncounterRate_noNAs$DaysSeen)
hist(EncounterRate_noNAs$NTrueAssocs)
# no days fed
# no days seen
crossmod_ziNB1_ND_noDaysSeen <- update(crossmod_ziNB1_ND, .~. - DaysSeen)
# no three-way int
crossmod_ziNB1_ND_SEXxSTAT <- glmmadmb(NTrueAssocs ~ Sex*SocialStatus + fSeason + DaysFedPerWeek +   
                                         DaysSeen + (1|Place) + (1|ShortCode),  
                                       data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, 
                                       debug=F, verbose=T,   admb.opts=admbControl(shess=F,noinit=F))

bbmle::ICtab(fullnestmod_ziNB1, fullnestmod_ziNB1_ND, type=qAIC)
summary(fullnestmod_ziNB1_ND)

# check fit
plot(fitted(fullnestmod_ziNB1_ND), resid(fullnestmod_ziNB_ND1), cex=1, main="ERmod fullnestmod_ziNB1_ND", ylab="Residuals", xlab="Fitted values") 
lines(lowess(fitted(fullnestmod_ziNB1_ND),resid(fullnestmod_ziNB1_ND)),col="red",lwd=2) # add a lowess smoothing line - should be approx flat
# NICE!!
dev.print(jpeg, "Rplot_Final contact rate model fullnestmod_ziNB1_ND fit.jpeg", res=800, height=6, width=8, units="in") # save as jpeg

#--------
#== 2. MAXIMAL MODEL (NB1) WITHOUT ZERO INFLATION - to test whether I need ZI model          
# 
oneint_nestmod_ziNB1_ND <- glmmadmb(NTrueAssocs ~ Sex*SocialStatus + fSeason + DaysFedPerWeek + DaysSeen +  
                                      (1|ShortCode/Place),  data=EncounterRate_noNAs, family="nbinom1",
                                    zeroInflation=T, debug=F, verbose=T, 
                                    admb.opts=admbControl(shess=F,noinit=F))

plot(fitted(fullnestmod_NB1_ND), resid(fullnestmod_NB1_ND)) # mod without duplicates
plot(fitted(fullnestmod_NB1), resid(fullnestmod_NB1)) # mod with duplicates
summary(fullnestmod_NB1)
summary(fullnestmod_NB1_ND)

bbmle::ICtab(fullnestmod_NB1_ND, fullnestmod_ziNB1_ND, type="qAIC") 
# zi-NB1 fits best still??

#------------------------------ ## FINAL MODEL TYPE = ZERO-INFLATED QUASIPOISSON

#== 3. Testing random effects: 
# no point as need them in and could see from boxplots there was high between-patch variation

#----

#== 4. zi-NB1 MODEL WITHOUT DAYS SEEN            
# ran 10:52-11:53   <- Doesnt fit as well as full model
noDaysSeen_nestmod_ziNB1 <- glmmadmb(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded +    
                                       (1|ShortCode/Place),  data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, debug=T, verbose=T)
# check fit
plot(fitted(noDaysSeen_nestmod_ziNB1), resid(noDaysSeen_nestmod_ziNB1), main="ERmod noDaysSeen_nestmod_ziNB1") 
# Compare fits with/without 
bbmle::ICtab(fullnestmod_ziNB1, noDaysSeen_nestmod_ziNB1, type="AIC") # full mod fits better
anova(fullnestmod_ziNB1, noDaysSeen_nestmod_ziNB1)
#--------

#= 6. MODEL WITHOUT DAYS-FED-PER-WEEK              
# ran 13:01-13:55  <- Doesnt fit as well as full model
noDaysFed_nestmod_ziNB1 <- glmmadmb(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysSeen +           
                                      (1|ShortCode/Place),  data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, debug=T, verbose=T)

# check fit
plot(fitted(noDaysFed_nestmod_ziNB1), resid(noDaysFed_nestmod_ziNB1), main="ERmod_zinb1_nest_noDaysFed") 
# Compare fits with/without 
bbmle::ICtab(fullnestmod_ziNB1, noDaysFed_nestmod_ziNB1, type="qAIC") # need days fed in
anova(fullnestmod_ziNB1, noDaysFed_nestmod_ziNB1)

#--------

#= 7. MODELs WITH TWO-WAY INTERACTIONS (rather than 3-way) 

# 3x two-way interactions: 14:39-15:33 <- Doesnt fit as well as full model
three_twowayints_nestmod_ziNB1 <- glmmadmb(NTrueAssocs ~ Sex*fSeason + SocialStatus*fSeason + Sex*SocialStatus + DaysFedPW_rounded +    
                                             DaysSeen + (1|ShortCode/Place),  data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, debug=T, verbose=T)

# 2x two-way interactions: 14:00-14:40 <- Doesnt fit as well as full model
twowayint_nestmod_ziNB1  <- glmmadmb(NTrueAssocs ~ Sex*fSeason + SocialStatus*fSeason + DaysFedPW_rounded + DaysSeen + 
                                       (1|ShortCode/Place),  data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, debug=T, verbose=T)

# check fit
plot(fitted(twowayint_nestmod_ziNB1), resid(twowayint_nestmod_ziNB1), main="ERmod twowayint_nestmod_ziNB1") 
# Compare fits with/without 
bbmle::ICtab(fullnestmod_ziNB1, twowayint_nestmod_ziNB1, type="AIC") # DONT THINK I NEED QAIC?
anova(fullnestmod_ziNB1, twowayint_nestmod_ziNB1)

#--------

#= 8. MODEL WITH NO INTERACTION/TERMS
# 15:59-16:27
noint_nestmod_ziNB1  <- glmmadmb(NTrueAssocs ~ DaysFedPW_rounded + DaysSeen + 
                                   (1|ShortCode/Place),  data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, debug=F, verbose=T)

#--------

#= 9. NULL MODEL WITH ONLY RESPONSE AND RANEFs 
# 15:34-15:59 <-  Why did I run this?!
nullnestmod_ziNB1 <- glmmadmb(NTrueAssocs ~ 1 + (1|ShortCode/Place), data=EncounterRate_noNAs, family="nbinom1", zeroInflation=T, debug=T, verbose=T)

