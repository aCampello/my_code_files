

#=======================================================================
## THIS IS OLD, before changed to daily contact rate model ~ 13.6.16
#=======================================================================

# Examine the relationship between sighting frequency and gregariousness: are non-residents more often observed alone?

setwd("G:/Statistics 2016/SNA_synced with uni 060716/SNA in R") # home
attribs2 <-read.csv(file="Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T, stringsAsFactors=FALSE)


# Make factors:
attribs2$fTerritory <- as.factor(attribs2$Territory)
attribs2$fSeason <- as.factor(attribs2$SeasonID)
str(attribs2)


# Combine before and after midnight to make one row per fox per season-territory combination:
library(plyr)
summary_attribs <-     ddply(attribs2, c("id", "ShortCode", "fSeason", "fTerritory"), 
                             summarise,
                             DaysSeen=max(DaysSeen),
                             NTrueAssocs=max(NTrueAssocs))
detach("package:plyr")



#====================================================================================================#
# CAN SKIP THIS SECTION AS USED glmmADMB model below in the end. This is just to show working.
#====================================================================================================#
# GENERALIZED linear mixed model of relationship between days seen and true associations (as these are counts)
#     multilevel to control for repeated measures
library(lme4)
library(lmerTest) # to get p-values from lmer models
# fitting a poisson glmer with an individual-level random effect makes the model a lognormal poisson model and should/may account for overdispersion (though I suspect they mean a sample-level or measurement-level random effect, not individual-level)
lm0 <- glmer(NTrueAssocs ~ 1 + (1|ShortCode) + (1|fTerritory) + (1|fSeason), data=summary_attribs, family=poisson(link = "log"), control = glmerControl(optimizer = "bobyqa")) 
lm1 <- glmer(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory) + (1|fSeason), data=summary_attribs, family=poisson(link = "log"), control = glmerControl(optimizer = "bobyqa")) 
lm2 <- glmer(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory), data=summary_attribs, family=poisson(link = "log"), control = glmerControl(optimizer = "bobyqa")) 
lm3 <- glmer(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fSeason), data=summary_attribs, family=poisson(link = "log"), control = glmerControl(optimizer = "bobyqa")) 
lm4 <- glmer(NTrueAssocs ~ DaysSeen + (1|fTerritory) + (1|fSeason), data=summary_attribs, family=poisson(link = "log"), control = glmerControl(optimizer = "bobyqa")) 

anova(lm0, lm1) # effect of days seen as fixed eff
anova(lm1, lm2) # effect of season as random eff
anova(lm1, lm3) # effect of territory as random eff
anova(lm1, lm4) # effect of ID as random eff
summary(lm1)

# Model checking - check distrib of residuals visually:
library(fitdistrplus)
plot(fitdist(resid(lm1), "norm")) # not normal
# test for overdispersion:
deviance(lm1)/df.residual(lm1) # 4.1>1 so overdispersed and need to use a negative binomial model
hist(summary_attribs$NTrueAssocs) # distrib is def negative binomial



#====================================================================================================#
# MODELS USED IN THESIS - TESTING FITS
#====================================================================================================#

library(glmmADMB) # for negative binomial GLMMs (lme4 doesnt allow for zero inflation)


### POISSON MODELS with/without the parameter 'zero-inflation=TRUE' - SLOW!!!
#-------------------------------------------------------------------------------
glmmADMB_mod.zi <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory) + (1|fSeason), data=summary_attribs, 
                            zeroInflation=TRUE, family="poisson")

glmmADMB_mod.zi.nest <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                                 zeroInflation=TRUE, family="poisson")

anova(glmmADMB_mod.zi,glmmADMB_mod.zi.nest) # nested model fits better
anova(glmmADMB_mod.zi.nest, glmmADMB_mod.zi)
summary(glmmADMB_mod.zi)
summary(glmmADMB_mod.zi.nest) # Nesting improves model fit and increases significance of effects


# Nested model with no zero inflation - THIS MODEL CAN'T BE FITTED FOR SOME REASON
glmmADMB_mod.nzi <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                             zeroInflation=FALSE, family="poisson")
anova(glmmADMB_mod.zi.nest, glmmADMB_mod.nzi)



### NEG BINOM MODELS with/without the parameter 'zero-inflation=TRUE'
#-------------------------------------------------------------------------------
# neg binom 1: parameterisation corresponds to a quasipoisson (linear)
glmmADMB_mod.nb1.ZI <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                                zeroInflation=TRUE, family="nbinom1")

glmmADMB_mod.nb1.ZI.notnested <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory) + (1|fSeason), data=summary_attribs, 
                                          zeroInflation=TRUE, family="nbinom1")


glmmADMB_mod.nb1 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                             zeroInflation=FALSE, family="nbinom1")

glmmADMB_mod.nb1.notnested <- glmmadmb(NTrueAssocs ~ DaysSeen +  (1|ShortCode) + (1|fTerritory) + (1|fSeason), data=summary_attribs, 
                                       zeroInflation=FALSE, family="nbinom1")

# neg binom 2: the standard neg binom (semi-quadratic so non-linear (curves up), aka lognormal poisson)
glmmADMB_mod.nb2.ZI <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                                zeroInflation=TRUE, family="nbinom")

glmmADMB_mod.nb2.ZI.notnested <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory)  + (1|fSeason), data=summary_attribs, 
                                          zeroInflation=TRUE, family="nbinom")


glmmADMB_mod.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                             zeroInflation=FALSE, family="nbinom")

glmmADMB_mod.nb2.notnested <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory)  + (1|fSeason), data=summary_attribs, 
                                       zeroInflation=FALSE, family="nbinom")


# Compare model fits to see if need to include zero-inflation=TRUE:
bbmle::AICctab(glmmADMB_mod.zi.nest, glmmADMB_mod.zi, glmmADMB_mod.nb1.ZI, glmmADMB_mod.nb1.ZI.notnested, glmmADMB_mod.nb1, glmmADMB_mod.nb1.notnested, glmmADMB_mod.nb2.ZI,glmmADMB_mod.nb2.ZI.notnested, glmmADMB_mod.nb2, glmmADMB_mod.nb2.notnested, logLik=TRUE)
MuMIn::AICc(glmmADMB_mod.zi.nest, glmmADMB_mod.zi, glmmADMB_mod.nb1.ZI, glmmADMB_mod.nb1.ZI.notnested, glmmADMB_mod.nb1, glmmADMB_mod.nb1.notnested, glmmADMB_mod.nb2.ZI,glmmADMB_mod.nb2.ZI.notnested, glmmADMB_mod.nb2, glmmADMB_mod.nb2.notnested)

# nb2 without zero inflation is best fitting.
# Nested ranef model only 0.05 AICc better fitting but makes more sense so will use nested random effects.

#================#
# Model checking
#================#
par(mfrow=c(2,1))
hist(resid(glmmADMB_mod.nb2))
hist(resid(glmmADMB_mod.nb2.notnested))

pred.nb2 <- (fitted(glmmADMB_mod.nb2)) # calculate fitted values
pred.nb2.ZI <- (fitted(glmmADMB_mod.nb2.ZI)) # calculate fitted values

plot(resid(glmmADMB_mod.nb2) ~ pred.nb2) # plot residuals vs. fitted values hoping for no pattern
plot(resid(glmmADMB_mod.nb2.ZI) ~ pred.nb2.ZI) 

plot(summary_attribs$NTrueAssocs ~ pred.nb2) # plot original dependent data vs. fitted values (hoping for straight line)
plot(summary_attribs$NTrueAssocs ~ pred.nb2.ZI)

qqnorm(resid(glmmADMB_mod.nb2.notnested))
qqline(resid(glmmADMB_mod.nb2.notnested))

qqnorm(resid(glmmADMB_mod.nb2))
qqline(resid(glmmADMB_mod.nb2))

qqnorm(resid(glmmADMB_mod.nb2.ZI))
qqline(resid(glmmADMB_mod.nb2.ZI))

# Test for overdispersion:
library(nlme) 
overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(MODEL) # calculates RATIO between deviance and resid df. 
# If <1 means underdispersed, if >1 means overdispersed and need neg binomial model.

overdisp_fun(glmmADMB_mod.nb2) # ratio (= 0.75) is much closer to zero than poisson model, so data are less overdispersed in this model
overdisp_fun(glmmADMB_mod.nb2.notnested)
overdisp_fun(glmmADMB_mod.zi.nest) # poisson model, ratio = 9.58


#==========================================================#
# Work out which random effects to include in final model:
#==========================================================#
FULL.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory) + (1|fSeason), data=summary_attribs, 
                     zeroInflation=FALSE, family="nbinom")

NoSeason.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory), data=summary_attribs, 
                         zeroInflation=FALSE, family="nbinom")

NoTerritory.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fSeason), data=summary_attribs, 
                            zeroInflation=FALSE, family="nbinom")

NoID.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|fTerritory) + (1|fSeason), data=summary_attribs, 
                     zeroInflation=FALSE, family="nbinom")

NoNesting.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode) + (1|fTerritory) + (1|fSeason), data=summary_attribs, 
                          zeroInflation=FALSE, family="nbinom") 

JustID.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode), data=summary_attribs, 
                        zeroInflation=FALSE, family="nbinom") 

anova(NoSeason.nb2, FULL.nb2) # effect of season as random eff - not significant
anova(NoTerritory.nb2, FULL.nb2) # effect of territory as random eff - not significant
anova(NoID.nb2, FULL.nb2) # effect of ID as random eff - significant
anova(FULL.nb2, NoNesting.nb2) # No difference - just use ID
anova(NoSeason.nb2, JustID.nb2) # compare territory+ID with just ID - not sig.


#====================================================================================================#
# MODELS USED IN THESIS  - OLD, before changed to daily contact rate model ~ 13.6.16
#====================================================================================================#

# Final reduced and null models
NULL.nb2 <- glmmadmb(NTrueAssocs ~ 1 + (1|ShortCode), data=summary_attribs, 
                     zeroInflation=FALSE, family="nbinom")

REDUCED.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode), data=summary_attribs, 
                        zeroInflation=FALSE, family="nbinom") # <- this is the final model

REDUCED_nested.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen + (1|ShortCode/fTerritory), data=summary_attribs, 
                        zeroInflation=FALSE, family="nbinom") 

MuMIn::AICc(REDUCED.nb2, NULL.nb2) 
anova(NULL.nb2, REDUCED.nb2) # effect of days seen as fixed eff: significant


REDUCED.NoID.nb2 <- glmmadmb(NTrueAssocs ~ DaysSeen, data=summary_attribs, 
                             zeroInflation=FALSE, family="nbinom")
anova(REDUCED.NoID.nb2, REDUCED.nb2) # effect of excluding random effect of ID


# Model checking
overdisp_fun(REDUCED.nb2) # 0.75 so not too overdispersed.
plot(fitted(REDUCED.nb2),resid(REDUCED.nb2)) # (on log scale)
plot(fitted(REDUCED.nb2), residuals(REDUCED.nb2, type="response")) # plot residuals on original scale and not log (think the fitted values are already on the response scale)
abline(h=0,lty=2,col="red")
scatter.smooth(fitted(REDUCED.nb2),resid(REDUCED.nb2, type="response")) # slight curve

hist(resid(REDUCED.nb2, type="response")) #looks normally distributed now plotting on original scale
qqnorm(resid(REDUCED.nb2)) # still not perfect but best I can do...

ranef(REDUCED.nb2)
ranef(REDUCED_nested.nb2)

################################ !GOOD:
par(mfrow=c(2,1))

# TO MAKE PREDICTIONS FROM MODEL AND PLOT LINE OVER ORIGINAL DATA:

# create object ('newdata') containing values of DaysSeen to predict N true assocs for from model
pdat<- expand.grid(DaysSeen = seq(0,40,1)) # 0,40,1 = make sequence from 0-40 at intervals of 1

# make predictions for each level of DaysSeen specified in object 'pdat'
pred <- predict(REDUCED.nb2, newdata=pdat, na.rm=T, type="response", interval = "confidence", level = 0.95) 
# SEs (se.fit=TRUE) can only be calculated on the link scale (i.e. type="link") so must use confidence intervals instead (default level=0.95)
#***NB confidence intervals do not incorporate uncertainty due to (and of) random effects

# save predictions for each level of DaysSeen in a data frame
predframe <- data.frame(pdat, pred) # table that contains 'fit' (=predicted N true assocs) and lwr/upr 95% CIs

# PLOT PREDICTIONS OVER ORIGINAL DATA:
plot(summary_attribs$DaysSeen, summary_attribs$NTrueAssocs, cex.lab=1.2, xlab=" ", ylab="Observed true associations")
lines(predframe$fit~predframe$DaysSeen, col="red", lwd=2, lty=1)
lines(predframe$lwr~predframe$DaysSeen, col="black", lwd=1, lty=2)
lines(predframe$upr~predframe$DaysSeen, col="black", lwd=1, lty=2)

# PLOT JUST THE PREDICTED VALUES WITHOUT OBSERVED DATA: FROM glmmADMB.predict helpfile by Ben Bolker
# Code doesn't have 'pdat' so predictions are made for all data points (i.e. 415 observations)
allpred <- predict(REDUCED.nb2,interval="confidence",type="response") 
head(allpred) # 415 rows, so one per observation
allpredframe <- data.frame(obs=summary_attribs$DaysSeen,allpred) # names DaysSeen as "observation number"
with(allpredframe,plot(obs,fit, cex.lab=1.2, xlab="Number of days observed", ylab="Predicted true associations")) # Days seen on X and predicted N true assocs on Y
with(allpredframe,arrows(obs,lwr,obs,upr,length=0)) # add error bars from 95% CIs
lines(predframe$fit~predframe$DaysSeen, col="red", lwd=2, lty=1)

dev.print(jpeg, "Rplot_Two plots_True assocs against days seen with glmmADMB prediction line and 0.95 CIs.jpeg", res=1000, height=12, width=9, units="in") # save as jpeg
