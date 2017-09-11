#=====================================================#
# Jo's R code : Group size defined by SO, SOT and STO
#=====================================================#

#== Useful website for poisson models: http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm

# For poisson with underdispersed data you could use a GLMM with a Poisson response 
# and adjust the standard errors yourself. (Ben Bolker Q&A 2016 http://bit.ly/1V2roMd)

# Poisson overdispersed will do better with negbinom model but poisson underdispersed
# can be fitted as a poisson model - coefficients from poisson and quasipoisson
# are the same; only the std.errors differ

#==============

setwd("E:/Statistics 2016/Group sizes and transients")

library(glmmADMB) 
library(lme4)
library(optimx) # for fitting glmer models with different optimisers
library(plyr)
library(bbmle)
library(MuMIn) # to calc AICc
library(aods3) # for goodness-of-fit test (can't use for glmmADMB)

# load data for SO, SOT and STO all in same data frame, with total residents, non residents and sexes
all3defs<-read.csv(file = "Groupsize_all3definitions.csv", header = TRUE)

# convert season and territory to factors
all3defs$T<-factor(all3defs$Territory) 
all3defs$season<-factor(all3defs$SeasonID) 


#===========================================#
# Procedure for fitting models: 
#===========================================#

# 1. Choose random and fixed effects: Random effects should have >5-6 levels and fixed should each have >10-20 samples

# 2. Choose error distribution and link function, e.g. poisson with log link for count data

# 3. Transform data according to chosen error distribution and check it graphically to ensure response
# variances are homogeneous (equal) across categories (or linear for continuous predictors.
# And that the distributions within groups match the assumed distribution.

# 4. Fit fixed-effect GLMs both to the full (pooled) data set and within each level of the random 
# factors. Estimated parameters should be approximately normally distributed across groups: 
# look for a median of zero at top of model summary under 'deviance residuals' & plot hist(model$resid)
#  Adjust model as necessary (e.g. change link function or add covariates)

# 5. Fit the full GLMM, but simplify if it won't converge.

# 6. Recheck assumptions for the final model (as in step 3) and check that parameter estimates and 
# confidence intervals are reasonable (gigantic confidence intervals could indicate fitting problems)
# Assess overdispersion using residual plots
# Do goodness of fit test chisq test between resid deviance and resid d.f. (in model output)


#==============================================================================#
# 1. Choose fixed and random effects: plot the data to see interactions/trends
#==============================================================================#

# plot responses vs treatments with diferent colours for territories:
lattice::stripplot(log(Group.size+1) ~ Definition|season, data = all3defs, groups = T)

# ggplot version is clearer
ggplot(all3defs,aes(x=season,y=log(Group.size+1),colour=Definition))+
  geom_point()+
  ## need to use as.numeric(amd) to get lines between points
  stat_summary(aes(x=as.numeric(season)),fun.y=mean,geom="line")+
  theme_bw()+
  facet_wrap(~T)

# consistent effect of Definition (SO is higher) but differences between territories 
# and seasons: this confirms we need territory and season in the model


# For count data, first fit a 'standard' Poisson regression (#1) and do a dispersion test (#2). 
# If the Pearson dispersion statistic (Pearson Chi2/(residual dof) is greater than 1, the 
# model is likely Poisson overdispersed. If under 1, then it may be underdispersed. If 
# underdispersed, a negative binomial model will not converge.

#===========================================#
# 2. Distributions: plotting data to check
#===========================================#

# A core property of the Poisson distribution is that the variance = the mean.
# If the variance is greater than the mean the data are overdispersed.
var(all3defs$Group.size) # calculate variance of response variable
mean(all3defs$Group.size) # calc the mean
hist(all3defs$Group.size) # plot histogram
plot(table(all3defs$Group.size), xlab = "observed count values", ylab = "frequency") # similar to histogram but doesn't bin the data

# Also check the group means - take into account the fixed effect/s of interest
# 1. Create variable for interaction (combo) between season and def. ordered by mean
all3defs <- within(all3defs,
                   {
                     seasdef.int <- interaction(season, Definition)
                     seasdef.int <- reorder(seasdef.int, Group.size, mean)
                   })

# 2. Use ddply to find mean and var for each combo of season and definition (group means and vars):
a<-ddply(all3defs, "seasdef.int",
         summarise,
         mean=mean(Group.size),var=var(Group.size))
# if variances are greater than the mean this indicates overdispersion so Poisson not suitable
# and must use quasipoisson or negative binomial
a # in some groups var>mean and in others mean>var...

#===========================================#
# 3. PLOT THE MEANS AND VARIANCES AGAINST DIFFERENT DISTRIBUTIONS TO VISUALISE SHAPE OF DATA:
#===========================================#

# We can get approximate estimates of the quasi-Poisson (linear) pattern using lm, as follows: 
# (The -1 in the formulas below specifies a model with the intercept set to zero.
plot(a$var~a$mean, xlab="group means", ylab="group variances" ) # plot
abline(c(0,1), lty=2) # fit a poisson line
text(105,500,"Poisson") # annotate at particular xy coords (doesnt seem to work here)

lm1 <- lm(a$var~a$mean-1) ## estimate the quasipoisson pattern
phi.fit <- coef(lm1)
curve(phi.fit*x, col=2,add=TRUE) # add line for quasipoisson or negbinom type 1 (NB1) [red]  (LOOKS BEST)
text(110,3900, bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)), col=2) # annotate: bquote() is used to substitute numeric values in equations with symbols

lm2 <- lm(a$var~I(a$mean^2)+offset(a$mean)-1) # estimate the neg binom pattern using an offset
k.fit <- 1/coef(lm2)
curve(x*(1+x/k.fit),col=4,add=TRUE) # Add line for neg binomial or lognormal-Poisson [blue]
text(104,7200,paste("NB: k=",round(k.fit,1),sep=""),col=4) # annotate

Lfit <- loess(a$var~a$mean) 
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5) # Add line for loess fit [cyan]
text(118,2000,"loess",col=5)

library(ggplot2)
ggplot(a,aes(x=mean,y=var))+geom_point()+
  geom_smooth(method='lm', formula=y~x,colour="green",fill="green") + # poisson curve
  geom_smooth(colour="cyan",fill="cyan")+                             # loess smoothing line [cyan]
  geom_smooth(method="lm",formula=y~x-1,colour="red",fill="red")+     # linear: quasipoisson or NB1 line [red]
  geom_smooth(method="lm",formula=y~I(x^2)+offset(x)-1,               # semi-quadratic: neg binomial or lognormal-Poisson [blue]
              colour="blue",fill="blue")

# poisson seems to fit best
# The group variances increase with the mean more rapidly than expected under the Poisson distribution
# suggests need to account for overdispersion in model

#===========================================#
# 4. Fitting GLM models
#===========================================#

# 4a. Fit a Poisson GLM

glm1 <- glm(Group.size ~ Definition*season, family="poisson", data=all3defs, trace=TRUE)
qqnorm(resid(glm1))
gof(glm1) # goodness of fit test (use row with PEARSON x2): low P-value indicates poor fit (prob due to excluded random eff. of territory)

# 4b. Dispersion test: Pearson Chi2/residual df. If >1 = overdispersed. <1 = underdispersed (for Poisson model resid deviance should be equal to resid df.)
deviance(glm1)/df.residual(glm1) # 1.55 = overdispersed
library(AER) # alternative method to test for overdispersion # only for poisson GLMs
dispersiontest(glm1)

#===========================================#
# 5. Fitting GLMM models
#===========================================#

# 5a. Fit a Poisson GLMM in lme4
defcomp.mod1<-glmer(Group.size~Definition*season + (1|T), data=all3defs, family=poisson(link = "log"),
                    control=glmerControl(optimizer="bobyqa",
                                         check.conv.grad=.makeCC("warning",0.05))) # optimiser makes it faster (but same estimates)

defcomp.mod2<-glmer(Group.size~Definition*season + (1|season/T), data=all3defs, family=poisson(link = "log"),
                    control=glmerControl(optimizer="bobyqa",
                                         check.conv.grad=.makeCC("warning",0.05)))

gof(defcomp.mod1) # goodness of fit test: high p-value indicates good fit
AICtab(defcomp.mod1, defcomp.mod2)
MuMIn::AICc(defcomp.mod1, defcomp.mod2)
AIC(defcomp.mod1)

# 5b. Dispersion test:
deviance(defcomp.mod1)/df.residual(defcomp.mod1) # = 0.528. Not close to 1. Underdispersed.
deviance(defcomp.mod2)/df.residual(defcomp.mod2) # = 0.435. Nested model is actually more underdispersed...

qqnorm(resid(defcomp.mod1))
qqline(resid(defcomp.mod1)) # not good

# 5c.  Fit a Poisson GLMM in glmmADMB (the basic glmmadmb fit)
fit_poiss <- glmmadmb(Group.size~Definition*season + (1|T), 
                      data=all3defs, 
                      zeroInflation=FALSE, 
                      family="poisson")

fit_poiss1.nest <- glmmadmb(Group.size~Definition*season + (1|T/season), 
                            data=all3defs, 
                            zeroInflation=FALSE, 
                            family="poisson") # see if nested random eff. improves fit

fit_poiss2.cross <- glmmadmb(Group.size~Definition*season + (1|T) + (1|season), 
                             data=all3defs, 
                             zeroInflation=FALSE, 
                             family="poisson") # see if crossed random eff. improves fit

AICtab(fit_poiss2.cross, fit_poiss1.nest, fit_poiss) 
# makes no difference having season as a random effect so remove


# 5d. Fit neg binom models: can use either lme4 or glmmadmb:

#========== Trying glmer.nb in lme4...

fit_glmernb<-glmer.nb(Group.size ~ Definition*season + (1|T), data=all3defs) # won't fit: prob because data not overdispersed!

fit_glmernb.bq<-glmer.nb(Group.size ~ Definition*season + (1|T), data=all3defs, 
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), # increased iteration limit to 100,000 but still having errors...
                                              check.conv.grad=.makeCC("warning",0.05))) # iteration limit reached

fit_glmernb.nm<-glmer.nb(Group.size ~ Definition*season + (1|T), data=all3defs, 
                         glmerControl(optimizer = "Nelder_Mead",
                                      check.conv.grad=.makeCC("warning",0.05)))  # iteration limit reached

AICtab(fit_glmernb, fit_glmernb.bq, fit_glmernb.nm) 

library(optimx)
fit_glmernb.nlminb<-glmer.nb(Group.size ~ Definition*season + (1|T), data=all3defs, 
                             control=glmerControl(optimizer="optimx", 
                                                  optCtrl=list(method="nlminb"))) # 27 warnings

fit_glmernb.LBFGSB <- glmer.nb(Group.size ~ Definition*season + (1|T), data=all3defs,
                               control=glmerControl(optimizer="optimx",
                                                    optCtrl=list(method="L-BFGS-B"))) # 6 warnings

AICtab(fit_glmernb.bq, fit_glmernb.nm, fit_glmernb.nlminb, fit_glmernb.LBFGSB) 



#======= Trying glmmadmb in glmmADMB (current version can only deal with 1 random effect)

# 5e. Fit a standard negative binomial = a 'NB2' (semi-quadratic so non-linear (curves up), aka lognormal poisson)
fit_nb <- glmmadmb(Group.size~Definition*season + (1|T), 
                   data=all3defs, 
                   zeroInflation=FALSE, 
                   family="nbinom")


# 5f. Fit a "NB1" - parameterisation corresponds to a quasipoisson (linear)
fit_nb1 <- glmmadmb(Group.size~Definition*season + (1|T), 
                    data=all3defs, 
                    zeroInflation=FALSE, 
                    family="nbinom1")

# Compare the 3 glmmADMB models:
AICtab(fit_poiss, fit_nb, fit_nb1, logLik=TRUE)  # poisson fits best: higher loglik and lower AIC: NB mods fit worse
# poss because data are underdispersed: see link http://ms.mcmaster.ca/~bolker/R/misc/foxchapter/bolker_chap.html

# Compare the glmer and glmmADMB poisson models
bbmle::AICtab(fit_poiss, defcomp.mod1)
AICc(fit_poiss, defcomp.mod1) # = glmer one is slightly lower...

#====================================================================================#
  AICc(defcomp.mod1,  fit_poiss, fit_nb, fit_nb1, k = 2, REML = FALSE) # needs MuMIn
#====================================================================================#


  
#=========== glmmPQL method: Fit GLMM with multivariate normal random effects, using 
# Penalized Quasi-Likelihood (PQL) in package nlme. 
# Can't have crossed random effects...

mp1Q <- glmmPQL(Group.size ~ season*Definition,
                random= ~ 1|T, 
                family="quasipoisson", data=all3defs)

mp1P <- glmmPQL(Group.size ~ season*Definition,
                random= ~ 1|T, 
                family="poisson", data=all3defs)

# nested ranefs (season/T = territory nested in season)
mp1Qn <- glmmPQL(Group.size ~ season*Definition,
                 random= ~ 1|season/T, 
                 family="quasipoisson", data=all3defs)

mp1Pn <- glmmPQL(Group.size ~ season*Definition,
                 random= ~ 1|season/T, 
                 family="poisson", data=all3defs)

summary(mp1P, type="response") # view coefficients in units of x

# Get predicted values from PQL GLMMs: specify the level (defaults = zero)
# Level zero = population predictions (mean per fixed effect averaged across all levels of the random effect)
# Level one = sample predictions
predict(mp1P,type="response", level=0:1) # multiple levels:  output is a data frame
predict(mp1P,type="response", level=1) # Level one only: output is a vector (list)

# PQL models not fitted with likelihood methods so don't have a LL or AIC
# not sure how to compare these models so maybe stick to glmer and glmmadmb.########


#====================#
# 6. Model checking
#====================#
plot(model) 

# The usual output from a Poisson log-link model has datapoints arranged in 'lines' (owing to 
# discrete counts) that are curved (due to the log transformation within the model)
# http://stats.stackexchange.com/questions/114045/validating-residual-plot-count-data-different-levels


library(coefplot2)
# Plot and compare all models:
coefplot2(list(glmer.P=defcomp.mod1,
               glmmPQL_P=mp1P,
               glmmPQL_Q=mp1Q,
               glmmADMB_P=fit_poiss,
               glmmADMB_NB1=fit_nb1,
               glmmADMB_NB2=fit_nb),
          merge.names=FALSE,intercept=TRUE,
          legend.x="right",legend=TRUE) # glmmPQL ones seem very different...



# Calculate overdispersion... (need to?)
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

overdisp_fun(fit_poiss) # underdispersed
overdisp_fun(fit_nb)    # underdispersed
overdisp_fun(fit_nb1)   # underdispersed

#================================


# Decided to use glmer poisson model, since others don't improve fit much.


#================================