######### For non-normal data I need to transform, or use GENERALIZED linear mixed models ###########
# Code is variations of glmm()

# Ben Bolker 2009: For GLMMS, most common distributions are binomial (including binary or Bernoulli, i.e. 0/1 responses) 
# and Poisson. Gamma distributions are the next most common, and useful for continuous, skewed distributions
# but treating gamma-distributed data as lognormal is simpler & usually gives similar results to trying to fit the 
# GLMM to a gamma distribution.

# Generalized linear mixed model famililes:
# Family objects provide a convenient way to specify the details of the models used by glm
family(object, ...)

gaussian(link = "identity")  # for normally distrib data or transformed-to-normal - default family for linear models
inverse.gaussian(link = "1/mu^2")
binomial(link = "logit")  # for binary responses i.e. 0/1
Gamma(link = "inverse") # hard to fit and easier to use lognormal. Needs capital G.
poisson(link = "log") # for count data #think the link is automatically used by default
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")

# NOTE these can all be fit in GLM but GLMM can only take the following:
FAMILY: g=Gamma, P=Poisson, G=Gaussian, B=binomial
LINK: l=log, i=inverse, c=cloglog, i=identity

# Negative binomial almost certainly doesn't make sense for a continuous response (Ben Bolker) - use for counts only
# The Gamma is the continuous counterpart of the negative binomial




library(MASS)
# Instead of taking all the fixed and random effects as one formula, the random effects get 
# their own argument in the glmmPQL function. 
# To set the distribution to log-normal, we set the family to gaussian (another word for normal) 
# and the link to log. The link can be anything, though if you want to use something besides log 
# or inverse then you'll have to research how to customize the link function.
PQL <- glmmPQL(VisDur.S ~ Sex + Season, ~1 | AnimalID/PatchID, family = gaussian(link = "log"),
               data = foxdatasubset, verbose = FALSE)

descdist(residuals(PQL)) #plot residuals against a normal distribution
abline(0, 1)

model<-lmer(VisDur.S ~ Sex:SocialStatus + Season + BeforeMidnight + OtherFoxesPresent + PatchQuality + 
              (1|Visit/AnimalID) + (1|PatchID), data=foxdatasubset)
# what is rank deficient?
# including VisitID (individual-level random effect) seems to be a problem

model<-lmer(VisDur.S ~ Sex:SocialStatus + Season + BeforeMidnight + OtherFoxesPresent + PatchQuality + 
              (1|AnimalID) + (1|PatchID), data=foxdatasubset)


summary(model)
model<-glmer(VisDur.S ~ Season + Sex + (1|AnimalID) + (1|PatchID), family="Gamma"(link="log"), data=foxdatasubset, verbose=F)
# can't include Visit as a variable
# using the 'inverse' link with Gamma estimation causes a lot of trouble (couldnt fit my model) but Ben Bolker says it's
# OK to substitute inverse for log link for Gamma