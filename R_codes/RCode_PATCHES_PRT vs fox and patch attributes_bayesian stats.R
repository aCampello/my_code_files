#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   PRT ~ fox and patch attribs models using BAYESIAN methods ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tested this to see if fit better
# took too long and didnt understand so stuck with lme4

setwd("E:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/PRT vs. food, sex, season and status/Status X food - DF & MJ combined (resident)")
save.image("brms PRT models.Rdata")

# (Gamma GLMMs are dodgy in lme4 at the moment (search 'Gamma GLMMs' here https://github.com/bbolker/mixedmodels-misc/blob/master/glmmFAQ.rmd)
# so Ben Bolker suggested using lognormal (tried, didnt work well), glmmPQL, glmmADMB or bayesian methods instead)

# NOW: Fitting Gamma GLMM in Bayesian framework - recommended by RSpace ####
# as  lognormal one predictions are too different from raw data, and R2 was very low.

# using brms or rstanarm packages (use same syntax as lme4, but interface with the 'Stan' language)
# See here for a quick overview of brms https://github.com/paul-buerkner/brms

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
install.packages("brms")
# If get errors see: https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows
library(brms)
library(rstan)
# Ensure there is a folder in the C: drive called Rtools containing the folder 'mingw_64' - may need to copy from Seagate Rtools folder.  

# May be slow as has to compile the model before fitting it
# Runs 4 markov chains by default; change to chains=1 to test but need 3 or so ideally.
# Reduce runtime by using update() function to avoid re-compiling model each time

# data
PRTsexstatcore <- subset(dailyPRT_subsexstat, Core=="1")

# INTENDED MODEL (HAVE NOT RUN THIS MODEL YET):
# Add interactions individually so can be removed using update() 
bayesPRTmod_FULL <- brms::brm(totalPRT ~ 
                                fSeason + Sex + SocialStatus + DaysFedPerWeek + MeanMJperFeedingDay + 
                                fSeason:Sex +
                                DaysFedPerWeek:SocialStatus +
                                MeanMJperFeedingDay:SocialStatus +
                                (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                              data=PRTsexstatcore,
                              family="Gamma"(link="log"), chains=3) 

# Tried running a reduced bayesian gamma model (just Sex + Season) in brms to test fitting/speed 
# Took 97 mins! See model output below.
bayesPRTmod_SexAddSeas <- brms::brm(totalPRT ~  fSeason + Sex + (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
                                    data=subset(dailyPRT_subsexstat, Core=="1"), family="Gamma"(link="log"))

# Gamma with default inverse link failed, so have to use log.

# Model output for bayesPRTmod_SexAddSeas (progress script that was printed as it ran) #### 
# Total runtime = 97 minutes!
SAMPLING FOR MODEL 'gamma(log) brms-model' NOW (CHAIN 1).

Chain 1, Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 1, Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 1, Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 1, Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 1, Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 1, Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 1, Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 1, Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 1, Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 1, Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 1, Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 1, Iteration: 2000 / 2000 [100%]  (Sampling)
Elapsed Time: 746.919 seconds (Warm-up)
544.994 seconds (Sampling)
1291.91 seconds (Total) # = 22 mins


SAMPLING FOR MODEL 'gamma(log) brms-model' NOW (CHAIN 2).

Chain 2, Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 2, Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 2, Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 2, Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 2, Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 2, Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 2, Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 2, Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 2, Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 2, Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 2, Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 2, Iteration: 2000 / 2000 [100%]  (Sampling)
Elapsed Time: 714.51 seconds (Warm-up)
787.023 seconds (Sampling)
1501.53 seconds (Total) # = 25 mins


SAMPLING FOR MODEL 'gamma(log) brms-model' NOW (CHAIN 3).

Chain 3, Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 3, Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 3, Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 3, Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 3, Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 3, Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 3, Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 3, Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 3, Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 3, Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 3, Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 3, Iteration: 2000 / 2000 [100%]  (Sampling)
Elapsed Time: 774.918 seconds (Warm-up)
601.205 seconds (Sampling)
1376.12 seconds (Total) # 23 mins

SAMPLING FOR MODEL 'gamma(log) brms-model' NOW (CHAIN 4).

Chain 4, Iteration:    1 / 2000 [  0%]  (Warmup)
Chain 4, Iteration:  200 / 2000 [ 10%]  (Warmup)
Chain 4, Iteration:  400 / 2000 [ 20%]  (Warmup)
Chain 4, Iteration:  600 / 2000 [ 30%]  (Warmup)
Chain 4, Iteration:  800 / 2000 [ 40%]  (Warmup)
Chain 4, Iteration: 1000 / 2000 [ 50%]  (Warmup)
Chain 4, Iteration: 1001 / 2000 [ 50%]  (Sampling)
Chain 4, Iteration: 1200 / 2000 [ 60%]  (Sampling)
Chain 4, Iteration: 1400 / 2000 [ 70%]  (Sampling)
Chain 4, Iteration: 1600 / 2000 [ 80%]  (Sampling)
Chain 4, Iteration: 1800 / 2000 [ 90%]  (Sampling)
Chain 4, Iteration: 2000 / 2000 [100%]  (Sampling)
Elapsed Time: 758.716 seconds (Warm-up)
541.845 seconds (Sampling)
1300.56 seconds (Total) # 22 mins

Warning messages:
  1: There were 98 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
2: Examine the pairs() plot to diagnose sampling problems # cannot plot - "figure margins too large"

# SEE: http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

#~~~~~~~~~~~~~~

# Exploring the bayesPRTmod_SexAddSeas model ####

# WAIC gets information criteria ((A)IC) - only informative if fit several models to compare:
WAIC(bayesPRTmod_SexAddSeas) 
# WAIC     SE
# 212688.2 449.21
# Warning message:
#   82 (0.5%) p_waic estimates greater than 0.4.
# "We recommend trying loo() instead."

loo(bayesPRTmod_SexAddSeas) 
#     LOOIC    SE
# 212698.6 449.5
# Warning message:
#  Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.

# View model results (= posteriors)
summary(bayesPRTmod_SexAddSeas) 
plot(bayesPRTmod_SexAddSeas, ask = FALSE) # NOT SURE WHAT ASK MEANS
head(predict(bayesPRTmod_SexAddSeas)) # View preds
pairs(bayesPRTmod_SexAddSeas) # can't plot - says "figure margins too large"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Try setting priors to reduce model fitting time for bayesPRTmod_SexAddSeas model ####

# Setting priors can reduce runtime and improve coefficient precision / reduce CIs:
# prior = set_prior("gamma(1,3000)"), chains = 2)

# view default priors for full intended model
a <- get_prior(totalPRT ~  fSeason + Sex + SocialStatus + DaysFedPerWeek + MeanMJperFeedingDay + 
                 fSeason:Sex +
                 DaysFedPerWeek:SocialStatus +
                 MeanMJperFeedingDay:SocialStatus +
                 (1|ShortCode) + (1|StationCode) + (1|ShortCode:StationCode),
               data=PRTsexstatcore,  family="Gamma"(link="log"))
# default priors = gamma(0.01, 0.01) or student_t(3, 0, 10)for the intercept only (for both inverse and log)
# Apparently the default priors are broad/vague/non-assuming, so are pretty good starting points and no need to change them.

summary(PRTsexstatcore$totalPRT) # view tail and range of data to help estimate priors
# used this site: based on http://keisan.casio.com/exec/system/1180573217
# to play around with scale and shape parameters to get flat shape that covers the 
# range of my data (up to 8000) priors (a=shape, b=scale, so prior=(shape,scale))

# Tried setting priors using set_prior("gamma(1,3000)"), based on the 'casio' site above, for the simple Sex + fSeason model (bayesPRTmod_SexAddSeas).
# But model failed immediately.
# I think 3000 wass too high. Decided to stick with the default priors (0.01, 0.01)

# As the bayesian model took so long to fit (as is the norm for bayesian models apparently) I decided to 
# try and fit the gamma model in lme4 again instead. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             



