#======================================
#   STRENGTH: BEFORE/AFTER MIDNIGHT
#======================================

Q: What is effect of food availability and individual and environmental attributes on gregariousness?

#== Variables to include in model:
# Rand: 
individ 
territory # explained no variance but kept in anyway as included in all other models. Individ:territory interaction made no difference to model fit/coefs as all core individs were members of ONLY ONE TERRITORY per season
# Fixed:
BeforeMidnight*SocialStatus
BeforeMidnight*Sex
BeforeMidnight*Season

# ORIGINALLY I ALSO FIT MODELS WITH DAYS SEEN AND NFOXES TO 'CONTROL' FOR SIGHTING FREQ AND NUMBER OF AVAILABLE PARTNERS
# This appeared to mask all but the most significant effects 
# Have not seen sighting frequency included in models in any other papers (prob for this reason, and because assoc indices are meant to allow for diffs in sighting freq)
# Found that nfoxes had sig effect but only due to the large network size for territory 6 in aut + wint (both n=21) compared to other networks that were all 4-14 foxes.

# To keep it simple I will use a Spearman's correlation test (for non-parametric data) to confirm why I excluded nfoxes from the model 
# 1. correlation test with all data:
cor.test(as.integer(strength_centrality_core$strength), strength_centrality_core$nfoxes,
         alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # significant (0.045)

# 2. correlation test excluding territory 6 in autumn and winter:
strength_centrality_core$outlier <- ifelse(strength_centrality_core$Territory==6 & strength_centrality_core$SeasonID==3, 1, 0)
strength_centrality_core$outlier <- ifelse(strength_centrality_core$Territory==6 & strength_centrality_core$SeasonID==4, 1, strength_centrality_core$outlier)
a <- subset(strength_centrality_core, outlier==0)
cor.test(as.integer(a$strength), a$nfoxes, alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # no longer significant (0.165)



#-----------------------------
# CHECK DISTRIBUTION OF DATA
#-----------------------------
hist(strength_centrality_core$strength)
hist(log10(strength_centrality_core$strength)) # log-transformation makes data approximate a normal distrib, suggesting they do follow a lognormal distribution
# But I have several zero values and log(0) or log10(0) = infinity, so need to add 1 to each value before transformation:
hist(log10(strength_centrality_core$strength+1)) 

library(fitdistrplus)
# Cullen & Frey graph:
descdist(strength_centrality_core$strength, discrete = FALSE, boot=5000)  # discrete=F specifies a continuous response
# For some distributions (normal, uniform, logistic, exponential), there is only one possible value for 
# skewness and kurtosis (e.g. for a normal distrib, skewness = 0 and kurtosis = 3), and the distrib is thus
# represented by a point on the plot. Lognotmal & gamma have wider range of values shown as lines. 
# Beta has greatest spread of possible values.

# for my data, the boostrapped values lie in beta but beta distributed values must be between 0-1, as they're expected to be proportions.
# Bootstrapped values follow trajectory of lognormal and gamma lines, and weibull is also similar to lnorm and gamma
# so my data could be normal, lognormal, gamma, weibull or beta

# Plot data against normal, logorm, gamma and weibull distributions
fit.norm <- fitdist(strength_centrality_core$strength, "norm")
fit.lnorm <- fitdist(strength_centrality_core$strength+1, "lnorm")
fit.gam <- fitdist(strength_centrality_core$strength+1, "gamma") # GAMMA wont work unless add 1 to data as log(0) or log10(0) = infinite
fit.weibull <- fitdist(strength_centrality_core$strength+1, "weibull") # weibull can't take zeros so added 1 to y

# Plots in order of crapness (from high crappiness to lower crappiness)
plot(fit.norm)
plot(fit.weibull)
plot(fit.gam)
plot(fit.lnorm) # lnorm best 

# ALTERNATIVE PLOTS USING MASS & car PACKAGES
require(MASS)
require(car)
qqp((strength_centrality_core$strength), "norm", main="normal")
qqp(strength_centrality_core$strength, "lnorm", main="lognormal") 
gamma <- fitdistr(strength_centrality_core$strength+1, "gamma", start=NULL)
qqp(strength_centrality_core$strength+1, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma") # gamma is poor 
# lognormal is best (but not perfect)




#--------
# MODELS
#--------

# Try both lognormal LMM with Gamma GLMM with log-link and see which looks better fit:

# lognormal LMM fit = LMM with log-transformed response (need to add 1 to all first as log(0) or log10(0) = infinity) 
# Can use log or log10, result is same but log10 easier to interpret as log10(1)=0, log10(10)=2, log10(100)=3 etc.
strength_mod_logLMM  <- lmer(log10(strength+1) ~ 
                               fBeforeMidnight*SocialStatus*Sex*fSeason +
                               (1|ShortCode) + (1|fTerritory),
                             data=strength_centrality_core, REML=F,
                             verbose=T,
                             control=lmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=1000000),
                                                 check.conv.grad=.makeCC("warning",0.05)))
#= Check residuals:
plot(fitted(strength_mod_logLMM), resid(strength_mod_logLMM), ylim=c(-0.6,0.4))   
lines(smooth.spline(fitted(strength_mod_logLMM), resid(strength_mod_logLMM))) 
hist(resid(strength_mod_logLMM), prob=T) # prob=T to plot as probability (so can overlay a smoothing line)
lines(density(resid(strength_mod_logLMM))) # residuals approx normal with v.little heteroscedasticity (3 outliers near zero)
lattice::dotplot(ranef(strength_mod_logLMM, condVar=TRUE))   # Pluto seems to be outlier

#--------------------------------------------------------------------------------------------
# Gamma GLMM - slower than LMM - max likelihood is default method for GLMM (ignores 'REML=F')
strength_mod_gamlogGLMM <- glmer((strength+1) ~ # cant run gamma on zeros
                                   fBeforeMidnight*SocialStatus*Sex*fSeason +
                                   (1|ShortCode) + (1|fTerritory),
                                 data=strength_centrality_core, family=Gamma(link = "log"), # log link = Multiplicative arithmetic mean model - assumes effects of predictors have multiplicative effect on response
                                 verbose=T,
                                 control=glmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=10000000),
                                                      check.conv.grad=.makeCC("warning",0.05))) 
# Fits as long as I exclude Days Seen
# Stick with lognormal as this distribution better fit the raw data anyway.

#= check resids of gamma GLMM
plot(fitted(strength_mod_gamlogGLMM), resid(strength_mod_gamlogGLMM),ylim=c(-0.6,0.4)) # line straighter than lognorm but same pattern so just use lognorm
lines(smooth.spline(fitted(strength_mod_gamlogGLMM), resid(strength_mod_gamlogGLMM))) 
hist(resid(strength_mod_gamlogGLMM), prob=T)
lines(density(resid(strength_mod_gamlogGLMM))) 
# Nice symmetrical distrib with homescedasticity, but wider spread (greater resid variance) than
# the lognormal LMM indicating less variance is explained by the fixed effects in the gamma model = worse fit.

#--------------------------------------------------------------------------------------------



#--------------------------
#== MODEL SIMPLIFICATION
#--------------------------
car::Anova(strength_mod_logLMM) # 4-way interaction not sig 

# Just three-way ints
strength_mod_threewayints  <- lmer(log10(strength+1) ~ 
                                     fBeforeMidnight*Sex*SocialStatus +
                                     fBeforeMidnight*Sex*fSeason +
                                     fBeforeMidnight*SocialStatus*fSeason +
                                     Sex*SocialStatus*fSeason +
                                     (1|ShortCode) + (1|fTerritory),
                                   data=strength_centrality_core, REML=F,
                                   verbose=T,
                                   control=lmerControl(optimizer="bobyqa",
                                                       optCtrl = list(maxfun=1000000),
                                                       check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_logLMM, strength_mod_threewayints)
car::Anova(strength_mod_threewayints) # All 3 way interactions not sig

# reduced complexity of interactions
strength_mod_twoandthreewayints  <- lmer(log10(strength+1) ~ 
                                           fBeforeMidnight*Sex +
                                           fBeforeMidnight*SocialStatus +
                                           fBeforeMidnight*fSeason +
                                           Sex*SocialStatus*fSeason +
                                           (1|ShortCode) + (1|fTerritory),
                                         data=strength_centrality_core, REML=F,
                                         verbose=T,
                                         control=lmerControl(optimizer="bobyqa",
                                                             optCtrl = list(maxfun=1000000),
                                                             check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_threewayints, strength_mod_twoandthreewayints)
car::Anova(strength_mod_twoandthreewayints) # sex*season is significant

strength_mod_twowayints  <- lmer(log10(strength+1) ~ 
                                   fBeforeMidnight*Sex +
                                   fBeforeMidnight*SocialStatus +
                                   fBeforeMidnight*fSeason +
                                   Sex*SocialStatus +
                                   Sex*fSeason +
                                   SocialStatus*fSeason +
                                   (1|ShortCode) + (1|fTerritory),
                                 data=strength_centrality_core, REML=F,
                                 verbose=T,
                                 control=lmerControl(optimizer="bobyqa",
                                                     optCtrl = list(maxfun=1000000),
                                                     check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_twoandthreewayints, strength_mod_twowayints)
car::Anova(strength_mod_twowayints) # beforemidnight*season poss sig.

# no b4statusint
strength_mod_nob4statusint  <- lmer(log10(strength+1) ~ 
                                      fBeforeMidnight*Sex +
                                      fBeforeMidnight*fSeason +
                                      Sex*SocialStatus +
                                      Sex*fSeason +
                                      SocialStatus*fSeason +
                                      (1|ShortCode) + (1|fTerritory),
                                    data=strength_centrality_core, REML=F,
                                    verbose=T,
                                    control=lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=1000000),
                                                        check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_twowayints, strength_mod_nob4statusint)
car::Anova(strength_mod_nob4statusint) 

# no sex*status
strength_mod_nosxstatint  <- lmer(log10(strength+1) ~ 
                                    fBeforeMidnight*Sex +
                                    fBeforeMidnight*fSeason +
                                    Sex*fSeason +
                                    SocialStatus*fSeason +
                                    (1|ShortCode) + (1|fTerritory),
                                  data=strength_centrality_core, REML=F,
                                  verbose=T,
                                  control=lmerControl(optimizer="bobyqa",
                                                      optCtrl = list(maxfun=1000000),
                                                      check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_nob4statusint, strength_mod_nosxstatint)
car::Anova(strength_mod_nosxstatint)

# no season*status
strength_mod_noseasstatint  <- lmer(log10(strength+1) ~ 
                                      fBeforeMidnight*Sex +
                                      fBeforeMidnight*fSeason +
                                      Sex*fSeason +
                                      SocialStatus +
                                      (1|ShortCode) + (1|fTerritory),
                                    data=strength_centrality_core, REML=F,
                                    verbose=T,
                                    control=lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=1000000),
                                                        check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_nosxstatint, strength_mod_noseasstatint)
car::Anova(strength_mod_noseasstatint)


# no b4*sex
strength_mod_nob4sex  <- lmer(log10(strength+1) ~ 
                                fBeforeMidnight*fSeason +
                                Sex*fSeason +
                                SocialStatus +
                                (1|ShortCode) + (1|fTerritory),
                              data=strength_centrality_core, REML=F,
                              verbose=T,
                              control=lmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=1000000),
                                                  check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_noseasstatint, strength_mod_nob4sex)
car::Anova(strength_mod_nob4sex)

# no status
strength_mod_nostatus  <- lmer(log10(strength+1) ~ 
                                 fBeforeMidnight*fSeason +
                                 Sex*fSeason +
                                 (1|ShortCode) + (1|fTerritory),
                               data=strength_centrality_core, REML=F,
                               verbose=T,
                               control=lmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=1000000),
                                                   check.conv.grad=.makeCC("warning",0.05)))
anova(strength_mod_nob4sex, strength_mod_nostatus)
car::Anova(strength_mod_nostatus)

# no b4*season 
strength_mod_nob4seas  <- lmer(log10(strength+1) ~ 
                                 fBeforeMidnight +
                                 Sex*fSeason +
                                 (1|ShortCode) + (1|fTerritory),
                               data=strength_centrality_core, REML=F,
                               verbose=T,
                               control=lmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=1000000),
                                                   check.conv.grad=.makeCC("warning",0.05)))

anova(strength_mod_nostatus, strength_mod_nob4seas)
car::Anova(strength_mod_nob4seas)

#----------------------------------------------------------------------------------------------------------

# FINAL MODEL FOR STRENGTH - CORE FOXES ONLY
strength_mod <-  lmer(log10(strength+1) ~ 
                        fBeforeMidnight +
                        Sex*fSeason +
                        (1|ShortCode) + (1|fTerritory),
                      data=strength_centrality_core, REML=F,
                      verbose=T,
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=1000000),
                                          check.conv.grad=.makeCC("warning",0.05)))


#----------------
# MODEL CHECKING
#----------------
plot(fitted(strength_mod), resid(strength_mod)) # 2 groups (as bunches of low and high values of strength) but same variance in both groups
lines(smooth.spline(fitted(strength_mod), resid(strength_mod))) 
hist(resid(strength_mod))
visreg::visreg(strength_mod) # clear diff in Before/After midnight

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
}
overdisp_fun(strength_mod) # p-value: If <0.05, data are overdispersed. Mine=1, not overdispersed.


#----------------------------
#== SAVE MODEL COEFFICIENTS 
#----------------------------
STRENGTH_MOD_coeffs <- coef(summary(strength_mod))  # save as DF
STRENGTH_MOD_cis <- confint(strength_mod)
summary(strength_mod) # get variance and SD for random effects



#----------------------------------------------------------------------------------------------
#== LR TESTS TO GET X2 AND P-VALUES FOR NON-SIG TERMS BY ADDING EACH TERM TO THE MINIMAL MODEL
#----------------------------------------------------------------------------------------------
# without sex*season
strength_mod_nosexseas <-  lmer(log10(strength+1) ~ 
                                  fBeforeMidnight +
                                  (1|ShortCode) + (1|fTerritory),
                                data=strength_centrality_core, REML=F,
                                verbose=T,
                                control=lmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=1000000),
                                                    check.conv.grad=.makeCC("warning",0.05)))
# without BeforeMidnight
strength_mod_noB4 <-  lmer(log10(strength+1) ~ 
                             Sex*fSeason+
                             (1|ShortCode) + (1|fTerritory),
                           data=strength_centrality_core, REML=F,
                           verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))


# with status added
strength_mod_withstatus <-  lmer(log10(strength+1) ~ 
                                   fBeforeMidnight +
                                   Sex*fSeason +
                                   SocialStatus +
                                   (1|ShortCode) + (1|fTerritory),
                                 data=strength_centrality_core, REML=F,
                                 verbose=T,
                                 control=lmerControl(optimizer="bobyqa",
                                                     optCtrl = list(maxfun=1000000),
                                                     check.conv.grad=.makeCC("warning",0.05)))
# with before*status interaction
strength_mod_withb4statint <-  lmer(log10(strength+1) ~ 
                                      fBeforeMidnight*SocialStatus +
                                      Sex*fSeason +
                                      (1|ShortCode) + (1|fTerritory),
                                    data=strength_centrality_core, REML=F,
                                    verbose=T,
                                    control=lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=1000000),
                                                        check.conv.grad=.makeCC("warning",0.05)))

# with before*sex interaction
strength_mod_withb4sexint <-  lmer(log10(strength+1) ~ 
                                     fBeforeMidnight*Sex +
                                     Sex*fSeason +
                                     (1|ShortCode) + (1|fTerritory),
                                   data=strength_centrality_core, REML=F,
                                   verbose=T,
                                   control=lmerControl(optimizer="bobyqa",
                                                       optCtrl = list(maxfun=1000000),
                                                       check.conv.grad=.makeCC("warning",0.05)))
# with before*seas interaction
strength_mod_withb4seasint <-  lmer(log10(strength+1) ~ 
                                      fBeforeMidnight*fSeason +
                                      Sex*fSeason +
                                      (1|ShortCode) + (1|fTerritory),
                                    data=strength_centrality_core, REML=F,
                                    verbose=T,
                                    control=lmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=1000000),
                                                        check.conv.grad=.makeCC("warning",0.05)))
# with status*seas interaction
strength_mod_withstatseasint <-  lmer(log10(strength+1) ~ 
                                        fBeforeMidnight +
                                        SocialStatus*fSeason +
                                        Sex*fSeason +
                                        (1|ShortCode) + (1|fTerritory),
                                      data=strength_centrality_core, REML=F,
                                      verbose=T,
                                      control=lmerControl(optimizer="bobyqa",
                                                          optCtrl = list(maxfun=1000000),
                                                          check.conv.grad=.makeCC("warning",0.05)))

# with sex*status interaction
strength_mod_withsexstatusint <-  lmer(log10(strength+1) ~ 
                                         fBeforeMidnight +
                                         SocialStatus*Sex +
                                         Sex*fSeason +
                                         (1|ShortCode) + (1|fTerritory),
                                       data=strength_centrality_core, REML=F,
                                       verbose=T,
                                       control=lmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun=1000000),
                                                           check.conv.grad=.makeCC("warning",0.05)))


# Compare minimal signif mod with and without each term to get chisq values:

# minimal model WITHOUT significant terms:
anova(strength_mod, strength_mod_noB4)
anova(strength_mod, strength_mod_nosexseas)

# minimal model WITH nonsignificant terms:
anova(strength_mod, strength_mod_withstatus)
anova(strength_mod, strength_mod_withb4statint)
anova(strength_mod, strength_mod_withb4sexint)
anova(strength_mod, strength_mod_withb4seasint)
anova(strength_mod, strength_mod_withstatseasint)
anova(strength_mod, strength_mod_withsexstatusint)

#---------------

# POST HOC TEST
library(lsmeans)
a <- pairs(lsmeans::lsmeans(strength_mod, ~Sex|fSeason)) # compare rates between seasons for each sexstat
b <- pairs(lsmeans::lsmeans(strength_mod, ~fSeason|Sex)) # compare rates between sexstat for each season
lsm_tukey <- test(rbind(a,b), type="response", adjust="tukey")

# MAKE PREDICTIONS FOR PLOTTING SEX*SEASON ON SEPARATE PANELS BEFORE/AFTER MIDNIGHT
pred.data <- expand.grid(fBeforeMidnight=unique(strength_centrality_core$fBeforeMidnight),
                         Sex=unique(strength_centrality_core$Sex),
                         fSeason=unique(strength_centrality_core$fSeason))

pred <- predict(strength_mod, newdata=pred.data, type="response", re.form=~0) 
modelmatrix <-model.matrix(~fBeforeMidnight+Sex*fSeason, pred.data) 
y <-modelmatrix%*%fixef(strength_mod) 
pvar1 <- diag(modelmatrix %*% tcrossprod(vcov(strength_mod),modelmatrix))
newdata <-data.frame(
  fBeforeMidnight=pred.data$fBeforeMidnight,
  Sex=pred.data$Sex,
  fSeason=pred.data$fSeason,
  y=y,
  ciLWR = y-1.96*sqrt(pvar1),             # confidence interval lower
  ciUPR = y+1.96*sqrt(pvar1))             # confidence interval upper
newdata$y_bt <-   (10^newdata$y)-1        # backtransform y
newdata$ciLWR_bt <- (10^newdata$ciLWR)-1  # backtransform CI
newdata$ciUPR_bt <- (10^newdata$ciUPR)-1  # backtransform CI
head(newdata)
strength_predsWithCIs <- newdata # save as new df incase overwrite

# Adjust names and object types
strength_predsWithCIs$fBeforeMidnight <- ordered(strength_predsWithCIs$fBeforeMidnight, levels = c(1,0),
                                                 labels=c("Before midnight", "After midnight")) # rename and reorder for ggplot2 facet labels
strength_predsWithCIs$Sex <- factor(strength_predsWithCIs$Sex,levels(strength_predsWithCIs$Sex)[c(2,1)]) # re-order sex
strength_predsWithCIs$Season <- ordered(strength_predsWithCIs$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

library(ggplot2)
# Plot on transformed scale (log10+1)
ggp <- ggplot(strength_predsWithCIs, aes(x=Season, y=y, group=Sex, colour=factor(Sex))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=Sex), size=1, position=position_dodge(0.2)) + 
  geom_point(size=3, position=position_dodge(0.2)) + 
  facet_wrap(~fBeforeMidnight) +
  scale_colour_manual(name="Sex", values=c('royalblue3','firebrick1')) + # rename legend
  xlab("") + ylab("Strength\n") +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black"),
        strip.text.x = element_text(face="bold"),
        strip.background=element_blank()) +
  geom_errorbar(data=strength_predsWithCIs, 
                mapping=aes(x=Season, ymin=ciLWR, ymax=ciUPR),
                width=0.2, size=0.6, position=position_dodge(0.2)) 

# save above in high res.
dev.print(jpeg, "RPlot_Lognormal LMM predictions for strength facet BeforeMidnight_LOG10PLUS1.jpeg", res=900, height=6, width=10, units="in") 

# Plot backtransformed values
ggp <- ggplot(strength_predsWithCIs, aes(x=Season, y=y_bt, group=Sex, colour=factor(Sex))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=Sex), size=1, position=position_dodge(0.2)) + 
  geom_point(size=3, position=position_dodge(0.2)) + 
  facet_wrap(~fBeforeMidnight) +
  scale_colour_manual(name="Sex", values=c('royalblue3','firebrick1')) + # rename legend
  xlab("") + ylab("Strength\n") +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.x = element_text(face="bold"),
        strip.background=element_blank()) +
  geom_errorbar(data=strength_predsWithCIs, 
                mapping=aes(x=Season, ymin=ciLWR_bt, ymax=ciUPR_bt),
                width=0.2, size=0.6, position=position_dodge(0.2)) 
# save above in high res.
dev.print(jpeg, "RPlot_Lognormal LMM predictions for strength facet BeforeMidnight_BACKTRANSFORMED.jpeg", res=900, height=6, width=10, units="in") 


#--------------------------------------------------------------
# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA
#--------------------------------------------------------------
library(sna)

# set up progress bar
perm <- 1000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrix to store the coefficient estimate from each randomised matrix
B4_perm <-rep(NA,perm) # Need to save 2 coefficients so make 2 matrices
SexM_perm <-rep(NA,perm)
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SexMfSeason2_perm <-rep(NA,perm)
SexMfSeason3_perm <-rep(NA,perm)
SexMfSeason4_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  input_strength <- rbind(
    # Before midnight:
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength=T1sprB4_rand_strength[,i], SeasonID="1", Territory="1", BeforeMidnight="1", id=row.names(T1sprB4_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength=T1sumB4_rand_strength[,i], SeasonID="2", Territory="1", BeforeMidnight="1", id=row.names(T1sumB4_rand_strength)), 
    data.frame(strength=T1autB4_rand_strength[,i], SeasonID="3", Territory="1", BeforeMidnight="1", id=row.names(T1autB4_rand_strength)),
    data.frame(strength=T1winB4_rand_strength[,i], SeasonID="4", Territory="1", BeforeMidnight="1", id=row.names(T1winB4_rand_strength)),
    
    data.frame(strength= T2sprB4_rand_strength[,i], SeasonID="1", Territory="2", BeforeMidnight="1", id=row.names(T2sprB4_rand_strength)),
    # data.frame(strength= T2sumB4_rand_strength[,i], SeasonID="2", Territory="2", BeforeMidnight="1", id=row.names(T2sumB4_rand_strength)), # no prefavoided
    data.frame(strength= T2autB4_rand_strength[,i], SeasonID="3", Territory="2", BeforeMidnight="1", id=row.names(T2autB4_rand_strength)),
    data.frame(strength= T2winB4_rand_strength[,i], SeasonID="4", Territory="2", BeforeMidnight="1", id=row.names(T2winB4_rand_strength)),
    
    data.frame(strength= T3sprB4_rand_strength[,i], SeasonID="1", Territory="3", BeforeMidnight="1", id=row.names(T3sprB4_rand_strength)),
    data.frame(strength= T3sumB4_rand_strength[,i], SeasonID="2", Territory="3", BeforeMidnight="1", id=row.names(T3sumB4_rand_strength)),
    data.frame(strength= T3autB4_rand_strength[,i], SeasonID="3", Territory="3", BeforeMidnight="1", id=row.names(T3autB4_rand_strength)),
    data.frame(strength= T3winB4_rand_strength[,i], SeasonID="4", Territory="3", BeforeMidnight="1", id=row.names(T3winB4_rand_strength)),
    
    # data.frame(strength= T4sprB4_rand_strength[,i], SeasonID="1", Territory="4", BeforeMidnight="1", id=row.names(T4sprB4_rand_strength)), # no prefavoided
    # data.frame(strength= T4sumB4_rand_strength[,i], SeasonID="2", Territory="4", BeforeMidnight="1", id=row.names(T4sumB4_rand_strength)), # no prefavoided
    data.frame(strength= T4autB4_rand_strength[,i], SeasonID="3", Territory="4", BeforeMidnight="1", id=row.names(T4autB4_rand_strength)),
    data.frame(strength= T4winB4_rand_strength[,i], SeasonID="4", Territory="4", BeforeMidnight="1", id=row.names(T4winB4_rand_strength)),
    
    data.frame(strength= T5sprB4_rand_strength[,i], SeasonID="1", Territory="5", BeforeMidnight="1", id=row.names(T5sprB4_rand_strength)),
    data.frame(strength= T5sumB4_rand_strength[,i], SeasonID="2", Territory="5", BeforeMidnight="1", id=row.names(T5sumB4_rand_strength)),
    data.frame(strength= T5autB4_rand_strength[,i], SeasonID="3", Territory="5", BeforeMidnight="1", id=row.names(T5autB4_rand_strength)),
    # data.frame(strength= T5winB4_rand_strength[,i], SeasonID="4", Territory="5", BeforeMidnight="1", id=row.names(T5winB4_rand_strength)), # no prefavoided
    
    data.frame(strength= T6sprB4_rand_strength[,i], SeasonID="1", Territory="6", BeforeMidnight="1", id=row.names(T6sprB4_rand_strength)), 
    data.frame(strength= T6sumB4_rand_strength[,i], SeasonID="2", Territory="6", BeforeMidnight="1", id=row.names(T6sumB4_rand_strength)), 
    data.frame(strength= T6autB4_rand_strength[,i], SeasonID="3", Territory="6", BeforeMidnight="1", id=row.names(T6autB4_rand_strength)), 
    data.frame(strength= T6winB4_rand_strength[,i], SeasonID="4", Territory="6", BeforeMidnight="1", id=row.names(T6winB4_rand_strength)),
    
    data.frame(strength= T7sprB4_rand_strength[,i], SeasonID="1", Territory="7", BeforeMidnight="1", id=row.names(T7sprB4_rand_strength)),
    data.frame(strength= T7sumB4_rand_strength[,i], SeasonID="2", Territory="7", BeforeMidnight="1", id=row.names(T7sumB4_rand_strength)), 
    data.frame(strength= T7autB4_rand_strength[,i], SeasonID="3", Territory="7", BeforeMidnight="1", id=row.names(T7autB4_rand_strength)),
    data.frame(strength= T7winB4_rand_strength[,i], SeasonID="4", Territory="7", BeforeMidnight="1", id=row.names(T7winB4_rand_strength)),
    
    # After midnight:
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength= T1sprAf_rand_strength[,i], SeasonID="1", Territory="1", BeforeMidnight="0", id=row.names(T1sprAf_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength= T1sumAf_rand_strength[,i], SeasonID="2", Territory="1", BeforeMidnight="0", id=row.names(T1sumAf_rand_strength)), 
    data.frame(strength= T1autAf_rand_strength[,i], SeasonID="3", Territory="1", BeforeMidnight="0", id=row.names(T1autAf_rand_strength)),
    data.frame(strength= T1winAf_rand_strength[,i], SeasonID="4", Territory="1", BeforeMidnight="0", id=row.names(T1winAf_rand_strength)),
    
    data.frame(strength= T2sprAf_rand_strength[,i], SeasonID="1", Territory="2", BeforeMidnight="0", id=row.names(T2sprAf_rand_strength)),
    # data.frame(strength= T2sumAf_rand_strength[,i], SeasonID="2", Territory="2", BeforeMidnight="0", id=row.names(T2sumAf_rand_strength)), # couldnt permute as  no true assocs in this network
    data.frame(strength= T2autAf_rand_strength[,i], SeasonID="3", Territory="2", BeforeMidnight="0", id=row.names(T2autAf_rand_strength)), 
    data.frame(strength= T2winAf_rand_strength[,i], SeasonID="4", Territory="2", BeforeMidnight="0", id=row.names(T2winAf_rand_strength)),
    
    data.frame(strength= T3sprAf_rand_strength[,i], SeasonID="1", Territory="3", BeforeMidnight="0", id=row.names(T3sprAf_rand_strength)),
    data.frame(strength= T3sumAf_rand_strength[,i], SeasonID="2", Territory="3", BeforeMidnight="0", id=row.names(T3sumAf_rand_strength)),
    data.frame(strength= T3autAf_rand_strength[,i], SeasonID="3", Territory="3", BeforeMidnight="0", id=row.names(T3autAf_rand_strength)),
    data.frame(strength= T3winAf_rand_strength[,i], SeasonID="4", Territory="3", BeforeMidnight="0", id=row.names(T3winAf_rand_strength)),
    
    # data.frame(strength= T4sprAf_rand_strength[,i], SeasonID="1", Territory="4", BeforeMidnight="0", id=row.names(T4sprAf_rand_strength)), # no prefavoided
    # data.frame(strength= T4sumAf_rand_strength[,i], SeasonID="2", Territory="4", BeforeMidnight="0", id=row.names(T4sumAf_rand_strength)), # no prefavoided
    data.frame(strength= T4autAf_rand_strength[,i], SeasonID="3", Territory="4", BeforeMidnight="0", id=row.names(T4autAf_rand_strength)),
    data.frame(strength= T4winAf_rand_strength[,i], SeasonID="4", Territory="4", BeforeMidnight="0", id=row.names(T4winAf_rand_strength)),
    
    data.frame(strength= T5sprAf_rand_strength[,i], SeasonID="1", Territory="5", BeforeMidnight="0", id=row.names(T5sprAf_rand_strength)),
    data.frame(strength= T5sumAf_rand_strength[,i], SeasonID="2", Territory="5", BeforeMidnight="0", id=row.names(T5sumAf_rand_strength)),
    data.frame(strength= T5autAf_rand_strength[,i], SeasonID="3", Territory="5", BeforeMidnight="0", id=row.names(T5autAf_rand_strength)),
    # data.frame(strength= T5winAf_rand_strength[,i], SeasonID="4", Territory="5", BeforeMidnight="0", id=row.names(T5winAf_rand_strength)), # no prefavoided
    
    data.frame(strength= T6sprAf_rand_strength[,i], SeasonID="1", Territory="6", BeforeMidnight="0", id=row.names(T6sprAf_rand_strength)), 
    data.frame(strength= T6sumAf_rand_strength[,i], SeasonID="2", Territory="6", BeforeMidnight="0", id=row.names(T6sumAf_rand_strength)), 
    data.frame(strength= T6autAf_rand_strength[,i], SeasonID="3", Territory="6", BeforeMidnight="0", id=row.names(T6autAf_rand_strength)), 
    data.frame(strength= T6winAf_rand_strength[,i], SeasonID="4", Territory="6", BeforeMidnight="0", id=row.names(T6winAf_rand_strength)),
    
    data.frame(strength= T7sprAf_rand_strength[,i], SeasonID="1", Territory="7", BeforeMidnight="0", id=row.names(T7sprAf_rand_strength)),
    data.frame(strength= T7sumAf_rand_strength[,i], SeasonID="2", Territory="7", BeforeMidnight="0", id=row.names(T7sumAf_rand_strength)), 
    data.frame(strength= T7autAf_rand_strength[,i], SeasonID="3", Territory="7", BeforeMidnight="0", id=row.names(T7autAf_rand_strength)),
    data.frame(strength= T7winAf_rand_strength[,i], SeasonID="4", Territory="7", BeforeMidnight="0", id=row.names(T7winAf_rand_strength)))
  
  # Convert factors to characters (to avoid warnings when using merge)
  input_strength$id <- as.character(input_strength$id)
  input_strength$SeasonID <- as.character(input_strength$SeasonID)
  input_strength$Territory <- as.character(input_strength$Territory)
  input_strength$BeforeMidnight <- as.character(input_strength$BeforeMidnight)
  # Add attributes to input_strength
  input_strength$ShortCode <- attribs_char[match(input_strength$id, attribs_char$id),4] 
  input_strength$Sex <- attribs_char[match(input_strength$id, attribs_char$id),5] 
  input_strength$SocialStatus <- attribs_char[match(input_strength$id, attribs_char$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_strength, y=attribs_char[ , c("id", "SeasonID", "Territory", "BeforeMidnight", "Core", "DaysSeen")], 
              by = c("id", "SeasonID", "Territory", "BeforeMidnight"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$Sex <- factor(df$Sex)
  df$fBeforeMidnight <- factor(df$BeforeMidnight)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  # run the model
  model_tmp <-lmer(log10(strength+1) ~ fBeforeMidnight + Sex*fSeason + (1|ShortCode) + (1|fTerritory), REML=F, 
                   data=subset(df, SocialStatus!="NA" & Core==1),
                   control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
  # get the coefficient estimate and save it
  B4_perm[i] <- coef(summary(model_tmp))[2,1] # fBeforeMidnight # [2,1] means save number in 2nd row of 1st column
  SexM_perm[i] <- coef(summary(model_tmp))[3,1] # SexM in spring (ref category)   
  fSeason2_perm[i] <-coef(summary(model_tmp))[4,1]
  fSeason3_perm[i] <-coef(summary(model_tmp))[5,1]
  fSeason4_perm[i] <-coef(summary(model_tmp))[6,1]
  SexMfSeason2_perm[i] <-coef(summary(model_tmp))[7,1]
  SexMfSeason3_perm[i] <-coef(summary(model_tmp))[8,1]
  SexMfSeason4_perm[i] <-coef(summary(model_tmp))[9,1]
  
  setTxtProgressBar(pb, i)
}

# P-value: proportion of coefficient estimates from randomised matrices that are greater than the coefficient
# estimate from the observed (real) data:
B4_obs <- coef(summary(strength_mod))[2,1] # First save the observed coefficient (from model based on real data, 'strength_mod')
SexM_obs <- coef(summary(strength_mod))[3,1] # ditto
fSeason2_obs <-coef(summary(strength_mod))[4,1]
fSeason3_obs <-coef(summary(strength_mod))[5,1]
fSeason4_obs <-coef(summary(strength_mod))[6,1]
SexMfSeason2_obs <-coef(summary(strength_mod))[7,1]
SexMfSeason3_obs <-coef(summary(strength_mod))[8,1]
SexMfSeason4_obs <-coef(summary(strength_mod))[9,1]

B4_Pvalue <- sum(B4_perm>B4_obs)/1000 # Then compare observed and random coeffs and divide by no. permutations/randomisations = p-value
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/1000 # ditto
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/1000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/1000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/1000
SexMfSeason2_Pvalue <- sum(SexMfSeason2_perm>SexMfSeason2_obs)/1000
SexMfSeason3_Pvalue <- sum(SexMfSeason3_perm>SexMfSeason3_obs)/1000
SexMfSeason4_Pvalue <- sum(SexMfSeason4_perm>SexMfSeason4_obs)/1000

