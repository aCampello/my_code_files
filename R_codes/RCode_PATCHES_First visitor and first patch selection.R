rm(list=ls()) 

save.image("FirstVisitor & FirstPatch models.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ARE THE FIRST PATCH VISITORS AFTER FEEDING TIME DOMINANT OR SUBORDINATE?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### FIRST VISITOR DATA PREPARATION ####

setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First visitor")
# import without converting to factor
firstvisitor <- read.csv("First visitor after feeding time per patch per day 130716.CSV", header=T, 
                         stringsAsFactors = FALSE) # MUST IMPORT AS strings as some foxes have unknown sex and status - otherwise no matter what I do R thinks there are 3 levels (M, F and '')... WHY!!
str(firstvisitor)

# Some foxes have unknown sex or status:
# FIRST replace with NAs
firstvisitor$Sex[firstvisitor$Sex  == ''] <- NA
firstvisitor$BinSex[firstvisitor$BinSex  == ''] <- NA #BinSex: 1=male, 0=female, NA=unknown
firstvisitor$SocialStatus[firstvisitor$SocialStatus  == ''] <- NA

# THEN make factors
firstvisitor$fSeason <- factor(firstvisitor$SeasonID)
firstvisitor$fTerritory <- factor(firstvisitor$Territory)
firstvisitor$Sex <- factor(firstvisitor$Sex)
firstvisitor$BinSex <- factor(firstvisitor$BinSex)
firstvisitor$SocialStatus <- factor(firstvisitor$SocialStatus) 
firstvisitor$Core <- factor(firstvisitor$Core) # Core vs not core (core=1, not=0)


# IMPORT PATCH ATTRIBUTES
setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding")
patchdata <- read.csv("Patch feeding freq_28surveys.csv")
firstvisitor$MeanMJperFeedingDay <- patchdata[match(firstvisitor$StationID, patchdata$StationID),14] 
firstvisitor$DaysFedPerWeek <- patchdata[match(firstvisitor$StationID, patchdata$StationID),8] 

View(firstvisitor)

# remove days when first visitor was MultiAdultPhoto
firstvisExclMultiAdPhotos <- subset(firstvisitor, MultiAdultPhoto==0)

# remove days when first visitor was non-resident
RESfirstvisitor <- subset(firstvisExclMultiAdPhotos, Core==1)



# N days when doms, subs and unknown status foxes were the first to visit after feeding time ####
library(plyr)
sum_status <-ddply(firstvisitor, c("SocialStatus"), summarise, 
                   freq=length(TRANScDate),
                   nfoxes=length(unique(ShortCode)))

sum_core <-ddply(firstvisitor, "Core", summarise, 
                 freq=length(TRANScDate),
                 nfoxes=length(unique(ShortCode)))

sum_Sex <-ddply(firstvisitor, "Sex", summarise, 
                 freq=length(TRANScDate),
                 nfoxes=length(unique(ShortCode)))

# remove NAs
sum_status_sub <- subset(sum_status, SocialStatus!="NA")
sum_Sex_sub <- subset(sum_Sex, Sex!="NA") # N days & individuals when first visitor was male/female


# Exact binomial test to see if DOMINANT foxes are more likely the first visitor ####
binom.test(2442, sum(sum_status_sub$freq), 0.5) #2442 is N times first visitor was Dom (in sum_status_sub)

# Exact binomial test to see if CORE foxes are more likely the first visitor ####
binom.test(3650, sum(sum_core$freq), 0.5)

# Exact binomial test to see if Females are more or less likely the first visitor than males ####
binom.test(3018, sum(sum_Sex_sub$freq), 0.5) # sig diff in probability that first vis is F/M

#~~~~~~~~~~~~~
### Core / residents only, for paper (used residents and nonresidents in thesis) ####
sum_status <-ddply(subset(firstvisitor, Core=='1'), c("SocialStatus"), summarise, 
                   freq=length(TRANScDate),
                   nfoxes=length(unique(ShortCode)))

sum_Sex <-ddply(subset(firstvisitor, Core=='1'), "Sex", summarise, 
                freq=length(TRANScDate),
                nfoxes=length(unique(ShortCode)))

# Exact binomial test to see if DOMINANTS are more likely the first visitor than subs ####
binom.test(2231, sum(sum_status$freq), 0.5) #2231 is N times first visitor was Dom (in sum_status)
# Sig diff in probability that first vis is Dom (probability=0.611; 95% CI=0.595:0.627; 2231/3650 trials; p<0.001)


# Exact binomial test to see if FEMALES are more or less likely the first visitor than males ####
binom.test(2613, sum(sum_Sex$freq), 0.5) #2613 is N times first visitor was F (in sum_sex)
# Sig diff in probability that first vis is F/M (probability=0.557; 95% CI=0.542:0.571; 2613/4695 trials; p<0.001)
#~~~~~~~~~~~~~



## Sep seasons - not in thesis ####

# status:
sum_statusseas <-ddply(firstvisitor, c("SocialStatus", "SeasonID"), summarise, 
                       freq=length(TRANScDate),
                       nfoxes=length(unique(ShortCode)))
# remove NAs
sum_statusseas_sub <- subset(sum_statusseas, SocialStatus!="NA")

#spring
binom.test(653, 1196, 0.5) # sig (1196=total number of trials, i.e. sum of freq for dom (653) and sub (543) in spring)
#summer
binom.test(665, 1181, 0.5) #sig
#autumn
binom.test(551, 1110, 0.5) #not sig 
#winter
binom.test(573, 1181, 0.5) #not sig

# sex
sum_sexseas <-ddply(firstvisitor, c("Sex", "SeasonID"), summarise, 
                freq=length(TRANScDate),
                nfoxes=length(unique(ShortCode)))
# remove NAs
sum_sexseas_sub <- subset(sum_sexseas, Sex!="NA")

#spring
binom.test(384, sum(384, 810), 0.5) #prob=0.322, p=2.2e-16
#summer
binom.test(443, sum(443, 736), 0.5) #prob=0.376, p=2.2e-16
#autumn
binom.test(351, sum(351, 752), 0.5) #prob=0.318, p=2.2e-16
#winter
binom.test(475, sum(475, 720), 0.5) #prob=0.397, p=1.413e-12
# females visit first in all seasons



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### BINOMIAL GLMM for first visitor: STATUS ONLY - Not including Core as a fixef - did not use this model in thesis ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aim is to determine the probability that the first visitor will be dominant, given the
# sex, season and patch attributes
library(lme4)

# Make new column with 0 and 1 for Dom vs not dom (dom=1, sub=0, unknown=NA)
firstvisitor$StatusBIN <- as.numeric(firstvisitor$SocialStatus)
firstvisitor$StatusBIN[firstvisitor$StatusBIN==2] <- 0
firstvisitor$StatusBIN <- factor(firstvisitor$StatusBIN)

# take subset without NA sex and status
fvsubset <- subset(firstvisitor, !is.na(SocialStatus) & !is.na(Sex))
write.csv(fvsubset, file="fvsubset.csv", row.names = FALSE) # saves as csv file in current working directory
#xxx import data here


# Full model
fvmod <- glmer(StatusBIN ~ 
                 DaysFedPerWeek +
                 MeanMJperFeedingDay +
                 Sex +
                 fSeason + 
                 (1|StationCode), family=binomial, data=fvsubset,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun=100000)))

# Model checking
plot(fitted(fvmod), resid(fvmod))
hist(resid(fvmod))
visreg::visreg(fvmod) 

car::Anova(fvmod) # season and var are sig.

# check for overly-influential patches: only pay attention to food variables, not sex or season
library(influence.ME) # Assess leverage (outliers with extreme x values likely to influence regression estimates)
inf <- influence(model=fvmod, count=TRUE, group="StationCode")
plot(inf, which="cook", cutoff=4/35, sort=TRUE, xlab="Cook´s Distance", ylab="StationCode") 
sigtest(inf, test=-1.96) 

# Check for multicollinearity using VIF
vif.mer(fvmod) # all <2

# Stepwise model refinement
# no MJ
fvmod_noMJ <- glmer(StatusBIN ~ 
                      DaysFedPerWeek +
                      Sex +
                      fSeason +
                      (1|StationCode), family=binomial, data=fvsubset,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=100000)))
anova(fvmod, fvmod_noMJ) # x2(1)=0.4171 , 0.5184   # DONT NEED MJ

# no DF
fvmod_noDF <- glmer(StatusBIN ~ 
                      Sex +
                      fSeason + 
                      (1|StationCode), family=binomial, data=fvsubset,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=100000)))
anova(fvmod_noMJ, fvmod_noDF) # x2(1)=1.0231,0.3118   # DONT NEED DF

# No sex
fvmod_noSex <- glmer(StatusBIN ~ 
                       fSeason + 
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))
anova(fvmod_noDF,fvmod_noSex) # x2(1)=1.0146, 0.3138    # DONT NEED SEX

# No season
fvmod_noSeas<- glmer(StatusBIN ~ 
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))

anova(fvmod_noSex, fvmod_noSeas) # x2(3)=41.482, 5.167e-09    # NEED SEASON

#~~~~~~~~
# FINAL MODEL
fvmod_final <- glmer(StatusBIN ~ 
                       fSeason + 
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))
# Model checking
plot(fitted(fvmod_final), resid(fvmod_final))
hist(resid(fvmod_final))
visreg::visreg(fvmod_final) 

a <- coef(summary(fvmod_final))

### LIKELIHOOD TESTS TO GET CHISQ TO REPORT 

# with DF
fvmod_final_withDF <- glmer(StatusBIN ~  
                              DaysFedPerWeek +
                              fSeason + 
                              (1|StationCode), family=binomial, data=fvsubset,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000)))
anova(fvmod_final, fvmod_final_withDF) # x2=0.9266,1,0.3358


# with MJ
fvmod_final_withMJ <- glmer(StatusBIN ~  
                              MeanMJperFeedingDay +
                              fSeason + 
                              (1|StationCode), family=binomial, data=fvsubset,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000)))
anova(fvmod_final, fvmod_final_withMJ) # x2=0.3819,1, 0.5366 


# With Sex
fvmod_final_withSex <- glmer(StatusBIN ~  
                               Sex +
                               fSeason + 
                               (1|StationCode), family=binomial, data=fvsubset,
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000)))
anova(fvmod_final, fvmod_final_withSex) # x2=1.0146,1,0.3138

#~~~~~~~~~~~~~~~~

### POST HOC TEST OF WHICH SEASONS WERE SIG DIFF 

library(lsmeans)
lsm1 <- lsmeans::lsmeans(fvmod_final, pairwise~fSeason, type="response")
lsm <- data.frame(summary(lsm1)$lsmeans) 
lsmcontrasts <- data.frame(summary(lsm1)$contrasts) 

# plot probability that first visitor is dominant
library(ggplot2)

ggplot(lsm, aes(x=fSeason,y=prob)) + 
  geom_point(size=4) +
  ylim(0,1) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Probability\n") +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1, width=0.2)

# save above in high resolution for publications
setwd("E:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First visitor")
dev.print(jpeg, "RPlot_lsmeans from binomial GLMM_probability first visitor is dominant by season.jpeg", res=700, height=14, width=18, units="cm") 

# View groups - diff letters mean sig diffs in probability
cld(lsm1)
library(multcomp)
a <-cld(summary(glht(fvmod_final, linfct=mcp(fSeason="Tukey")))) 
plot(a)

# plot regression estimates
coefplot2::coefplot2(fvmod_final, intercept=T) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### BINARY MODEL FOR FIRST VISITOR: WITH CORE (resident) AS PREDICTOR - USED IN THESIS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### FULL MODEL 
fvmod <- glmer(StatusBIN ~ 
                 DaysFedPerWeek +
                 MeanMJperFeedingDay +
                 Sex +
                 Core +
                 fSeason + 
                 (1|StationCode), family=binomial, data=fvsubset,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun=100000)))

# Check for multicollinearity using VIF
vif.mer(fvmod) # all <2

# Stepwise model refinement
# no MJ
fvmod_noMJ <- glmer(StatusBIN ~ 
                      DaysFedPerWeek +
                      Sex +
                      Core +
                      fSeason +
                      (1|StationCode), family=binomial, data=fvsubset,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=100000)))
anova(fvmod, fvmod_noMJ) # x2(1)=0.6477,0.4209   # DONT NEED MJ

# no DF
fvmod_noDF <- glmer(StatusBIN ~ 
                      Sex +
                      Core +
                      fSeason + 
                      (1|StationCode), family=binomial, data=fvsubset,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=100000)))
anova(fvmod_noMJ, fvmod_noDF) # x2(1)= 0.9259,0.3359   # DONT NEED DF

# No sex
fvmod_noSex <- glmer(StatusBIN ~ 
                       Core +
                       fSeason + 
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))
anova(fvmod_noDF,fvmod_noSex) # x2(1)=32.666,1,1.095e-08    # NEED SEX

# No season
fvmod_noSeas<- glmer(StatusBIN ~ 
                       Sex + 
                       Core +
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))

anova(fvmod_noDF, fvmod_noSeas) # x2(3)=24.575, 1.894e-05  # NEED SEASON

# No core
fvmod_noCore<- glmer(StatusBIN ~ 
                       Sex + 
                       fSeason +
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))

anova(fvmod_noDF, fvmod_noCore) # x2(3)=759.75, <2.2e-16  # NEED CORE

# FINAL MODEL for first visitor - status (USED IN THESIS) ####
fvmod_final <- glmer(StatusBIN ~ 
                       Sex +
                       Core +
                       fSeason + 
                       (1|StationCode), family=binomial, data=fvsubset,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))
# Model checking
plot(fitted(fvmod_final), resid(fvmod_final))
hist(resid(fvmod_final))
visreg::visreg(fvmod_final) 

a <- coef(summary(fvmod_final))

# Calculare R^2 (r-squared) value
require(MuMIn)
r.squaredGLMM(fvmod_final)  # R2m = marginal r-squared (% variance in Y explained by fixefs only), R2c = conditional r-squared (fixeds and ranefs)
# fixed effects explain 15% of variance in probability; random and fixed collectively explain 62%

#### LIKELIHOOD TESTS TO GET CHISQ TO REPORT

# with DF
fvmod_final_withDF <- glmer(StatusBIN ~  
                              DaysFedPerWeek +
                              Sex +
                              Core +
                              fSeason + 
                              (1|StationCode), family=binomial, data=fvsubset,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000)))
anova(fvmod_final, fvmod_final_withDF) # x2(1)=0.9259,0.3359


# with MJ
fvmod_final_withMJ <- glmer(StatusBIN ~  
                              MeanMJperFeedingDay +
                              Sex +
                              Core +
                              fSeason + 
                              (1|StationCode), family=binomial, data=fvsubset,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000)))
anova(fvmod_final, fvmod_final_withMJ) # x2(1)=0.5503,0.4582 # don't need MJ



# POST HOC TESTS #

library(lsmeans)
lsmcontrasts_SEAS <- lsmeans::lsmeans(fvmod_final, pairwise~fSeason, type="response")
a <- data.frame(summary(lsmcontrasts_SEAS)$contrasts)

# plot probability that first visitor is dominant
library(ggplot2)

lsm1 <- lsmeans::lsmeans(fvmod_final, ~fSeason|c(Sex,Core), type="response")
lsm <- data.frame(summary(lsm1))

lsm$CoreLONG <- ordered(lsm$Core, levels(lsm$Core)[c(2,1)], labels=c("Resident", "Non-resident"))
lsm$Sexord <- ordered(lsm$Sex, levels(lsm$Sex)[c(2,1)])

ggp <- ggplot(lsm, aes(x=fSeason, y=prob, colour=factor(Sexord))) 
# plot in colour
ggp + facet_grid(~CoreLONG) +
  ylim(0,1) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_colour_manual(name="", values = c("royalblue3","firebrick1")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Probability\n") +
  geom_point(size=4, aes(group=factor(Sexord)), position=position_dodge(0.3)) + 
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1, width=0.2,
                position=position_dodge(0.3))

# save above in high resolution for publications:
#setwd("E:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First visitor")
#dev.print(jpeg, "RPlot_lsmeans from binomial GLMM_probability first visitor is dominant by season & CORE - RESIDENT label.jpeg", res=700, height=16, width=25, units="cm") 

# plot in B&W
ggp + facet_grid(~CoreLONG) +
  ylim(0,1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
         strip.background = element_rect(color = "black", size = 1.2),
        legend.position = c(0.95,0.94),
        legend.background = element_rect(fill = NA)) +
  scale_colour_manual(name="", values = c("gray35","gray65")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Probability") +
  geom_point(size=4, aes(group=factor(Sexord)), position=position_dodge(0.3)) + 
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.3))

#dev.print(jpeg, "RPlot_lsmeans from binomial GLMM_probability first visitor is dominant by season & CORE_RESIDENT label_BW.jpeg", res=700, height=16, width=25, units="cm") 


# think it's clearer to just show the regression estimates
library(coefplot2)
snames<-c("(Intercept)", "Sex (male)", "Core (yes)", "Summer", "Autumn", "Winter")
coefplot2(fvmod_final,
          main= "Regression estimates\n\n",
          cex.var=0.8, cex.main=1, cex.pts=1, varnames=snames,
          intercept=TRUE, mar=c(1,6,5,1))   
#dev.print(jpeg, "RPlot_coefplot2 binom model DOMINANT vs. sex, season and core.jpeg", res=700, height=10, width=12, units="cm") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~ BINOMIAL FOR FIRST VISITOR: CORE vs. NOT CORE ~~~~~~~~~~~~~~~~~~ ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

corefvmod <- glmer(Core ~ 
                     DaysFedPerWeek +
                     MeanMJperFeedingDay +
                     Sex +
                     fSeason +
                     (1|StationCode), family=binomial, data=fvsubset,
                   control=glmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun=100000), 
                                        check.conv.grad=.makeCC("warning",0.05))) 

car::Anova(corefvmod) # Sex and Season are sig
summary(corefvmod)

# Model checking
plot(fitted(corefvmod), resid(corefvmod))
hist(resid(corefvmod))
visreg::visreg(corefvmod)

# Check for multicollinearity using VIF
vif.mer(corefvmod) # all <2


# LR tests for model reporting

# no DF
corefvmod_noDF <- glmer(Core ~ 
                          MeanMJperFeedingDay +
                          Sex +
                          fSeason +
                          (1|StationCode), family=binomial, data=fvsubset,
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05))) 
anova(corefvmod, corefvmod_noDF) #x2=0.6454 ,1,0.4218 # DONT NEED DF

# no MJ
corefvmod_noMJ <- glmer(Core ~ 
                          Sex +
                          fSeason +
                          (1|StationCode), family=binomial, data=fvsubset,
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05))) 
anova(corefvmod_noDF, corefvmod_noMJ) #x2=0.0631,1,0.8017 # DONT NEED MJ

# no Sex
corefvmod_noSex <- glmer(Core ~ 
                           fSeason +
                           (1|StationCode), family=binomial, data=fvsubset,
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05))) 
anova(corefvmod_noMJ, corefvmod_noSex) #x2=296.41,1,<2.2e-16 # NEED SEX

# no season
corefvmod_noSeason <- glmer(Core ~ 
                              Sex +
                              (1|StationCode), family=binomial, data=fvsubset,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000), 
                                                 check.conv.grad=.makeCC("warning",0.05))) 
anova(corefvmod_noMJ, corefvmod_noSeason) # x2=19.747,3,0.0001915

#~~~~~~~~~~~~~~~

# FINAL MODEL
corefvmodFINAL <- corefvmod_noMJ

# Save coefficients
a <- coef(summary(corefvmodFINAL))

# FINAL MODEL with MJ 
corefvmodFINAL_withMJ <- glmer(Core ~ 
                                 MeanMJperFeedingDay +
                                 Sex +
                                 fSeason +
                                 (1|StationCode), family=binomial, data=fvsubset,
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05))) 
anova(corefvmodFINAL_withMJ, corefvmodFINAL) #x2=0.0631,1,0.8017

# FINAL MODEL with DF
corefvmodFINAL_withDF <- glmer(Core ~ 
                                 DaysFedPerWeek +
                                 Sex +
                                 fSeason +
                                 (1|StationCode), family=binomial, data=fvsubset,
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05))) 
anova(corefvmodFINAL, corefvmodFINAL_withDF) # x2=0.6759,1,0.411

#~~~~~~~~~~~~~~

library(lsmeans)
lsm1 <- lsmeans::lsmeans(corefvmodFINAL, pairwise~Sex|fSeason, type="response")
lsm <- data.frame(summary(lsm1)$lsmeans) 
lsmcontrasts <- data.frame(summary(lsm1)$contrasts) 

# relevel sex to plot males first
lsm$Sex <- factor(lsm$Sex, levels(lsm$Sex)[c(2,1)]) 

# SE bars as CIs too big and looks really bad
# in colour
ggplot(lsm, aes(x=fSeason,y=prob, colour=Sex)) + 
  geom_point(size=4)  +
  ylim(0.5, 1) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_colour_manual(values = c("royalblue3","firebrick1")) +
  xlab(" ") + ylab("Probability\n") +
  geom_errorbar(aes(ymax=prob+SE, ymin=prob-SE), size=1, width=0.2) 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmeans from binomial GLMM_probability first visitor is CORE by sex & season.jpeg", res=700, height=14, width=18, units="cm") 

# B&W 
ggplot(lsm, aes(x=fSeason,y=prob, colour=Sex)) + 
  geom_point(size=4)  +
  ylim(0.5, 1.1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.93,0.89),
        legend.background = element_rect(fill = NA)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_colour_manual(values = c("gray35","gray65")) +
  xlab(" ") + ylab("Probability") +
  geom_errorbar(aes(ymax=prob+SE, ymin=prob-SE), size=1.2, width=0.2) 
# save
#dev.print(jpeg, "RPlot_lsmeans from binomial GLMM_probability first visitor is CORE by sex & season_BW2.jpeg", res=700, height=6, width=7, units="in") 




#~~~~~~~~~~~~~~~~~

# Binary model: DO FEMALES VISIT PATCHES FIRST? ####

# subset to exclude unknown sex
sex_fvsubset <- subset(firstvisitor, !is.na(Sex))

# reverse scores so 1=F and 0=M to test hypothesis that females visit first (so female=1=success and male=0=failure)
sex_fvsubset$BinSexF <- ifelse(sex_fvsubset$BinSex==1, 0, 1)

# binomial GLMM to test effect of season on probability that first visitor is male
sex_fvmod <- glmer(BinSexF ~ 
                     fSeason + 
                     (1|StationCode), family=binomial, data=sex_fvsubset,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000)))
# null model
sex_fvmod_null <- glmer(BinSexF ~ 1 + 
                     (1|StationCode), family=binomial, data=sex_fvsubset,
                   control=glmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun=100000)))

anova(sex_fvmod, sex_fvmod_null) #x2(3)=45.846, p=6.116e-10*** #sig effect of season

a<-coef(summary(sex_fvmod))

library(lsmeans)
lsm1 <- lsmeans::lsmeans(sex_fvmod, pairwise~fSeason, type="response")
lsm <- data.frame(summary(lsm1)$lsmeans) 
lsmcontrasts <- data.frame(summary(lsm1)$contrasts) # comparing diff in prob of being male between seasons

# plot probability that first visitor is male
ggplot(lsm, aes(x=fSeason,y=prob)) + 
  geom_point(size=4)  +
  ylim(0, 1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(colour="black", face="bold")) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab(" ") + ylab("Probability\n") +
  geom_errorbar(aes(ymax=prob+SE, ymin=prob-SE), size=1.2, width=0.2) 
# save
setwd("E:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First visitor")
#dev.print(jpeg, "RPlot_lsmeans from binomial GLMM_probability first visitor is female by season_BW.jpeg", res=700, height=6, width=7, units="in") 




## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~ FIRST VISITOR MODELS FOR PAPER ~~~~~~~~~~~ #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# On 06.05.2017 I realised original dataset used in thesis contained one row per patch/day for both
# the first resident and the first nonresident visitor, so some days had 2 first visitors. 
# Also need to exclude photos containing >1 adult fox, as can't tell who was first.
# If multifoxphotos contained an adult and a cub, the adult can be used as the first
# visitor, as did not consider cubs in this analysis.
# So the first visitor is the first (sub)ADULT visitor (if cubs visited first they 
# were ignored and the first adult was used)

setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First visitor")

#~~~
# To import prepared dataset (that was made using code in next section below):
RESfirstvisitor <- read.csv("CoreFirstVisitors_ExclMultiAdultPhotos_PreparedInR.CSV", header=T)
str(RESfirstvisitor) # will need to make factors before run models
#~~~


### To prepare data from scratch:

# import new corrected dataset with one first (adult) visitor per day 
# (import without converting to factor)
firstvisitor <- read.csv("CorrectedListOfFirstVisitors_withMultiAdultPhotosMarked_060517.CSV", header=T, 
                         stringsAsFactors = FALSE) # MUST IMPORT AS strings as some foxes have unknown sex and status, otherwise no matter what I do R thinks there are 3 levels (M, F and '')... WHY!!

# Replace unknown sex or status with NAs:
firstvisitor$Sex[firstvisitor$Sex  == ''] <- NA
firstvisitor$BinSex[firstvisitor$BinSex  == ''] <- NA #BinSex: 1=male, 0=female, NA=unknown
firstvisitor$SocialStatus[firstvisitor$SocialStatus  == ''] <- NA

# AND THEN convert character/string fields to factors
firstvisitor$fSeason <- factor(firstvisitor$SeasonID)
firstvisitor$fTerritory <- factor(firstvisitor$Territory)
firstvisitor$Sex <- factor(firstvisitor$Sex)
firstvisitor$BinSex <- factor(firstvisitor$BinSex)
firstvisitor$SocialStatus <- factor(firstvisitor$SocialStatus) 
firstvisitor$Core <- factor(firstvisitor$Core) # Core vs not core (core=1, not=0)


# IMPORT PATCH ATTRIBUTES
setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding")
patchdata <- read.csv("Patch feeding freq_28surveys.csv")
firstvisitor$MeanMJperFeedingDay <- patchdata[match(firstvisitor$StationID, patchdata$StationID),14] 
firstvisitor$DaysFedPerWeek <- patchdata[match(firstvisitor$StationID, patchdata$StationID),8] 

View(firstvisitor)
nrow(firstvisitor) # = 3640 provisioning days

# remove days when first visitor was MultiAdultPhoto
firstvisExclMultiAdPhotos <- subset(firstvisitor, MultiAdultPhoto==0)
nrow(firstvisExclMultiAdPhotos) # = 3300 provisioning days

# remove days when first visitor was non-resident
RESfirstvisitor <- subset(firstvisExclMultiAdPhotos, Core==1)
nrow(RESfirstvisitor) # = 3103 provisioning days

# saves as csv file in current working directory
# write.csv(RESfirstvisitor, file="CoreFirstVisitors_ExclMultiAdultPhotos_PreparedInR.csv", row.names = FALSE) 

#~~~~~~

# N days when Residents/Doms/Females were the first visitors ####
library(plyr)

# N days when residents were first visitors
sum_core <-ddply(firstvisExclMultiAdPhotos, "Core", summarise, 
                 freq=length(TRANScDate),
                 nfoxes=length(unique(ShortCode)))

sum_status <-ddply(RESfirstvisitor, c("SocialStatus"), summarise, 
                   freq=length(TRANScDate),
                   nfoxes=length(unique(ShortCode)))

sum_Sex <-ddply(RESfirstvisitor, "Sex", summarise, 
                freq=length(TRANScDate),
                nfoxes=length(unique(ShortCode)))

# Exact binomial test to see if CORE foxes are more likely the first visitor ####
binom.test(3103, sum(sum_core$freq), 0.5) #3103 is N times first visitor was Core (resident) (in sum_core)

# Exact binomial test to see if DOMINANT foxes are more likely the first visitor ####
binom.test(1819, sum(sum_status$freq), 0.5) #1819 is N times first visitor was Dom (in sum_status)
# probability first visitor was dominant = 0.586, 95% CI = 0.569-0.604, p<0.001; 1819/3103 trials.

# Exact binomial test to see if Females are more or less likely the first visitor than males ####
binom.test(2206, sum(sum_Sex_sub$freq), 0.5) # sig diff in probability that first vis is F/M
# probability first visitor was female = 0.711, 95% CI = 0.695-0.727, p<0.001; 2206/3103 trials.

#~~~~

# MODELS

# extract counts of first visitor dom/sub/m/f for paper:
RESfirstvisitor$DFr <-  round(RESfirstvisitor$DaysFedPerWeek)
d <- ddply(RESfirstvisitor, c("Sex", "SocialStatus", "DFr"), summarise,
           n = length(Sex))
d

# Make new column with 0 and 1 for Dom vs not dom (dom=1, sub=0, unknown=NA)
RESfirstvisitor$StatusBIN <- as.numeric(RESfirstvisitor$SocialStatus)
RESfirstvisitor$StatusBIN[RESfirstvisitor$StatusBIN==2] <- 0
RESfirstvisitor$StatusBIN <- factor(RESfirstvisitor$StatusBIN)

library(lme4)

# Full model
fvmod <- glmer(StatusBIN ~ 
                 DaysFedPerWeek +
                 MeanMJperFeedingDay +
                 Sex +
                 (1|StationCode), family=binomial, data=RESfirstvisitor,
               control=glmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun=100000)))

# Check for multicollinearity using VIF (function code in 'Useful Bits & Bobs' Rcode)
vif.mer(fvmod) # all <2

# Model checking
plot(fitted(fvmod), resid(fvmod))
hist(resid(fvmod))
visreg::visreg(fvmod) 

car::Anova(fvmod) # season and var are sig.

# Stepwise model refinement
# no MJ
fvmod_noMJ <- glmer(StatusBIN ~ 
                      DaysFedPerWeek +
                      Sex +
                      (1|StationCode), family=binomial, data=RESfirstvisitor,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=100000)))
anova(fvmod, fvmod_noMJ) # x2(1)=1.2074, p=0.2718   # DONT NEED MJ

# No DF
fvmod_noDF <- glmer(StatusBIN ~ 
                      Sex +
                      (1|StationCode), family=binomial, data=RESfirstvisitor,
                    control=glmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=100000)))
anova(fvmod_noMJ, fvmod_noDF)  # x2(1)=7.0771, p=0.007808    # need DF

# No sex
fvmod_noSex <- glmer(StatusBIN ~ 
                       DaysFedPerWeek +
                       (1|StationCode), family=binomial, data=RESfirstvisitor,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))
anova(fvmod_noMJ, fvmod_noSex) # x2(1)=16.641, p=4.515e-05***  # need Sex

# FINAL MODEL
fvmod_final <- glmer(StatusBIN ~ 
                       DaysFedPerWeek +
                       Sex +
                       (1|StationCode), family=binomial, data=RESfirstvisitor,
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=100000)))
# Model checking
plot(fitted(fvmod_final), resid(fvmod_final))
hist(resid(fvmod_final))
visreg::visreg(fvmod_final) 

# save coefficients
a <- coef(summary(fvmod_final))

# ranefs
summary(fvmod_final)

### LIKELIHOOD TESTS TO GET CHISQ TO REPORT 
# with MJ - can use full vs final model:
anova(fvmod_final, fvmod) # x2(1)=1.070, p=0.301 

# without DF
fvmod_final_noDF <- glmer(StatusBIN ~ 
                            Sex +
                            (1|StationCode), family=binomial, data=RESfirstvisitor,
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000)))
anova(fvmod_final, fvmod_final_noDF) # x2(1)=7.0771, p= 0.007808**

# without sex
# can use fvmod_final vs fvmod_noSex above: 
anova(fvmod_final, fvmod_noSex) # x2(1)=16.641, p=4.515e-05***



# PLOTTING probability that first visitor is dominant #

library(lsmeans)
lsm_preds <- lsmeans::lsmeans(fvmod_final, "DaysFedPerWeek", 
                              by = c("Sex"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        Sex=c("M","F")), 
                              type="response") 
lsmDF <- data.frame(summary(lsm_preds))

# rename sex for clearer plot legend
lsmDF$SexLong <- factor(lsmDF$Sex, levels = c("M", "F"), labels=c("Male", "Female")) 

library(ggplot2)

# B&W ribbons (cant use error bars as too big and overlap)
ggp <- ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, group=factor(Sex), colour=factor(Sex))) 
ggp +  # geom_point(size=5, position=position_dodge(0.3))  +
  geom_point(size=4)  +
  geom_line(size=1.1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.1,0.89),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  ylim(0,1) +
  scale_colour_manual(values = c("gray35","gray65"), name="Sex") +
  xlab("Days provisioned per week") + ylab("Probability") +
  # geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL, group=Sex), size=1.1, width=0.35, position=position_dodge(0.3)) +
  geom_ribbon(aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL, group=Sex, fill=Sex), lty=0, alpha=0.2)  +
  scale_fill_manual(values = c("gray35","gray65"), guide=F, name="") 
#dev.print(jpeg, "LSM prob firstvis is DOM_BESTMODEL_yAxis0-1.jpeg", res=700, height=18, width=20, units="cm") 

# B&W FACET - clearer, as M+F overlap a lot. # in paper
ggp + facet_grid(~Sex) +
  geom_line(size=1.2) +
  ylim(0,1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        strip.background = element_rect(color = "black", size = 1.2),
        axis.text = element_text(colour = "black")) +
  scale_colour_manual(name="", values = c("gray35","gray65"), guide=FALSE) +
  # scale_x_continuous(breaks=c(1:7)) +
  xlab("Days provisioned per week") + ylab("Probability that first visitor is dominant") +
  geom_point(size=4, aes(group=Sex), position=position_dodge(0.3)) + 
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1.2, width=0.2,
                position=position_dodge(0.3))
#dev.print(jpeg, "LSM prob firstvis is DOM_BESTMODEL_facet.jpeg", res=700, height=18, width=24, units="cm") 

# PRINT AS ABOVE BUT using special requirements for BEHECO paper (140617): ####
ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, group=factor(SexLong), colour=factor(SexLong))) +
  facet_grid(~SexLong) +
  geom_hline(yintercept=0.5, linetype="dotted", size=0.4) + # add horizontal line
  geom_line(size=1.1) +
  ylim(0,1) +
  theme_bw(base_size = 17, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        strip.text.y = element_text(size=NULL, face="bold"),
        strip.background = element_rect(color = "black", size = 1),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) +
  scale_colour_manual(name="", values = c("gray35","gray65"), guide=FALSE) +
  scale_x_continuous(breaks=c(1:7)) +
  xlab("Days provisioned per week") + ylab("Probability") +
  geom_point(size=3.5, aes(group=SexLong), position=position_dodge(0.3)) + 
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=0.6, width=0.3,
                position=position_dodge(0.3))

#dev.print(tiff, "Figure 5. 125 X 177mm_res300.tif", res=300, height=125, width=177, units="mm") 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIRST PATCH SELECTION - do foxes visit high quality patches first?   ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First patch")

### Import first patch per day for each fox   (without converting to factors)
firstpatch <- read.csv("First patch visited each day all foxes 28 surveys 130716.CSV", header=T, 
                       stringsAsFactors = FALSE) # MUST IMPORT AS strings as some foxes have unknown sex and status - otherwise no matter what I do R thinks there are 3 levels (M, F and '')... WHY!!
str(firstpatch)

#### Some foxes have unknown sex or status:
## FIRST replace Ust and Usx with NAs
firstpatch$Sex[firstpatch$Sex  == ''] <- NA
firstpatch$SocialStatus[firstpatch$SocialStatus  == ''] <- NA

## THEN make factors
firstpatch$fSeason <- factor(firstpatch$SeasonID)
firstpatch$fTerritory <- factor(firstpatch$Territory)
firstpatch$Sex <- factor(firstpatch$Sex)
firstpatch$SocialStatus <- factor(firstpatch$SocialStatus)
firstpatch$Core <- factor(firstpatch$Core) # Core vs not core (core=1, not=0)

### Add days seen
library(plyr)
daysseendata <- ddply(firstpatch, c("AnimalID", "Territory", "SeasonID"), summarise, 
                      DaysSeen = length(unique(TRANScDate)))
head(daysseendata)
daysseendata$ATS <- interaction(daysseendata$AnimalID, daysseendata$Territory, daysseendata$SeasonID)
firstpatch$ATS <- interaction(firstpatch$AnimalID, firstpatch$Territory, firstpatch$SeasonID)
firstpatch$DaysSeen <- daysseendata[match(firstpatch$ATS, daysseendata$ATS),4] 

### Add days camera was active: if not active fox couldnt visit that patch first
dayspatchactive <- read.csv(file = "g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/DaysCameraActive.csv", header = T, stringsAsFactors=FALSE) # stringsasfactors stores text as text rather than factors
firstpatch$PatchSurv <- interaction(factor(firstpatch$StationID), factor(firstpatch$SurveyID))
dayspatchactive$PatchSurv <- interaction(factor(dayspatchactive$StationID), factor(dayspatchactive$SurveyID))
# match
firstpatch$DaysActive <- dayspatchactive[match(firstpatch$PatchSurv, dayspatchactive$PatchSurv),6] 


### Add food availability data
patchdata <- read.csv("g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/Feeding/Patch feeding freq_28surveys.csv")
str(patchdata)

### TAKE SUBSET OF only core animals - non-core may have visited other patches in their own territories beforehand
firstpatchcore <- subset(firstpatch, Core==1)

### LOAD LIST OF DATES WHEN EACH PATCH WAS ACTIVE (so available to visit first) IN EACH SURVEY
patchavaildata <- read.csv(file = "g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/First patch/Patch availability - list of survey days active.csv", header = T, stringsAsFactors=FALSE) 

### MERGE WITH FIRSTPATCH DATA
firstpatchCOREmerged <- merge(x=firstpatchcore, y=patchavaildata, by = c("Territory", "TRANScDate"))

### Mark which patch was first using binary indicator: This will be the response in the models
firstpatchCOREmerged$FIRSTStationID <- firstpatchCOREmerged$StationID.x 
firstpatchCOREmerged$FIRSTStationCode <- firstpatchCOREmerged$StationCode.x
firstpatchCOREmerged$StationID.x <-  NULL
firstpatchCOREmerged$StationCode.x <-  NULL
firstpatchCOREmerged$firstpatch <- ifelse(firstpatchCOREmerged$FIRSTStationID==firstpatchCOREmerged$StationID.y, 1, 0)

# Add days fed and MJ per feeding day
firstpatchCOREmerged$DaysFedPerWeek <- patchdata[match(firstpatchCOREmerged$StationID.y,patchdata$StationID),8] 
firstpatchCOREmerged$MJperFeedingDay <- patchdata[match(firstpatchCOREmerged$StationID.y,patchdata$StationID),14] 

# Rank patches by MJ and DF in each territory and season, where 1=highest
patchdata$MJrank <- ave(patchdata$MeanMJperFeedingDay, interaction(patchdata$Territory,patchdata$SeasonID), FUN=rank) 
# add to data
firstpatchCOREmerged$MJrank <- patchdata[match(firstpatchCOREmerged$StationID.y, patchdata$StationID),16] 

# Add random effect variabe for AnimalID*Day as a measure of individual 'trials'
# as each day was more or less a random sample from all possible days an animal was aliv
firstpatchCOREmerged$AniDay <- interaction(firstpatchCOREmerged$AnimalID, firstpatchCOREmerged$TRANScDate)
firstpatchCOREmerged$StationCode <- firstpatchCOREmerged$StationCode.y


# save as csv for making data publicly available
#write.csv(firstpatchCOREmerged, file="firstpatchCOREmerged.csv", row.names = FALSE) # saves as csv file in current working directory
# XXXX import firstpatch data here


### CALC NUMBER OF DAYS WHEN FOX VISITED EACH PATCH FIRST

ndaysfirstpatch <- ddply(firstpatchCOREmerged, c("fTerritory", "fSeason", "StationID.y", 
                                                 "StationCode.y", "AnimalID", "ShortCode", 
                                                 "Sex", "SocialStatus","DaysFedPerWeek", 
                                                 "MJperFeedingDay", "MJrank", "DaysSeen"), summarise,
                         ndaysfirst = sum(firstpatch))
head(ndaysfirstpatch)

## Add N days camera was active (see code above to get 'dayspatchactive')
ndaysfirstpatch$DaysActive <- dayspatchactive[match(ndaysfirstpatch$StationID.y, dayspatchactive$StationID),6] 

## Calc min of days seen or days active
NDFP <- ddply(ndaysfirstpatch, c("fTerritory", "fSeason", "StationID.y",
                                 "StationCode.y", "AnimalID", "ShortCode", 
                                 "Sex", "SocialStatus","DaysFedPerWeek", 
                                 "MJperFeedingDay", "MJrank", "DaysSeen", 
                                 "DaysActive", "ndaysfirst"), summarise,
              minDSDA = min(DaysSeen, DaysActive))

## Use this to calc proportion of possible days when fox visited each patch first.
NDFP$propdaysfirst <- NDFP$ndaysfirst/NDFP$minDSDA

# On second thoughts the above method means proportions dont always add up to 1
NDFP$propdaysfirst <- NDFP$ndaysfirst/NDFP$DaysSeen

# Tidying
NDFP$StationID <- NDFP$StationID.y
NDFP$StationCode <- factor(NDFP$StationCode.y)
NDFP$ShortCode <- factor(NDFP$ShortCode)
NDFP$MJrank <- as.integer(NDFP$MJrank)

# NDFP contains all patches in territory

# Make alternative data frame with only patches the fox was seen in at least once during the survey:
visitdata <- read.csv(file = "g:/Bristol - PhD Camera Trapping Study/Statistics 2016/Patch visitation/MultilevelModels_PatchResTimes_NoCubs28surveys.csv", header = T, stringsAsFactors=FALSE) 
allpatches <- ddply(visitdata, c("Territory","SeasonID", "StationID","StationCode", "AnimalID", "Core"), summarise,
                    DaysSeenthispatch = length(unique(TRANScDate)))
allpatches$AniSur <- interaction(allpatches$StationID,allpatches$AnimalID)
NDFP$AniSur <- interaction(NDFP$StationID,NDFP$AnimalID)

# Add to NDFP data frame
NDFP$DaysSeenAtThisPatch <- allpatches[match(NDFP$AniSur, allpatches$AniSur),7]
NDFP$DaysSeenAtThisPatch[is.na(NDFP$DaysSeenAtThisPatch)] <- 0 # replace NAs with zeros

# subset NDFP to remove patches foxes never visited, which may not be part of their home range
NDFP_patchesinHR <- subset(NDFP, DaysSeenAtThisPatch>0)

# ~~~~~~~~~~~~~~~~~~ FINISHED DATA PREPARATION ~~~~~~~~~~~~~~~~~~~~~~~~~ ####


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FIT BINOMIAL GLMMs - All with core foxes only
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Do foxes visit the best quality patches first?
#- Expect positive correlation between probability of first patch and DF & MJ
#- Strength/direction of correlaton may differ between dominant and subordinate

### Separate models for DF and MJ, to avoid too many interactions in the model:
#  Q1: Do core foxes visit patches where food is more predictable first? (and does this effect vary with sex or status)
#  Q2: Do core foxes visit patches that supply higher energy value first? (and does this effect vary with sex or status)

# max df and mj in each survey
a <- ddply(firstpatchCOREmerged, c("fTerritory", "fSeason"), summarise,
           maxDF = max(DaysFedPerWeek),
           minDF = min(DaysFedPerWeek),
           maxMJ=max(MJperFeedingDay),
           minMJ=min(MJperFeedingDay))
# DEF NEED TERRITORY AS RANEF FOR MJ - MASSIVE VARIATION IN MIN/MAX MJ (and min/max DF) BETWEEN TERRITORIES



### NOTES TO EXPLAIN WHY I RAN THESE PARTICULAR MODEL STRUCTURES:

## 1. Tried binary for whether each patch was visited first or not (0/1) for each fox 
# on each day (tried MJ, DF and their ranks) - DIDN'T WORK

## 2. Considered using DF or MJ as responses in separate models, but they had weird
# distributions and Territory ranefs wouldn't accout for the huge between-territory 
# differences in both DF and MJ (min+max)
# Plus, this wouldn't answer my research question. I wasnt to know whether foxes are 
# MORE LIKELY to visit patches with high food availability, which can be inferred 
# from HOW OFTEN they visited the patch first (out of total days they could visit it
# first, so based on DaysSeen and DaysActive (=available))

## 3. Decided to model proportion of total possible days that each fox visited each patch first
# data frame has 1 row per fox per patch per day, but only for days when the fox was 
# seen and the patch was available.


#~~~~~~~~~~~~~~~~~ MODELS for FIRST PATCH SELECTION ~~~~~~~~~~~~~~~~~####

# TO FIT BINOMIAL MODEL WITH PROPORTION DATA (rather than binary 0/1) must include
# weights= argument in the model to specify the number of trials used to generate each proportion
# E.g. here I used the min of DaysSeen (for sighting history) and DaysActive (for cameras)

# Check distribution
library(fitdistrplus) 
descdist(firstpatchCOREmerged$firstpatch, discrete=T, boot=500) 

#### FIRST PATCH SELECTION: FEEDING FREQUENCY ####

# Binomial models
# LOGIT
FPmodDF_FULL <- glmer(firstpatch ~ 
                        DaysFedPerWeek +
                        DaysFedPerWeek:Sex +
                        DaysFedPerWeek:SocialStatus +
                        (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                      data=firstpatchCOREmerged, family=binomial(link="logit"), 
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=100000), 
                                           check.conv.grad=.makeCC("warning",0.05)))  
# Check for overdispersion first
deviance(FPmodDF_FULL)/df.residual(FPmodDF_FULL) # 0.75 (should be ~1; >1 indicates overdispersion)
library("blmeco") 
dispersion_glmer(FPmodDF_FULL) # 0.87 (should be between 0.75 and 1.4)

# Check for multicollinearity using VIF
vif.mer(FPmodDF_FULL) # all <2

# Can't tell much from binomial model residuals - just need line to be approx flat 
plot(fitted(FPmodDF_FULL), resid(FPmodDF_FULL))
lines(smooth.spline(fitted(FPmodDF_FULL), resid(FPmodDF_FULL)))
lines(lowess(fitted(FPmodDF_FULL), resid(FPmodDF_FULL)))
plot(FPmodDF_FULL, type=c("p","smooth"))


# PROBIT
FPmodDF_FULL_probit <- glmer(firstpatch ~ 
                               DaysFedPerWeek +
                               DaysFedPerWeek:Sex +
                               DaysFedPerWeek:SocialStatus +
                               (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode),
                             data=firstpatchCOREmerged, family=binomial(link="probit"), 
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000),
                                                  check.conv.grad=.makeCC("warning",0.05)))  
# Check for multicollinearity using VIF
vif.mer(FPmodDF_FULL_probit) # all <2

# COMPARE MODEL FITS WITH LOGIT AND PROBIT LINKS
bbmle::ICtab(FPmodDF_FULL, FPmodDF_FULL_probit) # compare likelihood ratios (AIC)
deviance(FPmodDF_FULL) 
deviance(FPmodDF_FULL_probit) # deviance is lower for probit, indicating better fit.

deviance(FPmodDF_FULL_probit)/df.residual(FPmodDF_FULL_probit) # 0.75
dispersion_glmer(FPmodDF_FULL_probit) # 0.87

plot(fitted(FPmodDF_FULL_probit), resid(FPmodDF_FULL_probit))
lines(smooth.spline(fitted(FPmodDF_FULL_probit), resid(FPmodDF_FULL_probit)))
lines(lowess(fitted(FPmodDF_FULL_probit), resid(FPmodDF_FULL_probit)))
plot(FPmodDF_FULL_probit, type=c("p","smooth"))


# Stepwise model refinement
FPmodDFprobit_noSex <- glmer(firstpatch ~
                               DaysFedPerWeek +
                               DaysFedPerWeek:SocialStatus +
                               (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                             data=firstpatchCOREmerged, family=binomial(link="probit"), 
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05)))  
anova(FPmodDF_FULL_probit, FPmodDFprobit_noSex) # x2(1)=6.7523, p=0.009363 # need sex interaction


FPmodDFprobit_noStatus <- glmer(firstpatch ~
                                  DaysFedPerWeek +
                                  DaysFedPerWeek:Sex +
                                  (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                                data=firstpatchCOREmerged, family=binomial(link="probit"), 
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl = list(maxfun=100000), 
                                                     check.conv.grad=.makeCC("warning",0.05))) 
anova(FPmodDF_FULL_probit, FPmodDFprobit_noStatus) # x2(1)=0.2935, p=0.588 # don't need status

# FINAL MODEL
FPmodDFprobit_FINAL <- FPmodDFprobit_noStatus

# Model checking
deviance(FPmodDFprobit_FINAL)/df.residual(FPmodDFprobit_FINAL) # 0.75
dispersion_glmer(FPmodDFprobit_FINAL) # 0.87

plot(fitted(FPmodDFprobit_FINAL), resid(FPmodDFprobit_FINAL))
lines(smooth.spline(fitted(FPmodDFprobit_FINAL), resid(FPmodDFprobit_FINAL)))
plot(FPmodDFprobit_FINAL, type=c("p","smooth")) # should be approx flat. Looks OK.

# Save model coefficients
a <- coef(summary(FPmodDFprobit_FINAL))

### LR tests to report chisq values:
# No sex
FPmodDFprobit_FINAL_nosexint <- glmer(firstpatch ~
                                  DaysFedPerWeek +
                                  (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                                data=firstpatchCOREmerged, family=binomial(link="probit"), 
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl = list(maxfun=100000), 
                                                     check.conv.grad=.makeCC("warning",0.05))) 
anova(FPmodDFprobit_FINAL, FPmodDFprobit_FINAL_nosexint) # x2(1) = 6.9803, p=0.008241

# No days fed per week
FPmodDFprobit_noDF <- glmer(firstpatch ~ Sex +
                                  (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                                data=firstpatchCOREmerged, family=binomial(link="probit"), 
                                control=glmerControl(optimizer="bobyqa",
                                                     optCtrl = list(maxfun=100000), 
                                                     check.conv.grad=.makeCC("warning",0.05))) 
anova(FPmodDFprobit_FINAL, FPmodDFprobit_noDF) # x2(1)=17.544, p=2.808e-05
# not sure how much sense this makes as didnt include main effect of sex in the original model, only the interaction
# don't report

# CONCLUSION: FOXES ARE MORE LIKELY TO VISIT PATCHES WITH HIGHER FEEDING FREQUENCY FIRST.
# & THIS EFFECT WAS STRONGER FOR FEMALES than males. 

# Can't tell much from binomial model residuals - just need line to be approx flat # Check for multicollinearity using VIF
vif.mer(FPmodDFprobit_FINAL) # all <2

#### PLOTTING FIRST PATCH ~ FEEDING FREQUENCY ####
library(lsmeans)

lsm_preds <- lsmeans::lsmeans(FPmodDFprobit_FINAL, "DaysFedPerWeek", 
                              by = c("Sex"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        Sex=c("M","F")), 
                              type="response") # to backtransform from the probit scale
lsmDF <- data.frame(summary(lsm_preds))


library(ggplot2)
ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, group=factor(Sex), colour=factor(Sex))) + 
  geom_point(size=4, position=position_dodge(0.2))  +
  geom_line(size=1) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_colour_manual(values = c("royalblue3","firebrick1"), name="Sex") +
  xlab("Days fed per week") + ylab("Probability\n") +
geom_ribbon(aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL, group=Sex, fill=Sex), 
            lty=0, alpha=0.2)  +
scale_fill_manual(values = c("royalblue3","firebrick1"), guide=F, name="") 
# optional error bar instead of ribbon
geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1, width=0.3, position=position_dodge(0.2)) 

# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmeans from binom GLMM_prob of visiting patch first - daysfedXsex_ribbon.jpeg", 
          res=700, height=6, width=8, units="in") 

# plot in B&W
ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, group=factor(Sex), colour=factor(Sex))) + 
  #geom_point(size=4, position=position_dodge(0.2))  +
  geom_point(size=5)  +
  geom_line(size=1) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.1,0.89),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_colour_manual(values = c("gray35","gray65"), name="Sex") +
  xlab("Days provisioned per week") + ylab("Probability") +
 # geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL, group=Sex), size=1, width=0.15, position=position_dodge(0.2)) +
  geom_ribbon(aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL, group=Sex, fill=Sex), lty=0, alpha=0.2)  +
  scale_fill_manual(values = c("gray35","gray65"), guide=F, name="") 
# save
dev.print(jpeg, "RPlot_lsmeans from binom GLMM_prob of visiting patch first - daysfedXsex_ribbon_BW.jpeg", res=700, height=6, width=7, units="in") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### ~~~~~~~~~~ FIRST PATCH SELECTION: MJ PER DAY RANKED ~~~~~~~~~~~ #### - in thesis; combined with DF in paper

# rescale MJrank so 1 (best patch) is zero, so will be included in the intercept.
firstpatchCOREmerged$MJrankadj <- firstpatchCOREmerged$MJrank-1

# binomial model
# LOGIT
FPmodMJ_FULL_logit <- glmer(firstpatch ~ 
                        MJrankadj +
                        MJrankadj:Sex +
                        MJrankadj:SocialStatus +
                        (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                      data=firstpatchCOREmerged, family=binomial(link="logit"), 
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=100000), 
                                           check.conv.grad=.makeCC("warning",0.05)))  
# PROBIT
FPmodMJ_FULL_probit <- glmer(firstpatch ~ 
                               MJrankadj +
                               MJrankadj:Sex +
                               MJrankadj:SocialStatus +
                        (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                      data=firstpatchCOREmerged, family=binomial(link="probit"), 
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=100000), 
                                           check.conv.grad=.makeCC("warning",0.05)))

# COMPARE MODEL FITS WITH LOGIT AND PROBIT LINKS
bbmle::ICtab(FPmodMJ_FULL_logit, FPmodMJ_FULL_probit) # compare likelihood ratios (AIC)
deviance(FPmodMJ_FULL_logit) 
deviance(FPmodMJ_FULL_probit) # deviance is lower for probit, indicating better fit.

# Check for overdispersion first
deviance(FPmodMJ_FULL_probit)/df.residual(FPmodMJ_FULL_probit) # 0.75 (should be ~1; >1 indicates overdispersion)
library("blmeco") 
dispersion_glmer(FPmodMJ_FULL_probit) # 0.87 (should be between 0.75 and 1.4)

# Check for multicollinearity using VIF
vif.mer(FPmodMJ_FULL_logit) # all <3 # slightly higher than probit model
vif.mer(FPmodMJ_FULL_probit) # all <3

# Can't tell much from binomial model residuals - just need line to be approx flat
plot(fitted(FPmodMJ_FULL_probit), resid(FPmodMJ_FULL_probit))
lines(smooth.spline(fitted(FPmodMJ_FULL_probit), resid(FPmodMJ_FULL_probit)))
lines(lowess(fitted(FPmodMJ_FULL_probit), resid(FPmodMJ_FULL_probit)))
plot(FPmodMJ_FULL_probit, type=c("p","smooth"))

# STEPWISE MODEL REFINEMENT
FPmodMJ_noSex <- glmer(firstpatch ~ 
                         MJrankadj +
                         MJrankadj:SocialStatus +
                               (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                             data=firstpatchCOREmerged, family=binomial(link="probit"), 
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=100000), 
                                                  check.conv.grad=.makeCC("warning",0.05)))
anova(FPmodMJ_FULL_probit, FPmodMJ_noSex) # x2(1)=0.0379, p=0.8455 # dont need sex


FPmodMJ_noStatus <- glmer(firstpatch ~ 
                              MJrankadj +
                              (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                            data=firstpatchCOREmerged, family=binomial(link="probit"), 
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=100000), 
                                                 check.conv.grad=.makeCC("warning",0.05)))
anova(FPmodMJ_noSex, FPmodMJ_noStatus) # x2(1)=0.7185, p=0.3966 # dont need status


FPmodMJ_null <- glmer(firstpatch ~ 1 +
                                 (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                               data=firstpatchCOREmerged, family=binomial(link="probit"), 
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05)))
anova(FPmodMJ_noStatus, FPmodMJ_null) # x2(1)=18.76, p=1.482e-05 ***

# FINAL MODEL
FPmodMJ_FINAL <- FPmodMJ_noStatus

a <- coef(summary(FPmodMJ_FINAL))


# LR TESTS TO GET CHISQ VALUES TO REPORT
# with MJrank*sex
FPmodMJ_FINAL_withSex <- glmer(firstpatch ~ 
                            MJrankadj +
                            MJrankadj:Sex +
                            (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                          data=firstpatchCOREmerged, family=binomial(link="probit"), 
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=100000), 
                                               check.conv.grad=.makeCC("warning",0.05)))
anova(FPmodMJ_FINAL, FPmodMJ_FINAL_withSex) # x2(1) = 0.0113, p=0.9152

## CONCLUSION: FOXES ARE MORE LIKELY TO VISIT PATCHES FIRST IF THEY PROVIDE GREATER QUANTITIES OF FOOD


#### PLOTTING FIRST PATCH ~ MJ PER DAY RANKED ####

library(lsmeans)
lsm_preds <- lsmeans::lsmeans(FPmodMJ_FINAL, "MJrankadj", 
                              at = list(MJrankadj = c(0:3)), 
                              type="response") # to backtransform from the probit scale
lsmMJ <- data.frame(summary(lsm_preds))

library(ggplot2)
ggplot(lsmMJ, aes(x=MJrankadj,y=prob)) + 
  geom_point(size=4)  +
  geom_line(size=1) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_x_continuous(breaks=c(0:3), labels=c(1:4)) +
  xlab("\nRank of MJ per provisioning day") + ylab("Probability\n") +
geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1, width=0.15) 

# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmeans from binom GLMM_prob of visiting patch first - MJrank.jpeg", 
          res=700, height=6, width=8, units="in") 



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~ FIRST PATCH MODELS FOR PAPER ~~~~~~~~~~~ #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 24/03/2017: Fitted DF and MJrank into the same model to see if can reduce total number of models
# presented:

# logit
FPmodDFMJ_FULL_logit <- glmer(firstpatch ~ 
                                DaysFedPerWeek +
                                DaysFedPerWeek:Sex +
                                DaysFedPerWeek:SocialStatus +
                                MJrankadj +
                                MJrankadj:Sex +
                                MJrankadj:SocialStatus +
                                (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                              data=firstpatchCOREmerged, family=binomial(link="logit"), 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=100000), 
                                                   check.conv.grad=.makeCC("warning",0.05)))  
summary(FPmodDF_FULL_logit)

#probit
FPmodDFMJ_FULL_probit <- glmer(firstpatch ~ 
                                 DaysFedPerWeek +
                                 DaysFedPerWeek:Sex +
                                 DaysFedPerWeek:SocialStatus +
                                 MJrankadj +
                                 MJrankadj:Sex +
                                 MJrankadj:SocialStatus +
                                 (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                               data=firstpatchCOREmerged, family=binomial(link="probit"), 
                               control=glmerControl(optimizer="bobyqa",
                                                    optCtrl = list(maxfun=100000), 
                                                    check.conv.grad=.makeCC("warning",0.05))) 
summary(FPmodDFMJ_FULL_probit)


# COMPARE MODEL FITS WITH LOGIT AND PROBIT LINKS
library(bbmle)
bbmle::ICtab(FPmodDFMJ_FULL_logit, FPmodDFMJ_FULL_probit) # compare likelihood ratios (AIC) - lower for probit
deviance(FPmodDFMJ_FULL_logit) 
deviance(FPmodDFMJ_FULL_probit) # deviance is lower for probit, indicating better fit.

# check probit model for overdispersion
deviance(FPmodDFMJ_FULL_probit)/df.residual(FPmodDFMJ_FULL_probit) # 0.75 (should be ~1; >1 indicates overdispersion)
library("blmeco") 
dispersion_glmer(FPmodDFMJ_FULL_probit) # 0.87 (should be between 0.75 and 1.4)
# = OK


# Dharma checks
library(DHARMa)
sim <- simulateResiduals(FPmodDFMJ_FULL_probit)
plotSimulatedResiduals(sim, quantreg=T) # qqplot should be straight and so should percentile lines on RHS plot
# LOOKS PERFECT!


plot(fitted(FPmodDFMJ_FULL_probit), resid(FPmodDFMJ_FULL_probit))
lines(smooth.spline(fitted(FPmodDFMJ_FULL_probit), resid(FPmodDFMJ_FULL_probit)))
lines(lowess(fitted(FPmodDFMJ_FULL_probit), resid(FPmodDFMJ_FULL_probit)))
plot(FPmodDFMJ_FULL_probit, type=c("p","smooth"))
# = OK, but binomial resids aren't assumed normal so use DHARMa checks instead.

# Check for multicollinearity using VIF
vif.mer(FPmodDFMJ_FULL_probit) # all <2

# Stepwise model refinement
summary(FPmodDFMJ_FULL_probit)

# without DaysFedPerWeek:SocialStatus interaction
FPmodDFMJ_noStatusDF <- glmer(firstpatch ~ 
                                DaysFedPerWeek +
                                DaysFedPerWeek:Sex +
                                MJrankadj +
                                MJrankadj:Sex +
                                MJrankadj:SocialStatus +
                                (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                              data=firstpatchCOREmerged, family=binomial(link="probit"), 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=100000), 
                                                   check.conv.grad=.makeCC("warning",0.05))) 

anova(FPmodDFMJ_FULL_probit, FPmodDFMJ_noStatusDF) # x2(1)=0.029, p=0.866 # don't need status:DF

summary(FPmodDFMJ_noStatusDF)

# without MJrankadj:SocialStatusSub interaction
FPmodDFMJ_noStatusMJ <- glmer(firstpatch ~ 
                                DaysFedPerWeek +
                                DaysFedPerWeek:Sex +
                                MJrankadj +
                                MJrankadj:Sex +
                                (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                              data=firstpatchCOREmerged, family=binomial(link="probit"), 
                              control=glmerControl(optimizer="bobyqa",
                                                   optCtrl = list(maxfun=100000), 
                                                   check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_noStatusDF, FPmodDFMJ_noStatusMJ) # x2(1)=0.932, p=0.335 # don't need status:MJ

summary(FPmodDFMJ_noStatusMJ)

# without SexM:MJrankadj interaction
FPmodDFMJ_noSexMJ <- glmer(firstpatch ~ 
                             DaysFedPerWeek +
                             DaysFedPerWeek:Sex +
                             MJrankadj +
                             (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                           data=firstpatchCOREmerged, family=binomial(link="probit"), 
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_noStatusMJ, FPmodDFMJ_noSexMJ) # x2(1)=2.203, p=0.138 # don't need sex:MJ

summary(FPmodDFMJ_noSexMJ)

# without DaysFedPerWeek:SexM interaction
FPmodDFMJ_noSexDF <- glmer(firstpatch ~ 
                             DaysFedPerWeek +
                             MJrankadj +
                             (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                           data=firstpatchCOREmerged, family=binomial(link="probit"), 
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=100000), 
                                                check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_noSexMJ, FPmodDFMJ_noSexDF) # x2(1)=7.1104, p=0.007664** # NEED sex:DF

# without MJrankadj
FPmodDFMJ_noMJ <- glmer(firstpatch ~ 
                          DaysFedPerWeek +
                          DaysFedPerWeek:Sex +
                          (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                        data=firstpatchCOREmerged, family=binomial(link="probit"), 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=100000), 
                                             check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_noSexMJ, FPmodDFMJ_noMJ) # x2(1)=22.489, p=2.113e-06*** # need MJ

# FINAL MODEL
FPmodDFMJ_FINAL <- glmer(firstpatch ~ 
                           DaysFedPerWeek +
                           DaysFedPerWeek:Sex +
                           MJrankadj +
                           (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                         data=firstpatchCOREmerged, family=binomial(link="probit"), 
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=100000), 
                                              check.conv.grad=.makeCC("warning",0.05)))

# Model checking
deviance(FPmodDFMJ_FINAL)/df.residual(FPmodDFMJ_FINAL) # 0.75
dispersion_glmer(FPmodDFMJ_FINAL) # 0.87

plot(fitted(FPmodDFMJ_FINAL), resid(FPmodDFMJ_FINAL))
lines(smooth.spline(fitted(FPmodDFMJ_FINAL), resid(FPmodDFMJ_FINAL)))
plot(FPmodDFMJ_FINAL, type=c("p","smooth")) # Can't tell much from binomial resids, just need line to be approx flat. 
# Looks OK

# Check for multicollinearity using VIF
vif.mer(FPmodDFMJ_FINAL) # all <2

# calculate R-squared
library(MuMIn)
r.squaredGLMM(FPmodDFMJ_FINAL) # fixed effects only explain 1% variance...

# Save model coefficients
a <- coef(summary(FPmodDFMJ_FINAL))
summary(FPmodDFMJ_FINAL)


## LR tests to quote non-sig stuff in model table:
# with MJ:sex
FPmodDFMJ_FINAL_plusSexMJ <- glmer(firstpatch ~ 
                                     DaysFedPerWeek +
                                     DaysFedPerWeek:Sex +
                                     MJrankadj +
                                     MJrankadj:Sex +
                                     (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                                   data=firstpatchCOREmerged, family=binomial(link="probit"), 
                                   control=glmerControl(optimizer="bobyqa",
                                                        optCtrl = list(maxfun=100000), 
                                                        check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_FINAL, FPmodDFMJ_FINAL_plusSexMJ) # x2(1)=2.203, p=0.138

# with MJ:status
FPmodDFMJ_FINAL_plusStatusMJ <- glmer(firstpatch ~ 
                                        DaysFedPerWeek +
                                        DaysFedPerWeek:Sex +
                                        MJrankadj +
                                        MJrankadj:SocialStatus +
                                        (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                                      data=firstpatchCOREmerged, family=binomial(link="probit"), 
                                      control=glmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun=100000), 
                                                           check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_FINAL, FPmodDFMJ_FINAL_plusStatusMJ) # x2(1)=0.606, p=0.436

# with DF:status
FPmodDFMJ_FINAL_plusStatusDF <- glmer(firstpatch ~ 
                                        DaysFedPerWeek +
                                        DaysFedPerWeek:Sex +
                                        MJrankadj +
                                        DaysFedPerWeek:SocialStatus +
                                        (1|StationCode) + (1|ShortCode) + (1|StationCode:ShortCode), 
                                      data=firstpatchCOREmerged, family=binomial(link="probit"), 
                                      control=glmerControl(optimizer="bobyqa",
                                                           optCtrl = list(maxfun=100000), 
                                                           check.conv.grad=.makeCC("warning",0.05)))

anova(FPmodDFMJ_FINAL, FPmodDFMJ_FINAL_plusStatusDF) # x2(1)=0.289, p=0.591


#### PLOTTING FIRST PATCH ~ MJ PER DAY RANKED #### 

# MJ
lsm_preds <- lsmeans::lsmeans(FPmodDFMJ_FINAL, "MJrankadj", 
                              at = list(MJrankadj = c(0:3)), 
                              type="response") # to backtransform from the probit scale
lsmMJ <- data.frame(summary(lsm_preds))

# DF
lsm_preds <- lsmeans::lsmeans(FPmodDFMJ_FINAL, "DaysFedPerWeek", 
                              by = c("Sex"),
                              at = list(DaysFedPerWeek = seq(1,7,1), 
                                        Sex=c("M","F")), 
                              type="response") # to backtransform from the probit scale
lsmDF <- data.frame(summary(lsm_preds))

# MJ
# with MJ rank re-labelled from 0-3 to 1-4
ggplot(lsmMJ, aes(x=MJrankadj,y=prob)) + 
  geom_bar(stat="identity", colour="black", fill="gray80", width=0.8)  +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(0:3), labels=c(1:4)) +
  xlab("Rank of MJ per provisioning day") + ylab("Probability") +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1, width=0.15) 
# save above in high resolution for publications
#dev.print(jpeg, "RPlot_lsmeans prob of visiting patch first_MJ_CORE_combi model_bar.jpeg", res=700, height=6, width=7, units="in") 

# with MJ labelled 0-3 as was entered into model
# MJ
ggplot(lsmMJ, aes(x=MJrankadj,y=prob)) + 
  geom_bar(stat="identity", colour="black", fill="gray80", width=0.8)  +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(0:3)) +
  xlab("Rank of MJ per provisioning day") + ylab("Probability") +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=1, width=0.15) 
# save above in high resolution for publications
dev.print(jpeg, "RPlot_lsm prob of visiting patch first_MJCORE_combimod_0-3Xaxis_better.jpeg", res=700, height=6, width=7, units="in") 


# DF
ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, colour=Sex, fill=Sex, shape=Sex)) + 
  geom_point(size=5)  +
  geom_line(size=1.2) +
  theme_bw(base_size = 22, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.2),
        strip.text.y = element_text(size=NULL, face="bold"),
        legend.position = c(0.1,0.89),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black")) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(breaks=c(0.05,0.10,0.15,0.20,0.25)) +
  scale_colour_manual(values = c("gray25","gray45"), name="Sex") +
  xlab("Days provisioned per week") + ylab("Probability") +
  geom_ribbon(aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL, fill=Sex), 
              lty=0, alpha=0.15)  +
  scale_fill_manual(values = c("gray15","gray45"), name="Sex") 
# save
#dev.print(jpeg, "RPlot_lsmeans prob of visiting patch first_Sex X DF_CORE_combi model_shapes.jpeg", res=700, height=6, width=7, units="in") 

#---

# Print the two plots above but using special requirements for BEHECO paper (140617):
setwd("G:/Presentations, Posters & Publications/Papers for publication/Accepted/Beh Ecol Nov 2016 patch use paper/Final draft to submit/Figures")

# DF
# rename sex for clearer plot legend
lsmDF$SexLong <- factor(lsmDF$Sex, levels = c("M", "F"), labels=c("Male", "Female")) 

g1 <- ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, colour=SexLong, fill=SexLong, shape=SexLong)) + 
  geom_point(size=3)  +
  geom_line(size=1.1) +
  theme_bw(base_size = 15, base_family = "") +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        legend.position = c(0.2,0.91),
        legend.background = element_rect(fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y= element_text(size=14),
        plot.title = element_text(size=14),
        legend.title = element_blank()) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(breaks=c(0.05,0.10,0.15,0.20,0.25)) +
  scale_colour_manual(values = c("gray25","gray45"), name="SexLong") +
  xlab("Days provisioned per week") + ylab("Probability") +
  geom_ribbon(aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL, fill=SexLong), 
              lty=0, alpha=0.15)  +
  scale_fill_manual(values = c("gray15","gray45"), name="SexLong") +
  ggtitle("(a)") 

# bar plot of MJrank
g2 <- ggplot(lsmMJ, aes(x=MJrankadj,y=prob)) + 
  geom_bar(stat="identity", colour="black", fill="gray80", width=0.8)  +
  theme_bw(base_size = 15, base_family = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1),
        axis.text = element_text(colour = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y= element_text(size=14),
        plot.title = element_text(size=14)) +
  scale_x_continuous(breaks=c(0:3)) +
  xlab("Rank of MJ per provisioning day") + ylab("Probability") +
  geom_errorbar(aes(ymax=asymp.LCL, ymin=asymp.UCL), size=0.6, width=0.3)  +
  ggtitle("(b)") 

# plot the 2 ggplots side by side on a panel (as can't use par() command for ggplots)
library(gridExtra)
grid.arrange(g1, g2, nrow=NULL, ncol=2) # where ggp is ggplot2 code for each plot

#dev.print(tiff, "Figure 3. 110 x 177mm_res300.tif", res=300, height=110, width=177, units="mm") 
