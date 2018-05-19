
rm(list=ls()) #to clear the R workspace

#
##
###
####
###
##
#
#### ARE MORE PHOTOS IDENTIFIABLE WHEN A GREATER PROPORTION OF FOXES IN SURVEY ARE TAGGED? ####
#
##
###
####
###
##
#

# Based on this paper:
# Cribari-Neto, F. & Zeileis, A. (2010) Beta regression in R. Journal of Statistical Software, 34.


setwd("g:/Statistics 2016/Methods chapter - stats and plots/Methods - Identifying individuals")
mydata <- read.table(file = "NUnidPhotos_NTaggedFoxes.txt", sep='\t', header = TRUE) 
mydata <- read.csv(file.choose()) # navigate to file
head(mydata)
str(mydata)

# take subset with just AllVisitors
mydataAllVis <- subset(mydata, FoxTypeIncl=="AllVisitors")
head(mydataAllVis)
str(mydataAllVis)

#
##
###
####
###
##
#

#### CORRELATION TEST ####

# check distrib of each variable that might be correlated:
hist(mydataAllVis$PropUnidPhotos)
require(car)
require(MASS)
qqp(mydataAllVis$PropUnidPhotos, "norm", main="normal") # all data points fall within normal range

hist(mydataAllVis$PropFoxesTagged)
require(car)
require(MASS)
qqp(mydataAllVis$PropFoxesTagged, "norm", main="normal") # ISH...

# Pearson correlation test: if p<0.05 the correlation is sig. diff. from zero
cor.test(mydataAllVis$PropUnidPhotos, mydataAllVis$PropFoxesTagged,
         alternative="two.sided", method = "pears", exact = T, conf.level = 0.95) # not significant
# alternative="two.sided" means null hypothesis is that PropUnidPhotos is correlated with PropFoxesTagged
# cor shows the strength and direction of the correlation (cor=0.5-1 is strong)

# Report as cor(df-2)=XXX, p=XXX; "cor(26)=-0.061, p=0.757"
# where df=number of pairs, so min length of the two variables being tested - calc by:
min(length(mydataAllVis$PropUnidPhotos), length(mydataAllVis$PropFoxesTagged))-2 # -2 as used two tailed test

#
##
###
####
###
##
#


### Calculate N and proportion of identified photos and save as new column
mydataAllVis$NidPhotos <- mydataAllVis$Total.photos-mydataAllVis$NUnidPhotos
mydataAllVis$PropIDphotos <- mydataAllVis$NidPhotos/mydataAllVis$Total.photos 


### BASIC LINEAR REGRESSION (doesnt make sense for proportions as lines not limited 0-1 (see below) ###
newmod <- lm(PropIDphotos ~ PropFoxesTagged, data=mydataAllVis)
mod <- lm(mydataAllVis$PropIDphotos~mydataAllVis$PropFoxesTagged)

# calc predictions + confidence interval
newx <- seq(min(mydataAllVis$PropFoxesTagged), max(mydataAllVis$PropFoxesTagged), length.out=28)
preds <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'confidence')

#  raw data
plot(PropIDphotos~PropFoxesTagged, data=mydataAllVis, xlab="Proportion of foxes tagged",
     ylab="Proportion of photos identified")
# add linear regression line
abline(mod)
# add CI lines
lines(newx, preds[ ,3], lty = 'dashed', col = 'red') # upper CI
lines(newx, preds[ ,2], lty = 'dashed', col = 'red') # lower CI
# line looks a bit curved

dev.print(jpeg, "Rplot_linear regression model predicted PropIDPhotos with increasing PropTaggedFoxes.jpeg", res=700, 
          height=6, width=7, units="in") # save as jpeg


#### Check for nonlinearity ####

# PLOT WITH LOWESS LINE
plot(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged);lines(lowess(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged),col="red")
# then compare a linear model with a quadratic model using an F-test:
model.lin<-lm(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged)
model.quad<-lm(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged+I(mydataAllVis$PropFoxesTagged^2))
anova(model.lin,model.quad) # no sig diff between the two, suggesting relationship is not nonlinear


### AIM: RUN GLM TO TEST WHETHER HAVING A GREATER PROPORTION OF TAGGED FOXES ON A TERRITORY
# INCREASES THE PROBABILITY THAT A PHOTO IS IDENTIFIED

# It doesn't make statistical sense to run a LINEAR REGRESSION on proportions, as predictions 
# from straight lines could include values <0 or >1, which are impossible in proportion data.

# Instead use a BINOMIAL GLM with LOGIT link: 

# binomial models for proportion data (logistic regression) require a response that is either:
# a vector, with 0 or 1 for each row (photo) to show whether it was identified or not,
# or a matrix, with a column of N successes and a column of N failures 
# use matrix if only have counts of successes/failures and not separate rows with a binary 0/1 score for each record, 
# e.g. showing which individual photos had visible tags/not

#
##
###
####
###
##
#

#### Studentized Breusch-Pagan test #### 

# to test for heteroskedasticity in a LM: low p means it is homoskedastic, high p = heteroskedastic
# i.e. if p<0.05, reject null hypothesis that resid variance is constant and infer that heteroscedasticity is present
library("lmtest")
bptest(newmod)# p=0.160 so there is heteroskedasticity
bptest(newmod, studentize = FALSE) # non-studentised Breusch-Pagan test. This is same as Non-constant Variance Score Test:
car::ncvTest(newmod) # Non-constant Variance Score Test for heteroskedasticity

summary(newmod)

# plot(newmod) plots plotted manually:
par(mfrow=c(2,2))
plot(fitted(newmod), resid(newmod))
lines(smooth.spline(fitted(newmod), resid(newmod))) # def looks heteroskedastic
lines(lowess(fitted(newmod), resid(newmod)), col="blue")
# or automatically:
plot(newmod)

#
##
###
####
###
##
#

#### LOGISTIC REGRESSION ####

# Use cbind() as the response in model to generate a response matrix:
logreg1.result <- glm(cbind(mydataAllVis$NUnidPhotos, mydataAllVis$NidPhotos) ~
                        PropFoxesTagged, family="binomial"(link="logit"), data=mydataAllVis)
summary(logreg1.result)# significant but v.poor fit as residual deviance is much larger (>3X) the resid df (Innes notes 'From Reg to Generalised Reg')
par(mfrow=c(2,2))
plot(logreg1.result) # residuals look bad too - some influential values (high leverage)

#
##
###
####
###
##
#

#### BETA REGRESSION ####
# The Beta distribution is the distribution of continuous proportions
# So beta reg is appropriate if response is continuous between 0-1 (but not including absolute zero or one)
library(betareg)

# formula to run a beta regression model:
betareg(formula, data, subset, na.action, weights, offset, # standard model-frame specifications (see
        link = "logit", link.phi = NULL, control = betareg.control(...), # arguments specific to beta regression models
        model = TRUE, y = TRUE, x = FALSE, ...) # to control some components of the return value.

## logit model (default) 
gy <- betareg(PropUnidPhotos ~ PropFoxesTagged, data = mydataAllVis) 
summary(gy) # large SE
# summary for betareg has two sections, report the Coefficients, not the Phi coefficients.

## model checking
par(mfrow=c(3,2))
plot(gy, which = 1:4, type = "pearson") # 'which' specificies which plots to show out of 6 possibilities in the function 'plot.betareg'
plot(gy, which = 5, type = "deviance", sub.caption = "")
plot(gy, which = 1, type = "deviance", sub.caption = "")

## REMOVE OUTLIERS to see if improves model fit
# observation #8 has large cook's distance, so try refitting model excluding it:
gy_8 <- update(gy, subset = -8)

# see if this changed the precision parameter:
coef(gy, model = "precision") # 398.23
coef(gy_8, model = "precision") # 447.25 - it increased precision
# replot
plot(gy_8, which = 1:4, type = "pearson") # observation 4 has large cooks distance,

# now exclude observation #4
gy_84 <- update(gy_8, subset = -4)
coef(gy_8, model = "precision") # 447.25
coef(gy_84, model = "precision") # 395.35 - reduced the precision, so keep observation #4 in
summary(gy_8) # large SE

## Test model against null model to confirm predictor is non-sig:
gy_null <- betareg(PropUnidPhotos ~ 1, data = mydataAllVis) # with all data points
gy_null8 <- update(gy_null, subset = -8)

library(lmtest)
lrtest(gy_null, gy) # no sig diff
lrtest(gy_null8, gy_8) # no sig diff
### report model including all data points as removing #8 increased precision, but removing #4 reduced it back to the level with all datapoints included

### Try improving model fit by using a different link function (should increase the psuedo Rsq):
gy_ll <- betareg(PropUnidPhotos ~ PropFoxesTagged, data = mydataAllVis, link = "loglog")
summary(gy)$pseudo.r.squared
summary(gy_ll)$pseudo.r.squared # loglog model fit is worse than default logit model (indicated by lower psuedo Rsq)

#
##
###
##
#

#### VARIABLE DISPERSION BETA REGRESSION MODEL ####

### Try improving model fit by specifying variable dispersion model, to account for heteroskedasticity:
gy_VD <- betareg(PropUnidPhotos ~ PropFoxesTagged|PropFoxesTagged, data = mydataAllVis)
gy_VD_NULL <- betareg(PropUnidPhotos ~ 1|1, data = mydataAllVis)
par(mfrow=c(2,2))
plot(gy_VD, which = 1:4, type = "pearson") # looks much better!

### likelihood ratio test to "test the null hypothesis of equidispersion vs variable dispersion":
lrtest(gy, gy_VD) # variable dispersion (VD) model is significantly better fit (higher LR) than equidispersion model
AIC(gy, gy_ll, gy_VD, gy_VD_NULL) # AIC is lowest for VD model (best fit)
summary(gy_VD) # looks significant...
lrtest(gy_VD, gy_VD_NULL) # lr test: tests whether model fits better with or without the fixed effect
# x2(2)=4.738, p=0.094 = NO SIG DIFF
waldtest(gy_VD, gy_VD_NULL) # Wald test: tests probability that the estimated effect is zero
# So Wald Test suggests there is a nonzero effect of artificial tags on identifiability
# and LR test shows this effect is not significant. 
#
##
###
####
###
##
#

#### PREDICTIONS ####

visreg::visreg(gy_VD) # rough plot of model predictions

library(lsmeans)
lsm_preds <- lsmeans::lsmeans(gy_VD, "PropFoxesTagged", # use by= to add more variables
                              at = list(PropFoxesTagged = seq(0,1,0.1)),
                              type="response")
# use by= and at= to specify the reference grid (like pred.data or newdata)
lsmpreds <- data.frame(summary(lsm_preds))

# plot predicted proportion of unidentifiable photos proportion of tagged foxes
ggplot(lsmpreds, aes(y=lsmean,x=PropFoxesTagged)) + 
  geom_line(size=1) + 
  geom_point(data=mydataAllVis, aes(y=PropUnidPhotos, x=PropFoxesTagged), colour="red", 
             position=position_jitter()) +
  geom_ribbon(data=lsmpreds, mapping=aes(x=PropFoxesTagged, ymin=asymp.LCL, ymax=asymp.UCL), 
              fill="grey40", lty=0, alpha=0.3) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("\nProportion of foxes tagged") + 
  ylab("Proportion of photos unidentifiable\n") 

dev.print(jpeg, "Rplot_betaregVD model predicted PropUnidPhotos with increasing PropTaggedFoxes.jpeg", res=700, 
          height=15, width=20, units="cm") # save as jpeg

#
##
###
##
#

# AS ABOVE BUT OTHER WAY ROUND SO PREDICTING prop photos identifiable
mydataAllVis$PropPhotosIDd <- (mydataAllVis$NidPhotos/mydataAllVis$Total.photos)

gy_VDvv <- betareg(PropPhotosIDd ~ PropFoxesTagged|PropFoxesTagged, data = mydataAllVis)
summary(gy_VDvv)
summary(gy_VDvv_null)
gy_VDvv_null <- betareg(PropPhotosIDd ~ 1, data = mydataAllVis)
lrtest(gy_VDvv_null, gy_VDvv) # x2=4.7375, p=0.0936

lsm_preds <- lsmeans::lsmeans(gy_VDvv, "PropFoxesTagged", # use by= to add more variables
                              at = list(PropFoxesTagged = seq(0,1,0.1)),
                              type="response")
# use by= and at= to specify the reference grid (like pred.data or newdata)
lsmpreds <- data.frame(summary(lsm_preds))

# save model equation
eq <- text(y = 4.191 + 1.467x, Psuedo Rsq = 0.0017)
eq2 <- eq
# plot predicted proportion of unidentifiable photos proportion of tagged foxes
ggplot(lsmpreds, aes(y=lsmean,x=PropFoxesTagged)) + 
  geom_line(size=1) + 
  geom_point(data=mydataAllVis, aes(y=PropPhotosIDd, x=PropFoxesTagged), colour="red", 
             position=position_jitter()) +
  geom_ribbon(data=lsmpreds, mapping=aes(x=PropFoxesTagged, ymin=asymp.LCL, ymax=asymp.UCL), 
              fill="grey40", lty=0, alpha=0.3) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("\nProportion of foxes tagged") + 
  ylab("Proportion of photos identified\n") +
  annotate("text", x = 0.8, y = 0.982, label="y = 4.191 + 1.467x\n psuedo Rsq = 0.0017", 
                size=4.5)

dev.print(jpeg, "Rplot_betaregVD model predicted PropPhotosIdentified with increasing PropTaggedFoxes_EQUATION.jpeg", res=700, 
          height=15, width=20, units="cm") # save as jpeg

#
##
###
####
###
##
#

#### PLOTTING ####

library(ggplot2)
#compare plotting all foxes and only those that visited >5 times
ggplot(mydata,aes(x=TaggedFoxes,y=PropUnidPhotos, linetype=FoxTypeIncl)) + geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) +
  theme_bw(base_size = 18, base_family = "") + facet_wrap(~ FoxTypeIncl) +
  xlab("N tagged foxes") + ylab("N unidentified photos\n")
#adding "\n" after axis title adds more space between the label and the axis

subsetALL <-subset(mydata, FoxTypeIncl == "AllVisitors") #to plot All foxes
subset5vis<-subset(mydata, FoxTypeIncl == "VisitorsMin5Visits") #to plot only foxes that visited 5+ times
subset10pic<-subset(mydata, FoxTypeIncl == "VisitorsMin10Photos") #to plot only foxes that visited 5+ times

subsetALL$SurveyFAC <-factor(subsetALL$SurveyID)

# plot only foxes that vistied min 5 times in the survey
ggplot(subset5vis,aes(x=TaggedFoxes,y=PropUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) + 
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nNumber of foxes tagged") + 
  ylab("Total photos unidentifiable\n") +
  ggtitle("Foxes that visited min 5 times\n") +
  scale_x_discrete() 

# plot all foxes - line isnt much different so better to use this one
ggplot(subsetALL,aes(x=TaggedFoxes,y=PropUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) + # SE=true adds a ribbon for the SE
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("\nNumber of tagged foxes") + 
  ylab("Proportion of photos unidentifiable\n") +
  # ggtitle("All foxes\n") +
  scale_x_continuous(breaks = 0:10) 

dev.print(jpeg, "Rplot_Proportion of unidentifiable photos with N tagged foxes.jpeg", res=700, 
          height=15, width=20, units="cm") # save as jpeg

# plot proportion of unidentifiable photos proportion of tagged foxes
ggplot(subsetALL,aes(x=PropFoxesTagged,y=PropUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) + 
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  xlab("\nProportion of foxes tagged") + 
  ylab("Proportion of photos unidentifiable\n")

dev.print(jpeg, "Rplot_Proportion of unidentifiable photos with proportion foxes tagged.jpeg", res=700, 
          height=15, width=20, units="cm") # save as jpeg



par(mfrow=c(2,2)) 
## plot only foxes that vistied min 5 times, EXCLUDING OUTLIERS
ggplot(subset5vis,aes(x=TaggedFoxes,y=NUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) + 
  ylim(0, 200) +
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nNumber of tagged foxes") + 
  ylab("Total unidentifiable photos\n") +
  ggtitle("Foxes that visited min 5 times excluding outliers\n (2 surveys with >200 unidentifiable photos)\n") +
  scale_x_discrete() 


# plot all foxes, EXCLUDING OUTLIERS
ggplot(subsetALL,aes(x=TaggedFoxes,y=NUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) + 
  ylim(0, 200) +
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nNumber of tagged foxes") + 
  ylab("Total unidentifiable photos\n") +
  ggtitle("All foxes excluding outliers\n (2 surveys with >200 unidentifiable photos)\n") +
  scale_x_discrete() 


# plot foxes with min 10 photos, EXCLUDING OUTLIERS
ggplot(subset10pic,aes(x=TaggedFoxes,y=NUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE) + 
  ylim(0, 200) +
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nNumber of tagged foxes") + 
  ylab("Total unidentifiable photos\n") +
  ggtitle("All foxes with min 10 photos, excluding outliers\n (2 surveys with >200 unidentifiable photos)\n") +
  scale_x_discrete() 



