rm(list=ls()) #to clear the R workspace
setwd("G:/Statistics 2016/Methods chapter - stats and plots/Methods - TimeIntervals_Mean_Median")

#load packages
library(lme4) 
library(MuMIn)
library(visreg)
library(lmerTest)
library(fitdistrplus)

# Load data
mydata <- read.table(file = "TimeIntsR_SeasonIDs.txt", sep='\t', header = TRUE) 
head(mydata)
which(is.na(mydata)) # check for missing values
which(complete.cases(mydata)) # to see number of complete rows

#Take subset of data that excludes unknown social status (!= means not-equal-to, == means equal-to)
mydatasub <- subset(mydata,Social.status != "Unknown")
seasonfac<-as.factor(mydatasub$SeasonID) #convert season from an integer to factor

# Plot hist sub$ti.threshold.s) #data are skewed
#Normalise ti.threshold.s (use an underscore to indicate original data has been transformed and stored as a new object)
mydatasub$ti.threshold.s_ <- with(mydatasub, (ti.threshold.s-min(ti.threshold.s))/(max(ti.threshold.s)-min(ti.threshold.s)))
hist(mydatasub$ti.threshold.s_) #data are still skewed but slightly less!


# Check distribution of raw data to determine appropriate model
library(fitdistrplus) 
descdist(mydatasub$ti.threshold.s, discrete=F, boot=1000) # suggests gamma or lnorm

require(car)
require(MASS)
qqp(mydatasub$ti.threshold.s, "norm", main="normal") # would require lmer with family = gaussian
qqp(mydatasub$ti.threshold.s, "lnorm", main="lognormal") # would require lmer with log or log10(response)
gamma <- fitdistr(mydatasub$ti.threshold.s, "gamma", start=NULL)
qqp(mydatasub$ti.threshold.s, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")
# normal looks best of these three but still not ideal


#plot interactions between all variables specified to see which should be included in the model:
with(mydata, pairs(data.frame(cbind(ti.threshold.s_, seasonfac, Sex, Social.status)), panel=panel.smooth))
# no clear interactions but something going on between ti.threshold.s and seasonfac, Sex and Social.status


# Linear model to test effect of all variables (and possible interactions between) AS RANDOM VARIABLES on 
# ti.threshold.s, accounting for AnimalID (the repeated measures) as a FIXED VARIABLE:
# (I could use an ANOVA, but I have repeated measures (a different value for each Animal ID in every season), so
# I must use lmer):
model1 <- lmer(ti.threshold.s ~ seasonfac + 
                                Sex*Social.status +
                                (1|Animal.ID), data = mydatasub, 
              na.action="na.pass", REML=FALSE) #na.action means ignore N/As


summary(model1) # P values are only shown if package lmerTest is installed
anova(model1) # anova test to get F values and transformed degrees of freedom (transformed to account for repeated 
# measures (=AnimalID))
Anova(model1) # Anova with a captital A should only be used for Generalized Linear Models (i.e. without repeated 
# measures) - small anova is fine for me.
AICc(model1) # Display AICc for model 1. AICc is similar to AIC, but a more appropriate value to quote for repeated 
# measures data than AIC.
visreg(model1) # Plot variation within the fixed effect/repeated measure, AnimalID (straight line is mean across 
# all AnimalIDs and dots show the scatter around the mean)
hist(resid(model1)) # A suitable model should have normally-distributed residuals. These are slightly skewed.


model1_set <- dredge(model1) # Dredge runs a set of different models that incorporate all possible combinations of 
# variables (but always including the fixed effect/s as fixed). As I only have one fixed effect (AnimalID), this is 
# the intercept. LOWEST AICc is best, even if negative (more negative=better). Delta shows difference in AICc 
# between the best (top) model and the currect model)
model1_set # Show all models found by Dredge - not ideal if there are lots!

top_model1 <- get.models(model1_set, subset=delta<10)  # Select the best models and store them in object top_model_1, 
# i.e. only models with an AIC within "the value of delta" of the best model. I.e. within 3 of the lowest AIC (3 is 
# default, but increase delta to show more models)
model.sel(top_model1) # Show a summary of the best models
model.avg(top_model1)$importance # Summarise the importance (on scale of 0-1) that each variable is included in the model

get.models(model1_set, subset = 1)[[1]] # Show details of a certain model within the list of best models. Specify using
# the line number in subset= e.g. to specify model in line 5, type subset=5.

model1.vis<- get.models(model1_set, subset=1)[[1]]
model2.vis <- get.models(model1_set, subset = 2)[[1]] # Select a model to work with...

anova(model1.vis, model2.vis) #compare the top 2 models - no sig.diff between models 1 and 2
summary(model2.vis) #summary doesn't contain P values - need to do ANOVA
AICc(model2.vis)


par(mfrow=c(2,2)) # Now plot the model to check residuals are normal - if not, the model is not explaining all of the 
# variance in the response (e.g. ti.threshold.s)
scatter.smooth(residuals(model1.vis)
               ~fitted(model1.vis)) # Scatter plot of residuals with a line-of-best-fit: should be random, but mine are 
# slightly skewed as expected
hist(residuals(model2.vis)) #Histogram of residuals
qqnorm(residuals(model1.vis)) # QQNormality plot (my data are skewed)
qqline(residuals(model1.vis)) # Add line to QQplot
descdist(residuals(model1.vis)) # GOOD PLOT - blue dot (observation) should ideally overlap the star (to indicate a
# normal distribution). If not, then model is not the best fit...
#... So try visualising another model from the list.

###############################################################################################################
XXX
# RUNNING MODELS SEPARATELY TO GET P VALUES IN ANOVA: 
# SEE RCode_METHODS_Plotting time intervals...SIMPLIFIED for code for models used in thesis methods chapter
# - here i excluded foxes of unknown status which was pointless when status not included in model!

lmm1 <- lmer(ti.threshold.s ~ seasonfac + 
              (1|Fox.name), data = mydatasub, na.action="na.pass", REML=FALSE) 

lmm2 <- lmer(ti.threshold.s ~ 1 + 
               (1|Fox.name), data = mydatasub, na.action="na.pass", REML=FALSE) 
anova(lmm1, lmm2) # x2(3)=14.636, p=0.002156** - need season

plot(fitted(lmm1), resid(lmm1))
lines(smooth.spline(fitted(lmm1), resid(lmm1))) 
hist(resid(lmm1))
# not great but prob OK

a <- coef(summary(lmm1)) # save coefficients

# post hoc to see which seasons differ significantly
a <- pairs(lsmeans::lsmeans(lmm1, ~seasonfac))
b <- data.frame(summary(a))
c <- data.frame(summary((lsmeans::lsmeans(lmm1, ~seasonfac))))

# show number of foxes in each season
library(plyr)

a <- ddply(mydatasub, "SeasonID", summarise, 
           N=length(unique(Animal.ID)))

length(unique(mydatasub$Animal.ID))
