#R code for models fitted in the MLWin Manual chapter 4#


#Setting up:

setwd("~/PhD/MLM Group stats practice/LEMMA ML Modelling")
mydata <- read.table(file = "tutorial.txt", sep = "\t", header = TRUE) #import dataset
mydata$school<-factor(mydata$school) #set school as a factor
mydata$girl<-factor(mydata$girl) #set gender as a factor
str(mydata) #view summary of dataset



#4.1 Random intercept models

plot(mydata$normexam~mydata$standlrt) #scatterplot of initial LRT score against final normexam score
library(ggplot2) #alternative package to plot graphs
ggplot(mydata, aes(standlrt, normexam))+geom_point() #ggplot of LRT against normexam


library(lme4) #load package lme4 to run regression models

#test effect of standlrt on normexam (fixed effects model) aka. linear regression
fit<- lm(normexam ~ standlrt, data=mydata) 
summary(fit) #summary of linear regression
summary(fit)$sigma #returns 0.8056 which is the residual standard error, to calculate the 
# error value given by MLwin by [0.8056^2] a.k.a.[0.8056**2] = 0.6489914 (but this value 
# has little meaning)
2*logLik(fit) #to get 2*LogLikelihood estimate as shown in MLWin


#variance components model to test effect of schools (as random effect) on normexam 
# after taking into account standlrt:
fit2<- lmer(normexam ~ standlrt + (1|school), data = mydata, REML=FALSE) #can't do anova to compare 2 models if they were fitted using REML 
#(R will give a warning and will refit the models using REML=F if you do ask it to do an anova on them)

summary(fit2)
2*logLik(fit2)
#in this model schools only vary in their intercepts: slopes are the same for all schools
#lmer reports residual variance while lm reports residual SE (tho it looks more like SD?)


library(lmtest) #load library required for likelihood ratio testing
lrtest(fit2, fit) # likelihood ratio test to compare models fit & fit2 (Page51 in MLWIN Manual)
#the lrtest finds the 2*LogLik from each model and subtracts one from the other to get a Chi-Sq statistic.



#4.2 Graphing  predicted  school  lines  from  a random intercept model (Page51 in MLWIN Manual)

plot(predict(fit2)~mydata$standlrt) #predict(model)~observedvalues# calculates predicted values based on the observed values, e.g. scores, without any random effects
#lmer models can't plot fixed effects without also plotting random effects, which can make plots messy and hard to interpret. 
#with lme models you can plot levels separately:

library(nlme) #load package to run linear mixed-effects models (command lme())

fit2a<-lme(normexam ~ standlrt, random= ~1|school,data=mydata)
summary(fit2a)

plot(predict(fit2a, level=0)~mydata$standlrt) #plot fixed effects ONLY (level 0)
plot(predict(fit2a, level=1)~mydata$standlrt) #plot fixed effects and random effects (level 1)
# Christos' note: WITH lme, predict(m2, level=0) GIVES PREDICTED VALUES JUST FOR THE FIXED EFFECTS; SET level=1 TO GIVE RANDOM EFFECTS INCLUDED AS WELL

#My addition: from lme() model outputs you can plot the levels separately (fixed (1) and random (0) effects), which you can't do with lmer models. THe trouble with
#lme() models is fine for data with a normal distribution, but for non-normal e.g. poisson/binomial you need to use lmer...
#However lmer doesn't calculate predicted values without the random effects (plotting predicted values without random effects often makes them easier to
#interpret because you get a single line with no noise) - (the only way to find the predicted values without random effects is to do it manually, which
#can get complicated if you have lots of levels and explanatory variables).



# DRAW GRAPH FOR COMPARISON OF PREDICTED FITS FROM lm AND lmer MODELS, by calculating predicted values manually (overkill but here is the code...):
fit2_predicted_fixed_effects<-fixef(fit2)[2]*mydata$standlrt+fixef(fit2)[1] #to manually cal.cualte the fixed effects for model fit2. 
#fixef() calculates a single average value for the fixed effects for something, e.g. average day of observation when observations were taken over several days 
#a  useful function for simplifying plots if you want to plot a single line despite having lots of variables in a model. [2] refers to column 2 in a data frame.

the model fit2 based on column [2]
plot(fit2_predicted_fixed_effects~mydata$standlrt) #shows how similar the predicted values are to the observed values
plot(mydata$normexam~mydata$standlrt)
abline(b=fixef(fit2)[2], a=fixef(fit2)[1],col="red") #draw trendline for model 'fit2' (lmer model)
abline(b=coefficients(fit)[2], a=coefficients(fit)[1],col="blue") #draw trendline for model 'fit' (lm model)




#Page54: extract school variances (random effects / MLWin calls them 'school residuals'... "ran-" "eff-")
ranef(fit2)

mydata$School_random<-ranef(fit2)$school[as.numeric(mydata$school),1]
# THIS IS A MESSY WAY OF DOING IT: IT ADDS A NEW COLUMN TO mydata FOR THE RANDOM EFFECT OF THAT SCHOOL

# THIS THEN ALLOWS THE PREDICTED VALUE FOR EACH STUDENT TO BE CALCULATED (SEE P 56 OF THE MANUAL TOO):
fit2_predicted_effects<-fixef(fit2)[2]*mydata$standlrt+fixef(fit2)[1]+mydata$School_random

# AS WITH fit2_predicted_fixed_effects BUT WITH THE RANDOM EFFECT ADDED ON
plot(fit2_predicted_effects~predict(fit2))

#Page56:
library(lattice) #load package LATTICE to draw nice scatterplots (can also draw simple scatterplots using plot())
xyplot(fit2_predicted_effects~mydata$standlrt,groups=mydata$school,auto.key=F)
# ALL SLOPES THE SAME, DIFFERENT INTERCEPTS


# Page58:
mydata$schgend<-relevel(mydata$schgend, ref="mixedsch") #set the reference category as mixedsch
fit3<-lmer(normexam ~ standlrt + schgend + (1|school), data=mydata, REML=F) #school is random effect, standlrt
# and schgend are fixed effects
summary(fit3)
-2*logLik(fit3) #likelihood ratio test?

#fit a single level model:
fit4<-lm(normexam ~ standlrt + schgend, data=mydata, REML=F)
summary(fit4)
-2*logLik(fit4)

#Page60: schools can have different intercepts AND slopes
fit5<-lmer(normexam ~ standlrt + (standlrt|school), data=mydata)
summary(fit5)
# RESIDUAL VARIANCE OF RANDOM EFFECTS (0.55385) MATCHES MANUAL (0.554) 
# UNDER Corr IS 0.50, WHICH IS THE CORRELATION BETWEEN THE RANDOM SLOPES AND INTERCEPTS
# THE MANUAL FINDS THIS IS 0.49, SO VERY CLOSE
-2*logLik(fit5)

plot(resid(fit5)~fitted(fit5))
VarCorr(fit5)$school #gives a variance-covariance matrix
# THE BOTTOM ONE THE CORRELATION BETWEEN SLOPES AND INTERCEPTS FOR school

ranef(fit5)
# THIS NOW HAS THE RANDOM SLOPE FOR EACH SCHOOL AS WELL

plot(ranef(fit5)$school[,1]~ranef(fit5)$school[,2])
# SHOWS THE CORRELATION BETWEEN RANDOM SLOPES AND INTERCEPTS

qqmath(ranef(fit5, condVar=T, whrichel = "school"))
# AUTOMATICALLY PLOTS THE CATEPILLAR PLOT FOR THE RANDOM SLOPE EFFECT AS WELL

qqmath(ranef(fit5, condVar=F, whichel = "school")) #whichell=school specifies that the random effect is 
#school (this bit is only needed if you have >1 random effects in the model
# MAKING condVar=FALSE GETS RID OF THE ERROR BARS FROM THE PLOT 
# AS condVar IS "if the conditional variance-covariance matrices of the random effects should be 
# added as an attribute", THE ERROR BARS HAVE SOMETHING TO DO WITH THIS???

# P 62:
lrtest(fit5, fit2) #compare the random slope model (fit5) with the single slope model (fit2)
-2*lrtest(fit5, fit2)$LogLik
# THESE ARE IN THE SAME BALL PARK AS THOSE QUOTED IN THE MANUAL

# P 63:
predict(fit5)
xyplot(predict(fit5)~mydata$standlrt,groups=mydata$school,auto.key=F) #plot model showing different 
# intercepts and slopes