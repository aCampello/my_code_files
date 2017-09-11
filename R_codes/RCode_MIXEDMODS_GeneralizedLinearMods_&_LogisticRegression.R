#############################################################################################
### Generalized linear models: glm()    ### (General) Linear Models are specified: lm() ###
#############################################################################################

# template: glm(response ~ explanantory_variables, family=binomial) 
    # setting family to 'binomial' tells R to perform logistic regression

# GLMs ARE FOR NON-NESTED, SINGLE LEVEL MODELS! I NEED G.L.MIXED.MODELS...
boxplot(foxdatasubset$VisDur.S~foxdatasubset$Sex)
b<-glm(newvisdur~Sex, poisson, data=foxdatasubset) # Poisson GLM
summary(b)
# residual deviance is >2x the residual degrees of freedom of 48, so indicative of a poor
# fit. Our solution is to fit a quasipoisson distribution:
summary(glm(newvisdur~Sex, quasipoisson, data=foxdatasubset)) # Quasipoisson GLM
# quasipoisson model still not fitting well
# poss because data is overdispersed (negative binomial distrib, i.e. long right tail)
# Try fitting a negative-binomial GLM using the R package MASS:

# run a negative binomial variant of a GLM:
negbin.model <- glm.nb(VisDur.S~Sex, data=foxdatasubset) # Negative binomial GLM
summary(negbin.model)
# residual deviance is now similar to the d.f.s (unlike Poisson model), so this seems a well-fitting model.


########################################################
# LOGISTIC REGRESSION (uses GLM family=binomial)
########################################################

# logistic regression is a GLM for binomial (0/1) error and a log-link function - part of the family of Generalized LMs
# logistic regression is for continuous explanatory variables
# loglinear regression is for factor explanatory variables
# The goal of a multiple logistic regression: find an equation that best predicts the probability of a value of the Y 
# variable as a function of the X variables
# i need multiple loglinear regression

# Q: what makes foxes stay longer than 1 photo(burst)?

nulllogreg<- glm(shortvisdur ~ 1, family=binomial, data=foxdatasubset) # null GLM
example <- glm(shortvisdur ~ ., # using "." means to include all variables not yet specified in the model - useful to identify which are significant
             family = binomial(logit), data=foxdatasubset)

logreg1<-glm(shortvisdur ~ Season, family=binomial, data=foxdatasubset) #logistic GLM
library(visreg)
visreg(logreg1) # visualise regression results: shorter visits become more common throughout the year

anova(nulllogreg, logreg1, test="LRT") # sig diff (Anova tests: Chisq for categorical)

logreg2<-glm(shortvisdur ~ Season*Sex, family=binomial, data=foxdatasubset)
anova(logreg2, logreg1, test="Chisq") # sig diff
summary(logreg2)

logreg3<-glm(shortvisdur ~ Season*Sex + Sex*SocialStatus, family=binomial, data=foxdatasubset)
anova(logreg3, logreg2, test="Chisq") # sig diff

logreg4<-glm(shortvisdur ~ Season*Sex +   # trying the top-down approach (prob worse)
               Season*SocialStatus + 
               Sex*SocialStatus +
               PatchQuality +
               BeforeMidnight +
               OtherFoxesPresent +
               OtherSpeciesPresent +
               Season*SampleSeasonYear,
             na.action="na.pass", family=binomial(logit), data=foxdatasubset) 
# na.action="na.pass" means ignore NAs

logreg4step <- step(logreg4, data = foxdatasubset) # (SLOW): Do Stepwise Variable Selection for model logreg4
                                                    # and save final (best) model as logreg4step
summary(logreg4step)

pr <- predict(logreg4step, foxdatasubset, type="response") #calc predicted values 
round(pr, 2)
hist(pr, breaks=20)
hist(pr[foxdatasubset$shortvisdur==TRUE], col="red", breaks=20, add=TRUE) # overlay short visits in red
table(actual=foxdatasubset$shortvisdur, predicted=pr>.5)

anova(logreg4, logreg3, test="Chisq")
anova(logreg4, test = "Chisq") # Enter 1 model only to use anova to test sequential addition of terms to model (SLOW)

plot(fitdist(residuals(logreg4),"binom")) 
descdist(residuals(logreg4), boot = 1000) # lognormal??
str(foxdatasubset)

# Dredge runs a set of different models that incorporate all possible combinations of variables
# Not recommended for final model selection, just for exploratory analysis. Need to also use own judgement.
d<-dredge(logreg2)
d # show all possible models
topmodels <- get.models(d, subset=delta<15) # select the best models
model.sel(topmodels) # Show a summary of the best models
model.avg(d)$importance # Summarise the importance (on scale of 0-1) that each variable is included in the model

viewmodels<-get.models(d, subset = delta < 10) # get models with AIC <10 different from top model........?

mod1<-get.models(d, subset = 1)[[1]] # get the top model
mod2<-get.models(d, subset = 2)[[1]] # get the top model
summary(mod1)
anova(mod1, mod2, test="Chisq") # model 1 is sig better fit than model 2

par(mfrow=c(2,2)) # Now plot the model to check residuals are normal
hist(resid(mod1))
scatter.smooth(residuals(mod1)
               ~fitted(mod1)) # Scatter plot of residuals with a line-of-best-fit: should be random, but mine are 
qqnorm(residuals(mod1)) # QQNormality plot (my data are skewed)
qqline(residuals(mod1)) # Add line to QQplot
descdist(residuals(mod1))

exp(coef(mod1)) # Compute how daily visit frequency changes as a function of income
exp(confint.default(mod1)) # get 95% CIs

