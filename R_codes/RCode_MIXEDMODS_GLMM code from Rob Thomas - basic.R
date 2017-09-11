  ##################################
  ### Rob's script for session 5 ###
  ##################################
  
dframe1 <- read.csv(file.choose()) # Select "Beaches.csv" 
summary(dframe1)
names(dframe1)

dframe1$fBeach <- factor(dframe1$Beach) # treat "Beach" as a factor = 'fBeach'

# Make sure you've got the latest versions of the packages...
install.packages("nlme")
install.packages("lme4")


#################################################
# REML-fitted models with various structures ####
#################################################

library(nlme) # loading up the nlme library

### A model with fixed factors but no random intercept ####

model1 <- gls (Richness ~ Height, 
               method = "REML", 
               na.action = na.exclude,
               data = dframe1)

sresid <- resid(model1, type = "normalized") # Extract the standardised residuals
hist(sresid)

plot(model1) # gives a heteroscedasticity plot - fans out, not good

plot(sresid ~ dframe1$Height) # plot the residuals against the independent variable(s)

anova(model1) # Various formats of model summaries
model1
summary(model1) 
AIC (model1) 

### random intercept, no fixed effects ####

model2 <- lme(Richness ~ 1, random = ~1| fBeach, 
              method = "REML", data = dframe1)

sresid <- resid(model2, type = "pearson") # Extract the standardised residuals
hist(sresid)

plot(model2)

plot(sresid ~ dframe1$Height) # plot the residuals against the independent variable(s)

anova(model2)
model2
summary(model2) # Various formats of model summaries

AIC (model2)

### Random intercept model, same slope for all levels of xr ####

model3 <- lme(Richness ~ Height, random = ~1| fBeach, 
              method = "REML", data = dframe1)

sresid <- resid(model3, type = "normalized") # Extract the standardised residuals
hist(sresid)

plot(model3)

plot(sresid ~ dframe1$Height) # plot the residuals against the independent variable(s)

anova(model3)
model3
summary(model3) # Various formats of model summaries

AIC (model3)

### Random intercept + slope model (different slope for each level of xr so each level of Height*Beach) ####

model4 <- lme(Richness ~ Height, random = ~ 1+Height | fBeach, 
              method = "REML", data = dframe1)

sresid <- resid(model4, type = "normalized") # Extract the standardised residuals
hist(sresid)

plot(model4)

plot(sresid ~ dframe1$Height) # plot the residuals against the independent variable(s)

anova(model4)
model4
summary(model4) # Various formats of model summaries 

# R-squared values for GLMMs... 
library(MuMIn)
r.squaredGLMM(model4)
# R2m = marginal R-squared
# R2c = conditional R-squared

AIC (model4)

AIC (model1, model3, model4) # model 4 has the lowest AIC

# Note that model2 is not directly comparable with models 1,3 and 4,
# ...as it has a different fixed structure (no fixed term in model2)
AIC (model1, model2, model3, model4)

# Some model comparisons

anova(model1, model3) # Model 3 is sig better than model 1
anova(model1, model4) # Model 4 is sig better than model 1

anova (model3, model4) # Model 4 is sig better than model 3
# ...so we'd prefer model 4

######################################
# GeneralISED Linear Mixed Models ####
######################################
library(lme4)

### A random intercept Gaussian GLMM (same slope for each level of xr) ####
model1 <- lmer(Richness ~ Height + (1|fBeach),
               data = dframe1)

summary(model1)
AIC(model1)


### Random intercept Poisson GLMM (same slope for each level of xr) #### #xr is random effect
model1 <- glmer(Richness ~ Height + (1|fBeach),
                family = poisson (link = "log"), 
                data = dframe1)
AIC(model1)
summary(model1) # Various formats of model summaries
anova(model1)
model1

plot(model1) # Check for homoscedasticity 

# Extract the standardised residuals
sresid <- resid(model1, type = "pearson") 
hist(sresid)
plot(sresid ~ dframe1$Height) # plot the residuals against the independent variable(s)

# A package for model validation in lmer models
library(LMERConvenienceFunctions)
mcp.fnc(model1) # Provides a QQ plot and heteroscedasticity plot
plotLMER.fnc(model1) # Plots the population mean relationship between fixed and dependent variables


### Random intercept + slope Poisson GLMM (different slope for each level of xr) ####
model2 <- glmer(Richness ~ Height + (Height|fBeach),
                family = poisson (link = "log"), 
                data = dframe1)
AIC (model2)
summary(model2) # Various formats of model summaries
anova(model2)
model2

plot(model2) # Check for homoscedasticity 

# Extract the standardised residuals
sresid <- resid(model2, type = "pearson") 
hist(sresid)
plot(sresid ~ dframe1$Height) # plot the residuals against the independent variable(s)

library(LMERConvenienceFunctions)
mcp.fnc(model2) # Provides a QQ plot and heteroscedasticity plot
plotLMER.fnc(model2) # Plots the population mean relationship between fixed and dependent variables

# Compare random intercept and random slope models
AIC(model1, model2)
anova(model1, model2, test="Chi") # adding a random slope signif. improves model fit


############################################