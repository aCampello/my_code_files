
#=====================================================================================#
# Generic script for plotting relationships in GLMMs --- From Rob Thomas via R-Space:
#=====================================================================================#

# Note: depending on the package version, predictions may come out as $pred.fit or $pred or $fit 
# and whether or not se.fit can/will be calculated!

###### lme version ##################
setwd("G:/Statistics 2016/R codes")
dframe1<-read.csv("Beaches.csv", head=T) # load example data for this file
names(dframe1)

## Plotting a graph of the model
plot(dframe1$Richness ~ dframe1$Height, pch=21, cex=1.8, 
     xlab="Height", ylab="Richness", cex.lab = 1.3,
     col = dframe1$fBeach)

text(dframe1$Height, dframe1$Richness, dframe1$Beach, cex=0.7) # labels each observation by Beach

# This is our standard method for plotting a prediction line (+/-1SE) onto the raw data
library(nlme)
model4 <- lme(Richness ~ Height, random= ~1|Beach, data=dframe1, method="ML")

# Step 1: Making a table of prediction data (pdat)
pdat <- expand.grid(Height = seq(-1.5,2.5,0.1))
pdat

# Step 2: Making a file containing the predicted data (pred) 
pred <- predict (model4, newdata = pdat, level = 0, # level = zero for the population fitted line
                 na.action = na.exclude, type= "response", se.fit=TRUE)
head(pred)

# Step 3: combine the predictions with the predictors, 
# into a final dataframe (predframe)
predframe <- data.frame (pdat, preds = pred)
head(predframe)

# Step 4: plot some graphs of predicted values of y vs x
lines (predframe$preds ~ predframe$Height, col="red", lwd = 2) 


# Finally, calculate and add lines for +/- 1SE
# *** Not working in some package versions!!!***
predframe$upperse <- (predframe$preds + predframe$preds.se.fit)
lines (predframe$upperse ~ predframe$Height, lty = 2, col = "red", lwd = 1.5) 
# Adds upper SE line
predframe$lowerse <- (predframe$preds.fit - predframe$preds.se.fit)
lines (predframe$lowerse ~ predframe$Height, lty = 2, col = "red", lwd = 1.5) 
# Adds lower SE line

# You can also plot separate lines for each level of the random factor

fitted.glmm <- fitted(model4, level=1) # level = 1 for examining each beach separately

lines(fitted.glmm ~ dframe1$Height, col="blue",
      subset=(dframe1$fBeach == "1")) ### Change the number to plot a different line for each beach

###### lmer and mgcv version ##################

## Plotting a graph of the model
plot(dframe1$Richness ~ dframe1$Height, pch=21, cex=1.8, 
     xlab="Height", ylab="Richness", cex.lab = 1.3)

text(dframe1$Height, dframe1$Richness, dframe1$Beach, cex=0.7) # labels each observation by Beach

# This is our standard method for plotting a prediction line (+/-1SE) onto the raw data
library(lme4)
model2 <- lmer(Richness ~ Height + (1|Beach), data=dframe1, REML=F)
# Step 1: Making a table of prediction data (pdat)
pdat <- expand.grid(Height = seq(-1.5,2.5,0.1))
pdat

# Step 2: Making a file containing the predicted data (pred) 
pred <- predict(model2, newdata = pdat, re.form=NA,
                na.action = na.exclude, type= "response", se.fit=T) #se.fit won't work for glmmADMB models so use interval="confidence" instead - because if glmmADMB library is loaded the predict() function is automatically glmmadmb.predict() and not just predict()
pred
?na.action
# Step 3: combine the predictions with the predictors, 
# into a final dataframe (predframe)
predframe <- data.frame (pdat, preds = pred)
predframe

# Step 4: plot some graphs of predicted values of y vs x

lines (predframe$preds.fit ~ predframe$Height, col="red", lwd = 2) 
# add SE lines
lines (predframe$preds.fit + predframe$preds.se.fit ~ predframe$Height, col="red", lwd = 2, lty = 2) 
lines (predframe$preds.fit - predframe$preds.se.fit ~ predframe$Height, col="red", lwd = 2, lty = 2) 

# Adding fitted lines for each beach...
pdat <- expand.grid(Height = seq(-1.5,2.5,0.1), 
                    fBeach=levels(dframe1$fBeach))
pdat
pred <- predict(model2, newdata = pdat, 
                re.form=NULL, # makes predictions for each level of the random factor
                na.action = na.exclude, type= "response")
pred

predframe <- data.frame (pdat, preds = pred)
predframe

lines (predframe$preds ~ predframe$Height, 
       subset=predframe$fBeach=="4",
       col="blue", lwd = 2) 

##### The end ####