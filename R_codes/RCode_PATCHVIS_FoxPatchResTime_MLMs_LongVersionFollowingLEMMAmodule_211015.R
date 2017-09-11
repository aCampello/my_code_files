rm(list=ls()) #to clear the R workspace
setwd("~/Analysis/MLM Group stats practice")
mydata <- read.csv(file = "MultilevelModels_PatchResTimes.csv", header = TRUE) #import dataset
str(mydata)

### TAKE SUBSET OF DATA TO EXCLUDE CUBS AND FOXES OF UNKNOWN STATUS AND SEX (SEE WAY BELOW FOR CODE) ###

library(plyr) # to do calculations on data
library(lme4) # to run regression models

# Count the total no.other foxes (nsharers) present and add in a new column:
### METHOD 1 - in 3 steps
foxdata<-mutate(mydata, Nfoxes=sapply(strsplit(as.character(AllFoxSharers), ','), length))
foxdata2<-mutate(foxdata, Nsharers1=as.numeric(foxdata$Nfoxes)-1)
foxdata3<-mutate(foxdata2, Nsharers=ifelse(Nsharers1==-1, 0, Nsharers1)) 
### METHOD 2 - in 1 step
totfoxes<-sapply(strsplit(as.character(mydata$AllFoxSharers), ','), length)
foxdata<-mutate(mydata, Nsharers=ifelse(totfoxes==0, 0, totfoxes-1))
str(foxdata)               

## Make new column with unique ID number for each patch in new dataset called foxdata2
foxdata2<-mutate(foxdata, PatchID=as.integer(foxdata$Patch))   
str(foxdata2)

#Plot to check distribution
hist(foxdata2$VisDur.S)  #data is skewed (negative binomial distribution)
plot(density(foxdata2$VisDur.S)) #ditto
foxdata2$log_visdur<-log(foxdata2$VisDur.S) #log-transform visit duration in attempt to normalise
hist(foxdata2$log_visdur) #still doesnt follow normal distribution, so will not transform data
plot(density(foxdata2$log_visdur)) #ditto

############################################################

# LEMMA 5.1: Comparing groups using multilevel modelling

### 5.1.1 A multi level model of visit duration (residence time) with patch effects

# Create null model with patch effects (intercept) only
# patch as a random effect, assuming patch-level residuals follow normal distribution
nullmodel <- lmer(VisDur.S ~ (1 | PatchID),  data = foxdata2, REML = FALSE)
# 1 | PatchID specifies random intercept and this is intercept only model (no predictors by slope)
summary(nullmodel)
# Mean visit duration across all patches is 209.2 seconds (approx 3.5 mins)

# Partitioning variance:
# calculate the variance partition coefficient (VPC) as Intercept variance/Total variance
# Between-patch variance is 5622 (PatchID intercept)
# within-patch variance is 166908 (residual)
VPC<-5622/(5622+166908)
VPC
#Patch accounts for 3% of the total variance


### LEMMA 5.1.2 Examining patch effects (residuals)

# test whether Patch as random effect improves the fit of the model compared to the intercept only
model1<- lm(VisDur.S ~ 1, data = foxdata2)
summary(model1)

#likelihood ratio test statistics:
logLik(nullmodel)
logLik(model1)
# Calculate likelihood ratio between the 2 models
LR <- 2*(-362963 - -363735.6)
LR #on 1 d.f. as there is only 1 parameter difference between the 2 models
# how to test this is significant / do chi-squared test? - easier to just use ANOVA!



### Calculate patch-level residuals and their SEs and store for each individual patch:

#To estimate the patch-level residuals
u0 <- ranef(nullmodel, condVar = TRUE) #PostVar is depreciated. Must use condVar, not condvar.
# U0 is a list of lists containing the 36 patch-level residuals

# create a random effects object which contains variance-covariance matrix called postVar
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

# get description of first column of u0, the list corresponding to the first set of random effects
str(u0[1])
## str(u0[1]) contains patch-level residuals and their posterior variances:
### line 1: there are 36 patches in the data
### line 2: ( $ (Intercept)) lists the school effects
### line 3: corresponds to the "postVar" attribute & lists their associated posterior variances (not shown here as only have 1 random effect (patch))

# There is only one set of random effects and therefore u0[1] is a list of only one object, u0[[1]]
# u0[[1]] is a dataframe containing the patch-level residuals and the "posterior variances" of these residuals
# within the attribute postVar
# To access the elements of u0[[1]], must use 2 sets of square brackets instead of 1.
str(u0[[1]])
head(attr(u0[[1]], "postVar")[1, , ]) # get simple vector containing the "posterior variances" for each residual
head(u0[[1]]) # view first few elements of this vector


# Create a dataframe with identifier, residual, and SE for each patch
RPatchID <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind(RPatchID, u0[[1]], u0se)
colnames(u0tab) <- c("RPatchID","u0","u0se")

# sort this table (called RPatchID) by ascending order based on the values of u0:
u0tab <- u0tab[order(u0tab$u0), ]

# create a new column containing the ranks:
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank"

# reorder the table based on the PATCH identifier:
u0tab <- u0tab[order(u0tab$RPatchID),  ]

# To see the patch residual, SE and ranking for a particular patch, we can list the data by using 
# the indexing structure of the R dataframe
# Do this for the first 10 patches in the data:
u0tab[1:10, ]
summary(nullmodel) #fixed effects estimate indicates the mean visit duration for all patches is 209.2 seconds
# For patch 8, which is ranked first (=36 places from the bottom) in u0tab[1:10, ], we estimate the mean visit 
# duration by 209.2 - 58.64 = 150.56 seconds

# use the plot and segments commands to produce a 'caterpillar plot' to show the patch effects in rank order of u0 (residual size)
# together with 95% confidence intervals:
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for RPatch_ID:_cons") # create the plot without any data
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se) # add 95% confidence intervals
points(u0tab$u0rank,  u0tab$u0, col = "blue") # superimpose the point estimates
abline(h = 0, col = "red") # horizontal line corresponding to y=0, the average patch

############################################################

# LEMMA 5.2: Adding visit-level Explanatory Variables: Random Intercept Models

str(foxdata2)

# fit regression with random intercept of Patch & SeasonID as an explanatory variable
model2 <- lmer(VisDur.S ~ SeasonID + (1 | PatchID), data = foxdata2, REML = FALSE)
summary(model2)
summary(nullmodel) # Model2 has a lower AIC than the nullmodel
# reduction in patch variance and overall variance compared to previous model, indicates distribution of visit 
# durations by season differs between patches 

# save predicted value for each school
predvisdur <- fitted(model2)

# create a variable to pick out the minimum amount of data required to plot the predicted school lines
datapred <- unique(data.frame(cbind(predvisdur = predvisdur, SeasonID =
                                      foxdata2$SeasonID, PatchID = foxdata2$PatchID)))

# plot
library(lattice) # need for xyplot
xyplot(predvisdur ~ SeasonID, data = datapred, groups = PatchID, type = c("p",
                                                                          "l"), col = "blue")
# in all patches, visit durations decline over the year


# to remove patches only observed during one season (e.g. 35 Cooper Rd & 66 Bell Barn Rd)
datapred <- datapred[order(datapred$PatchID, datapred$SeasonID), ]
datapred$multipleseasons <- rep(0, length(datapred$PatchID))
datapred$multipleseasons[datapred$PatchID %in%
                           unique(datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# plot graph of only patches observed during >1 season
xyplot(predvisdur ~ SeasonID, data = datapred[datapred$multipleseasons == 1, ],
       groups = PatchID, type = c("p", "l"), col = "blue")

############################################################

# LEMMA 5.3: Allowing for Different Slopes across Schools: Random Slope Models

# the random intercept model with season as explanatory variable
model3a <- lmer(VisDur.S ~ SeasonID + (1 | PatchID), data = foxdata2, REML = FALSE)
summary(model3a)

# a random slope model with season as an explanatory variable
model3 <- lmer(VisDur.S ~ SeasonID + (1 + SeasonID | PatchID), data = foxdata2, REML = FALSE)
summary(model3) # model 3 has lowest AIC
# now random effects section includes correlation (on RHS), because by defining the random intercepts and slopes 
# together, we indirectly specify that we want the random intercepts and slopes to covary
# if we wanted independent random intercepts and slopes, then replace (1 + cohort90 | schoolid) with 
# (1 | schoolid) #(for random intercept)#  + (0 + cohort90 | schoolid)


### 5.3.1 Testing for random slopes

# use a likelihood ratio test to test whether the season effect varies across patches
# can use anova for likelihood ratio test, to see whether adding a random slope improves model fit
anova(model3, model3a) #model3 is random slope, model 2 is random intercept only
# strong evidence that the season effect differs across patches

### 5.3.2: Interpretation of random cohort effects across schools
 # N/A

### 5.3.3: Examining intercept and slope residuals for schools

# to display variance/covariance matrix for the random effects:
VarCorr(model3)$PatchID
## table 1: value of covariance between patch and season is -1897.7015
## table 2, (attr(, "stddev")): standard deviation of the variance estimates of the random intercepts and slopes
## table 3, (attr(, "correlation"))`: correlation table between the random intercepts and slopes
## last value (attr(, "sc")) returns the standard deviation of the level 1 residuals

# estimate the school intercepts and slopes and store along with their posterior variances in object myrandomeff
myrandomeff <- ranef(model3, condVar = TRUE)

# draw plot of patch slopes versus the patch intercepts
plot(myrandomeff[[1]], xlab = "Intercept (u0j)", ylab = "Slope of SeasonID (u1j)")
abline(h = 0, col = "red")
abline(v = 0, col = "red")
# top left: patches with <average duration but a >average slope (i.e. steeper decline spring-winter/ more seasonal variation)
# bottom left: patches with <average duration and <average slope (i.e. duration more consistent across seasons)
# top right: patches with >average duration and >average slope (higher variation between seasons)
# bottom right: patches with >average duration and <average slope (more consistent across seasons)

# plot predicted visit duration lines
predvisdur <- fitted(model3)
# again, will only plot fitted regression lines for patches observed in >1 season:
datapred <- cbind(predvisdur = predvisdur, SeasonID = foxdata2$SeasonID, PatchID
                  = foxdata2$PatchID)
datapred <- data.frame(unique(datapred))
datapred <- datapred[order(datapred$PatchID, datapred$SeasonID), ]
datapred$multipleseasons <- rep(0, dim(datapred)[1])
datapred$multipleseasons[datapred$PatchID %in% unique
                           (datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# plot (finally)
xyplot(predvisdur ~ SeasonID, data = datapred[datapred$multipleseasons == 1, ]
         , groups = PatchID, type = c("p","l"), col = "blue")


### 5.3.4: Between-school variance as a function of cohort
# no idea what they did here

# P5.3.5 Adding a random coefficient for gender (dichotomous x )
# first, add a fixed effect for gender (only) to use as a comparison model for testing for a random coefficient

# create dummy variables for Sex, with male as the reference category 
foxdata2$sexfemale <- foxdata2$Sex == "Female"
foxdata2$sexunknown <- foxdata2$Sex == "Unknown"

model4 <- lmer(VisDur.S ~ SeasonID + sexfemale + sexunknown + (1 + SeasonID | PatchID), data = foxdata2, REML = FALSE) 

model5 <- lmer(VisDur.S ~ SeasonID + sexfemale + sexunknown + (1 + SeasonID + sexfemale + sexunknown | PatchID), 
              data = foxdata2, REML = FALSE)

# R does create dummies automatically, but assigns the reference category by alphabetical order
# so the ref category would be female if I just put "Sex" into the model, but you get the same results (AIC etc):
model4a <- lmer(VisDur.S ~ SeasonID + Sex + (1 + SeasonID | PatchID), data = foxdata2, REML = FALSE) 
model5a <- lmer(VisDur.S ~ SeasonID + Sex + (1 + SeasonID + Sex | PatchID), 
               data = foxdata2, REML = FALSE)

# LR test using anova to compare model4 (fixed effect of Sex) with model 5 (random Sex effect)
anova(model5, model4)
anova(model5a, model4a)
# p value significant and suggests Sex effect is not the same across all patches
# so Sex should be included as a random effect


### 5.3.6: Adding a random coefficient for social class (categorical x)

# Test the effect of social status ### SHOULD I EXCLUDE UNKNOWNS???

# first, create 3 dummy variables for social status (dom, sub, unknown), with dominant as the reference category
foxdata2$SSsub <- foxdata2$SocialStatus == "Subordinate"
foxdata2$SSunk <- foxdata2$SocialStatus == "Unknown"
str(foxdata2)
model6a <- lmer(VisDur.S ~ SeasonID + Sex + SSsub + SSunk + (1 + SeasonID | PatchID), data = foxdata2, REML = FALSE)

# add random coefficients for the social status dummy variables:
model6 <- lmer(VisDur.S ~ SeasonID + Sex + SSsub + SSunk + (1 + SeasonID + SSsub + SSunk | PatchID), 
               data = foxdata2, REML = FALSE)

# LR test
anova(model6, model6a)
# significant effect of social status

summary(model6)
# after accounting for effects of season and sex, subordinate foxes have mean visit durations approx 
# 15 secs longer than dominant foxes at the same patch
# Similarly, males have mean visit duration approx 14secs longer than females

## lots of parameters in the random part of this model, simpler to interpret the random coefficient for status by
# computing the between-school variance:
VarCorr(model6)$PatchID # output the variance covariance matrix for the random effects

# Current model equation: 
# scoreij=B0+B1SEASONIDij+B2SEXij+B3STATUSSUBORDij+B4STATUSUNKNOWNij+u0j+u1jSEASONIDij+u3jSTATUSSUBORDij+u4jSTATUSUNKNOWNij+eij
# B = BETA = COEFFICIENT, U = VARIANCE

# calculate the between-patch variance for each social status category using R:
## var(u0j+u3jSTATUSSUBORDij + u4STATUSUNKNOWNij + eij)

# u0j = dominant (ref category)
### for dominants (reference category) = 14764.269

## for subordinates:
14764.269 + 2*-6208.252 + 8969.217 # =11316.98

## for unknown status:
14764.269 + 2*-9143.011 + 8121.1437 # =4599.391

# between-patch variance is lowest for unknowns, highest for dominants
# indicates the patch visited matters most to dominants and least to unknowns.....
# .... but this is assuming a constant effect of sex across all patches (incorrect!)




###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################





### 5.1-5.4 on subset of data (adults only & no unknowns):

#foxdata set has blank cells for foxes of unknown sex and unknown status (= "")
#Take subset of data that excludes cubs & foxes of unknown sex or social status (!= means not-equal-to, == means equal-to)
foxdatasubset <- subset(foxdata2, SocialStatus != "" & Sex != "" & AgeClass != "Cub")
str(foxdata2)
str(foxdatasubset)
#####
# testing whether subset works - still listing 3 factors, even though the subset contains only dominant foxes:
sub<-subset(foxdata, SocialStatus== "Dominant")
sub$SocialStatus #seems to work
sub<-subset(foxdata, SocialStatus!= "Dominant" & SocialStatus!="Subordinate")
view(sub) #seems to work
#####


# change numeric/integer categorical variables to factors
foxdatasubset$AnimalID<-as.factor(foxdatasubset$AnimalID)
foxdatasubset$Territory<-as.factor(foxdatasubset$Territory)
foxdatasubset$PatchID<-as.factor(foxdatasubset$PatchID)
foxdatasubset$BeforeMidnight<-as.factor(foxdatasubset$BeforeMidnight)
foxdatasubset$OtherFoxesPresent<-as.factor(foxdatasubset$OtherFoxesPresent)
foxdatasubset$OtherSpeciesPresent<-as.factor(foxdatasubset$OtherSpeciesPresent)
# re-level seasons in logical order:
print(levels(foxdatasubset$Season)) #automatically listed in alphabetic order
foxdatasubset$Season <- factor(foxdatasubset$Season,levels(foxdatasubset$Season)[c(2,3,1,4)]) #change order
print(levels(foxdatasubset$Season)) #much better
str(foxdatasubset)


# view distribution
hist(foxdatasubset$VisDur.S)  

# null patch random-intercept model for subset
nullmodel <- lmer(VisDur.S ~ (1 | PatchID),  data = foxdatasubset, REML = FALSE)
# patch random-intercept model for subset
model1<- lm(VisDur.S ~ 1, data = foxdatasubset)
# LR test
anova(nullmodel, model1)

# plot residuals of the model
hist(resid(model1)) #negative binomial again...

#To estimate the patch-level residuals
u0 <- ranef(nullmodel, condVar = TRUE) 
  # create a random effects object which contains variance-covariance matrix called postVar
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 
  # Create a dataframe with identifier, residual, and SE for each patch
RPatchID <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind(RPatchID, u0[[1]], u0se)
colnames(u0tab) <- c("RPatchID","u0","u0se")
  # sort this table (called RPatchID) by ascending order based on the values of u0:
u0tab <- u0tab[order(u0tab$u0), ]
  # create a new column containing the ranks:
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank"
  # reorder the table based on the PATCH identifier:
u0tab <- u0tab[order(u0tab$RPatchID),  ]

# Caterpillar plot with 95% confidence intervals:
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional modes of r.e. for RPatch_ID:_cons") # create the plot without any data
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se) # add 95% confidence intervals
points(u0tab$u0rank,  u0tab$u0, col = "blue") # superimpose the point estimates
abline(h = 0, col = "red") # horizontal line corresponding to y=0, the average patch


#5.2:

# Patch random intercept & Season as explanatory variable
model2 <- lmer(VisDur.S ~ Season + (1 | PatchID), data = foxdatasubset, REML = FALSE)
predvisdur <- fitted(model2)

# create a variable to pick out the minimum amount of data required to plot the predicted school lines
datapred <- unique(data.frame(cbind(predvisdur = predvisdur, Season =
                                      foxdatasubset$Season, PatchID = foxdatasubset$PatchID)))

# plot
library(lattice) # need for xyplot
xyplot(predvisdur ~ Season, data = datapred, groups = PatchID, type = c("p","l"), col = "blue")
# in all patches, visit durations decline over the year


#####NOT SURE IF NEXT BIT IS WORTH IT FOR ME??? SEEMS TO ALSO REMOVE LINES!
#####AND IS STILL TREATING SEASON AS A CONTINUOUS VARIABLE...

# Remove patches only observed during one season (e.g. 35 Cooper Rd & 66 Bell Barn Rd)
datapred <- datapred[order(datapred$PatchID, datapred$Season), ]
datapred$multipleseasons <- rep(0, length(datapred$PatchID))
datapred$multipleseasons[datapred$PatchID %in%
                           unique(datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# replot graph with only patches observed during >1 season
xyplot(predvisdur ~ Season, data = datapred[datapred$multipleseasons == 1, ],
       groups = PatchID, type = c("p", "l"), col = "blue")



### 5.3:

# random intercept model with season explanatory variable as null/comparison model:
model3a <- lmer(VisDur.S ~ SeasonID + (1 | PatchID), data = foxdatasubset, REML = FALSE)
model3 <- lmer(VisDur.S ~ SeasonID + (1 + SeasonID | PatchID), data = foxdatasubset, REML = FALSE)
# LR test
anova(model3, model3a) 

# as above but with season as a factor not an integer
model3afac <- lmer(VisDur.S ~ Season + (1 | PatchID), data = foxdatasubset, REML = FALSE)
model3fac <- lmer(VisDur.S ~ Season + (1 + Season | PatchID), data = foxdatasubset, REML = FALSE)
anova(model3fac, model3afac) 


# estimate the school intercepts and slopes and store along with their posterior variances in object myrandomeff
myrandomeff <- ranef(model3, condVar = TRUE)
myrandomeff.fac <- ranef(model3fac, condVar = TRUE)

# draw plot of patch slopes versus the patch intercepts
plot(myrandomeff[[1]], xlab = "Intercept (u0j)", ylab = "Slope of Season (u1j)") #assumes seasonID is a continuous variable
abline(h = 0, col = "red")
abline(v = 0, col = "red")
#as above but with season as factor:
plot(myrandomeff.fac[[1]]) # produces a grid for each factor combo

# NOT SURE HOW TO INTERPRET THIS.


# plot predicted visit duration lines - WITH SEASONID (INTEGER)
predvisdur <- fitted(model3)
# again, will only plot fitted regression lines for patches observed in >1 season:
datapred <- cbind(predvisdur = predvisdur, SeasonID = foxdatasubset$SeasonID, PatchID
                  = foxdatasubset$PatchID)
datapred <- data.frame(unique(datapred))
datapred <- datapred[order(datapred$PatchID, datapred$SeasonID), ]
datapred$multipleseasons <- rep(0, dim(datapred)[1])
datapred$multipleseasons[datapred$PatchID %in% unique
                         (datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# plot (finally)
xyplot(predvisdur ~ SeasonID, data = datapred[datapred$multipleseasons == 1, ]
       , groups = PatchID, type = c("p","l"), col = "blue")



# plot predicted visit duration lines - WITH SEASON (FACTOR) - BETTER
predvisdur <- fitted(model3fac)
# again, will only plot fitted regression lines for patches observed in >1 season:
datapred <- cbind(predvisdur = predvisdur, Season = foxdatasubset$Season, PatchID
                  = foxdatasubset$PatchID)
datapred <- data.frame(unique(datapred))
datapred <- datapred[order(datapred$PatchID, datapred$Season), ]
datapred$multipleseasons <- rep(0, dim(datapred)[1])
datapred$multipleseasons[datapred$PatchID %in% unique
                         (datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# plot (finally)
xyplot(predvisdur ~ Season, data = datapred[datapred$multipleseasons == 1, ]
       , groups = PatchID, type = c("p","l"), col = "blue")
# SHOWS THAT THIS MODEL HELPS CONTROL FOR SEASONAL DIFFERENCES IN PATCH EFFECTS?


# adding level 2 explanatory variables: sex and social status
## Sex:
model4a <- lmer(VisDur.S ~ Season + Sex + (1 + Season | PatchID), data = foxdatasubset, REML = FALSE) 
model4 <- lmer(VisDur.S ~ Season + Sex + (1 + Season + Sex | PatchID), 
                data = foxdatasubset, REML = FALSE)
anova(model4, model4a)

## Social status:
  # random intercept only:
model5a <- lmer(VisDur.S ~ Season + Sex + SocialStatus + (1 + Season | PatchID), data = foxdatasubset, REML = FALSE)
  # add random coefficient for social status:
model5 <- lmer(VisDur.S ~ Season + Sex + SocialStatus + (1 + Season + SocialStatus | PatchID), 
               data = foxdatasubset, REML = FALSE)
anova(model5a, model5) #significant
summary(model5)
VarCorr(model5)$PatchID

# to test significance using a Shapiro test
lm.result<- summary(anova(model5a, model5))
shapiro.test(anova(model5a, model5))
hist(resid(model1)) # A suitable model should have normally-distributed residuals. These are slightly skewed.
shapiro.test(mod5resids$resid) #test if residuals deviate from normal - low P-value suggests they do. (H0="data are normal")
mod5resids<-residuals(model5)

#plot residuals
plot(model5) # automatically plots the resids
plot(resid(model5)) # looks messy
hist(resid(model5)) # still v.skewed?


##############################################

model6 <- lmer(VisDur.S ~ Season + Sex + SocialStatus + Territory + (1 + Season + Sex + SocialStatus + Territory | PatchID), 
               data = foxdatasubset, REML = FALSE)
hist(resid(model6))
plot(model6)
billy<-lm(foxdatasubset$VisDur.S~foxdatasubset$Season)
plot(billy)

 
# 5.4 Adding level 2 (patch) explanatory variables
# e.g. patch quality, Territory (? - patches nested within territory)


# COULD TRY FLO'S MUMIN TECHNIQUE TO BUILD MODEL INSTEAD OF ADDING VARIABLES ONE BY ONE
# NEED TO GET PATCH QUALITY OUT - SUMMARISE EXCEL SHEETS BY NO. DAYS FED & APPROX GRAMS (???)

