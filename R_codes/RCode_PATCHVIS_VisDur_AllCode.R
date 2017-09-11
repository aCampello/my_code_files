rm(list=ls()) #to clear the R workspace
dev.off()
setwd("~/Analysis/MLM Group stats practice")
setwd("C:/Users/User/Desktop/MLM Group stats practice")
mydata <- read.csv(file = "MultilevelModels_PatchResTimes.csv", header = TRUE) #import dataset

library(plyr) # to do calculations on data
library(lme4) # to run regression models
library(visreg) # to visualize regression models
library(MuMIn) # for command 'dredge' # only recommended for exploratory anaysis
library(lattice) # need for xyplot

# Count the total no.other foxes (nsharers) present and add in a new column:
totfoxes<-sapply(strsplit(as.character(mydata$AllFoxSharers), ','), length)
foxdata<-mutate(mydata, Nsharers=ifelse(totfoxes==0, 0, totfoxes-1))

#########
# Adjust visit durat. to not be VisDur+5sec
# This variable wouldn't make sense - can't have zero visit durations if the fox did visit!
foxdata<-mutate(mydata, newvisdur=ifelse(VisDur.S==6, VisDur.S-6, VisDur.S-5)) 
#########

# Add column for whether or not the visit contained only 1 photo (or photo-burst)
# (if yes, shortvisdur =1, otherwise =0)
foxdata<-mutate(mydata, shortvisdur=ifelse(VisDur.S<7, 1, 0))    

## Make new column with unique ID number for each patch in new dataset called foxdata
foxdata<-mutate(foxdata, PatchID=as.integer(foxdata$StationCode))     

#Take subset of data that excludes cubs, foxes of unknown sex/status
# (!= means not-equal-to, == means equal-to)
foxdatasubset <- subset(foxdata, SocialStatus != "" & Sex != "" & AgeClass != "Cub")

#################################
#Take subset of data that excludes cubs, foxes of unknown sex/status and visits with durations >6000secs
foxdatasubset.maxvisdur <- subset(foxdata, SocialStatus != "" & Sex != "" & AgeClass != "Cub" & VisDur.S<6000) #######
#Take subset of data that excludes cubs, foxes of unknown sex/status and visits with durations <=6secs or >6000secs
foxdatasubset.minmaxvisdur <- subset(foxdata, SocialStatus != "" & Sex != "" & AgeClass != "Cub" &  shortvisdur == "0" & VisDur.S<6000)
levels(as.factor(foxdatasubset.minmaxvisdur$shortvisdur)) #check subset does not include short visits (<=6 sec)
#################################

# change numeric/integer categorical variables to factors
foxdatasubset$AnimalID<-as.factor(foxdatasubset$AnimalID)
foxdatasubset$Territory<-as.factor(foxdatasubset$Territory)
foxdatasubset$PatchID<-as.factor(foxdatasubset$PatchID)
foxdatasubset$BeforeMidnight<-as.factor(foxdatasubset$BeforeMidnight)
foxdatasubset$OtherFoxesPresent<-as.factor(foxdatasubset$OtherFoxesPresent)
foxdatasubset$OtherSpeciesPresent<-as.factor(foxdatasubset$OtherSpeciesPresent)

# re-level Seasons, Sex & Status in logical order (and exclude "Unknown" from level lists for later plotting):
print(levels(foxdatasubset$Season)) #automatically listed in alphabetic order
foxdatasubset$Season <- factor(foxdatasubset$Season,levels(foxdatasubset$Season)[c(2,3,1,4)]) #change order
print(levels(foxdatasubset$Season)) #much better
foxdatasubset$Sex <- factor(foxdatasubset$Sex,levels(foxdatasubset$Sex)[c(3,2)]) #change order & remove unknown
foxdatasubset$SocialStatus <- factor(foxdatasubset$SocialStatus,levels(foxdatasubset$SocialStatus)[c(2,3)]) #remove unknown

# use rownames (auto row number) to add a new column for Visit ID number (as the Level 1 unit identifiers) 
foxdatasubset$Visit<-as.numeric(rownames(foxdatasubset))

############################################################
# Check distribution
hist(foxdatasubset$VisDur.S)  #data is skewed (negative binomial distribution?)
plot(density(foxdatasubset$VisDur.S)) #ditto
foxdatasubset$log_visdur<-log(foxdatasubset$VisDur.S) #log-transform visit duration in attempt to normalise
hist(foxdatasubset$log_visdur) #still doesnt follow normal distribution, so will not transform (see below)
############################################################

# Data exploration to identify trends / relationships

# METHOD 1: USING pairs()
attach(foxdatasubset)
data<-data.frame(Sex, SocialStatus, Season, SampleSeasonYear, VisDur.S, newvisdur, PatchQuality, OtherFoxesPresent, BeforeMidnight)
detach(foxdatasubset)
pairs(data)

# METHOD 2*: USING PerformanceAnalytics::chart.Correlation()
# This shows correlation sizes - *easier to interpret
attach(foxdatasubset)
datanum<-data.frame(as.numeric(Sex), as.numeric(SocialStatus), as.numeric(SeasonID), as.numeric(SampleSeasonYear), as.numeric(VisDur.S), as.numeric(PatchQuality), as.numeric(OtherFoxesPresent), as.numeric(BeforeMidnight))
detach(foxdatasubset)
str(datanum)
library(PerformanceAnalytics)
chart.Correlation(datanum, 
                  method="spearman", # Pearson is default but Spearman is more appropriate for non-normal data
                  histogram=TRUE,
                  pch=16)
#for chart.Correlation to work, all data need to be in numberic format
# LOTS OF CORRELATIONS!
# vISIT DURATION SEEMS CORRELATED WITH SEVERAL VARIABLES, INCLUDING SEX, SEASON AND BEFOREMIDNIGHT


###########
# Plotting certain relationships to explore them in more detail:

# interaction.plot(x1,x2,y)
interaction.plot(foxdatasubset$SocialStatus, foxdatasubset$Sex, foxdatasubset$VisDur.S) 
# males visit longer than females
# larger status effect for females on visit duration: dom females stay longer than sub females
# sub males stay slightly longer than dom males

interaction.plot(foxdatasubset$SocialStatus, foxdatasubset$Season, foxdatasubset$VisDur.S) 
# doms longer than subs in sum+aut
# subs longer than doms in spring and winter

interaction.plot(foxdatasubset$Sex, foxdatasubset$Season, foxdatasubset$VisDur.S) 
# doms longer visits than subs in sum+aut
# subs longer than doms in spring and winter

barplot(table(foxdatasubset$BeforeMidnight, foxdatasubset$Season), beside=T) # unstacked bar plot
legend("top",                                            # Add a legend to the plot  
       legend=c("Before midnight", "After midnight"),             # Text for the legend  
       fill=c("grey20", "grey90"))                # colours
# longer visits before midnight

###########################################################
### LEMMA 5.1: Comparing groups using multilevel modelling ###

# FIRST: MAKE NULL MODELS WITH AND WITHOUT RANDOM EFFECTS 
    # (model without = intercept only, i.e. only the response variable)

### 5.1.1 Null single level model of visit duration
SLnull1<- lm(VisDur.S ~ 1, data = foxdatasubset) 

# Null 2-level model of visit duration with only random effects (of Patch ID) = an 'unconditional model'
MLnull1 <- lmer(VisDur.S ~ 1 + (1 | PatchID),  data = foxdatasubset, REML = FALSE)
## 1 | PatchID specifies random effect (which is in this case treated as the random intercept, as
# this is an intercept only model (no predictors for slope)

#compare fit of single and 2-level null models (must put most complex model first?!)
anova(MLnull1, SLnull1) 
# multilevel AIC is significantly lower

# A 3-level null model (with added random effect AnimalID)
MLnull2 <- lmer(VisDur.S ~ 1 + (1|AnimalID) + (1|PatchID), data=foxdatasubset, REML=FALSE)

# compare fit of 2-level and 3-level null models
anova(MLnull2, MLnull1) 
# model with PatchID + AnimalID fits better (signif. lower AIC)


########################################################################################
# PARTITIONING THE VARIANCE
MLnull1 <- lmer(VisDur.S ~ 1 + (1 | PatchID),  data = foxdatasubset, REML = FALSE)
summary(MLnull1) # Mean visit duration across all patches is 197.8 seconds (3 mins 18 seconds)

# Between-patch variance is 5612 (PatchID intercept)
# within-patch variance is 156385 (residual)
# calculate the variance partition coefficient (VPC) by Intercept variance/Total variance
VPC<-5612/(5612+156385)
VPC
# 3.4% of the total variation is between patches and 96.6% is within patches.

MLnull2 <- lmer(VisDur.S ~ 1 + (1|PatchID) + (1|AnimalID), data=foxdatasubset, REML=FALSE)
summary(MLnull2)
# Between-animal variance is 5208 (AnimalID intercept)
# Between-patch variance is 6657 (PatchID intercept)
# Residual variance is 153752
VPC.Patch<-6657/(5208+6657+153752)
VPC.Ani<-5208/(5208+6657+153752)
VPC.Resid<-153752/(5208+6657+153752)
VPC.Patch # 4% of total variance is between patches (Between-Patch variance)
VPC.Ani # 3% variance is between animals (Within-Patch,Between-Fox variance)
VPC.Resid # 93% variance is between visits (Within-Patch,Within-Fox,Between-Visit variance)
########################################################################################
# ALTERNATIVE (EASIER) WAYS TO COMPARE MODEL FIT: LR TESTS AND ANOVA

#likelihood ratio test statistics:
logLik(MLnull1)
logLik(SLnull1)
# Calculate likelihood ratio between the 2 models
LR <- 2*(-309681.5 - -310380.8)
LR #LR = 1398.6 on 1 d.f. (as there is only 1 parameter difference between the 2 models)
# how to test LR is significant / do chi-squared test? - easier to just use ANOVA!

# LR test
anova(MLnull1, SLnull1)
########################################################################################

### LEMMA 5.1.2 Examining patch effects (residuals)

# Quick-plot residuals of the patchID-only model
MLnull1 <- lmer(VisDur.S ~ 1 + (1|PatchID),  data = foxdatasubset, REML = FALSE)
hist(resid(MLnull1)) # negative binomial (?) again...
qqnorm(resid(MLnull1)) # not normal
qqline(resid(MLnull1), col=2) # To add a red line to normality plot

# Calculate patch-level residuals and their SEs and store for each individual patch:
# 1. Estimate the patch-level residuals
u0 <- ranef(MLnull1, condVar = TRUE) #PostVar is depreciated. Must use condVar, not condvar.
  # U0 is a list of lists containing the 36 patch-level residuals

# 2. Create a random effects object which contains variance-covariance matrix called postVar
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 

# 3. Create a dataframe with identifier, residual, and SE for each patch
RPatchID <- as.numeric(rownames(u0[[1]])) #convert row name in table of residuals to PatchID
u0tab <- cbind(RPatchID, u0[[1]], u0se)
colnames(u0tab) <- c("RPatchID","u0","u0se")

# 4. Sort this table (called RPatchID) by ascending order based on the values of u0:
u0tab <- u0tab[order(u0tab$u0), ]

# 5. Create a new column containing the ranks:
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank"

# 6. Re-order the table based on the PATCH identifier (OPTIONAL and easier if don't do it!)
u0tab <- u0tab[order(u0tab$RPatchID),  ]

# To see the patch residual, SE and ranking for a particular patch, we can list the data by using 
# the indexing structure of the R dataframe
# Do this for the first 10 patches in the data:
u0tab[1:10, ]
summary(MLnull1) #fixed effects estimate indicates the mean visit duration for all patches is 197.8 seconds
# For patch 8, which is ranked first (=36 places from the bottom) in u0tab[1:10, ], we estimate the mean visit 
# duration by 197.8 - 176.01 = 21.79 seconds

# Caterpillar plot with 95% confidence intervals to show patch effects in rank order of u0 (residual size):
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "Rank of residual", ylab = "Residuals for PatchID variance components model") # create the plot without any data
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se) # add 95% confidence intervals
points(u0tab$u0rank,  u0tab$u0, col = "blue") # superimpose the point estimates
abline(h = 0, col = "red") # horizontal line corresponding to y=0, the average patch
# patches where CIs don't overlap with the line at zero (mean) differs significantly from average at P<0.05
# many patches are different from the mean: suggests there is variation in VisDur.S not explained by PatchID

# view which patches are at the extremes by looking up in the data frame:
b<-data.frame(foxdatasubset$PatchID,foxdatasubset$Patch) #extract patch name and ID number from dataset
b <- b[ order(b[,1], b[,2]), ] #order by PatchID number ascending
unique(b) # view only the unique values
u0tab # view the list of ranked residuals to look up and compare patches by ID number
# patch 8 has highest residual  (16 West Dene) and patch 4 (12 downs rd) has lowest
# Indicates patch quality might influence rank of residuals, because usually more photos were obtained
# (=larger samples) from high quality patches

# Do the same for AnimalID:
MLnull1.ani <- lmer(VisDur.S ~ (1|AnimalID),  data = foxdatasubset, REML = FALSE)
u0 <- ranef(MLnull1.ani, condVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) 
RAniID <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind(RAniID, u0[[1]], u0se)
colnames(u0tab) <- c("RAniID","u0","u0se")
u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
colnames(u0tab)[4] <- "u0rank"
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "Rank of residual", ylab = "Residuals for AnimalID variance components model") # create the plot without any data
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se) # add 95% confidence intervals
points(u0tab$u0rank,  u0tab$u0, col = "blue") # superimpose the point estimates
abline(h = 0, col = "red") # horizontal line corresponding to y=0, the average patch

b<-data.frame(foxdatasubset$AnimalID,foxdatasubset$Name) #extract ANIMAL NAMES and ID numbers from dataset
b <- b[ order(b[,1], b[,2]), ] #order by PatchID number ascending
unique(b) # view only the unique values
u0tab # view the list of ranked residuals to look up and compare foxes by ID number

# Seems that foxes either have a large or small residual 
    # Suggests a binomial distribution of sighting frequency
# Foxes seen more often have shorter/smaller CIs than those seen few times:
    # Sirius (lowest), Lara (7th lowest) & Zeus (2nd highest) vs. Garnet(highest) & Thalassa (2nd lowest))

# Most individuals are close to the mean, which may suggest there is less variation between Animals 
# than Patches

##########################

# LEMMA 5.2: Adding Fixed-effects / explanatory Variables: Random Intercept Models

# Multilevel model with Season as a fixed effect
ML1 <- lmer(VisDur.S ~ Season + (1|PatchID) + (1|AnimalID), data=foxdatasubset, REML=FALSE)
summary(ML1)
# That null model again
MLnull2 <- lmer(VisDur.S ~ 1 + (1|PatchID) + (1|AnimalID),  data = foxdatasubset, REML = FALSE)
summary(MLnull2)
anova(ML1, MLnull2) #ML1 with Season as fixed effect has a lower AIC than the null model


# Plot to visualize change in VisDur with season with sep lines for patches:
# Multilevel model with only PatchID and Season
ML0 <- lmer(VisDur.S ~ Season + (1|PatchID), data=foxdatasubset, REML=FALSE)
# save predicted value for each patch
predvisdur <- fitted(ML0)

# create a variable to pick out the minimum amount of data required to plot the predicted  lines
datapred <- unique(data.frame(cbind(predvisdur = predvisdur, Season =
                                      foxdatasubset$Season, PatchID = foxdatasubset$PatchID)))

# plot change in visit duration with Season, with sep lines for each patch
xyplot(predvisdur ~ Season, data = datapred, groups = PatchID, type = c("p","l"), col = "blue")

# Remove patches only observed during one season (e.g. 35 Cooper Rd & 66 Bell Barn Rd)
datapred <- datapred[order(datapred$PatchID, datapred$Season), ]
datapred$multipleseasons <- rep(0, length(datapred$PatchID))
datapred$multipleseasons[datapred$PatchID %in%
                           unique(datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# replot graph with only patches observed during >1 season
xyplot(predvisdur ~ Season, data = datapred[datapred$multipleseasons == 1, ],
       groups = PatchID, type = c("p", "l"), col = "blue")
# = IN ALL PATCHES, VISIT DURATION DECLINES THROUGHOUT THE YEAR

##########
##########
########## SAME BUT WITH SEPARATE LINES FOR ANIMAL ID INSTEAD OF PATCH:

# Plot to visualize change in VisDur with season with sep lines for AnimalIDs:
# Multilevel model with only AnimalID and Season
ML3 <- lmer(VisDur.S ~ Season + (1|AnimalID), data=foxdatasubset, REML=FALSE)
# save predicted value for each animal
predvisdur <- fitted(ML3)

# create a variable to pick out the minimum amount of data required to plot the predicted lines
datapred <- unique(data.frame(cbind(predvisdur = predvisdur, Season =
                                      foxdatasubset$Season, AnimalID = foxdatasubset$AnimalID)))

# plot change in visit duration with Season, with sep lines for each fox
xyplot(predvisdur ~ Season, data = datapred, groups = AnimalID, type = c("p","l"), col = "blue")
# for all foxes, visit durations decline over the year

# Remove foxes only observed during one season
datapred <- datapred[order(datapred$AnimalID, datapred$Season), ]
datapred$multipleanimals <- rep(0, length(datapred$AnimalID))
datapred$multipleanimals[datapred$AnimalID %in%
                           unique(datapred$AnimalID[duplicated(datapred$AnimalID)])] <- 1
# replot graph with only foxes observed during >1 season
xyplot(predvisdur ~ Season, data = datapred[datapred$multipleanimals == 1, ],
       groups = AnimalID, type = c("p", "l"), col = "blue")
# = VISIT DURATION DECLINES THROUGHOUT THE YEAR FOR ALL FOXES

########################

# LEMMA 5.3: Allowing for Different Slopes across patches: Random Slope Models

# random intercept model with season fixed effect as null model:
ML4null <- lmer(VisDur.S ~ Season + (1 | PatchID)+ (1|AnimalID), data = foxdatasubset, REML = FALSE)

# random slope model with season as fixed effect
ML4 <- lmer(VisDur.S ~ Season + (1 + Season | PatchID) + (1|AnimalID), data = foxdatasubset, REML = FALSE)
summary(ML4null) # don't panic, there are not meant to be stars here!
summary(ML4)

### 5.3.1 Testing for random slopes
anova(ML4, ML4null) # effect (slope) of Season does vary between patches

# Adding random slope between Season and AnimalID too
ML5null <- lmer(VisDur.S ~ Season + (1 + Season | PatchID) + (1|AnimalID), data = foxdatasubset, REML = FALSE)
ML5 <- lmer(VisDur.S ~ Season + (1+Season|PatchID) + (1+Season|AnimalID), data = foxdatasubset, REML = FALSE)
anova(ML5, ML5null)

# Data not meeting model assumptions may be causing everything to look significant!
# Linear mixed model assumptions: patch-level residuals follow a normal distribution...
# What distribution best fits my data?
library(MASS)
library(car)
qqp(resid(ML5), "norm")  # compare residuals to normal distribution
qqp(resid(ML5), "lnorm") # compare residuals to lognormal distribution

library(fitdistrplus) # 2014 package for fitting distributions in R
plot(fitdist(residuals(ML5),"norm"))  #Random slope multilevel model with season,patch+animal

#plot cullen & frey graph
descdist(residuals(ML5), boot = 1000) # possibly Weibull?

hist(foxdatasubset$VisDur.S) # left-skewed
hist(log(foxdatasubset$VisDur.S)) # large gap between shortvisits (0-6 secs, with 1 photo(burst)) and
# longer visits (with min 2 photo(bursts) - could indicate binomial distribution)

# Plot only longvisits
foxlongdur <- subset(foxdatasubset, shortvisdur == "0") #take subset of data with only visits >6secs
str(foxlongdur)
hist(foxlongdur$VisDur.S) # plot (skewed)
hist(log(foxlongdur$VisDur.S)) #  Log transformation produces an approx normal distribution! 
                               #  Peak at ~20sec due to camera functioning (recharge/recovery time)

# ANALYSE VISIT DURATION IN 2 PARTS:
# 1. Use multilevel logistic (binomial) regression to see Which explanatory variables affect the 
# likelihood of a fox remaining in a patch long enough to obtain >1 photo/burst? 
      # (short vs. long visdur, 0/1)
# 2. Use ordinary linear mixed models to analyse log-transformed long (>6sec) visit durations (only)

############################################################################################

### 5.3.3: Examining intercept and slope residuals for patches

# estimate the patch intercepts and slopes and store along with their posterior variances in object myrandomeff
myrandomeff <- ranef(ML5, condVar = TRUE)

# draw plot of patch slopes versus the patch intercepts
plot(myrandomeff[[1]]) # produces a grid for each factor combo

# plot predicted visit duration lines - WITH SEASONID (INTEGER)
predvisdur <- fitted(ML5)

# again, will only plot fitted regression lines for patches observed in >1 season:
datapred <- cbind(predvisdur = predvisdur, Season = foxdatasubset$Season, PatchID
                  = foxdatasubset$PatchID)
datapred <- data.frame(unique(datapred))
datapred <- datapred[order(datapred$PatchID, datapred$Season), ]
datapred$multipleseasons <- rep(0, dim(datapred)[1])
datapred$multipleseasons[datapred$PatchID %in% unique
                         (datapred$PatchID[duplicated(datapred$PatchID)])] <- 1
# plot (finally)
library(lattice)
xyplot(predvisdur ~ Season, data = datapred[datapred$multipleseasons == 1, ]
       , groups = PatchID, type = c("p","l"), col = "blue")
# again, will only plot fitted regression lines for patches observed in >1 season:
datapred <- cbind(predvisdur = predvisdur, Season = foxdatasubset$Season, PatchID
                  = foxdatasubset$PatchID)
# doesn't make much sense because ML5 includes random effects for both animal and patch

################################################################################################

# P5.3.5 Adding a random coefficient for Sex (dichotomous x )
# first, add a fixed effect for gender (only) to use as a comparison model for testing for a random coefficient
# adding level 2 explanatory variables: sex and social status
## Sex:
model4a <- lmer(VisDur.S ~ Season + Sex + (1 + Season | PatchID), data = foxdatasubset, REML = FALSE) 

model4 <- lmer(VisDur.S ~ Season + Sex + (1 + Season + Sex | PatchID), 
               data = foxdatasubset, REML = FALSE)

anova(model4, model4a)
# p value significant and suggests Sex effect is not the same across all patches
# so Sex should be included as a random effect


## Adding a random coefficient for social status:

# model with random intercept only:
model5a <- lmer(VisDur.S ~ Season + Sex + SocialStatus + (1 + Season | PatchID), data = foxdatasubset, REML = FALSE)

# add random coefficient for social status:
model5 <- lmer(VisDur.S ~ Season + Sex + SocialStatus + (1 + Season + SocialStatus | PatchID), 
               data = foxdatasubset, REML = FALSE)

anova(model5a, model5) #significant
summary(model5)
VarCorr(model5)$PatchID

#plot residuals
plot(model5) # automatically plots the resids
plot(resid(model5)) # looks messy
hist(resid(model5)) # still v.skewed?
###############################

### 5.3.6: Adding a random coefficient for social class (categorical x)

# Test the effect of social status

model6a <- lmer(VisDur.S ~ Season + Sex + SocialStatus + (1 + SeasonID | PatchID), data = foxdatasubset, REML = FALSE)
model6 <- lmer(VisDur.S ~ Season + Sex + SocialStatus + (1 + Season + Sex + SocialStatus | PatchID), 
               data = foxdatasubset, REML = FALSE)
hist(resid(model6))

# LR test
anova(model6, model6a)
# significant effect of social status

summary(model6)
# after accounting for effects of season and sex, subordinate foxes have mean visit durations approx 
# 15 secs longer than dominant foxes at the same patch
# Similarly, males have mean visit duration approx 14secs longer than females
