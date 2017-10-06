# (previously this file was named "RCode_GENERAL_Useful bits and bobs.R"

#=================================#
#     GLOSSARY OF USEFUL CODE     #
#=================================#

############## CONTENTS ################

# AICc - see model selection section below

# APPLY function in plyr package

# Annotate plots

# ANOVA

# Back-transformation

# Binomial exact test

# Categorise continuous variables - split into categories

# Center variables

# Cleaning data

# Clear workspace

# Colours

# Confidence intervals

# Contrasts

# Correlation test

# CSV - import and save dataframes as csv files

# Data frames

# Datetime formats in Excel

# Datetime formats in R
## Convert character time durations using lubridate
## Convert hh:mm:ss time to degrees and radians

# Distributions

# ddply - organising data
### remove duplicate rows
### combine two columns using tidyr

# Descriptive stats
### basic: mean, sd, se and N
### summary stats using ddply
### mutate to add summary value to new column without grouping data

# Dispersion tests (under vs. over-dispersion)

# Distributions: plotting data to check

# Effect sizes
## Interpretation
## Effect sizes of contrasts

# Errors in R scripts/packages

# For loops

# Functions

# General useful code

# ggplot2

# glmmADMB package - for -inflated mixed models, negbinoms etc

# Good coding practices

# Ifelse

# Interactions

# Linear regression / single level linear models

# Loops

# Match - to populate columns conditioned on values in other rows (package plyr)

# Matrices
## Making matrices
## Matrix structure - navigating 3D matrices

# Merge data frames

# MIXED MODELS
## Generalised linear mixed models (GLMMS)
### Problems with convergence: fitting different optimisers
## Linear mixed models
## Mixed models for overdispersed count data - poisson with observation-level ranef -> lognormal poisson

# Model checking
## Dharma package for checking GLMMs
## Check for multicollinearity
## Goodness-of-fit tests
## Check for influential data points in mixed models

# Model matrix

# Model selection table with AICc etc

# NA / missing values

# Network centrality measures

# Modelling network centrality measures

# Network analysis - mean edge weight

# Normalisation, normalising, standardising

# Normality testing

# Numbers
## seq() etc.

# Outliers

# Packages
## versions, citing, updating etc

# Pipes

# Plotting
### Basic plot in base R
### Multi-panel plots
### Custom axis labels with position specified by coordinates - i.e. add labels manually
### Make text italic
### Add model equation (or other text or annotations) to plot
### Export plot as high res for publications
### plotting model outputs
### Add shapes/symbols to geom_points

# Post hoc tests
## Of significant interactions: glht or lsmeans

# Predicted values from models

# Principal components analysis (PCA)

# Progress bar for model fitting

# Proportion data: binomial model woth weights argument in lme4 glmer

# Rank within groups

# Rearrange data frames

# Regression coefficients - interpretation

# Relevelling a varible

# Repeated measures data

# Repeatability & rptR

# Robust SEs and p-values - package 'sandwich'

# Round coefficients etc to nearest x number

# R-SQUARED FOR MIXED MODELS

# RStan

# Sort a data frame by a column

# Split character variables using strsplit

# Structure of data - view

# Subset data

# Time functions / code (timing)

# T-tests

# Variance inflation factor

# View structure of data / objects

# Wilcoxon signed rank test

# Zero-inflation: testing for ~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Annotate plots in base R ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# see http://statisticsr.blogspot.co.uk/2008/01/special-symbols-on-r-plot.html

text(x, y, "sometext") # coordinates refer to position on axis, so use the same scale as is on the plot
text(x, y, expression()) # to add mathematical or greek symbols or an equation, e.g.:
text(06, 0.2, expression(hat(Delta)[4]== 0.91  (0.894-0.920))) # lower case delta or uppercase Delta look different
[4] # means subscript 4
^4 # means superscript 4

# use paste(" ") to add spaces or display zeros (otherwise R ignores zeros, bad if need consistent d.p.)
text(x, y, expression(hat(Delta)[4]== paste("0.91 ", (0.89-0.92))), cex=1.1)
# if the equation was 0.90 (0.80-0.92) must use paste():
text(x, y, expression(hat(Delta)[4]== paste("0.91 (0.80-0.90)"))), cex=1.1)

# Add tabs using \t

# bquote() is used to add symbols to equations in axis titles and headings:
text(x,y, bquote(paste("QP: ",sigma^2==.(round(modelA,1))*mu)), col=2) 
plot(billy, main=bquote(paste(bold("Autumn "), hat(Delta)[4]==0.89 (0.88-0.90))))

bold("text") # to make text bold

# add title to plot
title("Spring", adj=0) # adj=0 means left-justify, adj=1 means right-justify, otherwise centered by default

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ANOVA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis of variance to run a linear regression model
anova.model<-(aov(mydata$Group.size~mydata$T))
summary(anova.model) 

# anova to compare the fit of two nested models - test the significance of a predictor by comparing model with and without it
anova(model1, model2) # gives a chi-sq goodness of fit if model includes categorical predictors, or an F-test for continuous predictors

# anova gives same result as a likelihood ratio test using package lrtest:
lmtest::lrtest(model1, model2)

#===================================#
# APPLY function in plyr package ####
#===================================#
# apply the same function to each row in a matrix
a <- apply(shortest.paths(T1winB4_net_graph), 1, sum) # get sum per node of shortest paths without takng reciprocal


#=================================================================================#
# Back-transform data from GLMMs to get it on the scale of X rather than the link: ####
#=================================================================================#

# Parameter values returned by coef() or summary() are on the scale of the link
# function, not the untransformed predictor variables.  Use the inverse of the link 
# function to get parameter values back on the scale of x, or use the function 
# predict with the type="response" argument.

# Back-transform model coefficients and save in new table: 
# can't inverse-log negative values so the meaning of coefficients (-/+) disappeared as
# they were all positive after transformation:-

# = Report model coefficients as they are in summary(model) and backtransform predictions in plots/lsmeans

# Save model coefficients as a table
mod<-coef(summary(glm1)) # add chisq, df and p-values from anova() likelihood ratio tests. For interactions, compare interaction model vs. additive model

# view particular random effects:
coef(erateMOD_full_intranef)$Place
coef(erateMOD_full_intranef)$`Place:ShortCode` # for interaction random effects put in apostrophes
ranef(erateMOD_full_intranef)$`Place:ShortCode` 
# plot random effects
lattice::dotplot(ranef(strength_model_full,condVar=TRUE)) # have to scroll between plots if have multiple random effects. Condvar adds CI (?) bars


# - or coef(summary(fit_poiss))can customise column names by doing it this way:
mod<-cbind(Estimate=coef(summary(glm1))[, 1], SE=coef(summary(glm1))[, 2],
           z.value=coef(summary(glm1))[, 3], p.value=coef(summary(glm1))[, 4])
# calculate the inverse of model coefficients and save as new table
inverse.mod<-exp(mod)


# to get parameter values back on the scale of X:
predict(glm1, type="response") # better!! Use for plotting. 

# Error family:	  Default link:	 Inverse of link:	  Use for:
# Gaussian	      identity	     1		              Normally distributed error
# Poisson		      natural log	   exp(x)		          Counts (many s, various integers)
# Binomial	      logit	      	 1/(1+1/exp(x))  	  Proportions or binary (0,1) data
# Gamma		        inverse	       1/x		            Continuous data with non-constant error (constant CV)


# To back-transform means in groups use lsmeans (detach lmerTest if get probs or use lsmeans::)
lsmeans(glmm3, ~ season * Origin) # least squares means on log scale (as model = glmer poisson with log-link)
lsmeans(glmm3, ~ season | Origin) # use vertical line separator to display in clearer table
summary(lsmeans(glmm3, ~ season | Origin), type = "response") 
# 'rate' = least sq mean backtransformed from the log scale = equal to 
# 'the adjusted mean' from package phia command InteractionsMeans
# but SEs are not same from phia and lsmeans


#=======================#
# Binomial exact test ####
#======================#
# syntax = binom.test(number of successes, total observations, expected proportion)
# expected proportion is typically 0.5 - so 50% chance of either 0 or 1, male or female etc
# E.g....
binom.test(2442, sum(sum_status_sub$freq), 0.5) # see p.37 Rob Thomas' R book



#=================================#
# Categorise continuous variables ####
#=================================#

# Split DF and MJ into equal-sized categories

# cut(...,3) divides the range of the original data into three ranges of equal lengths
# ggplot2::cut_number() & Hmisc::cut2() split by quartiles so equal observations in diff categories.

# Split into categories with EQUAL number of obs
patchdata$MJcateg <- as.numeric(cut_number(patchdata$MeanMJperDay,2)) 
# see n observations in each category
table(patchdata$MJcateg)
# view the levels:
cut_number(patchdata$MeanMJperDay,2) # Levels = 0.108-0.736 & 0.736-5.87.

# Split into categories of equal length 
patchdata$DFcateg <- as.numeric(cut(patchdata$DaysFedPerWeek, breaks = 2))
table(patchdata$DFcateg) # see n observations in each category



#======================#
# Center variables  ####
#======================#
#http://rtutorialseries.blogspot.co.uk/2012/03/r-tutorial-series-centering-variables.html
# to center a variable, subtract the overall  mean from each  data point
# z-scores are made by subtracting the overall mean from each data point and then dividing that by the overall SD
scale(variable, center = TRUE, scale = FALSE) # to center only
scale(variable, center = TRUE, scale = TRUE)  # to create z-scores



#==================#
# Cleaning data ####
#==================#

# Replace NAs with zeros
replace(df, is.na(df), 0) 
df[is.na(df)] <- 0 # better
centrality$maincomp[is.na(centrality$maincomp)] <- 0 # using column names

# Replace zeros with NAs
df[,25][df[,25] == 0] <- NA # assuming column of interest is column #25
centrality$eig_maincomp[centrality$eig_maincomp  == 0] <- NA  # with column names instead

# Replace data with missing value coded as -9999 with NA (across the whole dataframe)
df[df == -9999] <- NA        # = "look at df; take all the things in df that equal -9999, replace them with NA"

#== Replace empty cells in specific column with NAs
df$sex[df$sex==''] <- NA


# NOTE: sometimes empty cells are treated as factor levels and no matter what I can't get rid of the empty factors (converting to NA or zero doesnt work)
# avoid this problem by importing data with stringsAsFactors = FALSE & then remove or rename the empty 
# cells as NA before converting the variable to a factor.

#== Adjust or rename datapoints, e.g. if some rows are 'Male' and some are 'male' convert them all to 'male'
data$sex[data$sex=='Male'] <- 'male'

#== change signs of values: negative to positive
centrality$eig_maincomp <- abs(centrality$eig) # abs = absolute value irrespective of sign

#== Delete column from data frame
centrality$fragment <- NULL

#== Rename column in data frame
names(df)[names(df) == 'old.var.name'] <- 'new.var.name' # template
names(TAll_attr2)[names(TAll_attr2) == 'closeness'] <- 'closeness_maincomp' # example

#=== row numbers and row names
row.names(attribs) # view / refer to row id number in table (row number when first imported, doesn't change when re-order the dataframe)
1:nrow(attribs) # view row sequence

#=== Concatenate strings or numbers in a data frame
paste(item1, item2, sep=c("", "-", "_", "."))

# Concatenate rounded values in Excel
=ROUND(A1,1) & " ? " & ROUND(A2,1) # use ROUND with '&' to combine rather than concatenate with commas



#===========================================#
# CLEAR WORKSPACE (deletes all objects)  ####
#===========================================#
rm(list=ls()) 


#===========================================#
# Colours ####
#===========================================#
col.rainbow <- rainbow(12)
col.topo <- topo.colors(12)
palette(col.topo) #just run one of the palette lines and then run the plot() function to use that palette
palette(col.rainbow)
palette() #view palette
plot_colours <- c("blue","red","forestgreen", "black", "orange", "purple", "skyblue") #specify colours



#==============================#
# Compute confidence intervals ####
#==============================#
# Compute 95% CIs (takes a while to compute) # .sig01 etc are random effects
REDUCED.nb2_cis<-confint(REDUCED.nb2) 
data.frame(round(REDUCED.nb2_cis, 4)) # rounded to 4 d.ps

# Get 95% CI for median
sort(MEANdailyPRT$meanPRT)[qbinom(c(.025,.975), length(MEANdailyPRT$meanPRT), 0.5)]

# Alternative method using boostrapping to get 95% CI for median
x = MEANdailyPRT$meanPRT
bootmed = apply(matrix(sample(x, rep=TRUE, 10^4*length(x)), nrow=10^4), 1, median)
quantile(bootmed, c(0.025, 0.975))

# Inter-quartile range for median (like SD for median, measure of variation)
IQR(data$MeanMJperFeedingDay)

# Median absolute deviation (like SD for medians, more robust than IQR)
mad(data$MeanMJperFeedingDay)

#===========================================#
# Contrasts ####
#===========================================#

#== Types of contrasts:
# T-contrasts (vector (one line)) are between levels of the same factor e.g. spring-summer.
# T-contrasts are directional: +/- to indicate direction of effect.

# F contrasts (matrix) are between different factors, e.g. spring-neighbour vs summer-neighbour. 
# F-contrasts are unidirectional: just indicate size of difference but not its direction.


# Did not use contrasts for group size models: just did post hoc tests to compare all possible combinations using lsmeans
# But could have first compared first spring+summer with aut+winter, because
# expect diffs between these seasons (behav is centered on cub rearing in spr+sum, 
# but instead on dispersal & mating in aut+wint). 
# Then use additional contrasts to separate spr+sum and aut+wint

# To reset contrasts to default
options(contrasts = c("contr.treatment", "contr.poly"))


# Testing lme() origins*season model with specificcontrasts
levels(all3defs$season)
SprvsSum<-c(-1,1,0,0) #orthogonal contrasts - non-orthog would contain diff numbers of levels in each set being contrasted
SprvsAut<-c(-1,0,1,0)
SprvsWint<-c(-1,0,0,1)
SumvsAut<-c(0,-1,1,0)
SumvsWint<-c(0,1,0,-1)
AutvsWint<-c(0,0,1,-1)
contrasts(all3defs$season)<-cbind(SprvsSum, SprvsAut, SprvsWint, SumvsAut, SumvsWint, AutvsWint)

levels(origins$Origin)
NvsPGM<-c(-1,1,0)
NvsS<-c(-1,0,1)
PGMvsS<-c(0,-1,1)
contrasts(origins$Origin)<-cbind(NvsPGM, NvsS, PGMvsS)

# or use a package...
library(contrast) # doesn't work with glmmADMB models
CONTRASTsexstatINseason <- contrast(sexstatfullnestmod_ziNB1,
                                    MD = list(SexStat="M:Dom", Season=levels(fSeason), DaysSeen>19, DaysFed=mean(DaysFed)),
                                    FD = list(SexStat="F:Dom", Season=levels(fSeason), DaysSeen>19, DaysFed=mean(DaysFed)),
                                    MS = list(SexStat="M:Sub", Season=levels(fSeason), DaysSeen>19, DaysFed=mean(DaysFed)),
                                    FS = list(SexStat="F:Sub", Season=levels(fSeason), DaysSeen>19, DaysFed=mean(DaysFed)))


#==================#
# Correlation test ####
#==================#
# Test for correlations when choosing fixed effects: Spearman rank test for non-normal paired data
# ERate ~ DaysSeen
plot(DailyERate$DaysSeen, DailyERate$Erate) # looks correlated
# spearman rank for non-normal data (delete method="spearm" to run default pearson product-moment correlation)
cor.test(as.integer(DailyERate$DaysSeen), DailyERate$Erate,
         alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # yes, so need in model
# alternative="two.sided" means null hypothesis is that DaysSeen is correlated (either positively or negatively) with NTrueAssocs.
# correlation (rho) shows the strength and direction of the correlation (0.5-1 is strong)
# p-value shows likelihood that the observed correlation occured by chance, so if have strong correlation but non-sig p-val then corr is prob due to chance/error/data sampling and not really extant in the pop.

# Report as rs(df-2)=XXX, p=XXX
# where df=number of pairs, so min length of the two variables being tested - calc by:
min(length(visitdata$PatchDaysFedPW), length(visitdata$MeanMJPerDay))-2 # -2 as used two tailed test
# rs (r subscript s) = rho, the spearman correlation coefficient



#======================================#
# CSV - Save data frame as csv file ####
#======================================#

# typical method, but slow for big datasets (>2GB)
read.csv("Filename.csv") # open file in R

#= Reading CSVs using Fread

# For importing big datasets data.table::fread is much faster than read.csv or read_csv
mydata <- fread(csv_name, drop = c("Acorn", "Acorn_grouped")) # exclude irrelevant cols

# Can use fread to import and combine multiple CSV files 
# (might overload R's RAM and crash it/slow it down loads):

# first make a list of all the filenames in the target folder
file_list <- list.files(path = "~/my_code_files/R_codes/gla_interview_task_011017/smart_meter_data/separate_csvs") # this particular example did not work - files too large
# set wd to same directory as the files in file_list.
setwd("~/my_code_files/R_codes/gla_interview_task_011017/smart_meter_data/separate_csvs") 
# put all the csv files in a list
lst <- lapply(file_list, fread, sep=",") 
# combine them (rbindlist is faster than rbind)
mydata <- rbindlist(lst)

# Note Fread does not modify variable names if they include spaces, e.g. KWH (per hour) 
# is not changed (whereas read.csv() would change it to kwh.per.hour.)
# so to rename variables containing spaces use quote marks:
mydata <- rename(mydata, kwh = 'KWH/hh (per half hour)')  # changes name to kwh


# saves data as csv file in current working directory
write.csv(attribs.NC, file="Filename.csv", row.names = FALSE) 
fwrite(daily_kwh_household, file = "daily_kwh_household.csv") # 30x faster!



#==============================#
# Datetime formats in Excel ####
#==============================#
# Convert number of seconds to hours, minutes and seconds
=TEXT(A1/(24*60*60),"hh:mm:ss") # when cell A1 contains number of seconds
=TEXT(A1/(24*60*60),"mm:ss") # or just mins and secs



#==========================#
# Datetime formats in R ####
#==========================#

# Helpsheet for working with dates and times in R: http://www.aridhia.com/technical-tutorials/working-with-date-times-and-time-zones-in-r/

## Convert numerical or character values to dates
# http://www.statmethods.net/input/dates.html
date_list = c("2007-06-22", "2004-02-13")    # list of string dates
mydates <- as.Date(date_list)    # converts to date type, default format is yyyy-mm-dd
mydates = as.Date(date_list, "%m/%d/%y") # sometimes have to specify input date type

datetime <-as.POSIXct(paste(datacsv$MidEncounterDateTime),format="%d/%m/%Y %H:%M:%S", tz="UTC")
# as.POSIXct = calendar time: a numeric vector of N seconds since the origin (Jan 1st 1970).
# as.POSIXlt = local time: a LIST of time attributes (y,m,d,h,m,s)

class(mydata$datetime)  # check class of objects

# Think these two lines do the same thing - convert DAY to a date
datacsv$DAY<-as.Date(datacsv$DAY, format = "%d/%m/%Y") # this works on 'POSIXct'
datacsv$Date <- strptime(datacsv$DAY,format="%d/%m/%Y") # this works on 'character'

# POSIXCT data type messes with plyr so don't convert date before using plyr - or delete the column.
# BUT: POSIXCT seems ok with dplyr.

#= Good resource for extracting different components from datetimes:
# https://www.aridhia.com/technical-tutorials/working-with-date-times-and-time-zones-in-r/
# http://neondataskills.org/R/time-series-convert-date-time-class-POSIX/
  
# first convert column of datetimes into POSIXct format...
mydata$dt = as.POSIXct(paste(mydata$DateTime)) 
# ...then use strftime to convert the POSIXct to POSIXlt (list of dt components) and extract the year, day, time etc.
mydata$year = strftime(mydata$dt, format = "%Y") 
mydata$day = strftime(mydata$dt, format = "%D")
mydata$t = strftime(mydata$dt, format = "%H:%M:%S")





#==== Convert datetimes and durations using package lubridate
library(lubridate)
# Convert encounter durations to number of seconds 
datacsv$duration <- hms(datacsv$EncounterDuration)  # format character to 'hours:minutes:seconds'
datacsv$duration.s <- hour(datacsv$duration)*3600 + minute(datacsv$duration)*60 + second(datacsv$duration) # extract components of the datetime and convert to N seconds


#==== Convert hh:mm:ss time to degrees and radians
# Convert times in hh:mm:ss to decimal degrees (so betweeo 0-360 degrees based on the 24-hour clock)
times <- c("12:01:50", "18:00:10", "21:00:05", "23:59:30")
library(celestial)
degtimes <- hms2deg(times)

# Then convert the decimal degree to radians
library(circular)
radtimes <- rad(degtimes)

# convert radians back to degrees (to check it worked!)
deg(radtimes) # package 'circular'



#================#
# Data frames ####
#================#

# save a data frame or R object
save(mydata, file = "saved_data.RData") # filename doesn't rename the dataframe itself - i.e. when loaded back into R it will still be called 'mydata'

# transpose data frame
a <- t(df) 

#== wide vs long data formats
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

# convert from wide to long using tidyr::gather()
long_dat <- gather(wide_dat, weight_time, measurement_grams, Weight1:Weight3, factor_key = TRUE) 
# factor_key=TRUE makes the grouping variable into a factor

# convert from long to wide using tidyr::spread()
wide_dat_new <- spread(long_dat, weight_time, measurement_grams)

# can also use reshape2::melt() and reshape2::dcast()
# or base R's functions reshape() stack() and unstack() - but these 3 are harder to use.





#================================#
# ddply - organising datasets ####
#================================#
# POSIXCT data type messes with plyr so don't convert date

# Recode levels of factors and save in new column ***Can't do if import dataset using 'stringsAsFactors=FALSE' as factors will be saved as characters. Need to be factors.
library(plyr) 
levels(attribs$Sex)
attribs$sex2 <- revalue(attribs$Sex, c("Male"="M", "Female"="F", "Unknown" = "Usx"))
levels(attribs$SocialStatus)
attribs$status2 <- revalue(attribs$SocialStatus, c("Dominant"="Dom", "Subordinate"="Sub", "Unknown" = "Ust"))

# Remove duplicate rows (for diff seasons) for foxes in attribute data
library(plyr) 
unique_attribs<- subset(attribs, !duplicated(ShortCode))
attr <- attribs[!duplicated(attribs[1:3]),] # lists unique combinations of columns 1-3 (to remove duplicated rows for before/after midnight))


# Combine 2 columns using tidyr:
library(tidyr)
attribs<-unite(data=attribs, "sexstat", c(sex2, status2), sep="-")

# Change column names
### e.g. from AnimalID to ShortCode in a sampling periods array or network
colnames(net) <- attribs$ShortCode # rename columns: must be in same order as matrix
rownames(net) <- attribs$ShortCode # rename rows: must be in same order as matrix


### Use mutate to count number of unique values without grouping
a <-ddply(mydata, .(AnimalID, ShortCode, SeasonID), mutate, Nterritories = length(unique(Territory))) # counts number of different territories visited per territory for each fox


#===========================================#
# Descriptive stats ####
#===========================================#
mean(mydata$Group.size)
sd<-sd(mydata$Group.size)
N<-length(mydata$Group.size)
se<-(sd / sqrt(N))
se
table(data$variable) # view frequency of diff counts - like histogram but in numbers

# Get median and median confidence interval
library(asbio)
ci.median(mydata$MeanPhotosPerVisit)
          
# Summary stats using ddply:

# e.g. find mean group sizes in each territory
# 
groupsizesummary <- ddply(mydata, c("SiteID"), summarise,
                          N    = length(Group.size),
                          mean = mean(Group.size),
                          sd   = sd(Group.size),
                          se   = sd / sqrt(N),
                          overallmean = mean(mydata$Group.size)) # include mydata$ to calc mean across all rows ignoring grouping variables (i.e. SiteID here)
groupsizesummary # view summary table
plot(groupsizesummary$SeasonID, groupsizesummary$mean)
line(groupsizesummary$mean~groupsizesummary$SeasonID, col="black") # regression line (y~x) 
# NOTE: SD comes out as 'NA' when two variables have the same name, e.g. if calculating mean of means, can't say 
# nfoxes=mean(nfoxes), have to rename NEWnfoxes=mean(nfoxes), otherwise when say sd(nfoxes) it calculates it from 
# the new mean variable not the one in the original data frame. 

# NOTE on Error in attributes(out) <- attributes(col) : 'names' attribute [11] must be the same length as the vector [10]
# This seems to be if have a POSIXct date column in dataset. Make a new dataframe with just the required columns, excluding the POSIXct one
# e.g. somedata<-data.frame(mydata$AnimalID, mydata$SeasonID, mydata$Territory)
# And then run the ddply code on that


#===========================================================================================#
# Dispersion tests: only for POISSON models, or BINOMIAL models fit to proportional data ####
#===========================================================================================#

# ALWAYS CHECK FOR OVERDISPERSION FIRST WHEN MODEL CHECKING (poisson or binomial)

# For overdispersed poisson or binomial proportion data can use observation-level ranef (OLR) to convert model to lognormal poisson which can fit better than using a neg binom (Elston 2001 & Harrison 2014)
# But need to check models using a slightly different plot function:
# plot(resid, fitted) is not appropriate for poisson models with an OLR because the resid and fitted functions treat the OLR as part of the fitted values when for the Poisson-lognormal it should be part of the residuals

# Plot code from thread posted in R space for lognormal poisson resid plotting https://stat.ethz.ch/pipermail/r-sig-mixed-models/2013q3/020770.html
Fitted <- exp(log(fitted(mod)) - ranef(mod)$obs[[1]])
Resid <- (dat$response - Fitted) / sqrt(Fitted + (Fitted^2) * c(exp(VarCorr(mod)$obs) - 1)) 
plot(Fitted, Resid) # for model checking for overdispersed poisson models
lines(lowess(Fitted,Resid),col="red",lwd=2) # add a lowess smoothing line - should be approx flat

#Function to calculate a point estimate of overdispersion from a mixed model object 
# point estimate is just an estimate of a parameter like finding the mean, median, variance etc.
# = from Harrison 2014 PeerJ paper
od.point<-function(modelobject){
  x<-sum(resid(modelobject,type="pearson")^2)
  rdf<-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(PRTmodgamma_feedingday) # get estimate of overdispersion # seems to calculate this more extremely than the other methods


library("blmeco") 
dispersion_glmer(ERate_mod_FULL) # Computes the estimated scale in a binomial (and Poisson) mixed model
# According to recommendations by D. Bates, if the scale parameter is between 
# 0.75 and 1.4, there may not be an overdispersion problem. However this does
# not necessarily indicate good model fit so thorough residual analyses or 
# posterior predictive model checking is still needed

mean(data)/var(data) # should be close to 1 if data follow a poisson distribution

# Calculation: Pearson Chi2/residual df. If >1 = overdispersed. <1 = underdispersed.
deviance(finalmodel)/df.residual(finalmodel)

library(AER) # alternative method to test for overdispersion # only for poisson GLMs
dispersiontest(model)

# ALTERNATIVE FUNCTION to test for overdispersion: (calculates Pearson resids, 
#  resid df. and p-value). Works for glmmADMB models unlike 'gof' in aods3)
# source: http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(DFseas_mod) 

## FOR OVERDISPERSED BINOMIAL PROPORTION DATA
# instead of fitting model with OLR< can fit a beta-binomial model in JAGS (or R betareg?)
# See Harrison 2015, PeerJ paper
# R package 'runjags' - Denwood 2014


#===========================================#
# Distributions: plotting data to check ####
#===========================================#

### What distribution best fits my data? - check this before fitting models to ensure they are appropriate

require(car)
require(MASS)

# Drawing distributions in qqp:
# y axis: observations
# x axis: quantiles modeled by the distribution
# The solid red line represents a perfect distribution fit
# The dashed red lines are the confidence intervals of the perfect distribution fit
# Pick the distribution for which the largest number of observations falls between the dashed lines

qqp(duration_data_sub$duration.s, "norm", main="normal") # would require family = gaussian
qqp(duration_data_sub$duration.s, "lnorm", main="lognormal") # would require family = gaussian(link = "log"),

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter:
nbinom <- fitdistr(duration_data_sub$duration.s, "Negative Binomial") # wont work for continuous responses, only discrete. NB not suitable distribution for continuous data (Ben Bolker)
qqp(duration_data_sub$duration.s, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]], main="negative binomial")

poisson <- fitdistr(duration_data_sub$duration.s, "Poisson")
qqp(duration_data_sub$duration.s, "pois", poisson$estimate, main="poisson")

gamma <- fitdistr(duration_data_sub$duration.s, "gamma", start=NULL) # logs of s are infinite so may need to add 1 to run gamma or lognormal models (lognorm=LMM on log-transformed response)
qqp(duration_data_sub$duration.s, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")

weibull <- fitdistr(duration_data_sub$duration.s, "weibull")
qqp(duration_data_sub$duration.s, "weibull", shape = weibull$estimate[[1]], scale = weibull$estimate[[2]], main="weibull")

expo <- fitdistr(duration_data_sub$duration.s, "exponential")
qqp(duration_data_sub$duration.s, "exp", expo$estimate, main="exponential")

# can also use:
library(fitdistrplus) 
plot(fitdist(residuals(ind_durmod_glmernb),"norm"))  # seems to only work with lme4 models

#= POISSON distributions
# A core property of the Poisson distribution is that the variance = the mean.
var(all3defs$Group.size) # calculate variance of response variable
mean(all3defs$Group.size) # calc the mean
hist(all3defs$Group.size) # plot histogram
plot(table(all3defs$Group.size), xlab = "observed count values", ylab = "frequency") # similar to histogram but doesn't bin the data

# Also check the group means - take into account the fixed effect/s of interest
# 1. Create variable for interaction (combo) between season and def. ordered by mean
all3defs <- within(all3defs,
                   {
                     seasdef.int <- interaction(season, Definition)
                     seasdef.int <- reorder(seasdef.int, Group.size, mean)
                   })

# 2. Use ddply to find mean and var for each combo of season and definition (group means and vars):
a<-ddply(all3defs, "seasdef.int",
         summarise,
         mean=mean(Group.size),var=var(Group.size))
a
# if variances are greater than the mean this indicates overdispersion so Poisson not suitable
# and must use quasipoisson or negative binomial - POISSON & NB ARE FOR COUNT DATA ONLY

# PLOT THE MEANS AND VARIANCES AGAINST DIFFERENT DISTRIBUTIONS TO VISUALISE SHAPE OF DATA:

# We can get approximate estimates of the quasi-Poisson (linear) pattern using lm, as follows: 
# (The -1 in the formulas below specifies a model with the intercept set to .
lm1 <- lm(a$var~a$mean-1) ## estimate the quasipoisson pattern
phi.fit <- coef(lm1)

lm2 <- lm(a$var~I(a$mean^2)+offset(a$mean)-1) # estimate the neg binom pattern using an offset
k.fit <- 1/coef(lm2)

plot(a$var~a$mean, xlab="group means", ylab="group variances" ) # plot
abline(c(0,1), lty=2) # fit a poisson line

curve(phi.fit*x, col=2,add=TRUE) # add line for quasipoisson or negbinom type 1 (NB1) [red]
text(110,3900, bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)), col=2) # annotate: bquote() is used to substitute numeric values in equations with symbols

curve(x*(1+x/k.fit),col=4,add=TRUE) # Add line for neg binomial or lognormal-Poisson [blue]
text(104,7200,paste("NB: k=",round(k.fit,1),sep=""),col=4) # annotate

Lfit <- loess(a$var~a$mean) # Add line for loess fit [cyan]
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5)
text(118,2000,"loess",col=5)

library(ggplot2)
ggplot(a,aes(x=mean,y=var))+geom_point()+
  geom_smooth(colour="cyan",fill="cyan")+                         # loess smoothing line [cyan]
  geom_smooth(method="lm",formula=y~x-1,colour="red",fill="red")+ # quasipoisson or NB1 line [red]
  geom_smooth(method="lm",formula=y~I(x^2)+offset(x)-1,           # neg binomial or lognormal-Poisson [blue]
              colour="blue",fill="blue")

# The group variances increase with the mean more rapidly than expected under the Poisson distribution
# suggests need to account for overdispersion in model


#===========================================#
# Effect sizes ####
#===========================================#
# not same as model coefficients!
# for tips how to calc see:
http://stats.stackexchange.com/questions/95054/how-to-get-the-overall-effect-for-linear-mixed-model-in-lme4-in-r


# Get effect sizes using effects() in base R (I think) 
effects(fit_poiss) # doesnt work for glmmaDMB models

# Calculate effect sizes (? I think?) - same output as lsmeans but SE is different, think because it's estimated from a normal distribution so not reliable for GLMMs
library(effects) 
eff <-  Effect(c("Sex","SocialStatus", "fSeason"), erateMOD_full)
eff <-  Effect(c("DaysSeen"), erateMOD_full) # works for continuous predictors - estimates a range e.g. here it
# does it for 10, 20, 30 and 40 days - lsmeans just does it for one random number of days!
as.data.frame(eff) # view effects as table (way clearer)


#====== Calculation: TEMPLATE: rcontrast(t, df) ### t = the t-value from glht post hoc test, not the t-ratio from lsmeans test)

fixef(model) # show coefficients for fixed effects for each level (e.g. season)
ranef(model) # show coefficients for random effects for each level
# Random effects estimates are variances. Interpret a random effect parameter estimate as the magnitude of the 
# variability of "personal" coefficients from the mean fixed effects coefficient.

# Need to run this function first (From Andy Field's DSUR book:
rcontrast <- function(t, df){
  r <- sqrt(t^2/(t^2 + df))
  print(paste("r = ", r))
}
summary(lme.orimod3) # take t-values (from summary(modelname) - not t-ratio from Tukey's!!) and d.f 
rcontrast(t, df) # and enter them here to calculate effect size for comparisons


#===============================#
# Errors in R scripts/packages ####
#===============================#
# lme4 : non-integer : means y-variable is saved as numeric rather than an integer: change the data type using as.integer()



#===============================#
# For loops ###
#===============================#

for (dt in anomalies){
  print(paste(dt))
}


#===============================#
# Functions ###
#===============================#

myfun = function(vec){
  mean(vec)
}

myvec = c(2,4,6,8)

myfun(myvec)

sum(myvec)/length(myvec)




#========================#
# General useful code ####
#========================#

# Run line
Ctrl + R

# Run whole script
Ctrl + Shift + S

# Resends the previously sent chunk of code from the editor to the console
# Useful if you changed one parameter and want to run the chunk again
Ctrl + Shift + P

# Learn more about a function
# highlight function name, e.g. geom_point, and press F1

# view shortcuts in RStudio
Alt+Shift+K (Esc to return)

Ctrl+/- # increase text size in Rstudio
Ctrl_shift+1/2/3/4/5/6/7/8... # maximise panels in Rstudio (toggle to go between max/min) 

# Organise code by putting 4 hashtags # after the line of code
# This creates a subtitle which can be navigated to easily using the bottom ribbon
# - and collapsed to hide code if required

# To comment out a multi-line comment all at once, highlight the text and press:
Ctrl + Shift + C

Ctrl + up/down arrows # re-run previous commands in the console

#=== Load in data by opening the working directory folder and manually selecting the file
dframe1 <- read.csv(file.choose()) # Navigate to file 

# recreate data for stackoverflow questions
dput(mydata) # generates R code to recreate the data. 
# Run this in Rstudio, copy the output and paste it into your Stackoverflow question, where you can save it as a new mydata variable using mydata <- <pasted output from dput(mydata)>

#== Printing outputs
(x <- "hello world") # make assignments within parentheses to print output automatically


#== view things you can do with the type of model
methods(class="merMod") 

#== make a vector 
perm <- 1000 # 'perm' is just the number 1000
perm <- numeric(1000) # 'perm' is an empty vector containing 1000 spaces



#===========================#
# ggplot2 visualisations ####
#===========================#

# The Grammar of Graphics
# You can uniquely describe any plot as a combination of 7 parameters:
# a dataset, a geom (geometric function), a set of mappings, a stat, 
# a position adjustment, a coordinate system and a faceting scheme (subplots).

# Template code containing these 7 parameters:
ggplot(data = <DATA>) + 
  <GEOM_FUNCTION>(
    mapping = aes(<MAPPINGS>),
    stat = <STAT>, 
    position = <POSITION>
  ) +
  <COORDINATE_FUNCTION> +
  <FACET_FUNCTION>
  
# ggplot works by plotting layers 

# The first argument of ggplot() is the dataset to use in the graph.
# Then each geom_ (geometric) function makes a layer, based on this dataset. 
# Each geom_ function requires 'mappings': x,y coordinates and/or aesthetics.

# Aesthetics (visual properties) include:
colour = variable_name 
size = variable_name   # Note you should only map continuous/ordered variables to size
alpha = variable_name  # The transparency aesthetic
shape = variable_name  # Note ggplot2 will only use six shapes at a time
stroke = 2             # Shape border width, not specifiable inside aes()

# Specify aesthetics outside of aes() to apply them to all data, or 
# within aes() to associate them with a particular variable.

# Colour specified within aes() so diff manufacturers are diff colours.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = manufacturer)) 

# Colour specified outside of aes() so all points are blue.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue") 

# For shapes that have a border (like shape 21), you can colour the inside and
# outside separately. 
# Use the stroke aesthetic to modify the width of the border.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), shape = 21, fill = "white", size = 5, stroke = 2)
# a.k.a...
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(shape = 21, fill = "white", size = 5, stroke = 2)

# Can also colour based on a condition rather than the level/value.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 4))



#= FACETS

# facet_wrap wraps a 1d sequence of panels into 2d
# facet_grid forms a matrix of panels (best for discrete variables where all combos exist in the data)
# When using facet_grid() put the variable with more unique levels in the columns

facet_wrap(rows ~ columns) # replace either with a dot to inindicate there should be no faceting on this dimension

# To plot by a single (DISCRETE) variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl) # facet_wrap(~ cyl, nrow=1) to make it all on 1 row. Facet_grid doesnt have ncol or nrow arguments.

## Facet on the combination of two variables

#     drv = drive (f/r/4), cyl = N cylinders 

# facet_GRID plots grid squares for all combinations, with empty graphs for combos with no data
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# facet_WRAP plots grid squares for all combinations, with empty graphs for combos with no data
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(drv ~ cyl)

# facet by N cylinders only, in columns (all one row)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)

# facet by N cylinders only, in rows (all one column)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(cyl ~ .)


# plot facets with different y-axis scales
facet_grid(Variable ~ ., scales = "free") # must put dot after ~



#== The group aesthetic

# to display all data as a single object (line)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# to display data as multiple objects, set the group aesthetic to a categorical 
# variable: plots each level of variable as a separate line/colour/shape depending on the geom_ type.
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

# ggplot automatically sets the group aesthetic when you set linetype/colour/shape etc. as a categorical variable.
# This is better as the group aesthetic by itself does not add a legend or distinguishing features to the geoms so they're hard to identify.
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv), show.legend = FALSE)



#== Different geoms

# ggplot2 provides over 30 geoms
# extension packages provide even more: www.ggplot2-exts.org

# Some graphs, like scatterplots, plot the raw values of your dataset. 
# Others calculate new values to plot using statistical transformations ("stats"):
#   - Histograms, bar charts & area charts/frequency polygons calculate & plot bins of data (labelled as 'count').
#   - Smoothers fit a model to your data and then plot predictions from the model.
#   - Boxplots compute a robust summary of the distribution and then display a specially formatted box.

# see default "stats" for different geoms http://ggplot2.tidyverse.org/reference/#section-layer-geoms

# histogram geom - plots one x variable only: no y aesthetic
ggplot(data = mpg, mapping = aes(x = displ)) +
  geom_histogram()

# area geom - plots one x variable only: no y aesthetic
ggplot(data = mpg, mapping= aes(x = displ)) +
  geom_area(stat = "bin")

# frequency polygon
ggplot(data = mpg, mapping = aes(x = displ)) + 
  geom_freqpoly(binwidth = 10)


# BAR PLOTS 

# bar geom - takes a "stat" parameter.
# default stat="count": bar size represents count/freq of a value in the df rather than its actual value.
# using ggplot() + geom_bar(stat="count") is same as using ggplot() + stat_count()
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, group = 1))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut, group = 1))

# The stat function calculates both counts and proportions, but plots counts by default.
# To plot proportions, specify y as ..prop.. # see computed variables section in ?geom_bar.
# ..prop.. is a keyword
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))  
# group=1 tells ggplot to plot the proportion of ALL datapoints that have each 
# value of x. Important, as the default is to plot the proportion of datapoints 
# for each value of x that have each value of x, so every proportion would be 1!) 
# Can also specify <y = ..count..> but this is pointless as count is default.

# Can override geom_bar's default to specify a y aesthetic & so bar height 
# represents the raw y values.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_bar(stat = "identity")  

# geom_col is equivalent to geom_bar(stat="identity").
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_col()

# Can also plot a statistical summary of y values for each unique/binned x.
# ggplot2 provides >20 stats to use, e.g. min, max, median... See RStudio's ggplot2 cheatsheet.
# https://www.rdocumentation.org/packages/ggplot2/versions/1.0.1/topics/stat_summary
ggplot(data = diamonds) + 
  stat_summary(mapping = aes(x = cut, y = depth),
               fun.ymin = min,
               fun.ymax = max,
               fun.y = median)

# default geom of stat_summary is geom_pointrange() 
ggplot(data = diamonds)+
  geom_pointrange(mapping = aes(x = cut, y = depth),
                  fun.ymin = min, 
                  fun.ymax = max, 
                  fun.y = median, 
                  stat = "summary")

# stacked bar chart
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity)) 

# proportional stacked bar chart - compare proportions between groups
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# unstacked bar chart - compare individual values
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position=position_dodge())

# line chart geom
ggplot(data = mpg, mapping= aes(x = displ, y = hwy)) +
  geom_line()

# boxplot geom - must specify the group aesthetic
ggplot(data = mpg, mapping= aes(x = displ, y = hwy, group = displ)) +
  geom_boxplot()

# smooth geom, a smooth line fitted to the data
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth()

# violin plots 
# see http://rpackages.ianhowson.com/cran/ggplot2/man/geom_violin.html
ggp + geom_violin(aes(x=Sex, y=strength), stat = "ydensity", scale = "count", draw_quantiles = 0.5)



#== Positional adjustments 

# http://r4ds.had.co.nz/data-visualisation.html#position-adjustments

?position_dodge, ?position_fill, ?position_identity, ?position_jitter, ?position_stack

# jittering points on scatter plots to reduce overlap
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_jitter()  # equivalent to geom_point(position = "jitter")
# geom_jitter(width=0.4) # width specifies amount of vertical and horizontal jitter. Default = 40% of resolution of data.

# alternatively use geom_count() to plot larger dots where many points overlap
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_count()



#== Plotting coordinate systems

# http://r4ds.had.co.nz/data-visualisation.html#coordinate-systems

# swap the x and y axes (e.g. for horizontal boxplots / long labels) - coord_flip()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()


## Set correct aspect ratio for maps - coord_quickmap()
nz <- map_data("nz")

# plot with incorrect aspect ratio
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

# plot with correct aspect ratio
ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# Note: coord_map() and coord_quickmap() differ:
""" coord_map uses map projection to project a spherical portion of the earth 
    onto a flat 2D plane. This requires considerable computation as map 
    projections dont preserve straight lines.
    
    coord_quickmap is a quick approximation that does preserve straight lines. 
    It works best for smaller areas closer to the equator. """

# polar coordinates - coord_polar()
bar <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut), 
           show.legend = FALSE, 
           width = 1) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()




#== Plotting multiple geoms (layers) 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, colour = drv, linetype = drv))
# = Lots of repetition in code!

# Best to pass the xy mappings to ggplot() as global mappings 
# Avoids repetition and reduces errors when updating code.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# Can still specify (different) mappings in a geom_ function:
# ggplot2 will treat them as local mappings for the layer (to extend or 
# overwrite the global mappings for that layer only). 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = drv)) + 
  geom_smooth(mapping = aes(linetype=drv), se = FALSE)

# Can also specify different datasets in a geom_ function:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)



#== Formatting plots

## Reference lines
# Useful for annotation
# Drawn using geom_line so support the same aesthetics.
# Do not inherit aesthetics from the plot default, because they do not 
# understand global x and y aesthetics (unlike most other geoms). 
# Also do not affect the x and y scales.

geom_hline() # horizontal
geom_vline() # vertical
geom_abline() # diagonal

# e.g.
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

# add horizontal line to plot at specified intercept
ggp + geom_hline(yintercept=0.5, linetype="dotted", color = "gray10", size=0.3)


## Text annotations / labels
ggp +  annotate("text", x = 0.8, y = 0.982, label="y = 4.191 + 1.467x\n psuedo Rsq = 0.0017", 
                size=4.5)



# Change style of facet labels
ggp + theme(strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"))

# add border around facet labels
theme(strip.background = element_rect(color = "black", size = 1.2))

# plot on log scale or different scales
scale_x_log10() 
scale_y_log10() 
scale_x_reverse()

# getting lines to join points
geom_line(aes(group=Origin), size=1) # in geom_line must specify group so ggplot knows which points to connect with lines

# make sure lines for diff groups are diff colours: the group used for colour in aes must be a factor
ggp <- ggplot(predframe, aes(x=Season, y=pred, colour=factor(sexstat))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=sexstat), size=1) +  geom_point(size=3) + facet_grid(~DaysSeen))

# title
ggtitle("") 
# change text size of title
theme(plot.title = element_text(size=18))

# alpha = transparency: useful for fading points into the background.
geom_boxplot(size=0.7, colour="grey50", alpha=0.3) 

# remove background grid
theme_bw() + # must put theme_bw FIRST or it won't work
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

panel.grid.minor = element_line(color="grey90"), # to add light grey gridlines
panel.grid.major = element_blank(),

# Add border
theme(panel.border = element_rect(linetype = "solid", colour = "black"))

# axis breaks / limits
scale_x_continuous(breaks = 0:6) # specify limits and breaks will be every whole number in between
xlim(0,6) # specify limits but not breaks


# Change colour of geom_ribbon
ggplot(lsmDF, aes(x=DaysFedPerWeek,y=prob, group=factor(Sex), colour=factor(Sex))) + 
  geom_point(size=4) +
  scale_colour_manual(values = c("royalblue3","firebrick1"), name="Sex") +
  geom_ribbon(aes(x=DaysFedPerWeek, ymin=asymp.LCL, ymax=asymp.UCL, group=Sex, fill=Sex), 
              # (put the group and fill arguments within same brackets as ymin/ymax)
              lty=0, alpha=0.2)  +
  scale_fill_manual(values = c("royalblue3","firebrick1"), # specify fill colours or grey is default
                    name="", guide=FALSE) # means don't print legend

# plot regression (lm) line with SE shown as a ribbon
ggplot(subsetALL,aes(x=TaggedFoxes,y=PropUnidPhotos)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, size=1, se=TRUE)  # se=true adds a ribbon for the SE

# overlay raw data
geom_point(data=patchdata, aes(fSeason, DaysFedPerWeek), colour="grey70", 
           position=position_jitter()) 

# Axis text colour # see http://docs.ggplot2.org/0.9.2.1/theme.html
theme(axis.text = element_text(colour = "black"))

# geom_bar width & bar border & dodge to avoid overlapping (also need to dodge any error bars)
geom_bar(stat="identity", width=0.8, position=position_dodge(0.9), color="black") 



#== LEGENDS

# title
scale_colour_discrete(name="Days seen")  # rename legend in line or dot plots - e.g. when specify colour=factor
scale_fill_discrete(name="Days seen")  # rename legend in bar or box plots - e.g. when specify fill=factor

# Remove legend - use guide=FALSE in scale_fill_manual or scale_fill_discrete etc
scale_fill_manual(name="Sex", values=c('royalblue3','firebrick1'), guide=FALSE)
# or:
show.legend = FALSE # have to paste it in every geom_ function / layer

# reposition legend: add this to theme() section of ggplot code:
theme(legend.position = "top", legend.direction=c("horizontal", "vertical")) # Or:
theme(legend.position = c(x,y)) # e.g. where (1,1), (0,0), (0,1), (1,0) mean the four corners of plot: must be between 0-1 (coordinates)

# remove legend'd white background - to plot closer to panel border without overlapping
theme(legend.background = element_rect(fill = NA)) # add this to the theme() section of ggplot code - see http://docs.ggplot2.org/dev/vignettes/themes.html

# remove box around legend symbols
theme(legend.key = element_blank())

# remove legend title
theme(legend.title = element_blank())



#== Error bars
# use ribbon to shade CIs or use ribbon border and no fill to plot lines
geom_ribbon(data=lsmpreds, mapping=aes(x=MeanMJperDay, ymin=lower.CL, ymax=upper.CL), 
            fill="grey40", lty=0, alpha=0.3) 
# lty=0 removes lines from border so can use fill
# alpha of increasing values increases fill transparency (works for boxplot too)
# Can use diff data frame as other elements of the plot e.g. geom_line or geom_jitter

# Actual error BARS
geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), size=1, width=0.2,
              position=position_dodge(0.2)) # make sure position_dodge geom_point too


#== Layout

# plot 2+ ggplots side by side / on panel (as can't use par() command for ggplots)
library(gridExtra)
grid.arrange(ggp1, ggp2, nrow=2, ncol=NULL) # where ggp is ggplot2 code for each plot


#======================================#
# glmmADMB ####
#======================================#

# PROBLEMS WITH LOADING
# If have trouble getting glmmADMB to work, delete all versions from R library folders (on C: drive etc), 
# then run this code.
# Might help to restart R too.

install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")
library(glmmADMB)

#=== tips for fitting models / solving errors
# thread: http://lists.admb-project.org/pipermail/users/2009-April/000145.html


### USING OFFSET TERMS 

# OFFSET TERMS ARE FOR CALCULATING RATES, e.g. NCalls were measured for a whole nest but would depend on N chicks so offset(logBroodSize) means 
# model estimates are calculated per chick. Similarly, if copepods are counted in water samples, N depends on volumne of water sample analysed, 
# so offset(logVolume) means estimates are calculated per millilitre. 
# If I include an offset for DaysSeen, this is considered per row in the dataframe, so if a fox had 3 true associations on a day but visited 
# the territory on 40 days, this is like saying 'if foxes are seen on 40 days they will make 3 visits per day'. Better to include Days seen as
# a continuous variable and interpret as a correlation, e.g. foxes have a higher contact rate if seen in the patch more often
# DaysSeen better than NSightings as foxes can have multiple true assocs per visit/sighting (so could end up with negative values).

#= Offset term in glmmADMB
mydata$logDaysSeen <-log(mydata$DaysSeen) # need log variable for glmmADMB offset terms
ERmod_zinb1 <- glmmadmb(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded +
                          offset(logDaysSeen) + (1|ShortCode) + (1|Place),  data=EncounterRate_noNAs, 
                        family="nbinom1", zeroInflation=T, debug=T, verbose=T) 
# verbose=T shows model progress (so can see its running!), debug=T shows file locations where glmmADMB stores/finds things during the run.

#== Offset term in lme4
model <- glmer(NTrueAssocs ~ Sex*SocialStatus +
                 (1|ShortCode) + (1|Place) + 
                 data=EncounterRate_noNAs,  
               family=poisson(link="log"), 
               offset=log(DaysSeen), # can log within the model 
               verbose=T)


# Extract information from glmmADMB model for reporting (for lme4 models can save whole model in data frame in 1 step by coef(summary(model))
#===========================#
# save model coefficients
ERmod_zinb2_coeffs <- data.frame(beta=coef(ERmod_zinb2))

# and standard errors
ERmod_zinb2_se <- data.frame(SE=coef(summary(ERmod_zinb2))[, "Std. Error"])

# and p-values
ERmod_zinb2_p <- data.frame(p=coef(summary(ERmod_zinb2))[, "Pr(>|z|)"])

# Compute 95% CIs (takes a while to compute) # .sig01 etc are random effects
ERmod_zinb2_cis <- data.frame(confint(ERmod_zinb2, 3)) # round to 3dp # wouldnt compute (well, took too long) for model with individual-level random effect - too many ranefs to compute I imagine.

# save all above in a table
ERmod_zinb2_sumtab<-data.frame(ERmod_zinb2_coeffs,
                               ERmod_zinb2_se,
                               ERmod_zinb2_cis,
                               ERmod_zinb2_p)
#======================#

# ERRORS
# Warning message: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :Model failed to converge with max|grad| = 0.240412 (tol = 0.05, component 1)
# suggests some fixed effects are correlated  summary(mod, correlation=T)  shows it's the interaction. 

# Ideas to help models converge
# Add these optional control parameters: admb.opts=admbControl(shess=F,noinit=F, maxfn=5000) # shess and noinit are the most useful



#==========================#
# Good coding practices ####
#==========================#

# Never include install.packages() or setwd() in a script that you share. 
# - It’s very antisocial to change settings on someone else’s computer!

# Always start scripts with the packages required


#===============#
# Ifelse ####
#===============#
attribs.NC$OneTerritory <- ifelse(attribs.NC$Nterritories >1, "0", "1") # mark as '1' if fox was only seen in 1 territory during season, otherwise '0'



#===============#
# Interactions ####
#===============#
# Model parameterisation for interactions:

Sex*fSeason # two-way interaction 
(Sex+SocialStatus)*fSeason # three way interaction (see if have enough samples: for this int. sex and status have 2 levels & season=4 levels: model matri=2*2*4=16 cells: need min 5 foxes per cell = 16*5=80 individuals)
Sex*SocialStatus*fSeason # three-way interaction - exactly the same as above and gives exact same model fit and estimates.

# If a 3-way interaction is significant be careful when interpreting the two-way interaction
# factors may be nonsig alone or in a two-way but sig in a three-way

# In factorial experiments, the dependency between factor levels and the 
# response variable is usually represented in a contingency tabe with rows 
# and columns related to the different levels of both treatments, and each 
# cell contains the adjusted mean of the response for the corresponding 
# interaction of factors. When there is an interaction effect, the cell means
# are the most straightforward way of representing this effect. Cell means
# (Values) and their SE can be obtained from the model coefficients using function:
phia::interactionMeans(model) # by default calculates cell means for the interactions of highest order between factors.

# Could create a new column in the dataframe and run model on that once know interaction is significant, to
# then get coefficients for the interaction as well as each variable individually.
# NB - model coefficients will be different when use oriseas (combined column) than when use origin*season (interaction between separate columns)
# THIS IS DUE TO DIFF REF CATEGORIES: Model is still the same whether code as: x1*x2 or x1 + x2 + x1:x2

# But as an E.g.here's how to make new variable (column) for origin*season interaction:
origins$seasXori<- origins$season:origins$Origin
factor(origins$seasXori) # make it a factor
# Then run model with interaction and individual terms
model <- glmer(Count~season+Origin+seasXori + (1|oriT), data=origins, family=poisson(link = "log"))

# May have warning for "rank deficient" - think it's a problem of multicollinearity # when diff columns contain the same data so are correlated
# To see which columns were dropped:
fsf <- cbind(fixef(seasori.int.ti, add.dropped=T)) # NAs show columns that were dropped.

coplot(Count~Origin|season, data=origins) # draw a conditioning plot to view interactions, read from left-right, bottom-top
lsmip(seasori.int.t, Origin~season, type="link") # interaction plot without backtransformation (so log values are plotted)
lsmip(glmm3, Origin~season, type="response") # interaction plot with backtransformation
interaction.plot(x1, x2, y) # e.g. (model$sex, model$season, model$count)

# e.g. to view interaction effect of season and sex on count: (lsmeans stores y-variable as 'response')
interaction.plot(lsm$fSeason, lsm$Sex, 
                 lsm$response,
                 xaxt='n', xlab="", ylab="Predicted daily patch contact rate",
                 legend=F, bty="n",lty=c(1,2,1,2),lwd=2, col=c("red", "red", "black","black")) # plot without axes intially
xtick<-seq(1, 4, by=1)                                       # specify where to put tick marks
seasonlabels<-c("Spring", "Summer", "Autumn", "Winter")     # list of labels for the x-axis
axis(side=1, at=xtick, labels = seasonlabels)               # draw the x axis and add labels
legend("topright", c("M sub", "F sub", "M dom", "F dom"),bty="n",lty=c(2,2,1,1),lwd=2, # add customised legend (here I've specified order of levels but doesnt seem to work!)
       col=c("black", "red", "black","red"), title="",inset = .02)


# Interpreting two-way interactions
#==================================#
# Spring-Summer * Neighbour-PGM means 'is the difference between spring and summer simlar for 
# neighbours and PGMs?' So can say there were significantly more neighbours in summer than spring but not PGMs.


library(phia) # 'POST HOC INTERACTION ANALYSIS' to get means for interactions
# means and contrasts reported by interactionMeans and testInteractions are always in the link function domain (e.g. log)
Anova(model)  # Anova with capital A is in 'phia' and 'car' 
# Analysis of deviance - see which terms are signif 

interactionMeans(model) # table of average scores = simple main effects
# phia automatically back-transforms the mean ('adjusted mean') but not the SE (which 
# shouldn't be back-transformed anyway, just the c.is) #'SE of link' = the SE of the link function used in model

# Plot interactions from model:THIS COULD BE GOOD - only if there's a significant interaction though
phiameans <- interactionMeans(model)
plot(phiameans, atx="season", traces="Origin") # can't seem to plot with type="response"
plot(phiameans, atx="fSeason", traces=c("Sex", "SocialStatus")) # to plot 3-way interaction on panel - but lsnip plot looks better and can backtransform to response scale

testInteractions(model) # shows you all pairwise interactions (only for models with an interaction between factors 
# with two levels, otherwise there are loads of redundant tests, e.g. spring-N vs Summer-PGM makes no sense so is redundant...)

# pairwise interactions:
phia::testInteractions(model, fixed="season", across="Origin") # sig diff between definitions in all seasons
phia::testInteractions(model, pairwise="season", across="Origin") # specific pairwise comparisons

# plot lsmeans lsmip on separate panels (interaction plot, but no CIs)
lsmeans::lsmip(model, ~fSeason|Sex*SocialStatus, type="response",  xlab="Season", ylab="Rate") # 3-way interaction on same axes - GOOD!
# NOTE: if lsmeans doesnt work with just lsmeans(model...) need to specify lsmeans::lsmeans(model...)

# lsmeans lattice grid with CIs and arrows - if overlap=nonsig diff
plot(lsmeans::lsmeans(gammamod_noMJStatusInt, ~fSeason|Sex*SocialStatus), comparisons=TRUE) # comparisons=T means add arrows to show whether levels were sig.diff or not


# To obtain the slope estimate and its statistical significance for each level
# of a factor, you can perform the following tests:

# view just the estimate for Origin # 1 neighbour
testInteractions(model, custom=list(Origin=c(1,0,0)), slope="season", adjustment="bonferroni")

# view just the estimate for Origin # 2 PGM
testInteractions(model, custom=list(Origin=c(0,1,0)), slope="season", adjustment="bonferroni")

# view just the estimate for Origin # 3 stranger
testInteractions(model, custom=list(Origin=c(0,0,1)), slope="season", adjustment="bonferroni") 



#### Three-way interactions ####

# If significant, can make new variable with combination of two of the variables in the interaction
# e.g. if sex*status*season interaction was significant make new column in dataset for sex+status -> sexstatus
# re-run the same model with the new combo field to make a two-way interaction instead
sexstat*season
bbmle::ICtab(threewayint_model,twowayint_model, type="AIC") # exactly the same
summary(threewayint_model)
summary(twowayint_model) # exactly the same model but just summarised differently
# also confirmed using phia::interactionMeans(model) - cell means are the same for both models
phia::interactionMeans(threewayint_model) 
phia::interactionMeans(twowayint_model)


##### Interaction between continuous and categorical predictors ####

# OPTION 1. Just compare seasons for the mean of the continuous response
lsmeans::lsmeans(finalmod, ~fSeason*probfed, type="response")

# OPTION 2: specify levels of probfed (continuous variable) to compare seasons (categorical) within
lsm <- lsmeans::lsmeans(finalmod, ~ fSeason|probfed, 
                        at = list(probfed = c(0.2, 0.4, 0.6, 0.8, 1))) # supply a few typical values of probfed
# can also make lsmeans preds with multiple specifications:
lsm <- lsmeans::lsmeans(nvis_mod, "MeanMJperFeedingDay", 
                        by = c("DaysFedPerWeek", "varMJperDay"),
                        at = list(MeanMJperFeedingDay = seq(0,3,0.5), 
                                  DaysFedPerWeek=seq(1,7,1)), # mean is used for variables not in list() 
                        type="response")
print(lsm)  
pairs(lsm)  # shows how the seasons compare at each specified value of probfed

# OPTION 3. Use lstrends to compare the slopes (effects) of MeanMJperDay between seasons
# (as not interested in differences between seasons)
# compares the slopes of the meanMJperDay trends between seasons 
lst <- lsmeans::lstrends(gammamod_noMJStatusInt, pairwise~SocialStatus, var="DaysFedPerWeek") 
# alternative syntax (but the one above is best as get same output but  from one line of code)
lst <- lsmeans::lsmeans(gammamod_noMJStatusInt, ~SocialStatus, trend="DaysFedPerWeek", type="response")
lsmeans::cld(lst) # diff groups (1 and 2) so slopes differ between groups significantly
pairs(lst) # get p-value
# NOTE - need lsmeans:: before the code for it to work.


#======================#
# Linear regression ####
#======================#

# check the response has a normal distrib
hist(mydata$UnidPhotos)
lattice::densityplot(mydata$UnidPhotos)

# look for correlation between 2 continuous variables
plot(mydata$UnidPhotos~mydata$FoxesTagged)
lines(lowess(mydata$UnidPhotos~mydata$FoxesTagged))

# Test correlation with a linear model
tagmodel1 <- lm(UnidPhotos ~ FoxesTagged, data = mydata)

# model checking
plot(tagmodel1) #plots sequence of different plots: should have homogeneous variance 
# and straight line on QQplot + no overly influential data points
plot(gy, which = 1:4, type = "pearson") # plots pearson residual plots with clearer labels
plot(gy, which = 5, type = "deviance", sub.caption = "") # normality plot with boundaries (a bit slow)

summary(tagmodel1) 
anova(tagmodel1) 
# The summary(model) output shows the t-test for whether the slope is zero or not,
# The anova(model) output shows the F-test of whether the variance 'explained' by the regression line is significant
# t-test and F-test are equivalent so can report either - 
# F is the t-value for the predictor squared (i.e. here it is 0.441^2 = 0.194481)
### for t: PropPhotosUnidentified = 0.0095 + 0.0017.PropFoxesTagged, t82=0.441, p=0.660" # see Innes' handout ANOVA, ANCOVA & GLM, pg 4
### for F: "F1,82=0.195, p=0.660"

# Nice website that explains how to interpret model summary table
# http://bit.ly/1ThUJ3r

# add regression line to plot
plot(mydataUnidPhotos~mydata$FoxesTagged)
abline(tagmodel1, col="red") # line from model with predictor


### check for nonlinearity ###

# plot first:
plot(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged);lines(lowess(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged),col="red")
# then compare a linear model with a quadratic model using an F-test:
model.lin<-lm(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged)
model.quad<-lm(mydataAllVis$PropUnidPhotos~mydataAllVis$PropFoxesTagged+I(mydataAllVis$PropFoxesTagged^2))
anova(model.lin,model.quad) # no sig diff between the two, suggesting relationship is not nonlinear




#=======#
# LOOPS ####
#=======#
# useful website: https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r

# Example: loop to calc centrality for each fox in each random network (saved in a stack by package asnipe) and save the numbers in a matrix
net<-T1sprB4_rand # specify network
att <- T1sprB4_attr # specify attributes
N <-nrow(att) # Number of individuals
e_perm <- matrix(NA, nrow=N, ncol=1000) # make an empty N*perm matrix filled with NAs (assume T1sprB4_rand is stack of 1000 random networks)
rownames(e_perm) <- att$id # Name the rows to help interpretation
i=1
for (i in c(1:1000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustZhang(net_rand)$clustZhang # calculate the statistic (here=clustering coefficient using Zhang method)
  e_perm[,i] <- a # save the statisti
}


#===================================#
# MANTEL TESTS FOR NETWORK ANALYSIS ####
#===================================#
# Example from Asnipe package: Mantel test to see if there is a correlation betweek presence of edge and sex similarity

#== 1. Create a sex-similarity matrix

# Create an empty N x N matrix to store individual similarities in sex
N <-nrow(T1sprB4_attr) # Number of individuals
sex_sim <- matrix(0, nrow=N, ncol=N)
rownames(sex_sim) <- T1sprB4_attr$ShortCode
colnames(sex_sim) <- T1sprB4_attr$ShortCode

# Loop through each row and each column in the data (start and end loops with curly brackets) MAKE SURE CHANGE OBJECT NAMES IF USE THIS AS A TEMPLATE
for (row in c(1:N)) {        # loop 1 (outer loop): variable 'row' sequentially took values of 1 to N
  for (col in c(1:N)) {    # loop 2 (inner loop): for every value of 'row' the variable 'col' also sequentially took values of 1 to Nl
    # Test if the species are the same
    if (T1sprB4_attr$Sex[row] == T1sprB4_attr$Sex[col]) {
      # specify matrix to store results in:
      sex_sim[row,col] <- 1
    } else {
      # specify matrix to store results in:
      sex_sim[row,col] <- 0
    }
  }
}
# So for every value of row and col we test whether the sex associated with row number was 
# the same as the sex associated with col number
sex_sim # View the populated sex-similarity matrix

#== 2. Create binary network
network_binary <- T1sprB4_net # copy network to new variable
network_binary[network_binary > 0] <- 1 # make binary network

#== 3. Run the Mantel test via package vegan
library(vegan)
mantel(network_binary,sex_sim)# no clear relationship if 0.025<p<0.975


#===================#
# MATCH (plyr) - to populate new column in dataframe based on other row values ####
#===================#
datacsv$ShortCode<-attribs[match(datacsv$id, attribs$id),2] 
# '2' means 'use info from column 2 in unique_attribs to populate the new column in datacsv'
### the order matters: (put target dataframe first, then the one the info is coming from)

#========================#
# MAKE MATRIX / MATRICES ####
#========================#
# Set up a K x N x N matrix to store values from the random networks
N <-nrow(T1sprB4_attr_CORE) # Number of individuals
bindeg_randmatrix = array(0, dim=c(N,1000))
dim(bindeg_randmatrix)
mat<-matrix(rep(list(),1),nrow = N, ncol =1000) # make array of lists (N rows and 1000 columns)
mymat = matrix(nrow=1000, ncol=11) # make matrix of 1000 rows and N columns


#==================#
# Matrix structure ####
#==================#
df[1,] # row 1
df[,1] # column 1
df[1:20,] # rows 1-20
df[1:5] # columns 1-5

# Understanding stacked matrix dimensions and how to refer to them
# asnipe random networks are K*N*N so for 1000 permutations this = 1000*N*N = [1000,N,N]
matrixstack[,,1] # view all 1000 random association strength values for animal 1
matrixstack[,1,] # view all 1000 random association strength values for animal 1 (same values as above for symmetrical associations) 
matrixstack[1,,] # view the N*N matrix for permuted network #1, showing all animals
matrixstack[1,1,] # view the N*N matrix for permuted network #1, showing values for animal 1 with each other animal 
matrixstack[1,,1] # same as above
matrixstack[1,1,3] # view the N*N matrix for permuted network #1, showing the value for animal 1 with animal 3 only

# rename rows (second dimension)
dimnames(matrixstack)[[2]] <- att$id
# rename columns (3rd dimension)
dimnames(matrixstack)[[3]] <- att$id


#===================#
# MERGE DATA FRAMES ####
#===================#
# merge all columns in 2 data frames
merge_all <- merge(x=datacsv, y=attribs.NC, by = c("id", "SeasonID"))


# merge all columns from first df (x) and only certain ones from the second df (y) 
merge_rank_only <- merge(x=datacsv, y=attribs.NC[ , c("id", "SeasonID", "rank")], by = c("id", "SeasonID"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
a <-merge(datacsv, attribs, by="id", all.x=TRUE)
EncounterRate <- merge(x=EncounterRate_days, y=patchdata[ , c("SeasonID", "Place", "DaysFedPerWeek")], by.x=c("SeasonID", "Place"), all.x=T, all.y=F)
# to avoid warnings when using merge convert factors to characters first
# sometimes merge just won't work for no apparent reason - might be easier to merge all columns and then delete the unwante ones, or create a smaller data frame to use for merging.


### Extract valid rows:
# remove records of foxes in territories where they were not resident in that season
merge_all_rank1 <- merge_all[merge_all$rank==1, seq(1:38)] 

# As above but only keeping columns 1-10 and column 25
merge_all_rank1_ltd <- merge_all[merge_all$rank==1, seq(1:10,25)] 

# sort rows by day number
merge_all_rank1_ltd<-merge_all_rank1_ltd[order(merge_all_rank1_ltd$daynum), ] 




#==============##==============##==============#
#==============##==============##==============#
#                 MIXED MODELS                 ####
#==============##==============##==============#
#==============##==============##==============#

# Check distribution of raw data to determine appropriate model
library(fitdistrplus) 
descdist(firstpatchCOREmerged$firstpatch, discrete=T, boot=500) 


# According to Innes' notes it's best to compare the maximal model with and without different random effects first
# to determine the appropriate random effects first:
# 1. max model with random intercept (and slope if interested in the variance in fixed effects between random effects)
# 2. max model with random intercept only
# 3. anova to compare mods
# 4. Sequentially remove fixed effects to determine the minimal model, once random effects established.

# INTERPRETING OUTPUT OF A MIXED MODEL:
# p-value is test of whether estimate is sig. diff from zero. Intercept p-value can be ignored.

Fixed effects:
  Estimate  Std. Error z value Pr(>|z|)    
(Intercept)                          1.11292    0.25698   4.331 1.49e-05 *** # intercept for whole model (here=estimated value for neighbours in spring)
  # season1:OriginNeighbour is baseline so estimate and SE = zero
  season2                             -0.36289    0.32393  -1.120  0.26260     # OriginNeighbour (estimate=difference between no foxes that are Neighbour:season1 (reference factor levels) and Neighbour:season2, so here it's lower)
season3                              0.44806    0.26561   1.687  0.09162 .   # OriginNeighbour (diff between no foxes that are Neighbour:season1 and Neighbour:season3)
season4                              0.39093    0.26864   1.455  0.14561     # OriginNeighbour
OriginPrevious group member         -2.44223    0.73351  -3.330  0.00087 *** # in season1
  OriginStranger                      -0.04442    0.29674  -0.150  0.88100     # in season1
season2:OriginPrevious group member  0.36269    1.04643   0.347  0.72890    
season3:OriginPrevious group member -0.04274    0.94635  -0.045  0.96398    
season4:OriginPrevious group member  0.52524    0.87475   0.600  0.54821    
season2:OriginStranger              -1.11870    0.58987  -1.897  0.05789 .  
season3:OriginStranger              -0.07336    0.38274  -0.192  0.84801    
season4:OriginStranger               0.61239    0.36561   1.675  0.09394 . 

# Random effects: these are essentially 'noise' that is controlled for in the model, so the model can't tell us
# about specific effect sizes for each level of the ranef. Just report their covariances or define their distribution,
# e.g. SD and variance.

# View random effect variance and SDs
print(VarCorr(strength_mod_NULL), comp=c("Variance","Std.Dev."))
# NOTE: GLMMs don't report or have a value for 'Residual Variance' like LMMs do # http://bit.ly/29tmtAp

# Analysis of deviance table with Wald tests to show which factors were significant (but better to use likelihood-ratio tests to compare models with & without each parameter using anova(model1, model2))
car::Anova(modelname) 

# CHECK DISTRIBUTION OF RAW DATA
mean(EncounterRate_noNAs$NTrueAssocs)/var(EncounterRate_noNAs$NTrueAssocs) # check for overdispersion: var should = mean for poisson model
hist(EncounterRate_noNAs$NTrueAssocs)         # right skew (long tail on right side) 
# Right skew indicates overdispersion so need to account for this.
# Options for overdispersed count data include:
# quasipoisson (overdispersed poisson) model
# neg binom model
# Normal poisson with an observation-level random effect:
EncounterRate_noNAs$OLR <- rownames(EncounterRate_noNAs) # use row name as observation-level ranef (OLR)




# GLMMs: Generalised linear mixed models: for non-normal data (as GLMMs use a link-function to transform data within the model to make it normal/a straight line)
#========================================#
# http://glmm.wikidot.com/faq#randomsig - good

# Decisions:
# 1. Fixed: season and origin, random = territory
# 2. Choose distribution: Count data so need Poisson (unless overdispersed - check dispersion once model fitted)
# 3. check distribution is suitable: mean vs var, 
# 4. Fit GLMs (though on GLMM wikidot they say NOT to compare GLMs with GLMMs as the log-likelihoods are not commensurate (i.e., they include different additive terms))
# 5. Fit GLMMs
# 6. Check GLMMs: dispersion, residuals
# 7. Extract model coefficients for fixed effects and SD, 95% cis, x2 and P for random effects
# 8. Post hoc tests if needed.

#  lmer and lme assume response has continuous gaussian(normal) distribution
# don't need to specify REML=F in glmer as default estimation method is already max likelihood 
# For LMER mods default method is REML, but need to fit using max likelihood (specify by REML=F) to do anova to compare 2 models (if ask to do anova on REML-fitted models lme4 automatically refits the models using ML)
# REML assumes the two models being compared have the same fixed effects structure, so are good to compare models with different random effects,
# but need ML to compare models with/without diff fixed effects.

#==== PROBLEMS WITH CONVERGENCE: FITTING DIFFERENT OPTIMISERS:

# When adding complexity e.g. random slope to lmer and glmer models they sometimes fail to converge without error warnings.
# Changing the optimiser can improve model fit.
# Can also increase no. iterations before R gives up fitting model with 'maxfun' (default is ~50): 
# Syntax= "optCtrl = list(maxfun = 100000))" - add after the optimiser, e.g.:

modelname<-glmer(y ~ x1 + x2 + (1|x3), data=mydata, family=poisson(link="log"),
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl = list(maxfun=100000), 
                                      check.conv.grad=.makeCC("warning",0.05))) 

# Null model with random intercept converges without warnings as is:
SO.glmm0<-glmer(Group.size ~ (1|T), data=SO, family=poisson(link = "log")) 

# ...So does model with 1 fixed effect and a random intercept
SO.glmm1.RI<-glmer(Group.size ~ season + (1|T), data=SO, family=poisson(link = "log")) 

# ...But not model with random slope
SO.glmm2.RS.default<-glmer(Group.size ~ season + (season|T), data=SO, family=poisson(link = "log")) # need to have season as fixed and random effect for random slope - specifies that the effect of season can vary among territories rather than just having different intercepts but same slope (so winter vs spring is same in all territories, when it might have a stronger effect in territory 1 than 2 etc.)
# Random slope only appropriate if interested in the variation among levels of random variable (rather than just wanting to control for it), but make models hard to interpret. http://www.r-bloggers.com/getting-the-most-of-mix-models-with-random-slopes/

# 1. Re-fit model with the optimiser 'bobyqa' and see if mod will converge
SO.glmm2.RS.boby<-glmer(Group.size ~ season + (season|T), data=SO, family=poisson(link = "log"), 
                        control = glmerControl(optimizer = "bobyqa"))

# 2. change optimiser to nelder-mead and see if mod will converge
SO.glmm2.RS.NM <- update(SO.glmm2.RS.boby, control=glmerControl(optimizer="Nelder_Mead")) 

# 3. change optimiser to nlminb and see if mod will converge
library(optimx)
SO.glmm2.RS.nlminb <- update(SO.glmm2.RS.boby, control=glmerControl(optimizer="optimx",
                                                                    optCtrl=list(method="nlminb")))

# 4. change optimiser to L-BFGS-B and see if mod will converge
SO.glmm2.RS.LBFGSB <- update(SO.glmm2.RS.boby, control=glmerControl(optimizer="optimx",
                                                                    optCtrl=list(method="L-BFGS-B")))

# 5. change optimiser to nloptr and see if mod will converge. First run this function:
library(nloptr)
defaultControl <- list(algorithm="NLOPT_LN_BOBYQA",xtol_rel=1e-6,maxeval=1e5)
nloptwrap2 <- function(fn,par,lower,upper,control=list(),...) {
  for (n in names(defaultControl)) 
    if (is.null(control[[n]])) control[[n]] <- defaultControl[[n]]
    res <- nloptr(x0=par,eval_f=fn,lb=lower,ub=upper,opts=control,...)
    with(res,list(par=solution,
                  fval=objective,
                  feval=iterations,
                  conv=if (status>0) 0 else status,
                  message=message))
}
# Then rerun model with the new optimiser nloptr version of bobyqa, "nloptwrap2":
SO.glmm2.RS.boby2 <- update(SO.glmm2.RS.boby, control=glmerControl(optimizer=nloptwrap2))

# 6. Rerun model with the nloptr version of N-M optimiser:
SO.glmm2.RS.NM2 <- update(SO.glmm2.RS.boby2,control=glmerControl(optimizer=nloptwrap2,
                                                                 optCtrl=list(algorithm="NLOPT_LN_NELDERMEAD")))

# note which models ran without warnings

# 7. Now compare models with diff optimisers:
getpar <- function(x) c(getME(x,c("theta")),fixef(x))
modList <- list(bobyqa=SO.glmm2.boby,NM=SO.glmm2.NM,nlminb=SO.glmm2.nlminb,
                LBFGSB=SO.glmm2.LBFGSB, bobyqa2=SO.glmm2.boby2,NM2=SO.glmm2.NM2)
ctab <- sapply(modList,getpar)
library(reshape)
mtab <- melt(ctab) # REQUIRES library(reshape2)

# plot:
theme_set(theme_bw())
ggplot(mtab,aes(x=Var2,y=value,colour=Var2))+
  geom_point()+facet_wrap(~Var1,scale="free")

# View maximum loglikelihoods: think lower is better...?
likList <- sort(sapply(modList,logLik))
likList  # compare log-likelihood (the model's log data likelihood, with no correction for the number
# of parameters. Larger (i.e. closer to zero) is better. The value for log-likelihood should 
# always be negative, and AIC, BIC etc. are positive.
round(log10(max(likList)-likList),2) # view log10 transformed to 2 decimal places - clearer differences
# NM2 or bobyqa2 were only ones to converge and have very similar log-likelihoods 

# 8. Check residuals are normal (though only for LMMs; normal resids are not required for GLMMs...)
hist(resid(SO.glmm2.RS.default)) # v.skewed with default optimisation (bobyqa)
hist(resid(SO.glmm2.RS.boby2)) # new model with boby2 optimiser more normal distrib.
hist(resid(SO.glmm2.RS.NM2)) # residuals of both boby2 and NM2 look the same...
qqnorm(resid(SO.glmm2.RS.boby))
qqline(resid(SO.glmm2.RS.boby)) # kind of normal...?!!

# 9. Compare fits of null, random intercept only and random intercept & slope
SO.glmm0<-glmer(Group.size ~ (1|T), data=SO, family=poisson(link = "log")) # null
SO.glmm1.RI<-glmer(Group.size ~ season + (1|T), data=SO, family=poisson(link = "log")) # random intercept
SO.glmm2.RS.boby2<-glmer(Group.size ~ season + (season|T), data=SO, family=poisson(link = "log"), 
                         control = glmerControl(optimizer = nloptwrap2), nAGQ = 1) # random intercept & slope
anova(SO.glmm0, SO.glmm1.RI, SO.glmm2.RS.boby2) # adding a random slope doesn't improve model fit

##

# Alternative: use package AFEX to run all optimisers with one command

# Attempts to re-fit a [g]lmer model with a range of optimizers. The default is to use all known optimizers 
# for R that satisfy the requirements (do not require explicit gradients, allow box constraints), in three
# categories; (i) built-in (minqa::bobyqa, lme4::Nelder_Mead), (ii) wrapped via optimx (most of optimx's 
# optimizers that allow box constraints require an explicit gradient function to be specified; the two 
# provided here are really base R functions that can be accessed via optimx, (iii) wrapped via nloptr.
library(afex)
library(optimx)
library(nloptr)

# Will still use this model as the FINAL MODEL:
finalmodel<-glmer(Count~season*Origin + (1|oriT), data=origins, family=poisson(link = "log")) 

# FINAL MODEL:
finalmodel<-glmer(Count~season*Origin + (1|oriT), data=origins, family=poisson(link = "log")),
control=glmerControl(optimizer="bobyqa", check.conv.grad=.makeCC("warning",0.05))) 

gm_all <- allFit(f)
t(sapply(gm_all,fixef))
sapply(gm_all,getME,"theta") ## theta parameters
cbind(sapply(gm_all,logLik)) ## log-likelihoods
cbind(!sapply(gm_all,inherits,"try-error")) ## was fit OK?




# Linear mixed models: (package 'nlme') - see also note in 'Repeated measures data' section ####
#=========================================#

# Models using lme()
# It's preferable to use max likelihood estimation (ML) instead of the default restricted max likelihood (REML) - code using REML=F



# Linear mixed models (package 'lme4') ####
#==========================================#
require(lmerTest) # TO GET P VALUES FROM LMER MODELS USE PACKAGE "LMERTEST"
require(car) # or use car if lmerTest won't work
car::Anova(finalmodel)


### Syntax for random effects # SEE: http://bit.ly/2a89fHF ####
###
(1|Territory) # random intercept for territory
# CROSSED (or partially crossed)
(1|Territory)+(1|season) #  assumes effect of territory is same across all seasons
# NESTED = (1|Bigger/Smaller/Smallest) - if one level is *implicitly* connected/associated with another, i.e. it is meaningless without the other level, e.g. egg/nest, dog/owner
(1|individual/factor) or (1|nest/egg) # allows effect of site to vary between years (main effect of year plus interaction between year and site)
(1|patch/fox) = (1|patch) + (1|patch:fox) # main effect of patch plus interaction between individual and patch, which specifies there is variation between patches and between individuals for each patch
# if every subject was not measured at each level of the higher order factor, e.g. subject1 was measured in spring and summer but not autumn and winter. 
# e.g. the same foxes were not seen in same territories. Nesting creates a random term for every fox/territory combination to allow diff intercepts for foxes in diff territories.

# If there is random variation among INDIVIDUALS *and* random variation among PATCHES
# '*and* there is a consistent PATCH effect across INDIVIDUALS (and vice versa), then the random effects should be treated as CROSSED.
# If there is not a consistent PATCH effect across INDIVIDUALS and v.versa then code PATCH and INDIVIDUAL as an interaction:
(1|Patch) + (1|ShortCode) + (1|Patch:ShortCode) 
# my GLMMs seem to fit better when random effects are coded in this way: there are patch-specific effects, fox-specific effects and patch-by-fox effects.
# Using an interaction also accounts for fact that foxes were not observed in every patch



# Random slopes: ALWAYS PUT lower level grouping factor at end, 
# e.g. blocks within sites= |site, seasons within territories= |territory, |subject, |foxID, |schoolID

# Random slope for effect of season on visit duration in different territories: 
lmer(visdur~season+(season|Territory), data=mydata) # the effect of season on visit duration varies between territories (random effect should be a group, not an individual measurement)

(1+practice|participants) # random slope (and intercept) for the effect of practice for each participant, or effect of season for each territory
(season|territory) # random slope (and intercept) for the effect of season for each territory (can omit "1+" as is there by default)

(practice|participants:context) # random slope (and intercept) for the effect of practice for each participant and context combination (doesnt make much sense) 
(practice:context|participants) # random slope (and intercept) for the interaction effect of practice by context for each participant (makes more sense)
(season:Origin|Territory) # random slope (and intercept) for the interaction effect of season by Origin for each territory.

gsmod0<-lmer(Group.size ~ (1|Territory), data=mydata, REML=F) # null model with just the intercept (Territory) as random effect
gsmod1<-lmer(Group.size ~ season + (1|Territory), data=mydata, REML=F) # seasonfac as fixed effect, territory as random
# REML=F ensures estimation is done by max likelihood method (recommended)
anova(gsmod1, gsmod0) # compare models using AIC
# to see if season explains enough variation in group size to significantly improve model fit
# quote this as "X2(3) = 37.27, P<0.001" (X=chi-sq value plus d.f.) x2=chi-sq and (3)=dff between 
# x2 is equivalent to the 'change in (triangle symbol) deviance': Emily and Zhuoyu quoted this at the advice of Innes but papers also quote chi-sq.
# the df of the two models - because 3 seasons were added with gsmod1

gsmod1                    # view the whole model output
summary(gsmod1)           # view coefficient estimates for each season
anova(gsmod1)             # to get F value and num+den dfs. (quote as e.g. "F(3,21) = 2.82, p = 0.06")
lmerTest::anova(gsmod1)   # to get a p-value from an lmer model use package 'lmertest' 
# numdf=numerator d.f. and dendf = denominator d.f.

# Can have multiple random effects:
nrmod0<-lmer(nonres ~ (1|Territory) + (1|seasonfac), data=mydata, REML=F) # null model with no fixed effect but 2 random effects
nrmod1<-lmer(nonres ~ seasonfac + (1|Territory) + (1|seasonfac), data=mydata, REML=F) # season as fixed eff
anova(nrmod1, nrmod0) # compare models using AIC


# This could be the code for repeated measures - indicates nesting of territory witin season:
orimod0<-lmer(Count ~ (1|oriT/seasfac), data=origins, REML=F) # null model with no fixed effect
orimod1<-lmer(Count ~ Origin + (1|oriT/seasfac), data=origins, REML=F) # one fixed effect of season
orimod2<-lmer(Count ~ seasfac + Origin + (1|oriT/seasfac), data=origins, REML=F) # two fixed effects of season and origin
orimod3<-lmer(Count ~ seasfac * Origin + (1|oriT/seasfac), data=origins, REML=F)  # interaction between season and origin 
# - plus their individual main effects
anova(orimod0, orimod1) # sequentially compare more complex models to see if adding predictors improves fit (i.e. reduces AIC - lower/smaller is better)
anova(orimod1, orimod2)
anova(orimod2, orimod3) # orimod3 has lowest AIC - indicates interaction is the best model


#================================================================================================#
# MIXED MODELS FOR OVERDISPERSED COUNT DATA: FITTING AN OBSERVATION-LEVEL RANDOM EFFECT (OLR) ####
#=================================================================================================#

# lme4 glmer with observation-level ranef - doesn't need to be a factor
model1 <- glmer(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded + (1|ShortCode) + (1|OLR), data=mydata, 
                family=poisson, verbose=T) # verbose=T gives more info on progress of model fitting, akin to a progress bar


#===========================================#
# MODEL CHECKING ####
#===========================================#

# http://stats.stackexchange.com/questions/185491/diagnostics-for-generalized-linear-mixed-models-specifically-residuals/187108#187108

# Make sure model includes all relevant predictors (by plotting resids against all potential variables, including those not in the model)
# CHECK RESIDUALS in full model ARE NORMAL (if relevant, i.e. LMMs, not GLMMs) 
# CHECK RESIDUALS in full model VARY HOMEOGENOUSLY BETWEEN CATEGORIES / ALONG THE SCALE OF THE Y (for LMMs and GLMMs)
# Check fixed effects are not correlated using via. vcov(model) # 1.00 indicates correlation (I think)
# CHECK FOR OVERDISPERSION (in count models only)
# REPORT THE RANDOM EFFECTS' VARIANCE & SD (CIs AND P-VALUE optional)

# Checking the residuals once from the full model is 
# A) all that is required, and 
# B) a much better thing to check. 



# NEG BINOM MODEL RESID PLOTS: http://stats.stackexchange.com/questions/114045/validating-residual-plot-count-data-different-levels

# Check the residuals are normally distributed and variances are homogeneous - equal across all values of the fitted values
hist(resid(model))
# OR with smoothing (density line)
hist(resid(model), prob=T) # prob=T to plot as probability (so can overlay a smoothing line)
lines(density(resid(model))) # overlay line

qqnorm(resid(myModel))
# NOTE residuals are not expected to be normal from Poisson, negbinom or other models that are not fit on a gaussian (normal) distribution
# For non-parametric models best to check model fit using resid-fitted plots:
plot(resid(model), fitted(model)) # Poor fit indicated by nonlinearity and/or heteroscedasticity - curving and fanning/funnelling
lines(lowess(fitted(model),resid(model)),col="red",lwd=2) # add a lowess smoothing line - should be approx flat

# UsefuL link: https://danieljhocking.wordpress.com/tag/glmmpql/
# Useful package: 
library(LMERConvenienceFunctions)

#== Check homogeneity of residuals: plot(resid(model), fitted(model)): best way to check models

# Resids from normal data should be evenly dispersed with no correlation for normally distrib data (will be slightly different for generalized models like Poisson - see below)
# The usual output from a Poisson log-link model has datapoints arranged in 'lines' (owing to 
# discrete counts) that are curved (due to the log transformation within the model)
# http://stats.stackexchange.com/questions/114045/validating-residual-plot-count-data-different-levels

# binomial models also have fitted-resid plots with (two) lines

# PLOT RESIDUALS AGAINST FITTED VALUES
# Plot fitted-resid in response scale (from a GLMM)
plot(fitted(erateMOD_full), resid(erateMOD_full, type="response"), main="Response residuals")  # 'fitted' automatically plots on response scale
lines(smooth.spline(fitted(erateMOD_full), residuals(erateMOD_full, type="response"))) 

# For a Poisson model, variance increases as mean increases so ordinary residuals should increase in variance as values of the response (fitted) increases
# plot normal resids
plot(fitted(erateMOD_full), resid(erateMOD_full), main="Standard (poisson in this case) residuals")
lines(smooth.spline(fitted(erateMOD_full), residuals(erateMOD_full)))

# Extract the Pearson residuals - these are divided by the square root of the variance according to 
# an equation appropriate for the particular model type. So these standardised resids should have constant
# spread (variance) as y-hat (fitted aka. predicted y) increases.
sresid <- resid(erateMOD_full, type = "pearson") 
plot(fitted(erateMOD_full), sresid, main="Pearson residuals")
lines(smooth.spline(fitted(erateMOD_full), sresid))

# Just using plot(model) also displays the pearson residuals against fitted:
plot(erateMOD_full, col=EncounterRate_noNAs$fSeason) # colours for season to aid interpretation

# Alternatively plot the residuals against the real data (independent variables)
plot(sresid ~ EncounterRate_noNAs$DaysSeen) # variance of resids is similar across the whole range of DaysSeen

# View model trends using visreg
# plots contain a confidence band, prediction line, and partial residuals
visreg::visreg(erateMOD_full) # variance should be approx even across all levels of each model parameter
# can also save visreg predictions - see model predictions section


# Can use SIMULATIONS to compare residuals from model based on simulated data with those from model based on real data 
# (should be similar if model fits well)
# 1. Calculate simulated NTrueAssocs based on model and save as new column in dataset
EncounterRate_noNAs$sim <- simulate(ERate_mod_FULL)[[1]] 
# 2. Refit original model to simulated data
ERate_mod_FULL_SIM <- lmer(sim ~ Sex*SocialStatus*fSeason (1|ShortCode) + (1|Place), data=EncounterRate_noNAs, family=poisson, verbose = T) # verbose=T shows progress
# 3. Make residuals x fitted plot for model run on simulated data and plot against the simulated y-variable
Fitted <- exp(log(fitted(ERate_mod_FULL_SIM)) - ranef(ERate_mod_FULL_SIM)$OLR[[1]])
Resid <- (EncounterRate_noNAs$sim - Fitted) / sqrt(Fitted + (Fitted^2) * c(exp(VarCorr(ERate_mod_FULL_SIM)$OLR) - 1)) 
plot(Fitted, Resid) 
# 4. Plot both the simulated and real data against the covariates and hope that they look similar
plot(EncounterRate_noNAs$NTrueAssocs, EncounterRate_noNAs$fSeason) 
plot(EncounterRate_noNAs$sim,         EncounterRate_noNAs$fSeason) # they do look similar, just with diff xlim


# Plot distrib of residuals # need not be normal depending on the distribution, e.g. negbinom hists will be right-skewed 
hist(resid(glmm3a)) # seems to not work for lme at random! Restart R and retry if it buggers about
hist(resid(glmm3a, type="response")) # resids on response scale
qqnorm(resid(glmm3a))  # plot standardised residuals vs. normal values: should be a straightish line
qqline(resid(glmm3a)) #superimpose a line to a normal quantile-quantile plot which passes through the first and third quartiles
qqp(residuals(lme.orimod3), "norm", main="normal")  # needs CAR and MASS packages.  Most data points should fall between dotted lines
qqnorm(glmm3a, col=spatialoverlap$season) # colours for seasons (model results not residuals)

library(fitdistrplus)
plot(fitdist(resid(glmm3),"norm"))  # seems to only work with lme4 models
plot(model, col=spatialoverlap$fSeason)  # plot seasons as different colours to help ID outliers


### Distribution of studentized residuals (=same properties as standardised resids but provide a more precise estimate 
# of the error variance of a specific case/datapoint)
sresid <- studres(model1) # can only do this with lme() models, not lmer
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)




### Dharma package for checking GLMMs ####
library(DHARMa)
# Steps for model checking:
# 1. simulate standardised resids from 0-1
# 2. plot these simulated resids - QQplot and fitted-resid plot to check variance is homogenous (dharma adjusts plots dep on model family, unlike qqplot)
# 3. plot simulated resids against variables to check for autocorrelation and patterns

# Dharma: Simulate residuals standardised between 0-1 (most DHARMA functions only work on standardised simulated resids)
simgam <- simulateResiduals(dailyPRTmod_comb_FULLgamma)

# Dharma: residual plots 
# Website with examples of how 'perfect' Gamma model resids should look: https://github.com/florianhartig/DHARMa/blob/master/Code/Examples/GammaProblems.md
plotSimulatedResiduals(simgam, quantreg=T) # takes a while if quantreg=T
# QQ-plot should be straight line, RHS plot should be uniform in y direction (but 
# density of points depends on distrib of data so doesnt matter, e.g. i have lots of low values)

# Dharma: plot residuals against the predictors in the model (or potentially predictors that were not in 
# the model) to detect possible misspecifications (highly recommended) 
# See: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# Resids in 'simulationOutput$scaledResiduals' are scaled between 0-1 
plotResiduals(mydata$Sex, simulationOutput$scaledResiduals)
# Should show no patterns or correlations (autocorrelation), as with a fitted-resid plot, 
# otherwise the relationship could be non-linear / some predictors might be missing.

# Dharma: Kolmogorov-Smirnov goodness-of-fit test (=One-Sample Kologorov-Smirnov test)
#  Tests whether distrib of resids differs from uniform/normal distribution (normality test)
# not sure what to do if resids not normal/if this is OK for GLMMs?
testUniformity(simgam) # If p<0.05 the distrib does differ from normality

# Dharma: check OVERDISPERSION (need to do special, longer simulations in DHARMa for this to work)
# assume this is only meant for poisson and negbinom models; gamma mods cant be overdispersed due to the r parameter or something
simgam2 <- simulateResiduals(fittedModel = dailyPRTmod_comb_FULLgamma, refit = T, n = 20) # V.SLOW
testOverdispersion(simgam2) # gives dispersion parameter and a p-value
# Note on overdispersion from https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html:
# If you have a lot of data points, residual diagnostics will nearly inevitably become significant,
# because having a perfectly fitting model is very unlikely. That doesn't neccessarily mean you need
# to change your model. The p-values confirm that there is a deviation from your null hypothesis, but
# it is your discretion whether this deviation is worth worrying about. A dispersion parameter of 1.01
# is not a problem, even if the test is significant, but a significant value of 5 is clearly a reason 
# to move to a model that accounts for overdispersion.





### Goodness-of-fit tests ####
# goodness of fit test for the overall model: calculates the residual deviance (=the difference 
# between the model deviance and the max deciance of the ideal model where the predicted values
# are identical to the observed). If test is significant, difference is signif so model fit is poor.
with(model, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))

# goodness-of-fit test FOR COUNT DATA ONLY:
library(aods3) 
gof(glm1)

# OR calculate Omega?_0 -  coefficient of determination to represent goodness of model fit, similar to R^2 (r-squared)
# recommended by http://onlinelibrary.wiley.com/doi/10.1002/sim.1572/abstract
1-var(residuals(model))/(var(model.response(model.frame(model)))) 
# not sure how to interpret this, presume nearer to 1 is better as means model explains higher % 
# variance in Y?


# Kolmogorov-Smirnov goodness-of-fit test (=One-Sample Kologorov-Smirnov test)
#  Tests whether distrib of resids differs from uniform/normal distribution (normality test)
library(DHARMa)
# first need to simulate residuals standardised between 0-1 (most DHARMA functions only work on standardised simulated resids):
simgam <- simulateResiduals(dailyPRTmod_comb_FULLgamma)
testUniformity(simgam) # If p<0.05 the distrib does differ from normality



### R-squared (rsquared) from 0-1) ###
# - represents proportion of variance in Y that the model explains
# i.e. the scatter around the regression line (see http://bit.ly/2otiSw5 for nice illustrative plots)
# = residuals from high R2 are tightly clustered around regression line; resids from low R2 models have more spread
library(MuMIn)
r.squaredGLMM(dailyPRTmod_comb_FULL) # R2m = marginal r2, fixefs only; R2c = conditional, includes ranefs)
# But R2 is irrelevant when simply looking at relationships
# R2 is more relevant when predicting values (particularly when predicting values for specific individuals, e.g. PRT at particular patches)
#    - a higher R2 means more precise predictions (smaller confidence interval)
#    - see this blog: http://bit.ly/2n4522K 





#============================#
# INFLUENTIAL DATA POINTS ####
#============================#
# Assess leverage (outliers with extreme x values likely to influence regression estimates)

# can use Cullen & Frey plot, but influence.ME package below is more useful
# Cullen & Frey plots do sometimes indicate influential datapoints that may be outliers (but not in my limited experience)
library(fitdistrplus) 
descdist(residuals(PRTmodgamma_feedingdayno16WD), boot = 1000) 

library(influence.ME)
inf <- influence(model=PRTmodgamma_feedingday, count=TRUE, group="StationCode")
# NB this won't work for lognormal models where response is transformed within the model
# instead transform the response in the raw data frame before fitting the model.
# But then lsmeans won't backtransform predictions so for lsm need to refit model with log10(y)
# within the model itself.
dfbetas(inf)
plot(inf,
     which="dfbetas", # what to plot
     parameters=c(2,3,4), # which model parameters to plot
     xlab="DFbetaS", ylab="StationCode",
     cutoff=2/sqrt(35)) # 2/vn where N=no.grouping levels (patches) 
# recommended rule of thumb from Belsley et al., 1980.

# Cooks distance
# As a rule of thumb, cases are regarded as too influential if the associated value for Cook's Distance
# exceeds the cut-off value of 4/n (Van der Meer et al., 2010) 
cooks.distance(inf, parameter=c(1,2,3,4), sort=TRUE) # print values of Cook's distance
plot(inf, 
     which="cook", # plot cooks distance
     cutoff=4/35, # 2/vn where N=no.grouping levels (patches) - (Van der Meer et al., 2010)
     sort=TRUE, # sort in order of most-least influential
     xlab="Cook?s Distance", ylab="StationCode") 

# See whether exclusion of each patch in turn affects each model coefficient by changing the 
# t-value or z-value by >1.96, which would alter the significance of the coefficient
### (NOTE: z-value can be calculated in model tables by dividing the beta coefficient (estimate) by its standard error).
# Altered.sig = significance of coeff in the new model without the patch (true=sig, false=not sig)
# Changed.sig means 'did sig of model change when patch was excluded?'
sigtest(inf, test=-1.96)$varMJperDay        # negative sign of test= refers to sign of z-statistic in model
sigtest(inf, test=1.96)$MeanMJperFeedingDay # positive sign of test= as z-statistic in model is positive
sigtest(inf, test=1.96)$DaysFedPerWeek




#=======================================================#
#  Model matrix - view or make ####
#=======================================================#

# Save the model matrix
head(model.matrix(erateMOD_full)) # list of values for the intercept and fixed effect, for each level of random effect (IDfac) =rows
head(model.matrix(~ DaysSeen, data=EncounterRate_noNAs))

# alternative:
newdata <- expand.grid(DaysSeen = seq(0,40,10)) # specify vector of values of the explanatory variable x to calculate predicted values for
modelmatrix <- model.matrix(~DaysSeen,newdata) # make model matrix based on these values (basically the same as 'newdata' but with an added column for the intercept)

# to make custom model matrix with fixed effects set at particular values
# new data frame listing all variables in model (including ranefs in some cases)
newdata <- expand.grid(DaysSeen=c(10,20,30,40), # here want to make predictions at 10,20,30 and 40 days
                       Sex=unique(EncounterRate_noNAs$Sex),
                       SocialStatus=unique(EncounterRate_noNAs$SocialStatus),
                       fSeason=unique(EncounterRate_noNAs$fSeason),
                       DaysFedPW_rounded=mean(EncounterRate_noNAs$DaysFedPW_rounded))
# write fixed effects in same way as model formula. Have to include all variables in pred.data
modelmatrix <-model.matrix(~DaysSeen+Sex*SocialStatus*fSeason+DaysFedPW_rounded, newdata) 

# alternative: using function getME in lme4 - for lmer-type models
newmm<- getME(erateMOD_full,"X") 
# "X" means get fixed-effects model matrix
# "Z" means get random-effects model matrix

#=======================================================#
## Model selection: confirming why you chose what model ####
#=======================================================#

# AICc is the 'second-order' or 'small sample' AIC - Burnham & Anderson 2002 recommend its use when N/K < 40. N=sample size, K= number of estimated parameters
# better for small samples: to calculate AICc can use either 'MuMIn' or 'AICcmodavg':
library(MuMIn) 
AICc(defcomp.mod1,  k = 2, REML = NULL)

# QAIC: for overdispersed count data in poisson models # bbmle works for glmmADMB models
bbmle::ICtab(fm_nb, fm_nb_os, fm_zinb, fm_zinb_os, fm_pois, fm_pois_os, fm_zipois, fm_zipois_os, type="qAIC") # can change type to AIC, BIC, AICc etc

library(AICcmodavg) 
## S3 method for class 'lme' - for other templates see http://finzi.psych.upenn.edu/library/AICcmodavg/html/AICc.html
AICc(mod, return.K = FALSE, second.ord = TRUE, nobs = NULL, ...)
AICc(defc, return.K = TRUE) # return.K shows number of estimated parameters from the model
AICc(lme.orimod3, return.K = FALSE, second.ord = FALSE) # calculates the AIC if second.ord=F (first order=AIC)
AICc(lme.orimod3, return.K = FALSE, second.ord = TRUE) # calculates the AICc if second.ord=T (second order=AICc) - these settings are the default, so can just put:
AICc(lme.orimod3) # basic code to get AICc

## Or make a model selection table with AICcs, delta (change in) AICcs, AICc weights and log likelihood.
Cand.mods <- list("seas*ori" = lme.orimod3, "seas+ori" = lme.orimod2, "seas" = lme.orimod1, "null" = lme.orimod0)
aictab(cand.set = Cand.mods, second.ord = TRUE)
print(aictab(cand.set = Cand.mods, second.ord = TRUE),digits = 3, LL = TRUE)

# more long-winded approach to making a model table using the 'modnames' argument:
Cand.models <- list( ) # make template for list of candidate models
Cand.models[[1]]<-lme.orimod0 # add each model to a separate column
Cand.models[[2]]<-lme.orimod1
Cand.models[[3]]<-lme.orimod2
Cand.models[[4]]<-lme.orimod3
Modnames <- paste("mod", 1:length(Cand.models), sep = " ") # create vector of model names
aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE) # create model selection table
## round to 3 digits after decimal point and give log-likelihood
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE),
      digits = 3, LL = TRUE)

# Can use 'drop1' function (base R) - R sequentially refits model with and without each term and gives an F, Chisq or (default) dAIC each time
# Can take a while depending on model complexity
# Doesn't split interactions into parts, just tests model with and without the whole thing (i.e. if mod contains sex*status it won't remove sex and leave status in, it'll just remove the whole interaction)
drop1(model) # took 5 minutes with a glmer poisson GLMM / less code required than refitting loads of diff models manually & gives a good initial look at possible models, works like MuMIn::dredge
# example output:
Single term deletions
Model:
  NTrueAssocs ~ DaysSeen + Sex * SocialStatus * fSeason + (1 | Place) + (1 | ShortCode)
Df  AIC
<none>                      26781
DaysSeen                  1 27610
Sex:SocialStatus:fSeason  3 26825
# drop1(model, test="Chisq") for categorical predictors or "F" for continuous predictors, otherwise just gives dAIC
# drop1 p-values are more reliable than those in summary(model) - acc. to Rob Thomas stats book 'data analysis with R'
# don't use this on glmmADMB models! Went on for 3 hours and never finished till I cancelled it!


# My favourite technique for model simplification:

#1. fit full model
#2. use car::Anova(model) or summary(model) to view p-values and see which fixed eff is least significant
#3. Refit model without least significant fixef
#4. use anova(fullmodel, reducedmodel) to compare model deviance (goodness of fit) with and without the fixef
#5. If p<0.05 the fixef significantly improves model fit and should be kept in.
#6. Move onto the next least significant fixef until all combinations of fixed effects have been removed and compared

# Instead of writing out the whole lot of code for each reduced model, use the update() function
# to add or remove individual fixed effects from the previous model:
reducedmodel_noDF = update(fullmodel, . ~ . - DaysFedPerWeek)
anova(fullmodel, reducedmodel_noDF)



#===========================================#
# NA / missing values ####
#===========================================#
# useful website
http://www.ats.ucla.edu/stat/r/faq/missing.htm

# calc mean omitting NAs (otherwise if there are NAs the mean will be NA)
mean(x1, na.rm = TRUE)

na.action = na.omit # completely removes NAs from data
na.action = na.exclude # Does not use the missing values, but maintains their position for the residuals and fitted values

# Also see Rob Thomas's book on R Statistics for Biologists


#=============================#
# Network centrality measures # - see Excel file with graph to see how diff measures calc in diff packages/methods compare "Testing diff centrality measures in R for correlations and normalisation 020616.xlsx" ####
#=============================#
library(igraph) # need to convert network to an igraph 'graph' object to use igraph to calc centrality
# So can't use igraph on stacks of matrices without a complicated loop that includes the graph.adjacency conversion
T1sprB4_net_igraph <- graph_from_adjacency_matrix(T1sprB4_net,mode="undirected",weighted=TRUE,diag=FALSE)
# 'graph_from_adjacency_matrix' = same command as 'graph.adjacency' but think this is an older version - underscores seem preferred to dots in function names
E(T1sprB4_net_igraph)$weight # view weights
T1sprB4_attr$bindeg <- degree(T1sprB4_net_igraph) # unweighted degree (=binary degree)
T1sprB4_attr$strength <- graph.strength(T1sprB4_net_igraph) #weighted degree
T1sprB4_attr$eig.ig <- eigen_centrality(T1sprB4_net_igraph, scale=FALSE)$vector # eigenvector centrality - same values as given in SOCPROG and in R package sna
T1sprB4_attr$cc <- transitivity(T1sprB4_net_igraph, type = "local") # unweighted local CC (I NEED WEIGHTED SO DONT USE THIS)
T1sprB4_attr$cc.weighted <- transitivity(T1sprB4_net_igraph, type = "barrat") # weighted clustering coefficient using the Barratt 2004 method - same results as given by R package tnet - will use tnet not igraph (easier as don't need to convert network into an igraph object)
T1sprB4_attr$btw <- betweenness(T1winB4_net_graph, directed=F, normalized = T) # DONT USE as I'm not going to use betweenness
T1sprB4_attr$closeness <-igraph::closeness(T1sprB4_net_igraph, normalized = FALSE) # NB DONT USE!! - igraph expects weights that represent 'costs' instead of 'strength'. 
# So edge weights with higher values represent greater distance/cost rather than a closer bond in igraph. Better to use tnet, which uses the same algorithm as 
# igraph (Dijkstra's algorithm) but on the inverse of edge weights.
# igraph also computes closeness across whole network and doesnt ignore isolates/infinite distances

# igraph to calculate communities and modularity
# convert network to igraph object
T1_spr_net_graph <- graph_from_adjacency_matrix(T1spr_net,mode="undirected",weighted=T,diag=F)
# Detect communities using the Newman method (same as in SOCPROG)
a <-cluster_leading_eigen(T1_spr_net_graph) 
modularity(a) # get modularity of the community division
max(membership(a)) # number of groups/communities
plot(a, T1_spr_net_graph) # plot clusters


#=====  I WILL ONLY USE igraph FOR DISTANCE-WEIGHTED REACH:

# There is no inbuilt function in sna, R, tnet or qgraph for REACH, but I found this custom function that works within igraph:
# from http://www.shizukalab.com/toolkits/sna/node-level-calculations

# DISTANCE-WEIGHTED REACH - run this function to save it
# Calculates number of nodes reachable in 2 steps weighted by association strength
dwreach=function(x){
  distances=shortest.paths(x) # create matrix of geodesic distances
  diag(distances)=1 # replace the diagonal with 1s
  weights=1/distances # take the reciprocal of distances (reciprocal is simply one divided by the number)
  apply(weights,1,sum) # sum for each node (row)
}
dwr <- dwreach(T1winB4_net_graph) # works on igraph object as depends on some igraph code


# Dwreach relies on igraph, which can't accept stacked matrices so need to use a loop to compute REACH
# Loop to calculate distance-weighted reach using custom code within igraph (see 'Useful bits and bobs' R code file for source)
# Set up a K x N x N matrix to store values from the random networks
N <-nrow(T1sprB4_attr) # Number of individuals
dwreach_randmat <- matrix(NA, nrow=N, ncol=1000) # make N-by-perm matrix filled with NAs
rownames(dwreach_randmat) <- T1sprB4_attr$ShortCode
for (col in c(1:1000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- T1sprB4_rand[col,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  net_rand_ig <- igraph::graph_from_adjacency_matrix(net_rand, weighted=TRUE, mode="undirected", diag=FALSE)
  a <- dwreach(net_rand_ig)
  dwreach_randmat[,col] <- a[1:N]
}
T1sprB4_dwreach_randmat <- dwreach_randmat # save as the right name for future use / so can do other networks


# REACH 2: same as Reach used in Flack et al 2006 (defined as m-reach by Borgatti 2006, e.g. 2-reach, 3-reach)
# Calculates number of nodes reachable in 2 steps (in 1 step would be same as degree/strength)
reach2=function(x){
  r=vector(length=vcount(x))  # vcount means count number of vertices (nodes) in a graph
  for (i in 1:vcount(x)){
    n=neighborhood(x,2,nodes=i) # neighborhood finds the nodes not farther than a given limit from another fixed node
    ni=unlist(n)          # unlist flattens the list into a vector
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}
reach2(T1winB4_net_graph) # PROPORTION (Shows same pattern as dwreach but scaled between 0-1)


#==

library(sna) # some function names same as igraph so don't have both loaded at once
stackcount(T1sprB4_rand) # count number of networks (matrices) in the graph stack? - useful for random networks
T1sprB4_attr$bindeg.sna <- degree(T1sprB4_net, gmode="graph", ignore.eval=T)

#### sna degree won't work for stacked graphs as seems to ignore the argument 'ignore.eval=' so values are same as strength
T1sprB4_rand_degree <- degree(T1sprB4_rand, gmode="graph", g=c(1:1000), ignore.eval=TRUE)

degree(T1sprB4_net, gmode="graph", ignore.eval=F) #weighted degree  #ignore.eval=FALSE means don't ignore edge weights (doesn't work for stacked matrices e.g. random networks - default is weighted degree and wont calc unweighted degree)
evcent(T1sprB4_net, gmode="graph", use.eigen=T) # eigenvector centrality - same values as given in SOCPROG # use.eigen=T specifies slower but more robust computation method. Sign will be different for isolates and pairs (eg positive if all others are negative). Rescaling is not influenced by N or isolates.
(a<- abs(evcent(T1sumB4_net, gmode="graph", use.eigen = T)))/(length(a)-1) # standardise by dividing score/(n-1)
evcent(T1sprB4_net, gmode="graph", use.eigen=T, rescale=T) # standardise by score/sum(scores) - best

betweenness(T1winB4_net, gmode = "graph", cmode="undirected", ignore.eval = T) # DONT USE as I'm not going to use betweenness 
closeness(T1spr_net, gmode="graph", ignore.eval = FALSE, rescale=F, cmode = "suminvundir") ## For undirected networks with disconnected components set cmode = "suminvundir" (as other cmodes won't work for disconnected components)

# or clustering coefficient (only calculates global CC, not local i.e. one per fox)
T1sprB4_rand_cc <- gtrans(T1sprB4_rand, g=c(1:1000), mode="graph")   # clustering coef (transitivity) 

# Network density (weighted)
gden(T1sprB4_net, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE) 

# Get main component only (library sna)
component.largest(T1spr_net, result = "graph") # returns the network without isolates and pairs (i.e. the largest component)

#======#

library(qgraph) #alternative package for quick+easy centrality #BUT NOT CUSTOMISABLE AND CALC METHODS NOT WELL EXPLAINED!!
qgraph.centrality<-centrality_auto(T1sumAf_net)$node.centrality # DONT USE #computes betweenness, closeness and strength (same strength as igraph/sna but closeness different to igraph and tnet)
# only computes closeness within the largest component, unlike igraph (but largest component is logical)
qgraph.cc <- clustcoef_auto(T1sumAf_net) # compute local CC by several methods. Zhang method is most similar to that in SOCPROG so will use that. (See qgraph.pdf for details)
T1sprB4_attr$qgraph.cc.zhang <- clustZhang(T1sumAf_net)$clustZhang # compute weighted local CC by Zhang method (most similar CC to that calc by SOCPROG)

#======#

library(tnet) # alternative library for local CC and closeness # I TRUST THIS MORE THAN QGRAPH PACKAGE
# tnet works with edgelists rather than matrices, but automatically converts matrices into egelists within each function
# From manual: if a matrix is entered (more than 4 columns and rows), it is assumed to be a weighted one-mode network if square or a two-mode network if non-square.
as.tnet(T1sprB4_net, type=NULL) # check what tnet interprets the input matrix as. Seems to work well for my data!

# For networks with 4 individuals matrices have 4 columns and tnet interprets these as 'longitudinal' networks: need to convert to edgelist manually:
T4spr_EL <- sna::as.edgelist.sna(T4spr_net) # convert matrix to edgelist in sna
T4spr_tnet <- as.tnet(T4spr_EL, type="weighted one-mode tnet") # convert edgelist to tnet object

# CLOSENESS in tnet # https://toreopsahl.com/tnet/weighted-networks/shortest-paths/
# calculated as sum of inverse of distances (edge weights) so takes inverse of edge weight (=1/edge weight) and sum of this is 'farness' and inverse of this as 'closeness'
T1winB4_attr$tnet.closeness <- data.frame(closeness_w(T1winB4_net))$closeness # normal closeness
T1winB4_attr$tnet.normcloseness <- data.frame(closeness_w(T1winB4_net))$n.closeness # normalised closeness scores (i.e., divided by N-1).
# n.closeness is calculated by normalised edge weights (normalised by the average weight in the network) recommended by Tore Opsahl to help interpretation: 
# so a unit of distance will refer to one step with the average weight in the network, e.g. "The mean closeness of the matrix suggests that on average nodes are
# 2.29 steps with average edge weight away from each other. This measure would be comparable across networks with different ranges of tie weights. 
data.frame(closeness_w(T1winB4_net, gconly=F))
#gconly=F means don't just calc closeness within the main component: isolates have zero closeness (default=gconly=TRUE to calc only within largest component =BEST)

# NOTE: tnet won't calculate a closeness score for the last fox in a matrix (in last col + last row)
# If its column contains only zeros - because it never associated. 
T6aut_net # check by viewing the network matrix 
# correct by adding a zero at the end of list of closeness scores:
c((data.frame(closeness_w(T6aut_net, gconly=F))$closeness), "0")

# In tnet can adjust the weight given to edge number and edge weight when calculating closeness using a tuning parameter alpha, default=1
# If I set alpha = 0-1, a high closeness is considered favourable (so edge weight and number are both positive - larger = larger)
# If I set alpha > 1, a low closeness is favourable (so edge weight is positive and edge number is negative)

# NOTE: tnet won't calculate a closeness score for the last fox in a matrix (in last col + last row)
# If its column contains only zeros - because it never associated. 
T6aut_net # check by viewing the network matrix 
# correct by adding a zero at the end of list of closeness scores:
c((data.frame(closeness_w(T6aut_net, gconly=F))$closeness), "0")


# CC in tnet = 'generalised CC' - weighted. See # https://toreopsahl.com/tnet/weighted-networks/clustering/
# measure="gm" means use the geometric mean (rather than default arithmetic mean), which is more sensitive to differences in edge weights (rather than simply averaging across the edge weights which would cause bias from extreme values) )
T1winB4_attr$cc <- data.frame(clustering_local_w(T1winB4_net, measure="gm"))$gm # uses Barratt 2004 method
a<-data.frame(T1winB4_attr$ShortCode, T1winB4_attr$cc)

#====#

library(keyplayer)
# fragmentation: nodes to target to break up the network e.g. break up a terrorist network
# mreach.closeness: nodes to target to disseminate information rapidly

# To use keyplayer need to take inverse of edge weights first, so strong ties are interpreted as low geodesic distances rather than high ones:
# Define the adjacency matrix
net <- T1spr_net # network where edge values represent tie strength
a <- T1spr_net   # make a copy

# Transform the edge value to distance interpretaion
a[net !=0] <- 1/net[net !=0] # take inverse of non-zero ties to make shortest path correspond to strongest friendship

# Calculate mreach.closeness
mrc <- mreach.closeness(a, cmode="indegree") # indegree or else will sum in and outdegree and double the score (no way to specify that network is undirected)
mrc1sp <- data.frame(id=rownames(net), mrc) # view results

#==========================================#
# MODELLING NETWORK CENTRALITY MEASURES ####
#==========================================# 
# PROCEDURE
# 1. Make sampling period arrays
# 2. Make networks (association matrices)
# 3. Calculate centrality measures from real networks
# 4. Fit models to real centrality measures
# 5. Randomise networks 1000 times and store as a stack of 1000*N*N association matrices
# 6. Calculate centrality measures for foxes in each random network (use g=c(1:1000) to specify input as a matrix stack in package sna, or create loop for other packages to specify one network at a time in the stack)
# 7. Run a loop to run the model on the matrices of random centrality measures: Node scores are stored in columns (one per random network) so loop through each column sequentially as the model input
## a) create input_data using rbind to combine node centrality scores from the first random network for each territory/season into a single data frame. Add columns for territory and season to label data (to use for fixed effects in model) # IDs are in ascending order
## b) run the model with the input_data as the data
## c) save the specified model coefficient (specify using col:row coordinates) - to save multiple coefficients save them in separate predefined matrices
## d) plot the distribution of model coefficient estimates from random data (as histogram) and add line for the coefficiet based on the real data
## e) calculate p-value as proportion of coefficients from random matrices that were greater or less than the observed coefficient.


# Example of plotting differences and correlations in centrality measures 
T1sprB4_attr_CORE <-subset(T1sprB4_attr, Core==1) # Select core foxes only
par(mfrow=c(1,3))
boxplot(bindeg~Sex,data=na.omit(T1sprB4_attr_CORE), col=c("red","blue")) # na.omit excludes foxes of unknown sex
boxplot(strength~Sex,data=na.omit(T1sprB4_attr_CORE), col=c("red","blue"))
plot(bindeg~strength,data=na.omit(T1sprB4_attr_CORE), col=c("red","blue")[Sex])
legend("bottomright",c("Female","Male"),col=c("red","blue"),pch=1)

lattice::xyplot(data$meancc ~ data$fSeason|data$Sex)

#==================#
# Network analysis ####
#==================#

# Calculate mean edge weight in package igraph
T1sprAf_net_graph <- igraph::graph.adjacency(T1sprAf_net, weighted=T, diag=F, mode="undirected")
mean(E(T1sprAf_net_graph)$weight) # mean edge weight

#== Calculate network density (weighted) using package sna
gden(T1sprB4_net, g=NULL, diag=FALSE, mode="graph", ignore.eval=FALSE) 

#==============================================#
# Normalisation, normalising, standardising ####
#==============================================#
# to normalize between 0-1 in Excel
=(B3-MIN($B$3:$B$17))/(MAX($B$3:$B$17)-MIN($B$3:$B$17))

# in R
mydata$xnorm <- (x-min(x))/(max(x)-min(x)) # this is a linear transformation so doesn't change shape of distribution & hence doesn't convert to a normal distrib


#======================#
# Normality testing ####
#======================#
shapiro.test(mydata$Group.size) # test of normality (low P = sig diff from normality)
shapiro.test(mydata$Number.of.transient.visitors)

# ANOVA is robust to deviations from normality



#============#
# Numbers ####
#============#

# Logical operators: < > = == >= != | &
& is “and”, | is “or”, and ! is “not”.

# De Morgan’s law
!x | !y # is the same as: 
!(x & y) 

!x & !y # is the same as:
!(x | y) 

# Floats
1/49 * 49 == 1 # returns FALSE despite being true, because the division returns a float that is not exactly 1.
# Better to use near() instead of == when working with floats
near(1/49 * 49, 1) # returns TRUE

# Making sequences of numbers
seq() # makes regular sequences of numbers 
seq(0,60) # increments of 1
seq(0,60,5) # increments of 5
seq(0,60, length.out=3) # automatic number of increments to make 3 evenly spaced numbers)



#======================#
# IDENTIFYING OUTLIERS ####
#======================#

#== List outliers via boxplot function:
box<-boxplot(EncounterRate_noNAs$NTrueAssocs) # make boxplot and save
box$out # print values of outliers

#== Function to identify % datapoints that are outliers by calculating z-scores

# function from companion website for Field 2012 DSUR book: https://studysites.uk.sagepub.com/dsur/study/scriptfi.htm
outlierSummary<-function(variable, digits = 2){
  zvariable<-(variable-mean(variable, na.rm = TRUE))/sd(variable, na.rm = TRUE)
  outlier95<-abs(zvariable) >= 1.96
  outlier99<-abs(zvariable) >= 2.58
  outlier999<-abs(zvariable) >= 3.29
  ncases<-length(na.omit(zvariable))
  percent95<-round(100*length(subset(outlier95, outlier95 == TRUE))/ncases, digits)
  percent99<-round(100*length(subset(outlier99, outlier99 == TRUE))/ncases, digits)
  percent999<-round(100*length(subset(outlier999, outlier999 == TRUE))/ncases, digits)
  cat("N absolute z-scores >1.96 =", percent95, "%", "    #Should be <=5% in a normal distrib", "\n")
  cat("N absolute z-scores >2.58 =", percent99, "%", "    #Should be <=1% in a normal distrib", "\n")
  cat("N absolute z-scores >3.29 =", percent999, "%", "   #Should be zero in a normal distrib", "\n","\n")
  cat("Percentage of cases that are significant outliers =", percent999, "%", "\n")
}
outlierSummary(dailyPRT$totalPRT) # if there are any >3SD from the mean these are significant 
# outliers and need dealing with, wither by removal or changing to another value (removal or 
# transformation of whole dataset is best)



#=== Function to replace outliers with NA or a specified value

# Takes a vector as input and returns a vector of equal length, but with outliers either detected or replaced.
# From https://rpubs.com/hauselin/outliersDetect
outliersZ <- function(data, zCutOff = 1.96, replace = NA, values = FALSE, digits = 2) {
  #compute standard deviation (sample version n = n [not n-1])
  stdev <- sqrt(sum((data - mean(data, na.rm = T))^2, na.rm = T) / sum(!is.na(data)))
  #compute absolute z values for each value
  absZ <- abs(data - mean(data, na.rm = T)) / stdev
  #subset data that has absZ greater than the zCutOff and replace them with replace
  #can also replace with other values (such as max/mean of data)
  data[absZ > zCutOff] <- replace 
  
  if (values == TRUE) {
    return(round(absZ, digits)) #if values == TRUE, return z score for each value
  } else {
    return(round(data, digits)) #otherwise, return values with outliers replaced
  }
}
# data: the VECTOR you're passing to the function
# zCutOff: the z value you deem as an outlier (default is 1.96 since 95% of values fall within ? 1.96 in a normal distribution)
# replace: replace any outliers with this value (default is NA)
# values: default is FALSE; if TRUE, returns the absolute z-scores for each value
# digits: number of decimal places to return 

# Create new variable with NAs for scores more than 3 standard deviations from the mean (removes fewer values than the default cutoff 1.96).
dailyPRT$totalPRT_z <- outliersZ(dailyPRT$totalPRT, zCutOff=3.0)

# Replace outliers with the mean plus 3*the standard deviation - following Field et al 2012 DSUR book, chapter 5
# not usually recommended, better to just omit them
dailyPRT$totalPRT_z <- outliersZ(dailyPRT$totalPRT, zCutOff=3.0, replace=mean(dailyPRT$totalPRT)+3*sd(dailyPRT$totalPRT))



#===========================================#
# Packages ####
#===========================================#

# view current working directory
getwd() 

# set working directory
setwd("G:/Statistics 2016/Social network analysis") # use forward slashes or double-backslashes
# not good practice to use absolute paths ("C:/") in scripts, especially for shared projects. See http://r4ds.had.co.nz/workflow-projects.html

# Better to use R projects
""" R Projects in RStudio keep all files associated with a project together — 
    input data, R scripts, analytical results, figures. """

# loading packages in Jupyter
.libPaths() # see where R is looking for libraries
.libPaths('C:/Users/User/Documents/R/win-library/3.4') # tell R where to look explicitly


# save current workspace in current working directory (so can pick up where you left off on re-opening R)
save.image() # saves it as ".Rdata"
save.image("myworkspace.Rdata") # be patient - R will crash if you cancel it!

# loads the saved workspace with objects and any modifications to data frames, variable names etc
load("myworkspace.RData") 

# clear and restart the R session (to ensure code is reproducible)
Ctrl+Shift+Fn+F10
# or:
rs.restartR() 

# cite R itself
citation()

# cite packages
citation("qgraph")

packageVersion("asnipe")

sessionInfo() # view loaded packages, versions etc

# detach package
detach("package:lme4")

# check for package updates and optionally install them
packagename_update()

# see the order in which R looks for attached packages/enviros 
""" (it will use the first one it finds)
    because having multiple packages loaded at once can cause conflicts if they 
    contain functions with the same name """
search() 



#======================#
#      Pipes  %>%   ####
#======================#

# http://r4ds.had.co.nz/pipes.html
# read them like 'and then...'

# shortcut for pipes
Ctrl + Shift + M

library(tidyverse)  # uses pipes

# Get count per level of a factor variable
df %>% count(factor)
# gives the counts in a new df that you can save and use in more computations.

# Compute daily consumption per household  
daily_kwh_per_household <- mydata %>%  
  # use households on standard tariff only 
  filter(stdorToU=="Std") %>%
  # rename variables for ease of reference
  rename(kwh = 'KWH/hh (per half hour)') %>%
  # extract date from datetime
  mutate(
    dt = as.POSIXct(paste(DateTime)),
    day = as.Date(strftime(dt, format = "%D"), "%m/%d/%y")) %>%
  # compute total daily consumption for each household
  group_by(year, day, LCLid) %>%
  summarise(total_kwh = sum(kwh))



#===========================================#
#     PLOTTING ####
#===========================================#
# see http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/
# see also ggplot2 section

# turn off the graphics device
dev.off()

# basic plot() template: 
plot(x, y, main=heading) 

pairs(mydata) #to view possible correlations and interactions

# change text / font size in plots
cex.before <- par("cex") # par() gives the current settings in the R environment
par(cex = 1) # save the text size as a bit bigger by default
cex=1 # default size. Increase or decrease to alter size for all text in plot (sometimes doesnt work so have to specify separately as follows:)
cex.main=1.2 # plot title
cex.lab=1.2 # axis labels
cex.axis=1.2 # axis text

#== customise plot MARGINS
par()$mar # retrieve current margin settings
# mar = a vector of bottom/left/top/right for no. lines of margin on each side of plot. 
# Default is c(1,3,5.1,2)
par(mar=c(5.1,4.1,4.1,2.1)) # default R margins - bottom/left/top/right
par(mar=c(6,4,5,1)) # new ones  # mar = in number of lines
par(mai=c(1.02,0.82,0.82,0.42)) # mai =in inches

#== Custom axis labels with position specified by coordinates
# Plot 4*1 panel
par(mfrow=c(4,1))
par(mar = c(3.5, 1.25, 1.5, 1.5), oma = c(2, 4, 1, 1)) # mar = margins, oma = outer margins.
densityPlot(springdata, cex.main=1.2, cex.lab=1.2, cex.axis=1.2, xlab="", ylab="", xcenter = "midnight", rug=T, col="forestgreen", main="Spring")
densityPlot(sumdata, cex.main=1.2, cex.lab=1.2, cex.axis=1.2, xlab="", ylab="", xcenter = "midnight", rug=T, add=F, col="darkorange", main="Summer", cex=1.5)
densityPlot(autdata, cex.main=1.2, cex.lab=1.2, cex.axis=1.2, xlab="", ylab="", xcenter = "midnight", rug=T, add=F, col="red", main="Autumn", cex=1.5)
densityPlot(wintdata, cex.main=1.2, cex.lab=1.2, cex.axis=1.2, xlab="", ylab="", xcenter = "midnight", rug=T, add=F, col="blue", main="Winter", cex=1.5)
# add exis labels
mtext("Density", side = 2, outer = TRUE, cex = 1, line = 2, col = "black")
mtext("Time of day", side = 1, outer = TRUE, cex = 1., line = 0, col = "black") 


# make text italic or bold in labels
xlab=expression(italic("N")~"camera sites")
xlab=expression(bold("N")~"camera sites")
xlab=expression(bolditalic("N")~italic("camera sites"))

#================#
# Multi-panel plotting:
par(mfrow=c(2,1))
layout()
split.screen()

#================#
# colours, symbols and line types
http://www.statmethods.net/advgraphs/parameters.html

#===============#
# Add model equations to plots
plot(summary_attribs$DaysSeen, summary_attribs$NTrueAssocs, xlab="Number of days observed", ylab="Number of true associations")
coef<-fixef(REDUCED.nb2)
abline(coef) # add line based on model REDUCED.nb2 # only works for linear models: lm(y~x)

# Write equation to add to plot:
rounded_coef <-round(coef, 4)  # rounded to 4 dp
eq <- paste0("Number of true associations = ", rounded_coef[1],
             ifelse(sign(rounded_coef[2])==1, " + ", " - "), abs(rounded_coef[2]), "*days seen")
mtext(eq, 3, line=-2) # add equation to plot at the specified coordinates

## Density plots
plot(density(mydata$value))

# Density plot with vertical line at highest density and get the x value
sp.d <- density(sp$ti.threshold.s)
plot(sp.d, main = "Spring") 
abline(v=sp.d$x[which.max(sp.d$y)]) 
sp.v <- sp.d$x[which.max(sp.d$y)] # x value at highest density (= the mode?)

#=== Export plots in high resolution for publications:
# saves most recently drawn plot as the following file name in the current working directory
#dev.print(jpeg, "Rplot.jpeg", res=700, height=6, width=8, units="in") # save as jpeg
#dev.print(tiff, "Rplot.tif", res=300, height=100, width=150, units="mm") # save as TIFF
#dev.print(pdf, "RPlot.pdf") # Save as PDF
# ggsave("diamonds.pdf") # Save as PDF via ggplot



#=== To save ggplot2 plots as emf file types for circus report graphic designer
library(devEMF) # Though I'm not sure if this library was actually needed or whether ggsave is just in the ggplot2 library
# First save the desired plot with formatting as an object called 'plot'
plot<-myplot.beh + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                         panel.background = element_blank(), 
                         axis.line = element_line(colour = "black")) #to make axes darker
# Then save the plot as an emf file:
ggsave("behav.emf", plot=plot, device=emf, path = NULL, scale = 1, width = 10, height = 6, units = 'in', dpi = 1000, limitsize = TRUE)
# Should be able to open emf files in paint to check it saved OK but I cant on my lenovo. G.designer could 
# open the emfs though so this code does seem to work.

#=================#
# Plot regression estimates
library(coefplot2) 
install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source") # not on CRAN & might need to install 'reshape' and 'Rcpp' for the install to work!


### use sufficiently large upper margin to make sure codes all shown in the Tukey's plot (cld)
par(mar=c(5.1,4.1,4.1,2.1)) # default R margins - bottom/left/top/right
par(mar=c(6,4,5,1)) # new ones  # mar = in number of lines
par(mai=c(1.02,0.82,0.82,0.42)) # mai =in inches

# plot model estimates using the coefplot2 package:

coefplot2(model1, intercept=T, vertical=T) # default omits intercept and is horizontal
coefplot2(model1, intercept=T, add=TRUE) # add=true to compare models on same plot

coefplot2(lme.SO.gsmod1, main= "Regression estimates\n\n",    # plot regression estimates
          cex.var=1.1, cex.pts=1, varnames=longnames,
          intercept=TRUE, mar=c(1,6,5,1))   

# To customise the names of the factors:
snames<-c("Summer", "Autumn", "Winter") # create vector for season names (or factor names)
longnames <- c("(Intercept)", snames) # specify the intercept
# Then re-run the coefplot2 code above.

# Plot and compare all models:
coefplot2(list(glmer.P=defcomp.mod1,
               glmmPQL_P=mp1P,
               glmmPQL_Q=mp1Q,
               glmmADMB_P=fit_poiss,
               glmmADMB_NB1=fit_nb1, # nb1 = quasipoisson distrib (overdispersed poisson)
               glmmADMB_NB2=fit_nb), # nb2 = neg binomial distrib
          merge.names=FALSE,intercept=TRUE,
          legend.x="right",legend=TRUE) 


# alternative plotting tool for regression estimates
library(sjPlot)
sjp.setTheme("forestgrey") # plot theme
sjp.lmer(fvmod_final, type = "fe")


# Can also plot models using cld in the multcomp package:
library(multcomp)
# make your model, then save the Tukeys test results as an object (e.g. 'a')
a<-cld(summary(glht(fvmod_final, linfct=mcp(fSeason="Tukey")))) # save Tukeys results
a # To show which comparisons were signif. diff: if all letters shown then all levels were unique


plot(a, main="Tukey's test\n\n\n\n\n",  xaxt='n', xlab="")  # plot without axes intially
xtick<-seq(1, 4, by=1)                                      # specify where to put tick marks
seasonlabels<-c("Spring", "Summer", "Autumn", "Winter")     # list of labels for the x-axis
axis(side=1, at=xtick, labels = seasonlabels)               # draw the x axis and add labels
# letters at top of graph show similar (grouped) and different levels, 
# i.e. two levels labelled 'a' were not sig. diff. from each 
# other but were diff from 'b' or 'c'

###==== Plot points as shapes in ggplot
geom_point(aes(size=1, shape=season)) # specify to use different shapes for different seasons
scale_shape_manual=c(1,2,3,4,6,12) # to specify shape type for different levels of the factor





#=================#
# POST HOC TESTS ####
#=================#
# REPORTING TUKEY TESTS: z-value and p-value only. (Emily Bennitt thesis)
# "Pairwise Tukey tests revealed that summer groups were larger than spring (z = -0.56, p = 0.045)"

# Adjust p-values for multiple comparisons - though most packages do this automatically
p.adjust(p, method = p.adjust.methods, n = length(p))


library(multcomp) # for glht method: just for models with no interaction (or interaction as a single column of combined variables, e.g. "seasXorigin")

library(lsmeans) # for lsmeans method
# Least squares mean: mean adjusted to account for differences in other factors e.g. mean orange sales per day adjusted to account
# for daily variation in orange price. More robust than a normal mean.
# for lmer models lsmeans uses the pbkrtest package, which implements the Kenward & Rogers method 
# for the df of the "t" statistic - this method intends to provide better p-values and CIs than the asymptotic one 
# (but there's no diff between the asymptotic (asymp.LCL/asymp.UCL) and Kenward (lower.CL/upper.CL) methods when df is large).

library(phia) 
# PHIA = 'POST HOC INTERACTION ANALYSIS' to get means for interactions
# Phia is for the analysis of the expected values and other terms of in linear, 
# generalized, and mixed linear models, on the basis of multiple comparisons of 
# factor contrasts. Specially suited for the analysis of interaction effect
# testFactors: flexible user interface for defining combinations of factor levels and covariates
# to evaluate and test the model, using the function linearHypothesis from package car. 
# testInteractions uses this function for multiple comparisons of simple effects, interaction
# residuals, interaction contrasts, or user-defined contrasts. 
# interactionMeans may be used to explore the 'cell means' of factorial designs, and plot main 
# effects or first-order interactions.
# phia automatically back-transforms effects if the model was on the log scale (e.g. log-link GLMMs)

# Post hocs for anovas
TukeyHSD(anova.model.name)


####  Post hoc tests for significant interactions:

# ##To use glht method, need to make a column in dataset for the interaction and rerun the model with this column as predictor (fixed effect):
origins$oriseas.int<-interaction(origins$season,origins$Origin) 

# rerun model on interaction column - otherwise posthoc tests wont work (if using glht)
lme.orimod4<-lme(Count ~ oriseas.int, random= ~1|oriT/season, data=origins, method="ML") 

# Actually model is different from the season*Origin one when use a column for the interaction...
# posthoc tukeys test for all possible pairwise comparisons using glht function
lme.orimod.posthocs<-glht(lme.orimod4,linfct=mcp(oriseas.int = "Tukey")) 
summary(lme.orimod.posthocs) # lots of comparisons so correct for multiple testing:
summary(lme.orimod.posthocs, test = adjusted("bonferroni")) # no more errors now (though P values are lower...!)
confint(lme.orimod.posthocs, correction = ("bonferroni")) # to show confidence intervals: if they contain zero the mean could be zero and effect is non. sig.

# To plot: lsmeans
library(multcomp)
a<-cld(lsmeans::lsmeans(defcomp.mod.defseas, pairwise~season|Definition, adjust="tukey", type="response")) # save Tukeys results
plot(a) # plotting cld labels the groups with letters to indicate whether sig. diff or not

### OR use 'lsmeans' for post hoc tests on the original interaction model:
# Use vertical line in lsmeans to do pairwise comparisons of the seasons, within each origin
lsmeans(lme.orimod3, pairwise~seasfac|Origin, adjust="tukey") # Tukey is OK if have approx balanced design and many comparisons - has built in correction for multiple testing
lsmeans(lme.orimod3, pairwise~Origin|seasfac, adjust="tukey")
lsmeans(lme.orimod3, ~Origin|seasfac, adjust="tukey") # doesnt do contrasts if 'pairwise' is not specified in the code
# df are displayed as NA in lsmeans- this is simply lsmeans's way of noting that the tests and confidence intervals are asymptotic, based on zz statistics rather than tt statistics.
lsmip(glmm3, Origin~season) # basic interaction plot of model - drawn using the lsmeans package
lsmip(glmm3, Origin~season, type="response") # add type=response to backtransform if used GLMM with 
# log-link or used log10(y) - need to do manual transformations within the model for lsmeans to 
# recognise it.

# TO GET MEANS FOR EACH LEVEL in the UNITS OF THE RESPONSE: use lsmeans or phia package (for interactions)
# i.e. individuals, not effects/coefficients as would go in the model.
# To back-transform mean counts of non-residents
lsmeans(glmm3, ~ season * Origin) # on scale that model used (i.e. log if used a log-link)
lsmeans(glmm3, ~ season | Origin) # use vertical line separator to display in clearer table

summary(lsmeans(glmm3, ~ season | Origin), type = "response") # to backtransform if model transformed data # cant use type=response for pairwise~
# 'rate' = least sq mean backtransformed, equal to 'adjusted mean' from package phia's command
# InteractionsMeans which automatically backtransforms the values, (but SEs are not same in phia and lsmeans)

# Save lsmeans and CIs (on response scale) from a 3-way interaction in a table
model_lsm <- data.frame(summary(lsmeans::lsmeans(fullnestmod_ziNB1, ~ c(Sex*SocialStatus)|fSeason), type="response"))

# combine diff post hoc tests into same data frame  and correct for multiple testing
a <- pairs(lsmeans::lsmeans(erateMOD_FINAL, ~ fSeason|c(Sex,SocialStatus))) # compare rates between seasons for each sexstat
b <- pairs(lsmeans::lsmeans(erateMOD_FINAL, ~ c(Sex,SocialStatus)|fSeason)) # compare rates between sexstat for each season
lsm_tukey <- test(rbind(a,b), type="response", adjust="tukey")



#=============================#
# MODEL PREDICTIONS ####
#=============================#
# http://glmm.wikidot.com/faq # for code to predict from lme4 and glmmADMB models
# http://www.r-bloggers.com/confidence-intervals-for-prediction-in-glmms/ # for comparison between CIs calc from predict() and bootstrapping
# Usually recommended method for predictions is using bootstrapping, but takes too long for interactions
# And i can never get the code to work. So use the code below or LSMEANS, which are predictions themselves (get same values from both predict() and lsmeans().

# To calculate SEs from GLMMs have to backtransform predictions after adding/removing the SE
# So make predictions on the link scale initially:
# Can't use predict() to get CIs, need to make model matrix manually (see "RCode_SNA_Daily patch encounter rate.R")

# calculate and store predicted values (MuMIn)
all3defs$defcomp.pred <- predict(defcomp.mod1, type="response", se.fit=TRUE) # se.fit will not work for glmer objects (predict.merMod function does not compute SEs)

## order by season, def and group size
all3defs <- all3defs[with(all3defs, order(season)), ]

## order by ID
mydata <- datacsv[with(mydata, order(AnimalID)), ] 

## Plot predicted group sizes by each method in each season
ggplot(defcomp.mod1, aes(x = season, y =defcomp.pred, colour = Definition)) +
  geom_boxplot() +   labs(x = "Season", y = "Expected group size")


## Predictions using visreg
# Plotting predictions using visreg (combi core model, sex*season) #### 

### To save partial residuals from visreg:
library(visreg)
v <- visreg(dailyPRTmod_comb_FULL, "fSeason", by="Sex", scale="response", partial=T) 
# NOT SURE THIS CODE WORKS - data frame contains lots of rows of the same combinations of 
# fixefs and ranefs but different predictions...?!

plot(v) # blue lines are preds, grey shading is CI, dots are raw resids/raw data? (doesnt seem to backtransform...)
model.fit <- v$fit # save dataframe of 8 predictions (one per sex*season combo) - same as lsmeans predictions
model.fit$visregFit # = column of predicted values
model.fit$visregUpr # = column of upper CI
model.fit$visregLwr # = column of lower CI
# lineplot of the 8 sex*season preds in v$fit (have to backtransform from log10 using 10^ as "response" doesnt seem to work...)
ggplot(model.fit, aes(x=fSeason, y=10^(visregFit), colour=factor(Sex))) + 
  geom_point(aes(group=Sex)) +
  geom_line(aes(group=Sex)) +
  geom_errorbar(aes(ymin=10^(visregLwr), ymax=10^(visregUpr)))
# To save preds for each data row (rather than just one per combo of fixed effects)
v <- visreg(dailyPRTmod_comb_FULL, "fSeason", by="Sex", scale="response", partial=T)
model.res <- v$res
model.res$visregRes # preds
# boxplot visreg preds in v$res (have to backtransform from log10 using 10^ as "response" doesnt seem to work...)
ggplot(model.res, aes(x=fSeason, y=10^(visregRes), fill=factor(Sex))) + 
  geom_boxplot() 



# Predictions from model built in glmmADMB - dont need to specify random effects in pred.data (but do in lme4)
#=============================================#
# 1. Create a 'newdata' object containing values of DaysSeen to predict contact rate for foxes at increasing numbers of days seen
# expand.grid makes data frame with all combinations of the listed values: 
# (has to have all model coefficients in, with same names, e.g. 'DaysFedPW_rounded', not shortened to 'DaysFed')
pred.data <- expand.grid(DaysSeen=seq(0,40,1), # 0,40,2 = make sequence from 0-40 at intervals of 2
                         SexStatus=mean(EncounterRate_noNAs$SexStatus),
                         fSeason=mean(EncounterRate_noNAs$fSeason),
                         DaysFedPW_rounded=mean(EncounterRate_noNAs$DaysFedPW_rounded))

# 2. Make predictions for each number of DaysSeen specified in object 'pdat'
# with 95% confidence intervals (think this code only works for glmmADMB models, not lmer or glmer...)
pred <- predict(sexstatfullnestmod_ziNB1, newdata=pred.data, na.rm=T, type="response",  # need "response" for GLMM or get negative values!
                interval = "confidence", level = 0.95) 
# OR with 95% prediction intervals (usually wider than CIs) (ditto above for model types)
pred <- predict(sexstatfullnestmod_ziNB1, newdata=pred.data, na.rm=T, type="response",  interval = "prediction", level = 0.95)

# SEs (se.fit=TRUE) can only be calculated on the link scale (i.e. type="link") so must use confidence intervals instead (default level=0.95)
#***NB confidence intervals do not incorporate uncertainty due to (and of) random effects


# 3. Save predictions for each level of DaysSeen in a data frame
predframe <- data.frame(pred.data, pred)  # = atable that contains 'fit' (=predicted N true assocs) and lwr/upr 95% CIs


# 4a. Either PLOT PREDICTIONS OVER ORIGINAL DATA:
plot(summary_attribs$DaysSeen, summary_attribs$NTrueAssocs, cex.lab=1.2, xlab="Number of days observed", ylab="Observed true associations")
lines(predframe$fit~predframe$DaysSeen, col="red", lwd=2, lty=1)
lines(predframe$lwr~predframe$DaysSeen, col="black", lwd=1, lty=2)
lines(predframe$upr~predframe$DaysSeen, col="black", lwd=1, lty=2)

# 4b. Or PLOT JUST THE PREDICTED VALUES WITHOUT OBSERVED DATA: FROM glmmADMB.predict helpfile by Ben Bolker
# Code doesn't have 'pdat' so predictions are made for all data points (i.e. 415 observations)
allpred <- predict(REDUCED.nb2,interval="confidence",type="response") 
head(allpred) # 415 rows, so one per observation
allpredframe <- data.frame(obs=summary_attribs$DaysSeen,pred) # names DaysSeen as "observation number"
with(allpredframe,plot(obs,fit, cex.lab=1.2, xlab="Number of days observed", ylab="Predicted true associations")) # Days seen on X and predicted N true assocs on Y
with(allpredframe,arrows(obs,lwr,obs,upr,length=0)) # add error bars from 95% CIs - plotted as arrows without arrowheads (as length=0)
lines(predframe$fit~predframe$DaysSeen, col="red", lwd=2, lty=1) # line from previous graph

# example plotting predictions with ggplot2
library(ggplot2)
predframe$Season <- predframe$fSeason # add variables to table of predictions
predframe$Season <- ordered(predframe$fSeason, levels = c(1,2,3,4), # rename seasons 1-4 as spring-winter
                            labels=c("Spring", "Summer", "Autumn", "Winter"))
EncounterRate_noNAs$Season <- ordered(EncounterRate_noNAs$fSeason, levels = c(1,2,3,4), # same in original data table so they match
                                      labels=c("Spring", "Summer", "Autumn", "Winter"))

ggplot() + geom_point(data=EncounterRate_noNAs, aes(DaysSeen, NTrueAssocs, colour=factor(SexStatus)), 
                      position=position_jitter()) + 
  facet_wrap(~Season) + 
  scale_y_continuous(limits=c(0,5)) +
  geom_line(data=predframe, size=1, aes(DaysSeen, fit, colour=factor(SexStatus))) +
  labs(x = "Days seen", y = "Mean encounters per patch per day\n") + # axis labels
  geom_errorbar(data=predframe, mapping=aes(x=DaysSeen, ymin=lwr, ymax=upr), 
                width=0.2, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") + theme(legend.title=element_blank())+ # remove legend title
  scale_colour_manual(values = c("red", "blue", "green", "black"))

# OR plot original data as scatter plot with prediction  lines on top
plot(EncounterRate_noNAs$DaysSeen, EncounterRate_noNAs$NTrueAssocs, ylim=c(0,20), cex.lab=1.2, xlab=" ", ylab="Observed true associations")
plot(pred$cRate~pred$DaysSeen, col="red", lwd=2, lty=1)  # red line for predictions (here called CRate, could be 'fit')
lines(pred$lwr~pred$DaysSeen, col="black", lwd=1, lty=2) # dotted lines for CIs
lines(pred$upr~pred$DaysSeen, col="black", lwd=1, lty=2)


# Predictions from lme4 models - SPECIFY ALL MODEL PARAMETERS
?predict.merMod
pred.data <- expand.grid(DaysSeen=seq(0,40,1), # 0,40,2 = make sequence from 0-40 at intervals of 2
                         SexStatus=mean(EncounterRate_noNAs$SexStatus),
                         fSeason=mean(EncounterRate_noNAs$fSeason),
                         DaysFedPW_rounded=mean(EncounterRate_noNAs$DaysFedPW_rounded),
                         ShortCode=unique(EncounterRate_noNAs$ShortCode), # MUST SPECIFY RANDOM EFFECTS TO PREDICT FROM LMER AND GLMER MODELS
                         Place=unique(EncounterRate_noNAs$Place))
# Cant specify to compute confidence intervals for lme4 models
pred <- predict(sexstatfullnestmod_ziNB1, newdata=pred.data, na.action=na.pass, type="response", re.form=NULL) 
# na.pass (default na action) returns the object unchanged
# re.form=NULL means include all random effects. Otherwise set to re.form=NA or re.form=~0 to exclude all random effects,
# or re.form=~(1|ShortCode) to explicitly specify random effects



#== CALCULATE CIs & PREDICTION INTERVALS USING A PREDICT()-LIKE METHOD using code from: http://glmm.wikidot.com/faq
#==================================================================================================================#
# 1. Make data frame to calc predictions from: include levels of all FIXED EFFECTS included in the model (NO RANDOM EFFECTS)
pred.data <- expand.grid(DaysSeen=c(10,20,30,40),
                         Sex=unique(EncounterRate_noNAs$Sex),
                         SocialStatus=unique(EncounterRate_noNAs$SocialStatus),
                         fSeason=unique(EncounterRate_noNAs$fSeason),
                         DaysFedPW_rounded=mean(EncounterRate_noNAs$DaysFedPW_rounded))

# 2. Get model matrix: write fixed effects in same way as model formula. Have to include all variables in pred.data
modelmatrix <-model.matrix(~DaysSeen+Sex*SocialStatus*fSeason+DaysFedPW_rounded, pred.data) 
head(modelmatrix) # check all columns contain data

# 3. Calculate predictions (on link scale, so same values as get via predict(reateMOD_full, newdata=pred.data, re.form=NA, type="link")
y <-modelmatrix%*%fixef(erateMOD_full) 

# 4. Save variance due to fixed effects (i think)
pvar1 <- diag(modelmatrix %*% tcrossprod(vcov(erateMOD_full),modelmatrix))

# 5. Add variance due to random effects
tvar1 <- pvar1 + VarCorr(erateMOD_full)$ShortCode[1] + VarCorr(erateMOD_full)$Place[1] # Specifies to include intercepts (variance) from each random effect. [1] specifies to use the intercept value

# 6. Calc confidence intervals, backtransform and combine all in a data frame:
newdata <-data.frame(
  DaysSeen=pred.data$DaysSeen,
  Sex=pred.data$Sex,
  SocialStatus=pred.data$SocialStatus,
  fSeason=pred.data$fSeason,
  DaysFedPW_rounded=pred.data$DaysFedPW_rounded,
  y=exp(y),
  plo = exp(y-1.96*sqrt(pvar1))  # predict-function-derived confidence interval lower
  , phi = exp(y+1.96*sqrt(pvar1))  # predict-function-derived confidence interval upper
  , tlo = exp(y-1.96*sqrt(tvar1))  # predict-function-derived prediction interval lower
  , thi = exp(y+1.96*sqrt(tvar1))) # predict-function-derived prediction interval lower
head(newdata)
predsWithCIs <- newdata # save as new df incase overwrite



#== CALC BOOTSTRAPPED CIs USING BOOTMER FUNCTION IN LME4 (the 'gold standard' for preds, but never works on my models: think it assumes normal distributions so maybe only useful for gaussian LMMs)
#===========================#
# lme4::bootMer() quickly becomes time prohibitive because it involves re-estimating the model for each simulation
# for my model: glmer(sex*status*season + DaysSeen + DaysFed + (1|ShortCode) + (1|Place), family=Poisson)
# it didnt finish after 3.5 hours for nsim=100
# for nsim=10 it took...

# 1. Make data frame to calc predictions from: include levels of all FIXED EFFECTS included in the model (NO RANDOM EFFECTS)
pred.data <- expand.grid(DaysSeen=c(10,20,30,40),
                         Sex=unique(EncounterRate_noNAs$Sex),
                         SocialStatus=unique(EncounterRate_noNAs$SocialStatus),
                         fSeason=unique(EncounterRate_noNAs$fSeason),
                         DaysFedPW_rounded=mean(EncounterRate_noNAs$DaysFedPW_rounded))

# 2. Get model matrix: write fixed effects in same way as model formula. Have to include all variables in pred.data
modelmatrix <-model.matrix(~DaysSeen+Sex*SocialStatus*fSeason+DaysFedPW_rounded, pred.data) 

# 3. Define a function that will be applied to the nsim simulations: means 'get a merMod object and return the fitted values' nsim times
predFun<-function(.) exp(modelmatrix%*%fixef(.)) # (.) means 'refer to earlier named object'

# 4. Perform bootstrap estimation (takes ages as have lots of model parameters) - use system.time to time the process
PI.time <- system.time(boots <- bootMer(erateMOD_full,FUN=predFun,nsim=10)) # running 10 simulations (bootstraps)

# 5. Calculate CIs and save them (in the response scale?) in pred.data dataframe
bb_se <- apply(boots$t,2,function(x) x[order(x)])
pred.data$bootlwr <- bb_se[1,] # bootstrap lower CI
pred.data$bootupr <- bb_se[2,] # bootstrap upper CI

# 6. DONT DELETE THE ESTIMATES!! Save as a new dataframe before accidentally overwrite all the work!
predsWithBOOTSTRAPcis <- pred.data  



#=== Predictions using lsmeans: one line of code to obtain same predictions and slightly wider CIs (as think lsmeans accounts for random effect variation) 
# lsmeans CIs are reliable - they're comparable to effects package and other methods of getting CIs: see http://bit.ly/2aiXpim
# automatically backtransforms from log10, log, sqrt etc when use type="response"
library(lsmeans)
lsm_preds <- lsmeans::lsmeans(finalmodel, "MeanMJperDay", # when have continuous variable need to specify a few key values to predict values for, or it'll just use the mean
                              by = c("Core", "PatchDaysFedPW"), # use by= to add more variables
                              at = list(MeanMJperDay = seq(0,6,0.5),  # to specify values to predict response for
                                        Core=c("0","1"), 
                                        PatchDaysFedPW = seq(1,7,2)), 
                              type="response")
# use by= and at= to specify the reference grid (like pred.data or newdata)
lsmpreds <- data.frame(summary(lsm_preds))


#===================================#
### PRINCIPAL COMPONENTS ANALYSIS ####
#===================================#
# Following Rob Thomas stats book p.119: to combine >1 variable into a single variable

# combine DF & MJ into 1 variable for first patch selection model
pr1 <- prcomp(~DaysFedPerWeek + MeankJperDay, data=patchdata$, scale=T) 
# scale=T rescales each variable to have mean=0 and variance=1, in case the variables have v. dissimilar variances

names(pr1) # see what info is contained in the new object

# view % total variance captured by each component
summary(pr1) # report this (I think)

# plot the PCs
plot(pr1, type="lines")

# view eigenvalues of each PC - if eig<1 then PC is pretty useless 
pr1$sdev^2

# plot PC1 vs PC2 scores for each observation (takes a while...)
biplot(pr1, cex=0.5)

# Save PC1 scores as a new variable
patchdata$PC1 <- predict(pr1)[,1] 
summary(patchdata$PC1)

#=================================================#
#### Progress bar for model fitting / running loops ####
#=================================================#
perm <- 1000 # set number of repetitions
pb <- txtProgressBar(0,perm,0, style=3)
setTxtProgressBar(pb, i)



#~~~~~~~~~~~~~~~~~~~~~~~~
#### Proportion data ####
#~~~~~~~~~~~~~~~~~~~~~~~~

# Model proportion data using binomial GLMM but include the weights= argument:
# weights= is the number of trials used to generate each proportion, e.g. here I 
# used the min of DaysSeen (for sighting history) and DaysActive (for cameras)
FPmodDF_FULL <- glmer(propdaysfirst ~ DaysFedPerWeek +(1|fTerritory) + (1|ShortCode), 
                      data=NDFP, family=binomial, weights=minDSDA) 



#~~~~~~~~~~~~~~~~
#### RANKING ####
#~~~~~~~~~~~~~~~~

# Rank within groups
#~~~~~~~~~~~~~~
# Rank patches by food availability (PC1adj) in each territory and season, where 1=highest
patchdata$PC1rank <- ave(patchdata$PC1adj, interaction(patchdata$Territory,patchdata$SeasonID), FUN=rank) 


# Ranking rows
#~~~~~~~~~~~~~~
library(dplyr) 
a<- attribs.NC %>%                    # data frame
  group_by(ShortCode, SeasonID) %>%   # groups within which to rank the values - here want to rank within seasons for each fox
  mutate(rank = rank(-Nrecs,
                     ties.method="average"))     # Values to rank: use "-Nrecs" to rank highest number as rank=1, or "Nrecs" to rank lowest number as rank=1
a                                     # View first few rows of the table
print.data.frame(a)                   # View the whole table

ties.method=c("First", "max", "min", "average") # optional argument to define how to rank tied values (those that are similar)


### REARRANGE DATA FRAMES ####
# stack rows into columns using gather in 'tidyr'
# syntax: gather(dataframe, "new col name1", "new col name2", which columns to stack into rows) - 1:4 means columns 1 to 4 - but stacks all columns by default.
fpreds <- gather(f, "Dist", "pred", 1:4) 

library(reshape) # http://rspatial.org/intr/rst/11-dataprep.html
# function 'reshape' reshapes a data frame between 'wide' format with repeated measurements in sep columns
# of the same record and 'long' format with the repeated measurements in sep records/rows

d <- data.frame(PatchSet1=1:6, PatchSet2=6:1, Fox="15", MJ=3:5, Patchtype=c("a", "b", "c"))
melt(d, "MJ")

newdat <- reshape(d, varying=c("PatchSet1", "PatchSet2"), v.names="newcolname", direction="long")
# varying=() specifies variables/columns to combine into one


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Relevelling a variable : change order of levels in a variable / change reference category ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
levels(orisum$Origin)
orisum$Origin <- factor(orisum$Origin,levels(orisum$Origin)[c(2,3,1)]) # reorder by position in sequence/list
levels(orisum$Origin)
# OR
levels(spatialoverlap$Season)
spatialoverlap$Season <- ordered(spatialoverlap$Season, levels = c("Spring", "Summer", "Autumn", "Winter")) # or specify order by name of each level
predframe$Season <- ordered(predframe$fSeason, levels = c(1,2,3,4),
                            labels=c("Spring", "Summer", "Autumn", "Winter")) # rename numeric as factor for ggplot2 facet labels
# NOTE dont change reference category for models using the 'ordered' command. INstead use relevel:
mydata$StatusRef <- relevel(mydata$SocialStatus, ref = 2) # ref=2 refers to order in list of levels. Can't use ref="Dom" as not recognised as a level.

# Rename factor levels true/false as 0/1 like with the variable Core, WITHOUT ORDERING
library(plyr)
PRTsexstatcore$fFeedingDay <- revalue(PRTsexstatcore$FeedingDay, c("TRUE"="1", "FALSE"="0"))
# change reference level to 1 instead of 0
PRTsexstatcore$fFeedingDay <- relevel(PRTsexstatcore$fFeedingDay, ref="1")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Regression coefficients - interpretation ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - In the summary(model) output table
# E.g. if coefficient for summer is 0.07, this is the expected difference in log count (for Poisson models) between
# summer and the reference season (spring by default)

# Get values of parameters and (Wald) SEs (crude estimates of p-values)
mod<-coef(summary(fit_poiss)) 

# get more accurate, profile C.Is
mod.confint <- confint(fit_poiss,method="Wald")

# lsmeans for means in units of measurement: (use CIs not SE and 'rate' = backtransformed mean)
lsmeans(defcomp.mod.defseas, pairwise~season|Definition, adjust="tukey", type="response")

# Use phia interactionsMeans for adjusted SE (but CIs are best - can get backtransformed ones from lsmeans)
# lsmeans is better than phia I think...



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Repeated measures data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using multilevel linear model (linear mixed model) to model repeated measures data
# ML models are linear models that consider dependency in the data - 
# Repeated measures are likely correlated, e.g. seasonal measures on same territory likely to be similar
# Normal linear regression assumes independence of data points: repeated measures violate this assumption
# So best to use multilevel model such as lme() or lmer() to control for non-independence between repeated measures

# ANOVA assumes independence of data points: I have repeated measures so must use multilevel models to control for 
# non-independence between seasons in same territory.
# If I were to use ANOVA it would be much more complex coding and I'd have to test/control for the assumption of 
# sphericity (see Andy Field book 'Discovering stats with R')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Repeatability analysis - calculate using intra-class correlation coefficient (ICC) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Method 1: Manually from NULL mixed model, variable to calc repeatability over as a random effect
# MUST FIT MODEL USING REML=TRUE
strength_mod_NULL <- lmer(strength ~ 1 + (1|ShortCode) + (1|Territory), data=data)
a<-data.frame(print(VarCorr(strength_mod_NULL), comp=c("Variance","Std.Dev.")))
# ICC = variance explained by shortcode (individual ID) 
a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance
# 0.1851544


### Method 2: Package rptR
# installation - as not on CRAN # NOT WORKING FOR LATEST VERSION OF R AND NOT WELL-MAINTAINED, PLUS NO DECENT VIGNETTE!
install.packages(devtools)
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("mastoffel/rptR")
require(rptR)


### Method 3: Package psychometric
library(psychometric)
require(multilevel)
# ICC1 =  amount of individual-level variance that can be "explained" by group membership
ICC1.CI(strength, ShortCode, centrality_core) # fits an ANOVA and calculates 95% CIs
# mixed model approach: more similar value to that calculated manually from lmer output & more appropriate for unbalanced data:
ICC1.lme(strength, ShortCode, centrality_core) # fits a mixed model in lme using package nlme
# To compute 95% CIs for the correlation coefficient (r) from ICC1.lme
CIr(r=0.1851545, n=51, level = 0.95) 
# not sure about this method to calc CIs! Seems to simple and wider interval than calc in package ICC.

### Method 4: Package ICC # BEST METHOD AS OF 03/07/16: proper CRAN package with vignette, and have seen it cited
library(ICC) 
# calculates ICC via an ANOVA - v.similar values to ICC.lme so use this as narrower CIs
ICCbare(ShortCode, strength, centrality_core) # just the ICC
s<-ICCest(ShortCode, strength, centrality_core) # ICC with CIs
# k = the number of measurements per individual or group
# N = n foxes
# varw = within individual or group variance
# vara = among individual or group variance
e<-ICCest(ShortCode, eig, centrality_core) 
c<-ICCest(ShortCode, cc, centrality_core) # removes NAs automatically
iccs<-rbind(s,e,c) # save in df
data.frame(s)$ICC
# need to compare to random for networks using randomisations. Can report mean ICC from randomisations instead of 95% CIs


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Robust SEs and p-values - package 'sandwich' ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate adjusted (robust) means and SEs
library(sandwich)
# Cameron and Trivedi (2009) recommended using robust standard errors for the parameter estimates 
# to control for mild violation of the distribution assumption that the variance equals the mean. 
# We use R package sandwich below to obtain the robust standard errors and calculated the p-values 
# accordingly. Together with the p-values, we have also calculated the 95% confidence interval 
# using the parameter estimates and their robust standard errors.
cov.glm1 <- vcovHC(glm1, type="HC0")
std.err <- sqrt(diag(cov.glm1))
r.est <- cbind(Estimate= coef(glm1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(glm1)/std.err), lower.tail=FALSE),
               LL = coef(glm1) - 1.96 * std.err,
               UL = coef(glm1) + 1.96 * std.err)
r.est # see robust means and SEs
summary(glm1) # see normal output



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Round values to nearest x number ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# most logical - rounds up and down
data.frame(round(object_name)) # rounds 0.5 UP to 1 and 0.49 DOWN to 0. 

# Round UP (dont like this either)
ceiling(variable) # rounds 0.5 UP to 1 and 0.49 UP to 1.

# Round DOWN (don't like this)
floor(variable) # rounds 0.999 DOWN to 0, but doesnt round whole numbers, e.g. shows '1' as '1'.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CALCULATE R-SQUARED FOR MIXED MODELS ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculare R^2 (r-squared) value
require(MuMIn)
r.squaredGLMM(fvmod_final)  # R2m = marginal r-squared (% variance in Y explained by fixefs only), R2c = conditional r-squared (fixeds and ranefs)
# fixed effects explain 15% of variance in probability; random and fixed collectively explain 62%



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RStan ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#= Installing RStan
# follow instructions on here VERY closely https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows#toolchain

# set up toolchain by running (copied from website above)
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXXFLAGS=-O3 -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)

# modify path in this section to match your local setup:
# cat('Sys.setenv(BINPREF = "<File path for ...mingw_32/bin from running <Sys.getenv('PATH')>")',
#     file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
#     sep = "\n", append = TRUE)

# i.e. on my Lenovo...
cat('Sys.setenv(BINPREF = "C:\\RBuildTools\\3.4\\bin")',
    file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
    sep = "\n", append = TRUE)

# turn off irrelevant verbose warnings
cat("\nCXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations", 
    file = M, sep = "\n", append = TRUE)

# verify configuration
cat(readLines(M), sep = "\n")

# view filepath for makevars file (for explantion of what this is, see website above).
cat(M)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sort a data frame by a column ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# e.g. by column called PropFoxesTagged
mydataAllVisSORTED <- mydataAllVis[order(mydataAllVis$PropFoxesTagged),] 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SPLIT CHARACTER VARIABLE NAMES using strsplit() ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Need to split into territories using strsplit (this function only works with character vectors):
a<-strsplit(rownames(spring_y1_SP), "_")
b<-subset(a, "4")



#~~~~~~~~~~~~~~~~
# Subsetting ####
#~~~~~~~~~~~~~~~~

# subsetting a data frame
SO<-subset(all3defs, Definition=="SO") # 'is'
SO<-subset(all3defs, Definition!="SO") # 'is not'

EncounterRate_noNAs <- subset(EncounterRate, SocialStatus!= "NA" & Sex!="NA") # & means AND # use to exclude both unknown sex AND unknown status
EncounterRate_noNAs <- subset(EncounterRate, SocialStatus!= "NA" | Sex!="NA") # | means OR

# Subset excluding NAs using the above method doesn't work if the NA isn't a real cell entry (i.e. doesn't contain the letters NA in the csv file)
# If NAs in data are empty cells, import the dataset using stringsAsFactors=FALSE and then replace all empty cells with NAs
visitdata[visitdata==''] <- NA # tells R empty cells are NA, so they're marked with NA in grey italics
# Then subset this way:
MEANdailyPRT_sub <- subset(MEANdailyPRT, !(is.na(SocialStatus)))

# subsetting a matrix (here the matrix is called T1sprB4_net)
B4network_M <- T1sprB4_net[which(T1sprB4_attr$Sex=="M" & T1sprB4_attr$Core==1), # select rows where individual is male & core
                           which(T1sprB4_attr$Sex=="M" & T1sprB4_attr$Core==1)] # select cols where individual is male & core

#~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Time functions / code ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~#
start.time <- Sys.time()
#...functions or codes...#
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# T-test - example to test differences in network centrality measures  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Does mean edge weight differ between sexes? - example from Asnipe 2013 paper adapted to my fox data

#== 1. Calculate mean (non-zero) edge weights for each sex (core foxes only)
B4network_M <- T1sprB4_net[which(T1sprB4_attr$Sex=="M" & T1sprB4_attr$Core==1), which(T1sprB4_attr$Sex=="M" & T1sprB4_attr$Core==1)] # select rows & cols where individual is male & core
B4network_F <- T1sprB4_net[which(T1sprB4_attr$Sex=="F" & T1sprB4_attr$Core==1), which(T1sprB4_attr$Sex=="F" & T1sprB4_attr$Core==1)] 

Afnetwork_M <- T1sprAf_net[which(T1sprAf_attr$Sex=="M" & T1sprAf_attr$Core==1), which(T1sprAf_attr$Sex=="M" & T1sprAf_attr$Core==1)] 
Afnetwork_F <- T1sprAf_net[which(T1sprAf_attr$Sex=="F" & T1sprAf_attr$Core==1), which(T1sprAf_attr$Sex=="F" & T1sprAf_attr$Core==1)] 

#== 2. Calculate and store the t-statistic from the real (observed) network
T1sprB4_t <- t.test(B4network_F[B4network_F>0], B4network_M[B4network_M>0])$statistic
T1sprAf_t <- t.test(Afnetwork_F[Afnetwork_F>0], Afnetwork_M[Afnetwork_M>0])$statistic # Can't do t-test for after midnight as not enough observations

#== 3. Get 1000 networks from data-stream permutations
T1sprB4_rand <- network_permutation(association_data=T1sprB4_SP, 
                                    data_format="SP",
                                    association_matrix=T1sprB4_net,
                                    days=rownames(T1sprB4_SP),
                                    within_day=TRUE,
                                    permutations=1000)

T1sprAf_rand <- network_permutation(association_data=T1sprAf_SP, 
                                    data_format="SP",
                                    association_matrix=T1sprAf_net,
                                    days=rownames(T1sprAf_SP),
                                    within_day=TRUE,
                                    permutations=1000)

#== 4. Create a 1-dimensional matrix to store results
T1sprB4_rand_t <- rep(0,1000)
T1sprAf_rand_t <- rep(0,1000)

#== 5. Fill the matrix with t-statistics from each of the 1000 random networks - using another loop:
for (i in c(1:1000)) {
  B4net_M_rand <- T1sprB4_rand[i,which(T1sprB4_attr$Sex=="M" & T1sprB4_attr$Core==1), which(T1sprB4_attr$Sex=="M" & T1sprB4_attr$Core==1)]
  B4net_F_rand <- T1sprB4_rand[i,which(T1sprB4_attr$Sex=="F" & T1sprB4_attr$Core==1), which(T1sprB4_attr$Sex=="F" & T1sprB4_attr$Core==1)]
  # specify matrix to store results in:
  T1sprB4_rand_t[i] <- t.test(B4net_F_rand[B4net_F_rand>0],             
                              B4net_M_rand[B4net_M_rand>0])$statistic
}

# And for after midnight... Can't do t-test as not enough true associations were observed after midnight...
for (i in c(1:1000)) {
  Afnet_M_rand <- T1sprAf_rand[i,which(T1sprAf_attr$Sex=="M" & T1sprAf_attr$Core==1), which(T1sprAf_attr$Sex=="M" & T1sprAf_attr$Core==1)]
  Afnet_F_rand <- T1sprAf_rand[i,which(T1sprAf_attr$Sex=="F" & T1sprAf_attr$Core==1), which(T1sprAf_attr$Sex=="F" & T1sprAf_attr$Core==1)]
  # specify matrix to store results in:
  T1sprAf_rand_t[i] <- t.test(Afnet_F_rand[Afnet_F_rand>0],             
                              Afnet_M_rand[Afnet_M_rand>0])$statistic
}

#== 6. Plot t-values from random networks and overlay a red line to show the real (observed) t-value (here 'T1sprB4_t')
dev.off()
hist(T1sprB4_rand_t,breaks=100)
abline(v=T1sprB4_t, col="red") # If significant it should be outside or on very edge of random distribution


#== 7. Get a P value of whether real t-statistic (t_obs) is sig. diff from random (t_rand)
# Count number of random t-values that were greater or less than t_obs
# AND THEN divide this by the number of permutations to get the p-value:
# interpret using the histogram: if red line is to LHS of distrib p will be low and 
# if red line is to RHS of distrib it'll be high
# significant if p<0.025 OR p>0.975
sum(abs(T1sprB4_t) < abs(T1sprB4_rand_t))/1000 # abs(x) computes the absolute value of x (regardless of sign)
# = not sig.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time taken to run functions ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PI.time <- system.time(...actions...) # shows time at the end (seconds)

# e.g. 
PI.time <- system.time(model<- lm(y~x))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# variance inflation factor (VIF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# High VIF (>5) indicates collinearity that may inflate standard errors in models and cause 
# type 2 errors (false negatives)
# Categorical variables can't be collinear because they're not linear measures in Euclidean space
# i.e. their order is not meaningful. Core=1 and not core=0 could be core=10 and not core=67 and still have same meaning. 
# so high VIF is only concern for continuous variables (and interactions between continuous variables, not interactions between a categorical and a continuous)
# for predictors with high VIF, model predictions will still be OK but SEs are inflated so the 
# significance (p-value) of the predictor should be interpreted with caution:
# http://stackoverflow.com/questions/33397689/multi-collinearity-for-categorical-variables

#### FOR LINEAR MODELS (not mixed):
library(car)
vif(dailyPRTmod_FINAL) # variance inflation factors 
sqrt(vif(dailyPRTmod_FINAL)) > 2

#### FOR MIXED MODELS:
# See RCode_GENERAL_Functions for diagnosing multicollinearity in mixed models
# from https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/

## "values over 5 are troubling".
## "should probably 'investigate' anything over 2.5"

vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
vif.mer(model)

## Centering predictors can reduce multicollinearity
## Or changing the reference category to the one with the most samples
## useful links: http://bit.ly/2ae2EBi, http://bit.ly/2azhyli

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View structure of data or objects ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
str(mydata)
head(mydata) # view first 6 rows
tail(mydata) # view last 6 rows


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Wilcoxon signed rank test
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(equivalent to the Mann-Whitney test) - for matched pairs
# null hypothesis is that the distributions of x and y differ by a location shift of zero
# 'cannot compute exact p-value with ties' means there are two or more identical values in the dataset (so ranks are not unique)
WTspr <- wilcox.test(nonres_spr$Female, nonres_spr$Male, paired=T) # V = 1.5, p-value = 0.1344

# CALCULATE EFFECT SIZES
# Function from Andy Field 2012 DSUR book (section 15.5):
rFromWilcox <- function(wilcoxModel, N){ # N is no. observations across both groups, so 14 if counted males and females in 7 territories
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/sqrt(N)
  cat(wilcoxModel$data.name, "Effect size, r = ", r)
}
rFromWilcox(WTspr, N=14) # -0.4000661 

# Report as V=1.5, p=0.134, r=-0.4.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zero-inflation - testing for ~ ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# by comparing GLMs glm() with zero-inflated GLMs zeroinfl()

library(pscl) # for ZI single-level models: can't use ranefs in this package but can compare a fixef-only model with/without ZI to confirm if data are ZI
fm_nb     <- glm.nb(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded + DaysSeen, data = EncounterRate_noNAs) # requires MASS package
fm_pois   <- glm(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded + DaysSeen, data = EncounterRate_noNAs, family = "poisson")
fm_zinb   <- zeroinfl(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded + DaysSeen|1, data = EncounterRate_noNAs, dist = "negbin")
fm_zipois <- zeroinfl(NTrueAssocs ~ Sex*SocialStatus*fSeason + DaysFedPW_rounded + DaysSeen|1, data = EncounterRate_noNAs, dist = "poisson")

library(bbmle)
bbmle::ICtab(fm_nb, fm_zinb, fm_pois, fm_zipois, type="AIC") 
# NEG BINOM fits best by AIC or qAIC - DONT NEED ZERO INFLATION
# Though neg binom also fit better than poisson...

# see how much variance ZI causes (proportion between 0-1)
zi.logodds <- coef(fm_zinb)["zero_(Intercept)"]
plogis(zi.logodds) # get distribution function = the estimated ZI proportion, i.e. the contribution of ZI to the model




