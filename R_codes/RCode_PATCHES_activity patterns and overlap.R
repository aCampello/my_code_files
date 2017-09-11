
#================================================================================================
#### MEASURING COEFFICIENT OF OVERLAPPING FOR FOX ACTIVITY (INFERRED FROM INDEPENDENT VISITS AT PATCHES) ####
#================================================================================================

setwd("g:/Bristol - PhD Camera Trapping Study/Statistics 2016_150417/Patch visitation/Activity patterns")
mydata <- read.csv("Activity_NewVisitStarts_Adults28SurveyDates.CSV", header=T)

# for paper decided to only present data from resident foxes:
mydata <- read.csv("Activity_NewVisitStarts_Adults28SurveyDates_ResidentsMarked4Paper300317.CSV", header=T)


# Convert datetimes from character to POSIXct
mydata$datetime <-as.POSIXct(paste(mydata$cDateTime),format="%d/%m/%Y %H:%M:%S", tz="UTC") 
head(mydata) # this conversion seems to change the order of the data frame!

# convert degree datetimes to radians (this considers the time AND the date
mydata$cDateTimeRad <- circular::rad(mydata$cDateTimeDegrees)

#### CALC ACTIVITY BY THE HOUR IN EACH SEASON TO QUOTE % ACTIVITY BETWEEN 6PM-6AM IN THESIS ####

# mark datetimes of visits as hours
library(lubridate)
mydata$hour <- hour(mydata$datetime)

# calc number of visits in each hour in each season
library(plyr)
hourlyactivity <- ddply(mydata, c("SeasonID", "hour"),
                        summarise,
                        totvisits=length(AnimalID))
head(hourlyactivity)

# mark whether hours occured between 6pm-6am
hourlyactivity$sixTOsix <- ifelse(hourlyactivity$hour<6|hourlyactivity$hour>=18, 1, 0)

visitsatnight <- ddply(hourlyactivity, c("SeasonID", "sixTOsix"),
                                summarise,
                                nvisits = sum(totvisits))
propNightActivity <- ddply(visitsatnight, c("SeasonID", "sixTOsix"),
                           summarise,
                           ) # didnt end up using this code

### DOES HOUR ROUND UP OR DOWN???





#### PLOT SEASONAL ACTIVITY PATTERNS - ALL ADULTS AT ALL PATCHES ####
#--------------------------------------------------------------------

# First plots are based on datetimes (=not good so use code below)
# NOTE: this would be best when comparing overlap between individuals

# make vector of visit times for each season
springdata <- mydata$cDateTimeRad[mydata$SeasonID==1]
sumdata <- mydata$cDateTimeRad[mydata$SeasonID==2]
autdata <- mydata$cDateTimeRad[mydata$SeasonID==3]
wintdata <- mydata$cDateTimeRad[mydata$SeasonID==4]

# plot overlapping - based on datetime not just time, so the activity peak shifts unrealistically
library(overlap) # https://www.kent.ac.uk/smsas/personal/msr/overlap.html
densityPlot(springdata, xcenter = "midnight", rug=T, col="forestgreen", main="Spring")
densityPlot(sumdata, xcenter = "midnight", rug=T, add=F, col="darkorange", main="Summer")
densityPlot(autdata, xcenter = "midnight", rug=T, add=F, col="red", main="Autumn")
densityPlot(wintdata, xcenter = "midnight", rug=T, add=F, col="blue", main="Winter")

# or for two individuals
Hazeldata <- mydata$cDateTimeRad[mydata$AnimalID==1]
Orchiddata <- mydata$cDateTimeRad[mydata$AnimalID==2]

densityPlot(Hazeldata, col="forestgreen", main="Hazel")
densityPlot(Orchiddata, add=T, col="darkorange", main="Ash")
legend('topright', c("Hazel", "Orchid"), lty=1, col=c("forestgreen", "darkorange"), bty='n') # add legend without box border
# they roughly overlap - peaks appear at the same time in all seasons (peaks show seasons not times of day...?)



#### PLOT OVERLAP BASED ON TIMES ONLY: more realistic ####
#----------------------------------------------------------

# convert degree TIMES to radians
mydata$cTimeRad <- circular::rad(mydata$TIMEONLYDegrees)
alldata <- mydata$cTimeRad
densityPlot(alldata) # won't plot: bandwidth estimation failed

# Convert times in hh:mm:ss to decimal degrees (so betweeo 0-360 degrees based on the 24-hour clock)
library(celestial)
mydata$Time <-as.character(mydata$Time) # convert Time to character 
mydata$degtimes <- hms2deg(mydata$Time) # then convert Time to degrees

# Then convert the decimal degree to radians
library(circular)
mydata$radtimes <- rad(mydata$degtimes)



# SUBSET DATA TO INCLUDE TERRITORY RESIDENTS ONLY
mydataresidents <- subset(mydata, Resident=="1")


# vectors of visit times for seasons based on TIMES only, not DATETIMES
springdata <- mydataresidents$radtimes[mydataresidents$SeasonID==1]
sumdata <- mydataresidents$radtimes[mydataresidents$SeasonID==2]
autdata <- mydataresidents$radtimes[mydataresidents$SeasonID==3]
wintdata <- mydataresidents$radtimes[mydataresidents$SeasonID==4]

# plot seasons on same plot - summer first as has highest peak (to make sure y-axis is tall enough)
library(overlap)
par(mar = c(3.5, 1.25, 1.5, 1.5), oma = c(2, 4, 1, 1))
densityPlot(sumdata, xcenter = "midnight",  add=F, col="darkorange", main="", xlab="", ylab="", cex=0.8)
densityPlot(springdata, xcenter = "midnight", add=T,  col="forestgreen", main="", xlab="", ylab="") 
densityPlot(autdata, xcenter = "midnight",  add=T, col="red", main="", xlab="", ylab="") 
densityPlot(wintdata, xcenter = "midnight",  add=T, col="blue", main="", xlab="", ylab="")
legend('topleft', c("Spring", "Summer", "Autumn", "Winter"), lty=1, col=c("forestgreen", "darkorange","red", "blue"), bty='n') # add legend without box border
# add exis labels
mtext("Proportion of visits", side = 2, outer = T, cex = 1.4, line = 2, col = "black")
mtext("Time of day (GMT)", side = 1, outer = T, cex = 1.4, line = 0, col = "black") 
#dev.print(jpeg, "Rplot_Seasonal activity patterns adult residents.jpeg", res=700, height=16, width=20, units="cm") # save as jpeg
#dev.print(jpeg, "Rplot_Seasonal activity patterns adult ALL FOXES.jpeg", res=700, height=16, width=20, units="cm") # save as jpeg



# plot separately on panels - WITH A 'RUG' TO SHOW RAW DATA
par(mfrow=c(4,1))
par(mar = c(3.5, 1.25, 1.5, 1.5), oma = c(2, 4, 1, 1))
densityPlot(springdata, cex.main=1.5, cex.lab=1.5, cex.axis=1.5, xlab="", ylab="", ylim=c(0,0.2), xcenter = "midnight", rug=T, col="forestgreen", main="Spring")
densityPlot(sumdata, cex.main=1.5, cex.lab=1.5, cex.axis=1.5, xlab="", ylab="", ylim=c(0,0.2), xcenter = "midnight", rug=T, add=F, col="darkorange", main="Summer", cex=1.5)
densityPlot(autdata, cex.main=1.5, cex.lab=1.5, cex.axis=1.5, xlab="", ylab="", ylim=c(0,0.2), xcenter = "midnight", rug=T, add=F, col="red", main="Autumn", cex=1.5)
densityPlot(wintdata, cex.main=1.5, cex.lab=1.5, cex.axis=1.5, xlab="", ylab="", ylim=c(0,0.2), xcenter = "midnight", rug=T, add=F, col="blue", main="Winter", cex=1.5)
# add exis labels
mtext("Proportion of visits", side = 2, outer = T, cex = 1.4, line = 2, col = "black")
mtext("Time of day (GMT)", side = 1, outer = T, cex = 1.4, line = 0, col = "black") 
#dev.print(jpeg, "Rplot_Seasonal activity patterns adult residents_FACET.jpeg", res=700, height=10, width=7, units="in") # save as jpeg
#dev.print(jpeg, "Rplot_Seasonal activity patterns adult ALL FOXES_FACET.jpeg", res=700, height=10, width=7, units="in") # save as jpeg



#### Plot overlap between dominants and subordinates FOR PAPER ####

# NOTE:
# THIS DOESNT MAKE SENSE: NEED TO PLOT OVERLAP BETWEEN INDIVIDUALS AND NOT POOL ALL DATETIMES INTO A
# SINGLE AVERAGE 24HOUR DAY: NEED TO TREAT THE WHOLE SURVEY AS A CIRCLE, NOT EACH DAY AS A CIRCLE 
# OVERLAPPED WITHIN THE SURVEY



### Make vector of visit times for each social status ####

# SEPARATE SEASONS
springdata_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Dom"]
springdata_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Sub"]

sumdata_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Dom"]
sumdata_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Sub"]

autdata_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Dom"]
autdata_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Sub"]

wintdata_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Dom"]
wintdata_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Sub"]

# ALL SEASONS POOLED (NOT IDEAL as activity varied seasonally, so pooling seasons widens the activity range & masks patterns)
alldata_dom <- mydataresidents$radtimes[mydataresidents$ShortStatus=="Dom"] 
alldata_sub <- mydataresidents$radtimes[mydataresidents$ShortStatus=="Sub"]




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ CIRCULAR STATS TO GET MEAN VISIT TIMES ~~~~####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate mean visit time in each season (residents only) to see if coincides with mean feeding time 
# To calculate mean clock times must use circular stats (so times given in radians or degrees are treated as 0-360' angles)

## First calc mean in RADIANS, then use circular::deg() to convert radians to degrees and then celestial::deg2hms() to convert from deg to HH:MM:SS
library(circular) # for deg() to convert radians to degrees
library(celestial) # for deg2hms() to convert degrees to HMS


# Get mean radians (=angles) using command circular::mean.circular (=same as command psych::circular.mean; both accept radians as input)
# (ignore warning, as gives same result as psych:circular.mean comamnd so presume is accurate)
mean.circular(springdata) # -0.5007762
mean.circular(sumdata) # -0.5211834
mean.circular(autdata) # -0.8135644
mean.circular(wintdata) # -0.6620804

# MEDIAN (though not as useful as the mean, as on a circle there is limited space for outliers to hide - at most an outlier can be 180' from the next value)
median.circular(springdata) # median= -0.6216281, deviation =5.661484 5.661630 (gave two values)
median.circular(sumdata) # median= -0.6330455, deviation = 5.650067 5.650213 (gave two values)
median.circular(autdata) # median= -0.9195703, deviation = 5.363615 (only one)
median.circular(wintdata) # median= -0.6971863, deviation =  5.585999 (only one)

# To convert a negative angle (radian) to a positive, add 2*pi to it:
# (& to convert a positive angle to a negative, subtract 2*pi from it)
# e.g. if get a radian of -0.5521256...
# add 2*pi
-0.5007762+(2*pi)
#=5.782409
# convert to degrees:
deg(5.782409)
#=331.3076
# convert to HMS:
deg2hms(331.3076)
#=22:05:14

# spring:
# mean
deg2hms(deg(-0.5007762 + (2*pi))) # 22:05:13
# median
deg2hms(deg(-0.6216281 + (2*pi))) # 21:37:32

# summer:
# mean
deg2hms(deg(-0.5211834 + (2*pi))) # 22:00:33
# median
deg2hms(deg(-0.6330455 + (2*pi))) # 21:34:55

# autumn:
# mean
deg2hms(deg(-0.8135644+(2*pi))) # 20:53:33
# median
deg2hms(deg(-0.9195703 + (2*pi))) # 20:29:15

# winter:
# mean
deg2hms(deg(-0.6620804+(2*pi))) # 21:28:16
# median
deg2hms(deg(-0.6971863 + (2*pi))) # 21:20:13




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ CIRCULAR STATS TO GET MEAN PROVISIONING TIMES ~~~~####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# import provisioning times from Camera Base
provisidata <- read.csv("FeedingTimesWithinSurveyDates - to get circular mean.CSV", header=T)

# Convert datetimes from character to POSIXct
provisidata$datetime <-as.POSIXct(paste(provisidata$cDateTime),format="%d/%m/%Y %H:%M:%S", tz="UTC") 
head(provisidata) # this conversion seems to change the order of the data frame!

# convert degree datetimes to radians (this considers the time AND the date
provisidata$cDateTimeRad <- circular::rad(provisidata$cDateTimeDegrees)

# Convert times in hh:mm:ss to decimal degrees (so betweeo 0-360 degrees based on the 24-hour clock)
library(celestial)
provisidata$Time <-as.character(provisidata$Time) # convert Time to character 
provisidata$degtimes <- hms2deg(provisidata$Time) # then convert Time to degrees

# Then convert the decimal degree to radians
library(circular)
provisidata$radtimes <- rad(provisidata$degtimes)

# vectors of visit times for seasons based on TIMES only, not DATETIMES
provisi_springdata <- provisidata$radtimes[provisidata$SeasonID==1]
provisi_sumdata <- provisidata$radtimes[provisidata$SeasonID==2]
provisi_autdata <- provisidata$radtimes[provisidata$SeasonID==3]
provisi_wintdata <- provisidata$radtimes[provisidata$SeasonID==4]

# Get mean provisioning times
mean.circular(provisi_springdata) # -1.166795
mean.circular(provisi_sumdata) # -1.10187
mean.circular(provisi_autdata) # -1.493185
mean.circular(provisi_wintdata) # -1.420485

# Median provisioning times
# (median is less useful than mean, as on a circle there is limited space for outliers to hide - at most an outlier can be 180' from the next value)
median.circular(provisi_springdata) # median = -1.142972, deviation = 5.140213
median.circular(provisi_sumdata) # median = -1.055924, deviation = 5.227261
median.circular(provisi_autdata) # median =  -1.570796, deviation = 4.712389
median.circular(provisi_wintdata) # median =  -1.570796, deviation = 4.712389


# spring:
#  mean
deg2hms(deg(-1.166795 + (2*pi))) # 19:32:35
# median
deg2hms(deg(-1.142972 + (2*pi))) # 19:38:03

# summer:
#  mean
deg2hms(deg(-1.10187 + (2*pi))) # 19:47:28
# median
deg2hms(deg(-1.055924 + (2*pi))) # 19:58:00

# autumn:
#  mean
deg2hms(deg(-1.493185 + (2*pi))) # 18:17:47
# median
deg2hms(deg(-1.570796 + (2*pi))) # 18:00:00

# winter:
#  mean
deg2hms(deg(-1.420485 + (2*pi))) # 18:34:27
# median
deg2hms(deg(-1.570796 + (2*pi))) # 18:00:00


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#### Plot overlap between doms and subs #### With equation on the plot xxxxx
par(mfrow=c(2,2))
par(mar = c(4, 2.5, 2, 1.5), oma = c(2, 4, 1, 1)) # bottom/left/top/right

overlapPlot(springdata_dom, springdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", ylim=c(0,0.22), main="")
title("Spring", adj=0)
text(5.5, 0.21, expression(hat(Delta)[4]== paste("0.91 (0.89-0.92)")), cex=1.1) # annotate

overlapPlot(sumdata_dom, sumdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", ylim=c(0,0.22), main="")
title("Summer", adj=0)
text(5.5, 0.21, expression(hat(Delta)[4]== paste("0.90 (0.88-0.91)")), cex=1.1) # annotate 

overlapPlot(autdata_dom, autdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", ylim=c(0,0.22), main="")
title("Autumn", adj=0)
text(5.5, 0.21, expression(hat(Delta)[4]== paste("0.89 (0.88-0.90)")), cex=1.1) # annotate 

overlapPlot(wintdata_dom, wintdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", ylim=c(0,0.22), main="")
title("Winter", adj=0)
text(5.5, 0.21, expression(hat(Delta)[4]==  paste("0.94 (0.91-0.94)")), cex=1.1) # annotate

overlapPlot(alldata_dom, alldata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", rug=T, main="All seasons")
# dominants have higher peak - suggests activity in patches is less evenly spread throughout day (due to more efficient feeding)

# optional legend (run line after each graph)
legend(1.5,0.23, c("Dominant", "Subordinate"), lty=c(1,2), col=c(1,4), cex=1, bty='n') # add legend

# add exis labels
mtext("Proportion of visits", side = 2, outer = T, cex = 1.4, line = 2, col = "black")
mtext("Time of day (GMT)", side = 1, outer = T, cex = 1.4, line = 0, col = "black") 

# Save plots with and without equations:
#dev.print(jpeg, "Rplot_Seasonal activity overlap btw domsub adult residents_equation inside plot.jpeg", res=700, height=7.1, width=8.1, units="in")
#dev.print(jpeg, "Rplot_Seasonal activity overlap btw domsub adult residents_legend only.jpeg", res=700, height=7.1, width=8.1, units="in") 



# With Dhat4 above the plot - tidier: 
overlapPlot(springdata_dom, springdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main = bquote(paste(bold("Spring                   "), hat(Delta)[4]==0.91,paste(" (0.89-0.92)"))))

overlapPlot(sumdata_dom, sumdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main = bquote(paste(bold("Summer                "), hat(Delta)[4]==0.90,paste("0 (0.88-0.91)")))) 
             
overlapPlot(autdata_dom, autdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main=bquote(paste(bold("Autumn                 "), hat(Delta)[4]==0.89,paste(" (0.88-0.90)"))))

overlapPlot(wintdata_dom, wintdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main=bquote(paste(bold("Winter                    "), hat(Delta)[4]==0.94,paste(" (0.91-0.94)"))))

# optional legend (run line after each graph)
legend(1.5,0.23, c("Dominant", "Subordinate"), lty=c(1,2), col=c(1,4), cex=1, bty='n') # add legend

# add exis labels
mtext("Proportion of visits", side = 2, outer = T, cex = 1.4, line = 2, col = "black")
mtext("Time of day (GMT)", side = 1, outer = T, cex = 1.4, line = 0, col = "black") 

#dev.print(jpeg, "Rplot_Seasonal activity overlap btw domsub adult residents_equation above plot.jpeg", res=700, height=7.1, width=8.1, units="in")
#dev.print(jpeg, "Rplot_Seasonal activity overlap btw domsub adult residents_equation above plot & legend.jpeg", res=700, height=7.1, width=8.1, units="in") 



# As above but formatted to fit requirements for BEHECO paper:  xxxxx
par(mfrow=c(2,2))
par(mar = c(3.5, 2.5, 2, 1.5), oma = c(2, 3, 1, 1)) # bottom/left/top/right
library(overlap)
overlapPlot(springdata_dom, springdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main = bquote(paste(bold("Spring        "), hat(Delta)[4]==0.91,paste(" (0.89-0.92)"))))

overlapPlot(sumdata_dom, sumdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main = bquote(paste(bold("Summer      "), hat(Delta)[4]==0.90,paste("0 (0.88-0.91)")))) 

overlapPlot(autdata_dom, autdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main=bquote(paste(bold("Autumn      "), hat(Delta)[4]==0.89,paste(" (0.88-0.90)"))))

overlapPlot(wintdata_dom, wintdata_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.22), main=bquote(paste(bold("Winter         "), hat(Delta)[4]==0.94,paste(" (0.91-0.94)"))))

# legend (run this line after each graph)
legend(1.7,0.23, c("Dominant", "Subordinate"), lty=c(1,2), col=c(1,4), cex=0.9, bty='n') # add legend

# add axis labels
mtext("Proportion of visits", side = 2, outer = T, cex = 1.2, line = 1, col = "black")
mtext("Time of day (GMT)", side = 1, outer = T, cex = 1.2, line = 0, col = "black") 

#dev.print(tiff, "Figure 1. 145 X 177mm_res1200.tif", res=1200, height=145, width=177, units="mm")  




#### Estimate coefficient of overlap, Dhat (between 0-1) between dom + sub residents ####

# Dhat = coefficient of overlap. Can use either Dhat1, Dhat4 or Dhat 5:
# Dhat1 performs best when have <50 samples in each category
# Dhat4 is best when have >75 samples in each category.

# check minimum sample size to see which Dhat to use
min(length(autdata_sub), length(autdata_dom)) # = 3438 (if include residents + non-residents) or 3044 (residents only)
# = way more than 75 observations, so should use Dhat4.


## Estimate Dhat1, Dhat4 and Dhat5 (though only interested in Dhat4)
# (If some Dhats not calculated (so have NA in output) check radian and degree times are based on TIMES only and not DATETIMES).
springsubdom_obs <- overlapEst(springdata_dom, springdata_sub) #Dhat4=0.9094403
sumsubdom_obs <- overlapEst(sumdata_dom, sumdata_sub) #Dhat4 = 0.9039042
autsubdom_obs <- overlapEst(autdata_dom, autdata_sub) # Dhat4=0.8926275
wintsubdom_obs <- overlapEst(wintdata_dom, wintdata_sub) # Dhat4=0.9351160
allsubdom_obs <- overlapEst(alldata_dom, alldata_sub) # Dhat4=0.9179490




### Calculate confidence intervals by bootstrapping ####

# ('resample' is a function in package overlap) 
# (Ridout & Linkie recommend 10,000 bootstrap samples 

# spring (residents)
springsub_boot <- resample(springdata_sub, 5000)
springdom_boot <- resample(springdata_dom, 5000) 

# summer (residents)
sumsub_boot <- resample(sumdata_sub, 5000)
sumdom_boot <- resample(sumdata_dom, 5000)

# autumn (residents)
autsub_boot <- resample(autdata_sub, 5000)
autdom_boot <- resample(autdata_dom, 5000)

# winter (residents)
wintsub_boot <- resample(wintdata_sub, 5000)
wintdom_boot <- resample(wintdata_dom, 5000)




### Make matrix of Dhats based on bootstrapped estimates of Dhat #### TAKES A WHILE

# TAKES ~5 MINS for 1000 resamples (the minimum recommended), longer for more

springsubdom <- bootEst(springsub_boot, springdom_boot, adjust=c(NA, 1, NA)) # ran 9:45-10:?? (didnt check time)
# adjust=() specifies the smoothing parameter for Dhat1, Dhat4 and Dhat5 (e.g. adjust=c(1, 1, 1) will calc all Dhats &
# adjust=c(NA,1,NA) will calc Dhat1 and Dhat4 only). Leave NAs to calc only one Dhat (Dhat4) for speed. 
sumsubdom <- bootEst(sumsub_boot, sumdom_boot, adjust=c(NA, 1, NA)) # ran 10:51 - 11:11
autsubdom <- bootEst(autsub_boot, autdom_boot, adjust=c(NA, 1, NA)) # started 11:17-11:37ish
wintsubdom <- bootEst(wintsub_boot, wintdom_boot, adjust=c(NA, 1, NA)) # started at 11:49-12:12ish



### Mean Dhats from bootstrapped estimates ####

springsubdom_BSmean <- colMeans(springsubdom) # mean Dhat4=0.9111567(91.1%) with 1000 bootstrap samples / mean Dhat4=0.9142198 with 5000 bootstrap samples
sumsubdom_BSmean <- colMeans(sumsubdom) # mean Dhat4= 0.8968337(89.7%) with 1000 bootstrap samples / mean Dhat4=0.9060962 with 5000 bootstrap samples
autsubdom_BSmean <- colMeans(autsubdom) # mean Dhat4=0.9004745(90.0%) with 1000 bootstrap samples / mean Dhat4=0.9087352 with 5000 bootstrap samples
wintsubdom_BSmean <- colMeans(wintsubdom) # mean Dhat4=0.9232312(92.3%) with 1000 bootstrap samples / mean Dhat4=0.9349125 with 5000 bootstrap samples



#### Calc CI around the observed Dhat4, based on matrix of bootstrap estimates of Dhat4 #### 

# Spring
bootCI(springsubdom_obs[2], springsubdom[,2]) # the 'basic0' CIs are most reliable & recommended (Ridout & Linkie 2014).
#      lower     upper
#basic0 0.8936250 0.9197335 # from 1000 bootstraps
#basic0 0.8975685 0.9212068 # from 5000 bootstraps
#basic0 0.8945345 0.9201175 # from 10,000 bootstraps

bootCIlogit(springsubdom_obs[2], springsubdom[,2]) # calculates CIs on log scale and backtransform to ensure CIs are between 0-1 (only if bootCI too close to 0/1).
#      lower     upper
#basic0 0.8928448 0.9200099 # from 1000 bootstraps
#basic0 0.8967559 0.9216303 # from 5000 bootstraps
#basic0 0.8937924 0.9204174 # from 10,000 bootstraps


# Summer
bootCI(sumsubdom_obs[2], sumsubdom[,2])
#       lower     upper
#basic0 0.8762673 0.9134998 # from 1000 bootstraps
#basic0 0.8862145 0.9217297 # from 5000 bootstraps

bootCIlogit(sumsubdom_obs[2], sumsubdom[,2])
#       lower     upper
#basic0 0.8755411 0.9134673 # from 1000 bootstraps
#basic0 0.8853813 0.9217745 # from 5000 bootstraps


# Autumn
bootCI(autsubdom_obs[2], autsubdom[,2])
#       lower     upper
#basic0 0.8759485 0.9042173 # from 1000 bootstraps
#basic0 0.8781533 0.9064647 # from 5000 bootstraps

bootCIlogit(autsubdom_obs[2], autsubdom[,2])
#       lower     upper
#basic0 0.8744070 0.9052237 # from 1000 bootstraps
#basic0 0.8756054 0.9084002 # from 5000 bootstraps


# Winter
bootCI(wintsubdom_obs[2], wintsubdom[,2])
#       lower     upper
# basic0 0.9073477 0.9383890 # from 1000 bootstraps
# basic0 0.9190963 0.9501931 # from 5000 bootstraps
#

bootCIlogit(wintsubdom_obs[2], wintsubdom[,2])
#       lower     upper
#basic0 0.9067930 0.9381603 # from 1000 bootstraps
#basic0 0.9185803 0.9497903 # from 5000 bootstraps




#### ALTERNATIVE WAY TO CALCULATE OVERLAP USING LIBRARY(ACTIVITY) ####

# THIS METHOD DOES NOT GIVE CONFIDENCE INTERVAL, JUST A PERCENTAGE OVERLAP AND P-VALUE

### Calc p-value for estimated overlap between Resident Dom and sub visit starts (takes 10-15 mins)
# p = probability that the observed overlap arose by chance, so p=0 means it is highly unlikely to have arisen by chance.
library(activity)
compareCkern(springdata_dom, springdata_sub, reps=1000) # Overlap=0.9094403 (90.9%), p=0.0000000 
compareCkern(sumdata_dom, sumdata_sub, reps=1000) # Overlap=0.9039042 (90.4%), p=0.0000000
compareCkern(autdata_dom, autdata_sub, reps=1000) # Overlap=0.8926275 (89.3%), p=0.0000000 
compareCkern(wintdata_dom, wintdata_sub, reps=1000) # Overlap=0.935116 (93.5%), p=0.000000 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Estimate coefficient of overlap between doms, subs AND food provisioning time
par(mfrow=c(2,2))
par(mar = c(4, 2.5, 2, 1.5), oma = c(2, 4, 1, 1)) # bottom/left/top/right

overlapPlot(springdata, provisi_springdata, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.3), rug=F, main = bquote(paste(bold("Spring                    "), hat(Delta)[4]==0.6,paste("0 (0.58-0.62)"))))
legend(1.3,0.31, c("Patch visits", "Provisioning"), lty=c(1,2), col=c(1,4), cex=1, bty='n') # add legend            

overlapPlot(sumdata, provisi_sumdata, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.3), rug=F, main = bquote(paste(bold("Summer                ") , hat(Delta)[4]==0.57,paste(" (0.55-0.59)"))))
legend(1.3,0.31, c("Patch visits", "Provisioning"), lty=c(1,2), col=c(1,4), cex=1, bty='n') # add legend            

overlapPlot(autdata, provisi_autdata, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.3), rug=F, main = bquote(paste(bold("Autumn                 ") , hat(Delta)[4]==0.67,paste(" (0.64-0.70)"))))
legend(1.3,0.31, c("Patch visits", "Provisioning"), lty=c(1,2), col=c(1,4), cex=1, bty='n') # add legend  

overlapPlot(wintdata, provisi_wintdata, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", 
            ylim=c(0,0.3), rug=F, main = bquote(paste(bold("Winter                   ") , hat(Delta)[4]==0.65,paste(" (0.63-0.67)"))))
legend(1.3,0.31, c("Patch visits", "Provisioning"), lty=c(1,2), col=c(1,4), cex=1, bty='n') # add legend  

# add exis labels
mtext("Proportion of events", side = 2, outer = T, cex = 1.4, line = 2, col = "black")
mtext("Time of day (GMT)", side = 1, outer = T, cex = 1.4, line = 0, col = "black") 

dev.print(jpeg, "Rplot_Seasonal activity overlap btw visits & provisioning_equation & legend.jpeg", res=700, height=7.1, width=8.1, units="in") # save as jpeg



#### Estimate coefficient of overlap + CIs between visit times and provisioning times ####

# Estimate Dhat4:
visprovis_spring_obs <- overlapEst(springdata, provisi_springdata) #Dhat4=0.6029190
visprovis_sum_obs <- overlapEst(sumdata, provisi_sumdata) #Dhat4=0.5724022
visprovis_aut_obs <- overlapEst(autdata, provisi_autdata) #Dhat4=0.6671833
visprovis_wint_obs <- overlapEst(wintdata, provisi_wintdata) #Dhat4=0.6505424

## Bootstrap CIs:
provis_spring_boot <- resample(provisi_springdata, 5000)
provis_sum_boot <- resample(provisi_sumdata, 5000)
provis_aut_boot <- resample(provisi_autdata, 5000)
provis_wint_boot <- resample(provisi_wintdata, 5000)

allvis_spring_boot <- resample(springdata, 5000)
allvis_sum_boot <- resample(sumdata, 5000)
allvis_aut_boot <- resample(autdata, 5000)
allvis_wint_boot <- resample(wintdata, 5000)


### Make matrix of Dhats based on bootstrapped estimates of Dhat #### TAKES A WHILE
visprovis_spring_bootmat <- bootEst(provis_spring_boot, allvis_spring_boot, adjust=c(NA, 1, NA))
visprovis_sum_bootmat <- bootEst(provis_sum_boot, allvis_sum_boot, adjust=c(NA, 1, NA))
visprovis_aut_bootmat <- bootEst(provis_aut_boot, allvis_aut_boot, adjust=c(NA, 1, NA))
visprovis_wint_bootmat <- bootEst(provis_wint_boot, allvis_wint_boot, adjust=c(NA, 1, NA))

#### Get bootstrap CI (use 'basic0') around the observed Dhat4 (from 5000 bootstraps)
bootCI(visprovis_spring_obs[2], visprovis_spring_bootmat[,2]) 
#        lower     upper    
#basic0: 0.5827451 0.6225191

bootCI(visprovis_sum_obs[2], visprovis_sum_bootmat[,2]) 
#        lower     upper    
#basic0: 0.5528497 0.5917808

bootCI(visprovis_aut_obs[2], visprovis_aut_bootmat[,2]) # 
#        lower     upper    
#basic0: 0.6370447 0.6959420

bootCI(visprovis_wint_obs[2], visprovis_wint_bootmat[,2]) # 
#        lower     upper    
#basic0: 0.6289355 0.6694915

#~~~~~~

#### Calculate VISIT/PROVISIONING overlap and p-value using library(activity) ####
library(activity)
compareCkern(springdata, provisi_springdata, reps=1000) # Overlap=0.602919, p=0.000000 
compareCkern(sumdata, provisi_sumdata, reps=1000) # Overlap=0.5724022, p=0.0000000
compareCkern(autdata, provisi_autdata, reps=1000) # Overlap=0.6671833, p=0.0000000
compareCkern(wintdata, provisi_wintdata, reps=1000) # Overlap=0.6505424, p=0.0000000 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOOKED AT DOM-SUB OVERLAP IN A FEW INDIVIDUAL PATCHES (to see if same as overall pattern) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~
### 16WD:
#~~~~~~

# Make datasets
SP_16WD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="16WD"]
SP_16WD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="16WD"]
SU_16WD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="16WD"]
SU_16WD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="16WD"]
AU_16WD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="16WD"]
AU_16WD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="16WD"]
WI_16WD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="16WD"]
WI_16WD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="16WD"]

# Plot dom/sub overlap @ 16WD
par(mfrow=c(2,2))
par(mar = c(4, 2.5, 2, 1.5), oma = c(2, 4, 1, 1)) # bottom/left/top/right
overlapPlot(SP_16WD_dom, SP_16WD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Spring 16WD")
overlapPlot(SU_16WD_dom, SU_16WD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Summer 16WD")
overlapPlot(AU_16WD_dom, AU_16WD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Autumn 16WD")
overlapPlot(WI_16WD_dom, WI_16WD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Winter 16WD")

# Estimate dom/sub overlap @ 16WD
overlapEst(SP_16WD_dom, SP_16WD_sub) # Dhat4 = 0.8283005
overlapEst(SU_16WD_dom, SU_16WD_sub) # Dhat4 = 0.8924232
overlapEst(AU_16WD_dom, AU_16WD_sub) # Dhat4 = 0.7622220
overlapEst(WI_16WD_dom, WI_16WD_sub) # Dhat4 = 0.7627603


#~~~~~~
### 53TD:
#~~~~~~

# Make datasets
SP_53TD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="53TD"]
SP_53TD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="53TD"]
SU_53TD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="53TD"]
SU_53TD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="53TD"]
AU_53TD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="53TD"]
AU_53TD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="53TD"]
WI_53TD_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="53TD"]
WI_53TD_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="53TD"]

# Plot dom/sub overlap @ 53TD
par(mfrow=c(2,2))
par(mar = c(4, 2.5, 2, 1.5), oma = c(2, 4, 1, 1)) # bottom/left/top/right
overlapPlot(SP_53TD_dom, SP_53TD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Spring 53TD")
overlapPlot(SU_53TD_dom, SU_53TD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Summer 53TD")
overlapPlot(AU_53TD_dom, AU_53TD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Autumn 53TD")
overlapPlot(WI_53TD_dom, WI_53TD_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Winter 53TD")

# Estimate dom/sub overlap @ 53TD
overlapEst(SP_53TD_dom, SP_53TD_sub) # Dhat4 = 0.7630667
overlapEst(SU_53TD_dom, SU_53TD_sub) # Dhat4 = 0.6300071
overlapEst(AU_53TD_dom, AU_53TD_sub) # Dhat4 = 0.7419562
overlapEst(WI_53TD_dom, WI_53TD_sub) # Dhat4 = 0.8655802


#~~~~~~
### 7DR:
#~~~~~~

# Make datasets
SP_7DR_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="7DR"]
SP_7DR_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="7DR"]
SU_7DR_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="7DR"]
SU_7DR_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="7DR"]
AU_7DR_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="7DR"]
AU_7DR_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="7DR"]
WI_7DR_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="7DR"]
WI_7DR_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="7DR"]

# Plot dom/sub overlap @ 7DR
par(mfrow=c(2,2))
par(mar = c(4, 2.5, 2, 1.5), oma = c(2, 4, 1, 1)) # bottom/left/top/right
overlapPlot(SP_7DR_dom, SP_7DR_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Spring 7DR")
overlapPlot(SU_7DR_dom, SU_7DR_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Summer 7DR")
overlapPlot(AU_7DR_dom, AU_7DR_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Autumn 7DR")
overlapPlot(WI_7DR_dom, WI_7DR_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Winter 7DR")

# Estimate dom/sub overlap @ 7DR
overlapEst(SP_7DR_dom, SP_7DR_sub) # Dhat4 = 0.9341937
overlapEst(SU_7DR_dom, SU_7DR_sub) # Dhat4 = 0.8389337
overlapEst(AU_7DR_dom, AU_7DR_sub) # Dhat4 = 0.8653003
overlapEst(WI_7DR_dom, WI_7DR_sub) # Dhat4 = 0.9233813


#~~~~~~
### 57CBA:
#~~~~~~

# Make datasets
SP_57CBA_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="57CBA"]
SP_57CBA_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==1 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="57CBA"]
SU_57CBA_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="57CBA"]
SU_57CBA_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==2 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="57CBA"]
AU_57CBA_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="57CBA"]
AU_57CBA_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==3 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="57CBA"]
WI_57CBA_dom <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Dom" & mydataresidents$StationCode=="57CBA"]
WI_57CBA_sub <- mydataresidents$radtimes[mydataresidents$SeasonID==4 & mydataresidents$ShortStatus=="Sub" & mydataresidents$StationCode=="57CBA"]

# Plot dom/sub overlap @ 57CBA
par(mfrow=c(2,2))
par(mar = c(4, 2.5, 2, 1.5), oma = c(2, 4, 1, 1)) # bottom/left/top/right
overlapPlot(SP_57CBA_dom, SP_57CBA_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Spring 57CBA")
overlapPlot(SU_57CBA_dom, SU_57CBA_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Summer 57CBA")
overlapPlot(AU_57CBA_dom, AU_57CBA_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Autumn 57CBA")
overlapPlot(WI_57CBA_dom, WI_57CBA_sub, xcenter = "midnight", cex.main=1.2, cex.axis=1.2, xlab="", ylab="", main="Winter 57CBA")

# Estimate dom/sub overlap @ 57CBA
overlapEst(SP_57CBA_dom, SP_57CBA_sub) # Dhat4 = 0.7621736
overlapEst(SU_57CBA_dom, SU_57CBA_sub) # Dhat4 = 0.7461591
overlapEst(AU_57CBA_dom, AU_57CBA_sub) # Dhat4 = 0.6954377
overlapEst(WI_57CBA_dom, WI_57CBA_sub) # Dhat4 = 0.7069972
