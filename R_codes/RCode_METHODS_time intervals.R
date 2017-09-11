rm(list=ls()) #to clear the R workspace
setwd("G:/Statistics 2016/Methods chapter - stats and plots/Methods - TimeIntervals_Mean_Median")

mydata <- read.table(file = "TimeIntsR_SeasonIDs.txt", sep='\t', header = TRUE) #import dataset
str(mydata)
hist(mydata$ti.threshold.s, breaks=10, col="steelblue", xlab="Inter-record time interval (s)", 
     main="Frequency distribution of inter-record time intervals") #plot frequency distribution of data USE FOR THESIS

####Plots exported as 700*520####


median(mydata$ti.threshold.s) #find median of all without any grouping

#To create a new dataframe/table of means and medians for each animal on each date:
library(plyr)
timeintervalsummary.table <- ddply(mydata, c("AnimalID", "TRANScDate"), summarise,
                mean.ti=mean(SecondsBetween),
                median.ti=median(SecondsBetween),
                n.obs=length(SecondsBetween))
head(timeintervalsummary.table) #view top few rows of the table 

#GROUPING BY ANIMAL AND NOT DATE
#use function tapply to find mean or medians OF mean or medians, template:
#tapply(Summary Variable, Group Variable, Function)   #e.g. tapply(mydata$Seconds_Between, mydata$AnimalID, mean)
TImeanofmedians.table<-tapply(timeintervalsummary.table$median.ti, timeintervalsummary.table$AnimalID, mean) #find mean of medians for each animal on all days
TImedianofmedians.table<-tapply(timeintervalsummary.table$median.ti, timeintervalsummary.table$AnimalID, median) #find median of medians for each animal on all days

mean.meanofmedians<- mean(TImeanofmedians.table) #find mean of the mean-of-medians for all animals on all days
mean.medianofmedians<- mean(TImedianofmedians.table) #find mean of the mean-of-medians for all animals on all days
median.meanofmedians<- median(TImeanofmedians.table)
median.medianofmedians<-median(TImedianofmedians.table)

mean.meanofmedians
mean.medianofmedians
median.meanofmedians
median.medianofmedians



####################

TImeanofmeans.table<-tapply(timeintervalsummary.table$mean.ti, timeintervalsummary.table$AnimalID, mean) #find mean of means for each animal on all days
TImedianofmeans.table<-tapply(timeintervalsummary.table$mean.ti, timeintervalsummary.table$AnimalID, median) #find median of means for each animal on all days

mean.meanofmeans<- mean(TImeanofmeans.table) #find mean of the mean-of-means for all animals on all days
mean.medianofmeans<- mean(TImedianofmeans.table) #find mean of the median-of-means for all animals on all days
median.meanofmeans<- median(TImeanofmeans.table)
median.medianofmeans<-median(TImedianofmeans.table)

mean.meanofmeans
mean.medianofmeans
median.meanofmeans
median.medianofmeans


###################################################################

#Better method to use TIs - judge threshold by eye in Excel by plotting time intervals per season per fox
#import suitable thresholds to R, along with individual attribute details
#use plyr package to wuickly summarise mean thresholds in a table

rm(list=ls()) #to clear the R workspace

mydata <- read.table(file = "TimeIntsR.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
#and not columns (R interprets separate columns as separate variables)

str(mydata) #view data structure

as.matrix(levels(mydata$Fox.name)) #list fox names if need to check which ones are in the dataset

plot(mydata$ti.threshold.s) #view the data (looks messy)


QUESTION: ARE THERE SEX, STATUS AND SEASONAL DIFFERENCES IN LENGTH OF INDEPENDENT EVENTS?

library(plyr) #package needed to group data for summary stats

TIsum.season.sex.status <- ddply(mydata, c("Social.status", "Sex", "Season"), summarise,
               mean.ti=mean(ti.threshold.s),
               n.obs=length(ti.threshold.s))  #make summary table 
TIsum.season.sex.status #view the table 

####

#summarise just by sex & season
TIsum.season.sex <- ddply(mydata, c("Sex", "Season"), summarise,
               mean.ti=mean(ti.threshold.s),
               n.obs=length(ti.threshold.s))  #make summary table (n.obs doesnt work: multiplies foxes by seasons)
TIsum.season.sex #view the table 

####

#summarise just by status & season
TIsum.season.status <- ddply(mydata, c("Social.status", "Season"), summarise,
               mean.ti=mean(ti.threshold.s),
               n.obs=length(ti.threshold.s))  # make summary table #make summary table (n.obs doesnt work: 
                                              # multiplies foxes by seasons)
TIsum.season.status #view the table 

####

#summarise just by sex & status
TIsum.sex.status <- ddply(mydata, c("Sex", "Social.status"), summarise,
               mean.ti=mean(ti.threshold.s),
               n.obs=length(ti.threshold.s))  # make summary table(n.obs doesnt work - multiplies foxes by 
                                              # seasons to give sample size as number of estimated thresholds 
                                              # rather than number of foxes)
TIsum.sex.status #view the table 

####

#summarise just by season
TIsum.season <- ddply(mydata, c("Season"), summarise,
                          mean.ti=mean(ti.threshold.s),
                          n.obs=length(ti.threshold.s))  #make summary table 
TIsum.season #view the table 


#######################################################################################################

####GRAPHS####

#draw a crude barplot that doesnt make much sense...
barplot(TIsum.season$mean.ti, xlab= "Season", ylab= "Mean inter-record time interval threshold (s)")

#draw exciting-looking strip-chart but not sure which season is the different one...
stripchart(TIsum.season$mean.ti, xlab= "Season", ylab= "Mean inter-record time interval threshold (s)")

#draw a boxplot!?!
boxplot(TIsum.season$mean.ti, xlab= "Season", ylab= "Mean inter-record time interval threshold (s)")






#To re-order Season to be Spring-Wint instead of alphabetical, use ORDERED()
levels(mydata$Season) #check what order the levels appear in by default
x1  = factor(mydata$Season, levels=c("Spring", "Summer", "Autumn ", "Winter")) #specify order of levels in the new object "x1" (for some reason Autumn has a space after it in the dataset, so must haev it here)
levels(x1) #check the order of levels now (in the new object)
                      
#Re-calculate using the new ordered seasons:
TIsum.season.sex.status <- ddply(mydata, c("Social.status", "Sex", "x1"), summarise,
                                 mean.ti=mean(ti.threshold.s),
                                 n.obs=length(ti.threshold.s))  #make summary table 
TIsum.season.sex.status #view the table 


# Draw a boxplot of seasonal differences in time interval threshold (template is plot(x range, y range, type="n", xlab=" ", ylab=" "))
plot(TIsum.season.sex.status$x1, TIsum.season.sex.status$mean.ti, type="n",
     ylab="Time interval threshold (min)" ) 

head(TIsum.season.sex.status)

###############################

# Alternative way of plotting distribution of Time Interval Thresholds ####

# subset data by season
sp <- subset(mydata, SeasonID==1)
su <- subset(mydata, SeasonID==2)
au <- subset(mydata, SeasonID==3)
wi <- subset(mydata, SeasonID==4)

# plot
par(mfrow=c(2,2))
plot(density(sp$ti.threshold.s), main="Spring")
plot(density(su$ti.threshold.s), main="Summer")
plot(density(au$ti.threshold.s), main="Autumn")
plot(density(wi$ti.threshold.s), main="Winter")

# plot line at highest density and get the x value
sp.d <- density(sp$ti.threshold.s)
plot(sp.d, main = "Spring") 
abline(v=sp.d$x[which.max(sp.d$y)]) 
sp.v <- sp.d$x[which.max(sp.d$y)] # value

su.d <- density(su$ti.threshold.s)
plot(su.d, main = "Summer") 
abline(v=su.d$x[which.max(su.d$y)]) 
su.v <- su.d$x[which.max(su.d$y)] # value

au.d <- density(au$ti.threshold.s)
plot(au.d, main = "Autumn") 
abline(v=au.d$x[which.max(au.d$y)]) 
au.v <- au.d$x[which.max(au.d$y)] # value

wi.d <- density(wi$ti.threshold.s)
plot(wi.d, main = "Winter") 
abline(v=wi.d$x[which.max(wi.d$y)]) 
wi.v <- wi.d$x[which.max(wi.d$y)] # value

