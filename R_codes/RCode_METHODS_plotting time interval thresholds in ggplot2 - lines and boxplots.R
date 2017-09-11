setwd("~/Analysis/TimeIntervals_Mean_Median")
mydata <- read.table(file = "TimeIntsR.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
View(mydata)

library(plyr) #package needed to group data for summary stats
library(ggplot2)

#To re-order Season to be Spring-Wint instead of alphabetical, use ORDERED()
levels(mydata$Season) #check what order the levels appear in by default
x1  = factor(mydata$Season, levels=c("Spring", "Summer", "Autumn ", "Winter")) #specify order of levels in the new object "x1" (for some reason Autumn has a space after it in the dataset, so must haev it here)
levels(x1) #check the order of levels now (in the new object)

#Re-calculate using the new ordered seasons:
TIsum.season.sex.status <- ddply(mydata, c("Social.status", "Sex", "x1"), summarise,
                            mean.ti=mean(ti.threshold.s),
                             n.obs=length(ti.threshold.s))  #make summary table
TIsum.season.sex.status #view the table


#Take subset of data that excludes unknown social status (!= means not-equal-to, == means equal-to)
data3 <- subset(mydata,Social.status != "Unknown")
#plot bar charts with grey background
ggplot(data3,aes(x=Season,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot()
#plot bar charts with white background
ggplot(data3,aes(x=Season,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw()

#Changing colour schemes...
ggplot(data3,aes(x=Season,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + scale_colour_brewer()
ggplot(data3,aes(x=Season,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + scale_colour_brewer(palette=10)
ggplot(data3,aes(x=Season,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + scale_colour_brewer(palette=2)

#Plot sex and social status on a panel
ggplot(data3,aes(x=Season,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_grid(Social.status ~ Sex)

#check order seasons are displaying in
levels(data3$Season)
#reorder seasons ########fAfter this I can refer to mydata$season3 as just Season3 if want to order seasons in this way
mydata$Season3 <- factor(mydata$Season, levels=c("Spring", "Summer", "Autumn ", "Winter"))
#Update the subset of data that does not include foxes with social status=unknown 
data3 <- subset(mydata,Social.status != "Unknown")
#Plot the subset as separate boxplots for sex, in the same column, with the newly ordered axes
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_wrap( ~ Sex,ncol=1)


#Order Sex as Male-Female instead of the default Female-Male
mydata$Sex <- factor(mydata$Sex, levels=c("Male", "Female"))
#Update the subset of data that does not include foxes with social status=unknown 
data3 <- subset(mydata,Social.status != "Unknown")
#Plot the subset as separate boxplots for sex, in the same column, with the newly ordered axes
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_wrap( ~ Sex,ncol=1)


#Plot this subset as separate boxplots for sex, IN ONE ROW (can put nrow=1 or just leave blank)
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_wrap( ~ Sex,nrow=1)
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_wrap( ~ Sex)

#Plot this subset as separate boxplots for sex and status IN ONE ROW
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_wrap(Social.status ~ Sex,nrow=1)

#Facet_Grid command to plot this subset as separate boxplots for sex and status, as a panel (is automatically a 2x2 panel based on number of variables)
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_grid(Social.status ~ Sex)

########Change which attributes are at the top and side by reordering the facet_grid() command###########
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_boxplot() + theme_bw() + facet_grid(Sex ~Social.status)



#################TO DRAW A POINTS JOINED BY LINES#################

#Plot all points from original dataset on panels on the same row (using subset of data that excludes Unknown social status)
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) + geom_point() + theme_bw() + facet_wrap(~ Sex)
###GGPLOT WONT CONNECT THE POINTS WITH LINES AS IT DOESNT THINK SEASONS ARE CORRELATED/ORDERED/IN ANY WAY CONNECTED

#Plot means as points - if we refer to Season as 'x1', this is a factor so GGPLOT won't join the points with lines...
ggplot(TIsum.season.sex.status,aes(x=x1,y=mean.ti,colour=Sex,shape=Social.status)) + geom_point() + theme_bw() + facet_wrap(~ Sex) + geom_line()

#SO NEED TO SAVE SEASON AS A NUMERIC VALUE - So R realises the groups (seasons) are ordered and will join the points with lines
TIsum.season.sex.status$num <- as.numeric(TIsum.season.sex.status$x1) #x1 refers to Season in the ti.sum.means table, as we only want to 
#plot the single mean values rather than the entire spread of points
#NOTE - to convert numerics back to factors, first change them to characters (as.character()) and then back to numeric, otherwise the 
#categories may be converted back to the wrong group i.e. spring -> 1 -> autumn instead of -> spring.

###Re-run the ti.sumtable code to get means omitting foxes with social status=unknown
TIsum.subset <- ddply(data3, c("Social.status", "Sex", "Season3"), summarise, #Season3 refers to ordered levels
                                 mean.ti=mean(ti.threshold.s),
                                 n.obs=length(ti.threshold.s))  #make summary table 
#Re-convert season to numeric
TIsum.subset$num <- as.numeric(TIsum.subset$Season3)

#Plot means as points joined by lines ...because we refer to the numeric form of Season, called 'num'
ggplot(TIsum.subset,aes(x=num,y=mean.ti,colour=Sex,linetype=Social.status)) + geom_point() + theme_bw() + facet_wrap(~ Sex) + geom_line()
