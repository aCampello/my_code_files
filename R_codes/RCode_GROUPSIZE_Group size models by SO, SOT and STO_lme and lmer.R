
#==========================================#
# Group size defined by SO, SOT and STO
#==========================================#

rm(list=ls()) #to clear the R workspace

getwd() # view current working directory
setwd("~/PhD")
setwd("F:/Bristol - PhD Camera Trapping Study/Statistics 2016/Group size/Data files for input to R")

.libPaths() # view current library location
.libPaths( c( .libPaths(), "~/userLibrary") ) # set new library location
.libPaths( c( .libPaths(), "E:/Statistics 2016/R/R-3.3.2/library") )


sessionInfo() # view what's what

#==========#
# CONTENTS:
#==========#
# Libraries
# Data
# Prepare data
# ddply data summaries
# Plotting to visualise raw data
# GLMM group size ~ season + definition (used in thesis)
# GLMM group size ~ season (not used in thesis but kept code for reference)
# GLMM count of nonresidents ~ season + Origin
#======================================

#==========#
# Libraries
#==========#
library(plyr)
library(glmmADMB) # for negative binomial GLMMs
library(nlme) # for lme models
library(lme4)
library(lmerTest) # to get p-values from lmer models
library(aods3) # for goodness-of-fit test (can't use for glmmADMB)

library(optimx) # for fitting glmer models with different optimisers
library(nloptr) # for fitting glmer models with different optimisers... 
# ...and run function code below to make optimiser work

library(fitdistrplus) # for fitting distributions
library(car) # to draw graph to check for normality of resids
library(MASS) # to draw graph to check for normality of resids (works alongside package 'car')
library(AICcmodavg) # to make model selection table or calculate AICc

library(arm)
library(multcomp) # to do post-hoc tests and plot Tukeys comparisons
library(multcompView) # to do cld() methods - plotting Tukeys results etc (cld=compact letter display, e.g. "a","a","b","b")
library(lsmeans) # to do post-hoc tests 
library(phia) # 'POST HOC INTERACTION ANALYSIS' to get means for interactions


library(ggplot2) # for plotting
library("coefplot2", lib.loc="E:/Statistics 2016/R/R-3.2.4revised/library") # specify library location or it has trouble loading...
library(coefplot2)                                                                            # to plot regression estimates # also needs package 'coda'
library(coefplot) # to plot regression estimates - if coefplot2 unavailable
library(visreg)    # plot regression estimates (last resort)         


#============#
#   DATA
#============#

# SOT data with nonresidents and group sizes
mydata <- read.table(file = "Transients and group sizes.txt", sep='\t', header = TRUE) #import dataset

# Origins of nonresidents from SOT data
origins <- read.csv(file = "Visitor_Origins.csv", header = TRUE) 


# load spatial overlap-defined group sizes (no sighting threshold)
spatialoverlap <- read.csv(file = "Groupsize_spatial-nothreshold_060416.csv", header = TRUE)

# load spatial overlap-defined group sizes with 50% sighting threshold
spatialoverlap.thresh <- read.table(file = "Transients and group sizes.txt", sep='\t', header = TRUE) #import dataset

# load spatiotemporal overlap-defined group sizes
spatiotemp.overlap <- read.csv(file = "Groupsize_spatiotemporal_060416.csv", header = TRUE) #import dataset

# load data for SO, SOT and STO all in same data frame, with total residents, non residents and sexes
all3defs<-read.csv(file = "Groupsize_all3definitions.csv", header = TRUE)

#==========================#
#   PREPARE THE DATA
#==========================#

# mydata: SOT residents and nonresidents ('transients)
mydata$nonres<-mydata$Number.of.transient.visitors
mydata$T<-factor(mydata$SiteID)
mydata$season.name <- factor(mydata$SeasonID, labels = c("Spring", "Summer", "Autumn", "Winter")) #has to be a factor
#convert season temporarily to factor to order it and store as column in mydata
mydata$seasonfac <- as.factor(mydata$SeasonID) #convert season to factor to allow plotting in different colours...
#convert ordered season back to numeric and store as column in mydata
mydata$seasonfacnumeric <- as.numeric(mydata$seasonfac) 
SG <- as.factor(mydata$SiteID) #set group as a factor for plotting in different colours


# origins (SOT non-resident origins: strangers, prev group members and neighbours)
origins$oriT<-factor(origins$Territory)
origins$season<-factor(origins$Season)


# SO, SOT and STO: save SiteID / Territory AS A FACTOR INSTEAD OF AN INTEGER IN EACH DATASET:

spatialoverlap$T<-factor(spatialoverlap$Territory) 
spatialoverlap$Group.size<-spatialoverlap$Total # rename Total as Group size for simplicity
spatialoverlap$season<-factor(spatialoverlap$SeasonID) 

spatialoverlap.thresh$T<-factor(spatialoverlap.thresh$SiteID) 
spatialoverlap.thresh$season<-factor(spatialoverlap.thresh$SeasonID) 

spatiotemp.overlap$T<-factor(spatiotemp.overlap$Territory)
spatiotemp.overlap$season<-factor(spatiotemp.overlap$SeasonID) 

all3defs$T<-factor(all3defs$Territory) 
all3defs$season<-factor(all3defs$SeasonID) 

# Take subsets from the 'all3defs' dataset for plotting facet graphs:

SO<-subset(all3defs, Definition=="SO") # subset for spatialoverlap definition of group
SOT<-subset(all3defs, Definition=="SOT") # subset for spatialoverlap-thresh definition of group
STO<-subset(all3defs, Definition=="STO") # subset for spatiotemporal overlap definition of group
notSO<-subset(all3defs, Definition!="SO") # subset of SOT and STO only, for plotting (due to scale differences)

#==========================#
# Generate summary stats
#==========================#

# Get mean sex ratio for each definition method
summary <- ddply(all3defs, c("Definition"), summarise,
                 M.mean = mean(Male.res),
                 F.mean = mean(Female.res),
                 All.mean = mean(Group.size),
                 sd.All.mean = sd(Group.size))
summary

# Output from R:
#Definition   M.mean   F.mean    All.mean    sd.All.mean
#1         SO 8.357143 5.428571  15.000000    7.282653
#2        SOT 3.107143 3.321429  6.428571    3.225231
#3        STO 3.785714 3.392857  7.214286    3.224411

# Converted to ratios in excel:
#Mean.M  Mean.F  Ratio M:F	
#SO	      8.4	    5.4	    1	: 0.65	(1:0.65)
#SOT	    3.1	    3.3	    1	: 1.07	(1:1.07)
#STO	    3.8	    3.4	    1	: 0.90	(1:0.90)




# To find mean group sizes in each season # (SOT definition) - same as "SO.groupsizesummary.T" below, but if try and merge them I'll have to change all graph code!
mydata.gssum.siteid <- ddply(mydata, c("SiteID"), summarise,
                             N    = length(Group.size),
                             mean = mean(Group.size),
                             sd   = sd(Group.size),
                             se   = sd / sqrt(N))   # make summary table from mydata (SOT method)

# Find mean group sizes in each season # (SOT definition) - same as "SO.groupsizesummary" below, but if try and merge them I'll have to change all graph code!
mydata.gssum.seasonid <- ddply(mydata, c("SeasonID"), summarise,
                               N    = length(Group.size),
                               mean = mean(Group.size),
                               sd   = sd(Group.size),
                               se   = sd / sqrt(N))   #make summary table from mydata (SOT method)

##############

# To find standard devs for group size defined by each definition method:
SO.groupsizesummary <- ddply(spatialoverlap, c("season"), summarise,
                             N    = length(Group.size),
                             mean = mean(Group.size),
                             sd   = sd(Group.size),
                             se   = sd / sqrt(N))   #make summary table from SO method (spatialoverlap)

SOT.groupsizesummary <- ddply(spatialoverlap.thresh, c("season"), summarise,
                              N    = length(Group.size),
                              mean = mean(Group.size),
                              sd   = sd(Group.size),
                              se   = sd / sqrt(N))   #make summary table from SOT data (spatialoverlap.thresh)

STO.groupsizesummary <- ddply(spatiotemp.overlap, c("season"), summarise,
                              N    = length(Group.size),
                              mean = mean(Group.size),
                              sd   = sd(Group.size),
                              se   = sd / sqrt(N))   #make summary table from STO data (spatiotemp.overlap)

SO.groupsizesummaryT <- ddply(spatialoverlap, c("T"), summarise,
                              N    = length(Group.size),
                              mean = mean(Group.size),
                              sd   = sd(Group.size),
                              se   = sd / sqrt(N))   #make summary table from SO method (spatialoverlap)

SOT.groupsizesummaryT <- ddply(spatialoverlap.thresh, c("T"), summarise,
                               N    = length(Group.size),
                               mean = mean(Group.size),
                               sd   = sd(Group.size),
                               se   = sd / sqrt(N))   #make summary table from SOT data (spatialoverlap.thresh)

STO.groupsizesummaryT <- ddply(spatiotemp.overlap, c("T"), summarise,
                               N    = length(Group.size),
                               mean = mean(Group.size),
                               sd   = sd(Group.size),
                               se   = sd / sqrt(N))   #make summary table from STO data (spatiotemp.overlap)

orisum <- ddply(origins, c("Season", "Origin"), summarise,
                N    = length(Count),
                mean = mean(Count),
                sd   = sd(Count),
                se   = sd / sqrt(N))   #make summary table from 'origins' dataset for plotting

#==========================#
#   VISUALISE THE DATA
#==========================#

plot(factor(mydata$SiteID),mydata$Group.size)
plot(factor(mydata$SeasonID),mydata$Group.size)

# Facetted graph with all 3 definitions shown:
myplot<-ggplot(all3defs, aes(x = OverallSequence, y = Group.size, colour = T)) + 
  geom_line(size=1) + geom_point(size=4) +
  facet_grid(Definition ~ .) +
  scale_colour_manual(values = c("red","darkorange", "maroon", "gray35", "blue", "forestgreen", "black"),
                      name="Territory") +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_continuous(breaks=c(1:8), 
                     labels = c("Summer\n2013", "Autumn\n2013", "Winter\n2013-14", "Spring\n2014", 
                                "Summer\n2014", "Autumn\n2014", "Winter\n2014-15", "Spring\n2015")) +
  xlab("") + ylab("Group size\n")

print(myplot)

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
# save above in high resolution for publications
dev.print(jpeg, "RPlot_Facetted group size by all 3 definitions_nogrid.jpeg", res=800, height=14, width=10, units="in")

#================

# Plot SOT and STO without SO because the scales are very different... doesnt look as good as facet with all 3 definitions together
myplot<-ggplot(notSO, aes(x = OverallSequence, y = Group.size, colour = T)) + 
  geom_line(size=1) + geom_point(size=4) +
  facet_grid(Definition ~ .) +
  scale_colour_manual(values = c("red","darkorange", "maroon", "gray35", "blue", "forestgreen", "black"),
                      name="Territory") +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_continuous(breaks=c(1:8), 
                     labels = c("Summer\n2013", "Autumn\n2013", "Winter\n2013-14", "Spring\n2014", 
                                "Summer\n2014", "Autumn\n2014", "Winter\n2014-15", "Spring\n2015")) +
  xlab("") + ylab("Group size\n")

print(myplot)

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

# save above in high resolution for publications
dev.print(jpeg, "RPlot_Facetted group size defined by SOT and STO only_nogrid.jpeg", res=800, height=8, width=10, units="in")

#================

#plot change in group size over time for SO definition only... doesnt look as good as facet with all 3 definitions together
myplot<- ggplot(SO, aes(x = OverallSequence, y = Group.size, colour = T)) + 
  geom_line(size=1) + geom_point(size=4) +
  scale_colour_manual(values = c("red","darkorange", "maroon", "gray35", "blue", "forestgreen", "black"),
                      name="Territory") +
  scale_y_continuous(breaks=c(5,10,15,20,25,30)) +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_continuous(breaks=c(1:8), 
                     labels = c("Summer\n2013", "Autumn\n2013", "Winter\n2013-14", "Spring\n2014", 
                                "Summer\n2014", "Autumn\n2014", "Winter\n2014-15", "Spring\n2015")) +
  xlab("") + ylab("Group size\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_Groupsize by SO definition only.jpeg", res=800, height=6, width=10.5, units="in")

##############################################################################################################

# PLOT MEAN GROUP SIZES WITH ERROR BARS FROM SEASONS # # (SOT definition)
gsize<-ggplot(mydata.gssum.siteid, aes(x = SiteID, y = mean)) + 
  geom_point(size=4) +
  scale_x_continuous(breaks=c(1:7)) +
  scale_y_continuous(limits=c(0,14), breaks=c(0,2,4,6,8,10,12,14)) +
  geom_errorbar(data=mydata.gssum.siteid, mapping=aes(x=SiteID, ymin=mean-sd, ymax=mean+sd), width=0.2, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nTerritory") + ylab("Group size\n")

#draw the above graph without grid in background
gsize + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
## saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "Rplot_Mean group sizes with SD bars_Territory.jpeg", res=800, height=6, width=10.5, units="in")

#####

# PLOT MEAN GROUP SIZES WITH ERROR BARS FOR SEASONS # # (SOT definition)
groupsizeplot<-ggplot(mydata.gssum.seasonid , aes(x = SeasonID, y = mean)) + 
  geom_point(size=5) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_y_continuous(limits=c(0,14), breaks=c(0,2,4,6,8,10,12,14)) +
  geom_errorbar(data=mydata.gssum.seasonid , mapping=aes(x=SeasonID, ymin=mean-sd, ymax=mean+sd), width=0.1, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") +
  xlab("") + ylab("Group size\n")
#add raw data as background jittering to show spread/range of values
groupsizeplot + geom_jitter(data=mydata, mapping=aes(x=SeasonID, y=Group.size), size=2.5, color = "gray35",
                            position=position_jitter(width=0.1, height=0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
## saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "Rplot_Mean group sizes by season with SD bars.jpeg", res=800, height=6, width=10.5, units="in")

####

# PLOT MEAN GROUP SIZES BY SEASON AS BOXPLOTS WITH 95% CIs # # (SOT definition)
ggplot(mydata, aes(x=factor(SeasonID), y=Group.size)) + 
  stat_boxplot(geom ='errorbar', width=0.2, size=0.5) + 
  geom_boxplot() + # add this as a layer on top
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_y_continuous(limits=c(0,14), breaks=c(0,2,4,6,8,10,12,14)) +
  theme_bw(base_size = 18, base_family = "") +
  xlab("") + ylab("Group size\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
dev.print(jpeg, "Rplot_Mean group sizes by season_boxplot-95CIs.jpeg", res=800, height=6, width=10.5, units="in")

#######################################

# plot social group on x-axis and 'group #1 as labels # # (SOT definition)
plotcode<-ggplot(mydata, aes(x = SiteID, y = Group.size, colour = seasonfac)) + 
  geom_line(size=1) + geom_point(size=4) +
  scale_y_continuous(breaks=c(2,4,6,8, 10, 12, 14)) +
  scale_colour_manual(values = c("forestgreen","darkorange", "red", "blue"),
                      name="Season",
                      breaks=c("1", "2", "3", "4"),
                      labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 18, base_family = "")
plotcode + scale_x_discrete(breaks=c(1,2,3,4,5,6,7), labels=c("Group 1", "Group 2", "Group 3", 
                                                              "Group 4", "Group 5", "Group 6", "Group 7")) +
  xlab(" ") + ylab("Group size\n")

############

#plot social group on x-axis and just group numbers as labels ### CLEARER ### # # (SOT definition)
myplot<- ggplot(mydata, aes(x = SiteID, y = Group.size, colour = seasonfac)) + 
  geom_line(size=1) + geom_point(size=4) +
  scale_y_continuous(breaks=c(2,4,6,8, 10, 12, 14)) +
  scale_colour_manual(values = c("forestgreen","darkorange", "red", "blue"),
                      name="Season",
                      breaks=c("1", "2", "3", "4"),
                      labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_continuous(breaks=c(1:7)) +
  xlab("\nSocial group ID") + ylab("Group size\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
## saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "RPlot_Groupsize.jpeg", res=800, height=6, width=10.5, units="in")

#plot season on x-axis
ggplot(mydata, aes(x = SeasonID, y = Group.size, colour = SG, linetype="")) + geom_line(size=1) + 
  geom_point(size=4) + scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 

#to plot season on x axis and join points for each group, need to make SiteID into a factor:
str(mydata$SiteID)
mydata$sitefac<-as.factor(mydata$SiteID)
str(mydata$sitefac)
#plot season on x-axis and just group numbers as labels ### CLEARER ### # # (SOT definition)
ggplot(mydata, aes(x = SeasonID, y = Group.size, colour = sitefac)) + 
  geom_line(size=1) + geom_point(size=4) +
  scale_colour_manual(values = c("forestgreen","darkorange", "maroon", "blue", "black", "red", "gray35"),
                      name="Social group territory") +
  theme_bw(base_size = 18, base_family = "") +
  scale_y_continuous(breaks=c(2,4,6,8, 10, 12, 14)) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab("\nSeason") + ylab("Group size\n")
# This graph doesnt make sense as no territories were surveyed spring-winter in order so lines are connecting non-consec seasons

###############################################

#plot change in group size over time in R
myplot<- ggplot(mydata, aes(x = OverallSequence, y = Group.size, colour = T)) + 
  geom_line(size=1) + geom_point(size=4) +
  scale_colour_manual(values = c("red","darkorange", "maroon", "gray35", "blue", "forestgreen", "black"),
                      name="Territory") +
  scale_y_continuous(breaks=c(2,4,6,8, 10, 12, 14)) +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_continuous(breaks=c(1:8), 
                     labels = c("Summer\n2013", "Autumn\n2013", "Winter\n2013-14", "Spring\n2014", 
                                "Summer\n2014", "Autumn\n2014", "Winter\n2014-15", "Spring\n2015")) +
  xlab("") + ylab("Group size\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

###############################################

#plot number of nonresidents
myplot2<-ggplot(mydata, aes(x = SeasonID, y = nonres, colour = SG)) + geom_line(size=1) + 
  geom_point(size=4) + scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_colour_manual(values = c("red", "darkorange", "green4", "darkblue", "turquoise4", "deeppink3", "black"),
                      name="Social group",
                      breaks=c("1", "2", "3", "4", "5", "6", "7"),
                      labels=c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5", "Group 6", "Group 7")) +
  theme_bw(base_size = 18, base_family = "") + xlab("") + ylab("Number of non-residents recorded\n")

#draw the above graph without grid in background
myplot2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
# data not recorded in this order (spring-winter) so line graph not appropriate for these data!
# bar chart is better:
ggplot(mydata, aes(x = SeasonID, y = nonres, fill = SG)) + 
  geom_bar(colour="black", stat="identity", position=position_dodge()) + 
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_fill_hue(name="Territory") +   # legend title
  theme_bw(base_size = 18, base_family = "") + xlab("") + ylab("Non-residents\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(mydata, aes(x = SG, y = nonres, fill = seasonfac)) + 
  geom_bar(colour="black", stat="identity", position=position_dodge()) + 
  scale_fill_hue(name="Season",
                 breaks=c("1", "2", "3", "4"),
                 labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 18, base_family = "") + xlab("\nTerritory") + ylab("Non-residents\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
## saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "RPlot_Nonresidentvisitors_bar_TerritoryOnX.jpeg", res=800, height=6, width=10.5, units="in")
dev.print(pdf, "Nonresidentvisitors_highres.pdf") #to save as PDF

# Draw facetted bar chart of non-residents by season and territory:
ggplot(mydata, aes(x = SeasonID, y = nonres, fill = seasonfac)) + 
  geom_bar(colour="black", stat="identity", position=position_dodge()) + 
  facet_grid(~SiteID) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 18, base_family = "") + xlab("") + ylab("Non-residents\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x  = element_text(angle=90, vjust=0.5)) +
  theme(legend.position="none") # get rid of legend

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_Nonresidentvisitors_facetbar.jpeg", res=800, height=6, width=10.5, units="in")

# Facet-wrap by season with coloured territories
# first name seasons for facet labels (only way to rename them as can't do it on the graph)
mydata$season.name <- factor(mydata$SeasonID, labels = c("Spring", "Summer", "Autumn", "Winter")) #has to be a factor

ggplot(mydata, aes(x = SiteID, y = nonres, fill = T)) + 
  geom_bar(colour="black", stat="identity", position=position_dodge()) + 
  facet_wrap(~season.name) +
  scale_x_continuous(breaks=c(1:7)) +
  theme_bw(base_size = 18, base_family = "") + xlab("\nTerritory") + ylab("Non-residents\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") # get rid of legend
# BEST GRAPH#

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_Nonresidentvisitors_facetgrid.jpeg", res=800, height=8, width=10.5, units="in")


###############################

# Plot boxplots of mean number of nonresident visitors by season (all territories grouped) 
box<-ggplot(mydata, aes(x=factor(SeasonID), y=nonres)) + 
  stat_boxplot(geom ='errorbar', width=0.2, size=0.5) + 
  geom_boxplot() + # add this as a layer on top
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 18, base_family = "") +
  xlab("") + ylab("Non-residents\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
box

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_Nonresidentvisitors_boxplot.jpeg", res=800, height=6, width=10.5, units="in")

#####################

# Boxplot of nonresident origins by season
ori <- ggplot(origins, aes(x=Origin, y=Count, fill=factor(Season)))
ori + geom_boxplot() +
  xlab("\nOrigin") + ylab("Count\n") +
  scale_fill_hue(name="Season",
                 breaks=c("1", "2", "3", "4"),
                 labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
dev.print(jpeg, "Rplot_Non-resident origins boxplot.jpeg", res=800, height=6, width=10.5, units="in")

########################

# Interaction plot between season ~ origin on no. non-residents
ori <- ggplot(orisum, aes(x=Season, y=mean, colour=Origin))
oriplot<-ori +  geom_line(size=1) + geom_point(size=5) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10)) +
  scale_fill_hue(name="Origin") + # legend title
  geom_errorbar(data=orisum, mapping=aes(x=Season, ymin=mean-se, ymax=mean+se), width=0.1, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nSeason") + ylab("Mean count\n") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
oriplot
# draw above in high resolution for publications
dev.print(jpeg, "Rplot_Non-resident origin-season interaction plot.jpeg", res=800, height=6, width=10.5, units="in")


#### Interaction plot for IAT talk (March 2017) without previous group members ####
orisum_sub <- subset(orisum, Origin!="Previous group member")

ori <- ggplot(orisum_sub, aes(x=Season, y=mean, colour=Origin))
ori +  geom_line(size=1.5) + geom_point(size=5) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10)) +
  geom_errorbar(data=orisum_sub, mapping=aes(x=Season, ymin=mean-se, ymax=mean+se), 
                width=0.1, size=0.8) +
  theme_bw(base_size = 22, base_family = "") +
  xlab("") + ylab("Mean N non-residents") +
  theme(axis.text = element_text(colour = "black"),
        legend.title = element_blank(), # remove legend title
        legend.position = c(0.12,0.91),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
dev.print(jpeg, "Rplot_Non-resident origin-season interaction plot_without PGM.jpeg", res=800, height=15, width=20, units="cm")


#=====================================================#
# Test of normality (low P = sig diff from normality)
#=====================================================#
shapiro.test(all3defs$Group.size)  #  far from normal




############################
####      MODELS        ####
############################

# ANOVA assumes independence of data points: I have repeated measures so must use multilevel models to control for 
# non-independence between seasons in same territory.
# Random effects Territory and Season are crossed as recorded all territories in all seasons.

#==================================================================================================================================#
##### MODEL 1: Does group size vary between the different definitions (SO, SOT and STO)? # FINAL MODEL FOR THESIS = defcomp.mod.defseas ####
#==================================================================================================================================#

# *** Tried negative binomials and quasipoissons in glmmADMB but poisson was best fit (chapter 4. SNA)
# *** Could not get glmer.nb code to work in lme4 (would not converge) which is why used glmmADMB. Also tried glmmPQL for poisson and 
# quasipoisson but don't understand these models and they don't have a logLik so not sure how to evaluate the fits. Plus acc. to 
# Bolker et al 2009 paper on GLMMs, PQL is a less accurate estimation method than Laplace, as used in lme4 and (I think) glmmADMB.
# See file: "RCode_Poissons and negbinoms for overdispersed count data_group.size-season-definition" for negative binomial codes. Model names: fit_poiss, fit_nb and fit_nb1

# Determining which random effects to include in model (use the maximal model (i.e. with interaction) for comparing models with/without random effects)
# 1. single random effect
defcomp.mod1<-glmer(Group.size~Definition*season + (1|T), data=all3defs, family=poisson(link = "log"),
                    control=glmerControl(optimizer="bobyqa",
                                         check.conv.grad=.makeCC("warning",0.05))) # optimiser makes it faster (but same estimates)
# 2. nested random effects
defcomp.mod.nest<-glmer(Group.size~Definition*season + (1|season/T), data=all3defs, family=poisson(link = "log"),
                        control=glmerControl(optimizer="bobyqa",
                                             check.conv.grad=.makeCC("warning",0.05)))

# 3. crossed random effects
defcomp.mod.cross<-glmer(Group.size~Definition*season + (1|T) + (1|season), data=all3defs, family=poisson(link = "log"),
                         control=glmerControl(optimizer="bobyqa",
                                              check.conv.grad=.makeCC("warning",0.05))) 

AICc(defcomp.mod.cross, defcomp.mod.nest, defcomp.mod1) # best model has only Territory as random effect


# Determining which fixed effects to include...

defcomp.mod.T<-glm(Group.size~ Definition + season, data=all3defs, family=poisson(link = "log")) # model without territory as ranef.

defcomp.mod0<-glmer(Group.size ~ (1|T), data=all3defs, family=poisson(link = "log"),
                    control=glmerControl(optimizer="bobyqa",
                                         check.conv.grad=.makeCC("warning",0.05)))

defcomp.mod.def<-glmer(Group.size~ Definition + (1|T), data=all3defs, family=poisson(link = "log"),
                       control=glmerControl(optimizer="bobyqa",
                                            check.conv.grad=.makeCC("warning",0.05)))

defcomp.mod.seas<-glmer(Group.size~ season + (1|T), data=all3defs, family=poisson(link = "log"),
                        control=glmerControl(optimizer="bobyqa",
                                             check.conv.grad=.makeCC("warning",0.05)))

defcomp.mod.defseas<-glmer(Group.size~ Definition + season + (1|T), data=all3defs, family=poisson(link = "log"),
                           control=glmerControl(optimizer="bobyqa",
                                                check.conv.grad=.makeCC("warning",0.05)))

defcomp.mod.int<-glmer(Group.size~Definition*season + (1|T), data=all3defs, family=poisson(link = "log"),
                       control=glmerControl(optimizer="bobyqa",
                                            check.conv.grad=.makeCC("warning",0.05)))

# Compare model fits using AICc (as n/K<40, where n=samples and K=number of parameters estimated in model)
library(AICcmodavg)
Cand.mods <- list("null" = defcomp.mod0, "def"= defcomp.mod.def, "seas" =defcomp.mod.seas, "season+def" =defcomp.mod.defseas, "season*def" = defcomp.mod.int)
AICcmodavg::aictab(cand.set = Cand.mods, second.ord = TRUE)
defcomp.tab<-cbind(aictab(cand.set = Cand.mods, second.ord = TRUE),digits = 3, LL = TRUE)
# interaction is not significantly better than seas+def

# Confirm inclusion of each fixed and random effect by sequential removal and addition
anova(defcomp.mod0, defcomp.mod.def, defcomp.mod.defseas, defcomp.mod.int) # compares from simple to most complex
anova(defcomp.mod.defseas, defcomp.mod.int) # compare interaction and additive models # x2(6)=6.8559, p=0.3344
anova(defcomp.mod.defseas, defcomp.mod.def) # delta-deviance of season
anova(defcomp.mod.defseas, defcomp.mod.seas) # delta-deviance of definition
anova(defcomp.mod.defseas, defcomp.mod.T) # calc significance (delta-deviance) for inclusion of Territory

# FINAL MODEL:
finalmodel<-defcomp.mod.defseas<-glmer(Group.size~ Definition + season + (1|T), data=all3defs, family=poisson(link = "log"),
                                       control=glmerControl(optimizer="bobyqa",
                                                            check.conv.grad=.makeCC("warning",0.05)))
#==================#
# MODEL CHECKING
#==================#

summary(finalmodel, corr=F)  # exclude correlation from output for clarity
# median is almost zero suggesting resids are normally distributed
# variance of random effect (T) is close to zero with a low SD. 
a <- coef(summary(finalmodel)) # save model in table to copy to Excel and thesis


# CHECK OVERDISPERSION:
#===========
library("blmeco") 
dispersion_glmer(finalmodel) # =0.776. D. Bates: if scale parameter = 0.75-1.4 there may not be an overdispersion problem. 
deviance(finalmodel)/df.residual(finalmodel) # 0.576

# CHECK RESIDUALS:
#===========

# Check model fit:
qqnorm(resid(defcomp.mod.defseas))
qqline(resid(defcomp.mod.defseas))
Plot(fitdist(resid(defcomp.mod.defseas), "norm")) # resids are normal
plot(fitted(defcomp.mod.defseas),resid(defcomp.mod.defseas))  # plot residuals vs fitted


# diagnostic plots SPECIFICALLY for poisson models - to check residuals are normal:
poiss.res <- data.frame(all3defs,resid=residuals(defcomp.mod.defseas,type="pearson"),
                        fitted=fitted(defcomp.mod.defseas)) # extract residuals from model

ggplot(poiss.res,aes(x=season,y=resid, colour=Definition)) + geom_boxplot() # plot residuals from model

ggplot(all3defs,aes(x=season,y=Group.size, colour=Definition)) + geom_boxplot() # plot actual (raw) data


# Plot residuals for individual factors to compare homogeneity of variance between categories (levels)
plot(defcomp.mod.defseas,factor(Territory)~resid(.)) # can't use 'T' for territory as R interprets that as 'TRUE' 
plot(defcomp.mod.defseas,factor(Definition)~resid(.))
plot(defcomp.mod.defseas,factor(season)~resid(.))

# Plot fitted~residuals and add a loess smoother or smoothing spline (solid line) and a horizontal line at zero (broken line). 
# The smoothing line should be approximately straight and horizontal around zero. Basically it should overlay the horizontal zero line.
plot(fitted(defcomp.mod.defseas), residuals(defcomp.mod.defseas),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted(defcomp.mod.defseas), residuals(defcomp.mod.defseas)))


# RANDOM EFFECTS:
#===========
# For mixed models need to say something about the random effects: quote the SD and 95% confid intervals, plus the 
# chisq, df and p value from likelihood comparison test between full model with and without the random effect.

ranef(defcomp.mod.defseas, condVar = T) # show coefficients for each level of the random effects (for interest)

# Variance and SD is shown in summary(model) under Random effects: 

VarCorr(defcomp.mod1) # see whether there is a +ve between-territory variance
# value of 'intercept' is variance in intercept across grouping factors (e.g. territory),
# residual is the individual-level error for the underlying mixed model and can prob be ignored.

print(VarCorr(defcomp.mod.defseas),comp=c("Variance","Std.Dev.")) # print variances too (for the random effects only)
# this is already shown in summary(model) output)

# Use confidence intervals to compute the significance of these effects (REPORT CIs, x2 and P value)
cc<-confint(defcomp.mod.defseas) # compute 95% CIs (takes a while to compute) 


# If needed, backtransform these to response scale (adding type="response" doesn't work):
atab <- cbind(est=fixef(defcomp.mod.defseas),cc) # NB: ROWS DON'T ALIGN! See that top and bottom ests are same -
# repeated. Shift all down one as there is no est for random effects (.sig01)
btab <- exp(ztab) # exponentitate to get on response scale. 
print(btab,digits=3) # .sig01 = the random effect, e.g. territory. If C.I.s cross zero = non significant.


# FIXED EFFECTS: view 
#===========
cbind(fixef(defcomp.mod.defseas)) # show coefficients for each level of the fixed effects (from summary(model))


coefplot2(defcomp.mod.defseas, intercept=TRUE) # plot regression estimates (log scale)

# To customise label names:
snames<-c("Definition: SOT", "Definition: STO", "Summer", "Autumn", "Winter") # create vector for all levels of fixed effects
longnames <- c("(Intercept)", snames) # specify the intercept

# sort out the margins:
par()$mar #retrieve current margin settings

coefplot2(defcomp.mod.defseas, main= "Regression estimates\n\n",  # plot regression estimates (log scale)
          cex.var=1.1, cex.pts=1, varnames=longnames,
          intercept=TRUE, mar=c(2,10,5,1))     # adjust margins within the plot code (bottom, left, top, right)

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM group size, season, definition_regression estimates log scale.jpeg", res=900, height=6, width=6, units="in")

dev.off() # to reset margins to default

#=============
# GET MEANS IN EACH CATEGORY - for plotting: (averaged across territories/random effects)

# Back transformed means: 
### Use phia for adjusted mean and SE (as not back-transformed) 
### use LS means for mean (same as phia) and back-transformed confidence intervals
phia::interactionMeans(defcomp.mod.defseas) # table of average scores = simple main effects
# phia automatically back-transforms the mean ('adjusted mean') but not the SE (which shouldn't be back-transformed anyway, just the c.is)

# ...with lsmeans: can use type="response" to backtransform - gives output as 'rate' and 'rate ratio' (note order of contrasts: reflects =/- estimate)
# View just means for each season (not that meaningful as these are averaged across definitions, which itself had sig effect on group size)
cld(lsmeans(defcomp.mod.defseas,"season"), type="response")

# means for season in each definition # more meaningful!! BUT SEE BELOW FOR PROPER COMPARISON TESTS
summary(lsmeans(defcomp.mod.defseas, ~ season | Definition), type = "response") # view (automatically does Tukey adjustment for multiple comparisons, otherwise could specify , adjust="tukey")

# USE THIS CODE TO PLOT FACET WITH SEASONS IN ORDER FROM 1-4
PH.seasdef<-cld(lsmeans(defcomp.mod.defseas, ~ season | Definition), type = "response") # add 'group' column to show signif. contrasts between seasons

cld(lsmeans(defcomp.mod.defseas, ~ Definition | season), type = "response") # likewise between definitions (but see below for proper comparison tests)
cld(lsmeans(defcomp.mod.defseas, ~ Definition | season, type = "response")) # can put the bracket wherever

#===========
### Post hoc tests

summary(defcomp.mod.defseas)

# ...with glht: does not backtransform (note order of contrasts - reflects the =/- sign of the estimate)
library(multcomp)
SO.posthocs<-glht(defcomp.mod.defseas, linfct=mcp(season="Tukey")) 
summary(SO.posthocs) # group size sig diff between all seasons
confint(SO.posthocs) # view 95% CIs
cld(SO.posthocs) # view cld (compact letter display) showing groups
plot(cld(SO.posthocs)) # boxplot (though averaged across definitions so don't report this)

# ... in lsmeans: does backtransform
PH.def<-lsmeans(defcomp.mod.defseas, pairwise~Definition, adjust="tukey") # view as is
summary(PH.def, type="response") # back-transformed (NB this shows sig. contrasts but the means are meaningless alone as season also has influence)
PH.seas<-lsmeans::lsmeans(defcomp.mod.defseas, pairwise~season, adjust="tukey", type="response") # ditto
PH.def # view sig diffs # quote z-ratio and p-value as these are the same with and without transformation.
PH.seas # view sig diffs 

# BEST: "pairwise~" does comparison test AND shows the means for each category combination
PH.seasdef.pairs<-lsmeans::lsmeans(defcomp.mod.defseas, pairwise~ season | Definition, adjust="tukey", type = "response") # best to look at both - p-values and CIs
PH.seasdef.ord<-lsmeans::lsmeans(defcomp.mod.defseas, ~ season | Definition, adjust="tukey", type = "response") # remove "pairwise" to plot in order
pairs(PH.seasdef.ord) # same results as 'pairwise~'

plot(PH.seasdef.pairs) # can't plot pairwise~ with just the plot() function, need cld too
plot(cld(PH.seasdef.pairs),xlab="Group size", ylab="Season", cex=1.1) # plot in size order
plot(PH.seasdef.ord, xlab="Group size", ylab="Season", cex=1.1, comparisons=TRUE, intervals=T, borders=F) # plot without #'cld' to plot in logical order (1-4 seasons) (and ensure 'pairwise' and 'cld' were not used in command)
# comparisons=TRUE draws arrows :pairs of means with overlapping arrows = those grouped together in the cld display (overlap=nonsig difference)

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM group size, season, definition.arrows.jpeg", res=900, height=6, width=6, units="in") # not ideal: need to reverse order so SO at top and spring at top



# Spring and summer are the same and aut+wint are the same. Compare each pair using custom contrasts:

# 1. specify the contrasts based on mydata (levels must be words not numbers (even if = factors) or R gets confused)
library(lsmeans)
custom.contrasts2 <- as.data.frame(contrastCoefficients(
  Season ~ (Spring + Summer)/2,
  Season ~ (Autumn + Winter)/2,
  data = all3defs))

# 2. name the contrasts
colnames(custom.contrasts2) <- c("SS",
                                 "AW")

# 3. Run this bit of code
custom.contrasts2.lsmc <- function(...) return(custom.contrasts2)

# 4. Run lsmeans on the original model to view the contrasts based on the model
PH.contrasts<-lsmeans(defcomp.mod.defseas, custom.contrasts2 ~ season | Definition, adjust="tukey", type = "response")$contrasts # just show the contrasts
PH.lsmeans<-lsmeans(defcomp.mod.defseas, custom.contrasts2 ~ season | Definition, adjust="tukey", type = "response")$lsmeans # just show the means
# above means are just the same as the normal ones (PH.seasdef.ord) - each season individually
# to get means for the grouped spr+sum and aut+wint
library(plyr) # make new column in original dataset with recoded variables:
all3defs$seasgrp <- revalue(all3defs$season, c("1"="SS", "2"="SS", "3" = "AW", "4"="AW")) # spring+sum / aut+wint grouped
all3defs$defgrp <- revalue(all3defs$Definition, c("SO"="SO", "SOT"="ST", "STO" = "ST")) # and STO and SOT grouped
# Rerun model (I hope this is OK...)
mod<-glmer(Group.size~ defgrp + seasgrp + (1|T), data=all3defs, 
           family=poisson(link = "log"),
           control=glmerControl(optimizer="bobyqa",
                                check.conv.grad=.makeCC("warning",0.05)))
hist(resid(mod)) # check fit
gof(mod) # check fit
# Calculate the grouped means - they're same as if I calculated them myself taking mean of lsmeans from each season separately but this way I get an SE and CI
lsmeans(mod,  ~ seasgrp | defgrp, adjust="tukey", type = "response")
groupedlsmeans<-cld(lsmeans(mod,  ~ seasgrp | defgrp, adjust="tukey", type = "response"))


# 5. To use ggplot2, need to save the $lsmeans and $contrasts bits of lsmean output separately (using code above) and  extract the 
# confidence intervals to plot error bars. Rename x= and y= in ggplot code according to the lsmeans output table col headings:
PH.seasdef.cont.ci <- confint(PH.contrasts, type="response")
PH.seasdef.lsm.ci <- confint(PH.lsmeans, type="response")

#plot $contrasts (estimate = value of the contast not the mean)
ggplot(PH.seasdef.cont.ci, aes(x = contrast, y = estimate, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point() + geom_errorbar() + facet_wrap(~ Definition, ncol = 4) + xlab("Season")

#plot $lsmeans
ggplot(PH.seasdef.lsm.ci, aes(x = season, y = rate, ymin = asymp.LCL, ymax = asymp.UCL)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(size=2) + geom_errorbar() + facet_wrap(~ Definition) + xlab("") + ylab("Group size\n") 

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM group size, season ggplot facet.jpeg", res=900, height=6, width=10, units="in") # not ideal: need to reverse order so SO at top and spring at top


#==============================================================================================================================================================================#
#### MODEL 1a: Does group size vary between seasons? (WITHOUT DEFINITION AS FIXED EFFECT) - NOT IN THESIS: see above for final mod that also included Definition) ####
#==============================================================================================================================================================================#

#=== Lmer method: couldnt find how to code repeated measures so use lme() results below, but for future ref here's the lmer code!
SO.gsmod0<-lmer(Group.size ~ (1|T), data=spatialoverlap, REML=F)
SO.gsmod1<-lmer(Group.size ~ season + (1|T), data=spatialoverlap, REML=F)
anova(SO.gsmod1, SO.gsmod0) # sig effect of season for spatial overlap method, i.e. adding season sig

SOT.gsmod0<-lmer(Group.size ~ (1|T), data=spatialoverlap.thresh, REML=F)
SOT.gsmod1<-lmer(Group.size ~ season + (1|T), data=spatialoverlap.thresh, REML=F)
anova(SOT.gsmod1, SOT.gsmod0) # no sig effect of season for spatial overlap with threshold

STO.gsmod0<-lmer(Group.size ~ (1|T), data=spatiotemp.overlap, REML=F)
STO.gsmod1<-lmer(Group.size ~ season + (1|T), data=spatiotemp.overlap, REML=F)
anova(STO.gsmod1, STO.gsmod0) # sig effect of season for spatiotemporal overlap method


#=== Lme method - give similar result to lmer() but here I control for repeated measures design
lme.SO.gsmod0<-lme(Group.size ~ 1, random= ~1|T/season, data=spatialoverlap, method="ML") #null model with no fixed effect
lme.SO.gsmod1<-lme(Group.size ~ season, random= ~1|T/season, data=spatialoverlap, method="ML") #null model without season 
anova(lme.SO.gsmod0, lme.SO.gsmod1) # sig effect of season (x2(3) 35.03, p < 0.0001)

anova(lme.SO.gsmod1) #get F test and num+den dfs
SO.posthocs<-glht(lme.SO.gsmod1, linfct=mcp(season="Tukey")) # run comparisons between all the groups
summary(SO.posthocs) # group size sig diff between all seasons
confint(SO.posthocs) # view 95% CIs

lme.SOT.gsmod0<-lme(Group.size ~ 1, random= ~1|T/season, data=spatialoverlap.thresh, method="ML") #null model with no fixed effect
lme.SOT.gsmod1<-lme(Group.size ~ season, random= ~1|T/season, data=spatialoverlap.thresh, method="ML") #null model without season 
anova(lme.SOT.gsmod0, lme.SOT.gsmod1) # no sig effect of season - NO POINT DOING POSTHOCS
anova(lme.SOT.gsmod1)

lme.STO.gsmod0<-lme(Group.size ~ 1, random= ~1|T/season, data=spatiotemp.overlap, method="ML") #null model with no fixed effect
lme.STO.gsmod1<-lme(Group.size ~ season, random= ~1|T/season, data=spatiotemp.overlap, method="ML") #null model without season 
anova(lme.STO.gsmod0, lme.STO.gsmod1) # sig effect of season
summary(lme.STO.gsmod1)
STO.posthocs<-glht(lme.STO.gsmod1, linfct=mcp(season="Tukey")) # run comparisons between all the groups
summary(STO.posthocs) # group size sig diff. between winter-spring and winter-summer.
confint(STO.posthocs) # view 95% CIs

#------------------------------------------------------------------------------------
#== GLMM method **didn't use these models in thesis: used the additive model above for 
# definition+season.

# Summary of SO, SOT and STO models (after running all the glmer optimisers - see 'RCode_Useful bits and bobs')
## SO definition:
SO.glmm0<-glmer(Group.size ~ (1|T), data=SO, family=poisson(link = "log")) # null
SO.glmm1.RI<-glmer(Group.size ~ season + (1|T), data=SO, family=poisson(link = "log")) # random intercept
SO.glmm2.RS.boby2<-glmer(Group.size ~ season + (season|T), data=SO, family=poisson(link = "log"), 
                         control = glmerControl(optimizer = nloptwrap2), nAGQ = 1) # random intercept & slope
anova(SO.glmm0, SO.glmm1.RI, SO.glmm2.RS.boby2) # adding a random slope doesn't improve model fit
summary(SO.glmm1.RI) # significant effect of season

## SOT definition:
SOT.glmm0<-glmer(Group.size ~ (1|T), data=SOT, family=poisson(link = "log")) # null
SOT.glmm1.RI<-glmer(Group.size ~ season + (1|T), data=SOT, family=poisson(link = "log")) # random intercept
SOT.glmm2.RS.boby2 <- glmer(Group.size ~ season + (season|T), data=SOT, family=poisson(link = "log"), 
                            control=glmerControl(optimizer=nloptwrap2)) # random intercept & slope
anova(SOT.glmm0, SOT.glmm1.RI, SOT.glmm2.RS.boby2) # adding a random slope doesn't improve model fit
summary(SOT.glmm1.RI) # nonsig effect of season

## STO definition:
STO.glmm0<-glmer(Group.size ~ (1|T), data=STO, family=poisson(link = "log")) # null
STO.glmm1.RI<-glmer(Group.size ~ season + (1|T), data=STO, family=poisson(link = "log")) # random intercept
STO.glmm2.RS.boby2 <- glmer(Group.size ~ season + (season|T), data=STO, family=poisson(link = "log"), 
                            control=glmerControl(optimizer=nloptwrap2)) # random intercept & slope
anova(STO.glmm0, STO.glmm1.RI, STO.glmm2.RS.boby2) # adding a random slope doesn't improve model fit:
summary(STO.glmm1.RI) # nonsig effect of season

# GROUP SIZE WAS ONLY DIFFERENT BETWEEN SEASONS BY THE SO METHOD.
summary(SO.glmm1.RI)
anova(SO.glmm1.RI) # get F test and num+den dfs #no p-value even with lmerTest
Anova(SO.glmm1.RI, type="III") # requires library(phia)
visreg(SO.glmm1.RI) # plot model 

# Model checking
hist(resid(SO.glmm1.RI)) # plot residuals - approx normal
qqnorm(resid(SO.glmm1.RI)) 
qqline(resid(SO.glmm1.RI)) 
plot(fitdist(residuals(SO.glmm1.RI),"norm")) 

# POSTHOC TUKEYS TESTS ON THE SO METHOD (on RI model only as RS didnt improve model fit)
SO.posthocs<-glht(SO.glmm1.RI, linfct=mcp(season="Tukey")) # run comparisons between all the groups
summary(SO.posthocs) # group size sig diff between all seasons
confint(SO.posthocs) # view 95% CIs

# Plot results of Tukey's test for SO method
par(mfrow=c(2,1))
par("mar"=c(1, 1, 1, 1))
snames<-c("Summer", "Autumn", "Winter") # create vector for season names
longnames <- c("(Intercept)", snames) # specify the intercept
coefplot2(SO.glmm1.RI, main= "Regression estimates\n\n",    # first plot regression estimates
          cex.var=1.1, cex.pts=1, varnames=longnames,
          intercept=TRUE, mar=c(1,6,5,1))               

par(mar=c(2,4,10,1)) 
a<-(cld(summary(SO.posthocs)))
plot(a,  xaxt='n', xlab="") # plot without axes intially
xtick<-seq(1, 4, by=1)                                      # specify where to put tick marks
seasonlabels<-c("Spring", "Summer", "Autumn", "Winter")     # list of labels for the x-axis
axis(side=1, at=xtick, labels = seasonlabels)

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM Tukey test of group size by season_SO definition.jpeg", res=800, height=14, width=10, units="in")
dev.off()
#------------------------------------------------------------------------------------



#================================================================================================#
####  MODEL 2: Does COUNT of non-residents vary with season or origin? Model used in thesis = glmer ####
#================================================================================================#
# Again need to control for repeated measures in territories and have COUNTS so can't use lme or lmer.

#== POISSON GLMM (USED IN THESIS):

#===========================================================#
## Model: does no.nonresidents vary with season or origin? ##
#===========================================================# 
setwd("E:/Statistics 2016/Group size/Data files for input to R")
origins <- read.csv(file = "Visitor_Origins.csv", header = TRUE) 
origins$oriT<-factor(origins$Territory)
origins$season<-factor(origins$Season)
str(origins)

library(lme4)
library(car)
# Decisions:
# 1. Fixed: season and origin, random = territory
# 2. Choose distribution: Count data so need Poisson (unless overdispersed - check dispersion once model fitted)
# 3. check distribution is suitable: mean vs var, 
# 4. Fit GLMs
# 5. Fit GLMMs
# 6. Check GLMMs: dispersion, residuals
# 7. Extract model coefficients for fixed effects and SD, 95% cis, x2 and P for random effects
# 8. Post hoc tests if needed.

#==============
# 2-3. Checking distributions

var(origins$Count) # calculate variance of response variable
mean(origins$Count) # calc the mean (variance>mean)
hist(origins$Count) # plot histogram: negatively skewed - underdispersed again?
plot(table(origins$Count), xlab = "observed count values", ylab = "frequency") # similar to histogram but doesn't bin the data

# Check the group means - take into account the fixed effect/s of interest
# 1. Create variable for interaction (combo) between season and def. ordered by mean
origins <- within(origins,
                  { 
                    seasori.int <- interaction(season, Origin)
                    seasori.int <- reorder(seasori.int, Count, mean)
                  })

# 2. Use ddply to find mean and var for each combo of season and definition (group means and vars):
a<-ddply(origins, "seasori.int",
         summarise,
         mean=mean(Count),var=var(Count))
a # variances>mean in all groups: indicates overdispersion so Poisson not suitable, so must use quasipoisson or negative binomial

# Plot variances against diff distributions
plot(a$var~a$mean, xlab="group means", ylab="group variances" ) # plot
abline(c(0,1), lty=2) # fit a poisson line
text(105,500,"Poisson") # annotate at particular xy coords (doesnt seem to work here)

lm1 <- lm(a$var~a$mean-1) ## estimate the quasipoisson pattern
phi.fit <- coef(lm1)
curve(phi.fit*x, col=2,add=TRUE) # add line for quasipoisson or negbinom type 1 (NB1) [red]  (LOOKS BEST)
text(110,3900, bquote(paste("QP: ",sigma^2==.(round(phi.fit,1))*mu)), col=2) # annotate: bquote() is used to substitute numeric values in equations with symbols

lm2 <- lm(a$var~I(a$mean^2)+offset(a$mean)-1) # estimate the neg binom pattern using an offset
k.fit <- 1/coef(lm2)
curve(x*(1+x/k.fit),col=4,add=TRUE) # Add line for neg binomial or lognormal-Poisson [blue]
text(104,7200,paste("NB: k=",round(k.fit,1),sep=""),col=4) # annotate

Lfit <- loess(a$var~a$mean) 
mvec <- 0:120
lines(mvec,predict(Lfit,mvec),col=5) # Add line for loess fit [cyan]
text(118,2000,"loess",col=5)

library(ggplot2)
ggplot(a,aes(x=mean,y=var))+geom_point()+
  geom_smooth(method='lm', formula=y~x,colour="green",fill="green") + # poisson curve
  geom_smooth(method="lm",formula=y~x-1,colour="red",fill="red") +    # linear: quasipoisson or NB1 line [red]
  geom_smooth(method="lm",formula=y~I(x^2)+offset(x)-1,               # semi-quadratic: neg binomial or lognormal-Poisson [blue]
              colour="blue",fill="blue") +
  geom_smooth(colour="cyan",fill="cyan")                              # lowess smoothing line [cyan] # loess/lowess = ''locally weighted scatterplot smoothing'

# Either poisson or NB1 (quasipoisson) look best. 

#=====
# 4. Fit a GLM
glm1 <- glm(Count ~ season*Origin, family="poisson", data=origins, trace=TRUE)
qqnorm(resid(glm1))
qqline(resid(glm1))
library(aods3)
gof(glm1) # goodness of fit test (use row with PEARSON x2): low P-value (0-1) indicates poor fit (prob due to excluded random eff. of territory)

# 4b. Dispersion test: Pearson Chi2/residual df. If >1 = overdispersed. <1 = underdispersed (for Poisson model resid deviance should be equal to resid df.)
deviance(glm1)/df.residual(glm1) # 1.88 = overdispersed

#======
# 5. Fitting GLMMs: used poisson glmer model in thesis, but see below for poisson, NB1 and NB2 mods fit in glmmADMB to check if they fit better (they didn't)

# 5a. Fit a Poisson GLMM in lme4
# null models - bobyqa optimiser stops convergence errors here:
seasori0t<-glmer(Count ~ (1|oriT), data=origins, family=poisson(link = "log"),
                 control=glmerControl(optimizer="bobyqa",
                                      check.conv.grad=.makeCC("warning",0.05)))

seasori0st<-glmer(Count ~ (1|season) + (1|oriT), data=origins, family=poisson(link = "log"),
                  control=glmerControl(optimizer="bobyqa",
                                       check.conv.grad=.makeCC("warning",0.05))) 

# nested model: nesting must be specified in the model, as season + territory are crossed but not coded differently like Spring.T1, Spring.T2
seasori0nest<-glmer(Count ~ (1|season/oriT), data=origins, family=poisson(link = "log"),
                    control=glmerControl(optimizer="bobyqa",
                                         check.conv.grad=.makeCC("warning",0.05)))

MuMIn::AICc(seasori0t, seasori0st, seasori0nest) # crossed random effects model fits best

hist(resid(seasori0st)) # only null model with season and territory has normally distrib resids
#=================
seas.st<-glmer(Count~season + (1|season) + (1|oriT), data=origins, family=poisson(link = "log"),
               control=glmerControl(optimizer="bobyqa",
                                    check.conv.grad=.makeCC("warning",0.05)))

ori.st<-glmer(Count~Origin + (1|season) + (1|oriT), data=origins, family=poisson(link = "log"),
              control=glmerControl(optimizer="bobyqa",
                                   check.conv.grad=.makeCC("warning",0.05)))

seasori.add.st<-glmer(Count~season+Origin + (1|season) + (1|oriT), data=origins, family=poisson(link = "log"),
                      control=glmerControl(optimizer="bobyqa",
                                           check.conv.grad=.makeCC("warning",0.05)))

seasori.int.st<-glmer(Count~season*Origin + (1|season) + (1|oriT), data=origins, family=poisson(link = "log"),
                      control=glmerControl(optimizer="bobyqa",
                                           check.conv.grad=.makeCC("warning",0.05))) 

# Random slopes:
seasori.add.rs<-glmer(Count~season+Origin + (season|oriT), data=origins, family=poisson(link = "log"),
                      control=glmerControl(optimizer="bobyqa",
                                           check.conv.grad=.makeCC("warning",0.05)))

seasori.int.rs<-glmer(Count~season*Origin + (season|oriT), data=origins, family=poisson(link = "log"),
                      control=glmerControl(optimizer="bobyqa",
                                           check.conv.grad=.makeCC("warning",0.05))) 

# Just territory as random effect
seas.t<-glmer(Count~season + (1|oriT), data=origins, family=poisson(link = "log"),
              control=glmerControl(optimizer="bobyqa",
                                   check.conv.grad=.makeCC("warning",0.05)))

ori.t<-glmer(Count~Origin + (1|oriT), data=origins, family=poisson(link = "log"),
             control=glmerControl(optimizer="bobyqa",
                                  check.conv.grad=.makeCC("warning",0.05)))

seasori.add.t<-glmer(Count~season+Origin + (1|oriT), data=origins, family=poisson(link = "log"),
                     control=glmerControl(optimizer="bobyqa",
                                          check.conv.grad=.makeCC("warning",0.05)))

seasori.int.t<-glmer(Count~season*Origin + (1|oriT), data=origins, family=poisson(link = "log"),
                     control=glmerControl(optimizer="bobyqa",
                                          check.conv.grad=.makeCC("warning",0.05))) 

# nested random effects - STUPID IDEA!!
seasori.int.nest<-glmer(Count ~ season*Origin + (1|season/oriT), data=origins, family=poisson(link = "log"),
                        control=glmerControl(optimizer="bobyqa",
                                             check.conv.grad=.makeCC("warning",0.05)))

library(AICcmodavg)
# list all models in descending order
Cand.mods <- list("null.t" = seasori0t, "null.st" = seasori0st, "null.nest" = seasori0nest, "seas.st" = seas.st, "seas.t" = seas.t, "ori.st" = ori.st, "ori.t" = ori.t, "seasori.add.st" = seasori.add.st,
                  "seasori.int.st" = seasori.int.st, "seasori.int.t" = seasori.int.t, "seasori.add.t" = seasori.add.t, "seasori.add.rs.st" = seasori.add.rs, "seasori.int.rs.st" = seasori.int.rs)
AICcmodavg::aictab(cand.set = Cand.mods, second.ord = TRUE)
defcomp.tab<-cbind(aictab(cand.set = Cand.mods, second.ord = TRUE),digits = 3, LL = TRUE)

# just the potential models
Cand.mods <- list("null.t" = seasori0t, "seas.t" = seas.t, "seasori.int.t" = seasori.int.t, "seasori.add.t" = seasori.add.t)
AICcmodavg::aictab(cand.set = Cand.mods, second.ord = TRUE)
defcomp.tab<-cbind(aictab(cand.set = Cand.mods, second.ord = TRUE),digits = 3, LL = TRUE)

anova(seasori.add.t, seasori.int.t) # interaction is better fit (whether incl season as ranef or not. But this is AIC, not AICc...)
anova(seasori.add.t, seas.t) # think makes more sense to compare seas.t with additive model rather than interactive...
anova(seas.t, seasori0t)

# according to AIC, the interaction model with just territory as a ranef is best, but acc to AICc, the additive model is best...

# AIC
# seasori.add.t     7 311.5785 <- best
# seasori.int.t    13 313.4407

# AIC
# seasori.add.t     7 310.1049
# seasori.int.t    13 308.2407 <- best

# Basically choose whichever. I chose the interaction model with territory as random effect.

#=============
#### ALTERNATIVE WAY TO CODE INTERACTION MODEL, SO CAN EXTRACT INDIVID COEFFICIENTS FOR SEASON AND ORIGIN AS WELL AS EXAMINE THE INTERACTION EFFECT: ####

# make new variable for origin*season interaction to get coefficients for season and origin individually, as well as for the interaction
origins$seasXori<- origins$season*origins$Origin
factor(origins$seasXori) # make it a factor

# run model with interaction and individual terms
seasori.int.ti<-glmer(Count~season+Origin+seasXori + (1|oriT), data=origins, family=poisson(link = "log"),
                      control=glmerControl(optimizer="bobyqa",
                                           check.conv.grad=.makeCC("warning",0.05))) 
# may get warning about rank deficiency: see which columns were dropped:
fsf<-cbind(fixef(seasori.int.ti, add.dropped=T)) # NAs show columns that were dropped.

# don't trust these estimates - what are the reference categories? - just comparing them all to spring-neighbour but comparing
# PGM winter with neighbour spring isn't sensible.

summary(seasori.int.ti, cor=F) 
car::Anova(seasori.int.ti)  # same 
MuMIn::AICc(seasori.int.t, seasori.int.ti, seasori.add.t) # this way of coding the interaction is the same as just using origins*season in the model code
hist(resid(seasori.int.ti))   # exactly same as other interaction model

# Will still use this model as the FINAL MODEL:
finalmodel<-seasori.int.t<-glmer(Count~season*Origin + (1|oriT), data=origins, family=poisson(link = "log"),
                                 control=glmerControl(optimizer="bobyqa",
                                                      check.conv.grad=.makeCC("warning",0.05))) 
car::Anova(seasori.int.t)
summary(seasori.int.t, cor=F) 
a <- coef(summary(seasori.int.t))
#=================

# Check residuals for normality
qqnorm(resid(seasori.int.t))
qqline(resid(seasori.int.t))
hist(resid(seasori.int.t))
plot(resid(seasori.int.t)~fitted(seasori.int.t))
library(fitdistrplus)
plot(fitdist(resid(seasori.int.t), "norm")) # resids are normal

# plot residuals against levels of each predictor separately: check homogeneity of variance
plot(seasori.add.t,factor(oriT)~resid(.))   # approx homeogenous
plot(seasori.add.t,factor(Origin)~resid(.)) # homeogenous
plot(seasori.add.t,factor(season)~resid(.)) # homeogenous

#plot model showing different intercepts and slopes. Autokey=TRUE adds legend.
lattice::xyplot(predict(seasori.int.t)~origins$Count,groups=origins$Origin,auto.key=T)

# Dispersion test
deviance(seasori.int.t)/df.residual(seasori.int.t) # 1.326 = overdispersed
library("blmeco") # to compute estimated scale parameter
dispersion_glmer(seasori.int.t) # = 1.09. OK. <1 = under and >1 = overdispersion, but if between 0.75-1.4 should be OK.


# For model coefficient table - get chi-sq calues
anova(seasori.int.t, glm1) # x2 for inclusion of territory
anova(seasori.int.t, ori.t) #x2 for inclusion of season
anova(seasori.int.t, seas.t) #x2 for inclusion of origin
anova(seasori.int.t, seasori.add.t) #x2 for interaction vs. additive: x2=13.864,df=0,p=.03119*
# Extract coefficients
lmerTest::anova(seasori.int.t) # view significant effects
car::Anova(seasori.int.t) # if lmerTest won't work
# interaction is borderline significant (p=0.052)
summary(seasori.int.t, cor=F) # only 1 significant effect - fewer PGMs in spring. Maybe should use the additive model after all...
# at least with the additive model the coefficients will be useful!
car::Anova(seasori.int.t) 
summary(seasori.int.t, cor=F)

coefs<-coef(summary(seasori.int.t, cor=F)) # save coefficients in table

# plot regression estimates
coefplot2::coefplot2(finalmodel)

# Random effects
print(VarCorr(seasori.int.t),comp=c("Variance","Std.Dev."))
cc<-confint(finalmodel) 

cbind(fixef(finalmodel)) 


# Do posthoc tests
library(lsmeans) # automatically does Tukey adjustment for multiple comparisons, otherwise could specify , adjust="tukey"

# save contrasts to quote in thesis
acontrasts<-lsmeans(seasori.int.t, pairwise~ season | Origin, adjust="tukey", 
                    type = "response")$contrasts

# Save means and CIs for plotting 
alsmeans<-lsmeans(seasori.int.t, pairwise~ season | Origin, adjust="tukey", 
                  type = "response")$lsmeans
### this is a lsmobj object so can't use for plotting in ggplot...

# Interaction model allows means to differ between groups whereas additive one doesn't
pairs_so <-(lsmeans(seasori.int.t, pairwise~ season | Origin, adjust="tukey", type = "response")) # best to look at both - p-values and CIs
pairs_os <-(lsmeans(seasori.int.t, pairwise~ Origin|season, adjust="tukey", type = "response"))
ord <-lsmeans(seasori.int.t, ~ season | Origin, adjust="tukey", type = "response") # remove "pairwise" to plot in order
pairs(ord) # same results as 'pairwise~' - means for comparisons between seasons for each Origin
ord # same results as InteractionMeans in Phia, but with back-transformed SEs (which make no sense so use the CIs - SEs shouldn't be backtransformed)


# present these as a contingency table with all levels of each factor
contingencytab<-interactionMeans(seasori.int.t) # mean across all category combos - doesnt look at interaction. Not sure what se link means/is
# use CIs from lsmeans instead of SE of link
a<-cld(ord) # use cld() to coax it into a table to export to excel

#-------
# save lsmeans tukey tests 
lsmcontrasts.so<-cld(pairs_so$contrasts)
lsmcontrasts.os<-cld(pairs_os$contrasts)

#---------------KEEP
# save lsmeans and their back-transformed 95% CIs as OBJECTS for plotting in ggplot
cont.ci <- confint(pairs_so$contrasts, type="response")
lsm.ci <- confint(pairs_so$lsmeans, type="response")
#---------------

# plot $contrasts
library(ggplot2)
ggplot(cont.ci, aes(x = contrast, y = rate.ratio, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point() + geom_errorbar() + facet_wrap(~ Origin, ncol = 4) + xlab("Season") # looks crap

#plot $lsmeans - more useful
ggplot(lsm.ci, aes(x = season, y = rate, ymin = asymp.LCL, ymax = asymp.UCL)) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(size=2) + geom_errorbar() + facet_wrap(~ Origin) + xlab("") + ylab("Group size\n") 

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM nonres - origin and season ggplot facet.jpeg", res=900, height=6, width=10, units="in") 

# alternative plot (ggplot is best though)
plot(cld(pairs_so),xlab="Count", ylab="Season", cex=1.1) # plot in size order
plot(ord, xlab="Count", ylab="Season", cex=1.1, comparisons=TRUE, intervals=T, borders=F) # plot without #'cld' to plot in logical order (1-4 seasons) (and ensure 'pairwise' and 'cld' were not used in command)
# comparisons=TRUE draws arrows :pairs of means with overlapping arrows = 
# those grouped together in the cld display (overlap=nonsig difference)


# as it's an interaction, an interaction plot will prob be more useful:

# lsmip in lsmeans: (no CIs)
lsmlsm.noci<- lsmip(seasori.int.t, Origin~season, type="response") # back-transformed lsmeans



# Line plot based on lsmeans object lsm.ci (lsmeans with 95% cis) - see above
lsm.ci$seasfac<-as.numeric(lsm.ci$season) # seasons are better spaced along x-axis if season is numeric rather than a factor

lsmggp.ci<- ggplot(lsm.ci, aes(x = seasfac, y = rate, ymin = asymp.LCL, ymax = asymp.UCL, colour=Origin)) +
  geom_line(aes(colour=Origin, group=Origin), size=1) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_y_continuous(breaks=c(2,4,6,8,10,12)) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(size=5) + geom_errorbar(width=0.1, size=0.5, color="black")  + xlab("\nSeason") + ylab("Mean N non-residents\n") 
lsmggp.ci
# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM nonres - origin and season ggplot interaction.jpeg", res=900, height=6, width=10, units="in") 


# Plot interactions from model using phia
ams.means <- interactionMeans(seasori.int.t)
library(phia)
phia.se<-plot(ams.means, atx="season", traces="Origin", cex=1.1) # plot with standard errors of the adjusted means
phia.ci<-plot(ams.means, atx="season", traces="Origin", errorbar="ci95") # plot with 95% cis of the adjusted means (not exact acc. to vignette)
plot(ams.means, atx="season", errorbar="ci95") # plot single line for all origins with 95% ci

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_GLMM phia nonres - origin and season interaction.jpeg", res=900, height=6, width=10.5, units="in")

# try plotting in ggplot to look nicer
ggmeans <- cbind(ams.means)
ggmeans$mean<-ggmeans$`adjusted mean` # make column name one word # adj mean=backtransformed
ggmeans$se<-ggmeans$`SE of link`      # # SE of link = not backtransformed-makes no sense plotted with backtransformed mean...

gg <- ggplot(ggmeans, aes(x=season, y=mean, colour=Origin)) 
phiaggp.se<-gg +  geom_line(aes(group=Origin), size=1) + # in geom_line must specify group so ggplot knows which points to connect with lines
  geom_point(size=4) +
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  scale_fill_hue(name="Origin") + # legend title
  geom_errorbar(data=ggmeans, mapping=aes(x=season, ymin=mean-se, ymax=mean+se), width=0.1, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") +
  xlab("") + ylab("Mean non-residents\n") +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# SUMMARY OF ALL GRAPHS DRAWN: (some won't plot on their own like ggplot does)
lsmlsm.noci<-lsmip(seasori.int.t, Origin~season, type="response") # from lsmeans with no SE bars or CIs (won't plot on its own!)
lsmggp.ci # from lsmeans DRAWN IN ggplot with 95% CIs from lsmeans output
phia.ci<-plot(ams.means, atx="season", traces="Origin", errorbar="ci95")  # from phia with (rough) 95% CIs (won't plot on its own!)
phia.se<-plot(ams.means, atx="season", traces="Origin", cex=1.1) # from phia with SE bars (won't plot on its own!)
phiaggp.se # from phia with SE bars DRAWN IN ggplot 
oriplot # interaction plot from raw data with SE bars (see above for code)

# Think the ggplot LSMEANS graph with 95% cis (lsmggp.ci) is the best: is on the scale
# of the response (backtransformed) and so are the CIs ('SE of link'  is in scale of link function in model, i.e logged)

#--------------------

# To quantify effects seen in interaction plot:
phia::testInteractions(seasori.int.t, fixed="Origin", across="season") # test effect of season while holding Origin fixed
# season has a signif. effect on neighbours and strangers but not PGMs
fixedconts<-cbind(phia::testInteractions(seasori.int.t, fixed="Origin", across="season")) # save results

# interaction contrasts = constrasts between contrasts / differences of differences
phia::testInteractions(seasori.int.t, pairwise="Origin", across="season")
# effect of season differs between neighbours and strangers
intconts<-cbind(phia::testInteractions(seasori.int.t, pairwise="Origin", across="season")) # save results

# see which seasons affected neighbours and strangers differently: pairwise interaction contrasts
phia::testInteractions(seasori.int.t)
# the difference in count between summer and winter is different for neighbours and strangers 
testints<-cbind(phia::testInteractions(seasori.int.t)) # save output

#=== END










#==============================================================================================
# 5c. Tried negative binomials in glmmADMB (but didn't fit as well so ended up using glmer Poisson)
# First do poissons with and without controlling for zero inflation:
admbPOIS <- glmmadmb(Count~season*Origin + (1|oriT), 
                     data=origins, 
                     zeroInflation=FALSE, 
                     family="poisson")

admbPOIS.z <- glmmadmb(Count~season*Origin + (1|oriT), 
                       data=origins, 
                       zeroInflation=TRUE, 
                       family="poisson")

admbPOIS.st <- glmmadmb(Count~season*Origin + (1|season) + (1|oriT), 
                        data=origins, 
                        zeroInflation=FALSE, 
                        family="poisson")

admbPOIS.st.z <- glmmadmb(Count~season*Origin + (1|season) + (1|oriT), 
                          data=origins, 
                          zeroInflation=TRUE, 
                          family="poisson")

# do an additive model to check (though I am interested in the interaction so will keep it in if poss..)
admbPOIS.add <- glmmadmb(Count~season+Origin + (1|oriT), 
                         data=origins, 
                         zeroInflation=FALSE, 
                         family="poisson")

MuMIn::AICc(admbPOIS, admbPOIS.z, admbPOIS.st, admbPOIS.st.z, admbPOIS.add) # controlling for zero inflation doesn't help.

# NB2 = neg binomial
admbNB2 <- glmmadmb(Count~season*Origin + (1|oriT), 
                    data=origins, 
                    zeroInflation=F, 
                    family="nbinom")

# NB1 = quasipoisson
admbNB1 <- glmmadmb(Count~season*Origin + (1|oriT), 
                    data=origins,  
                    zeroInflation=F, 
                    family="nbinom1")

MuMIn::AICc(seasori.int.t, seasori.add.t, admbPOIS, admbPOIS.st, admbPOIS.add, admbNB2, admbNB1, k=2, REML=F) # poisson is still the best (just)

# model checking
qqnorm(resid(admbNB2))
qqline(resid(admbNB2))
hist(resid(admbNB2)) # v.left-skewed - not good fit

qqnorm(resid(admbNB1))
qqline(resid(admbNB1))
hist(resid(admbNB1)) # extremely left-skewed - not good fit

qqnorm(resid(admbPOIS))
qqline(resid(admbPOIS))
hist(resid(admbPOIS)) # kind of symmetrical
plot(resid(admbPOIS)) # even scatter about zero
plot(resid(admbPOIS)~fitted(admbPOIS))

qqnorm(resid(admbPOIS.add))
qqline(resid(admbPOIS.add))
hist(resid(admbPOIS.add)) # interaction model was slightly more symmetrical

anova(admbPOIS, admbPOIS.add) # interaction model is significant better fit for ADMB models
anova(seasori.int.t, seasori.add.t) # interaction model is significant better fit for GLMER models too
AIC(admbPOIS, seasori.int.t) # ADMB and GLMER poisson models are basically the same: use glmer for ease

#===========================================================================================

# List sex ratios:

setwd("G:/Statistics 2016/Group size/Data files for input to R")

mydata<- read.csv("Visitor_Origins_Long.csv", header=TRUE)
str(mydata)
library(plyr)
nonres<-subset(mydata, res.non.res=="NR")

nonres_summary <- ddply(nonres, c("Season", "origin", "Sex"), summarise,
                        N    = length(Sex))

# with territory
nonres_summary <- ddply(nonres, c("Territory", "Season", "origin", "Sex"), summarise,
                        N    = length(Sex))

# without origin for model below
nonres_summary <- ddply(nonres, c("Territory", "Season", "Sex"), summarise,
                        N    = length(Sex))
nonres_summary$Sex[nonres_summary$Sex=="Unknown"] <- NA # replace unknown category with NAs
nonres_summary$fTerritory <- factor(nonres_summary$Territory)
nonres_summary$fSeason <- factor(nonres_summary$Season)


# Are non-residents more often male than female?
sexmodel <- glmer(N ~ Sex*fSeason + (1|fTerritory), family=poisson(link="log"), data=nonres_summary)
deviance(sexmodel)/df.residual(sexmodel) # 0.505 underdispersed but NB prob wont be better so leave it.

# Stepwise model refinement
sexmodel_add <- glmer(N ~ Sex + Season + (1|Territory), family=poisson(link="log"), data=nonres_summary)
anova(sexmodel, sexmodel_add) # x2(1)=11.105, p=0.0008611*** # NEED INTERACTION

# Model checking
plot(sexmodel)
plot(fitted(sexmodel), resid(sexmodel)) 
lines(lowess(fitted(sexmodel), resid(sexmodel)))
# residuals in arch-shape. Bad. 
# Can't be bothered with trying to get this model to fit
# use wilcoxon test instead (to compare MEDIAN between pairs - as can only be male or female)
# exclude unknown sexes

### See Andy Field book section 15.5.4 ###
# wilcoxon signed rank test for matched pairs (i.e. paired data - to compare N males vs N females seen in each individual territory)
## specify by wilcox.test (a,b, paired=T)
# wilcoxom rank sum test for unmatched pairs (i.e. not pairs - to compare N males vs N females seen across whole dataset, regardless of territory)
## specify by wilcox.test (a,b, paired=F)


# Change rows in data frame to columns 
library(tidyr)
# syntax is: newdata <- tidyr::spread(mydata, predictor, response) 
tidy_nonres <- tidyr::spread(nonres_summary, Sex, N)
tidy_nonres$`<NA>` <- NULL # delete column
tidy_nonres$Female[is.na(tidy_nonres$Female)] <- 0  # replace NAs with zeros
tidy_nonres$Male[is.na(tidy_nonres$Male)] <- 0

# subset seasons
nonres_spr <- subset(tidy_nonres, Season==1)
nonres_sum <- subset(tidy_nonres, Season==2)
nonres_aut <- subset(tidy_nonres, Season==3)
nonres_win <- subset(tidy_nonres, Season==4)

# Wilcoxon rank sum test (equivalent to the Mann-Whitney U test) - for unmatched pairs
# null hypothesis is that the distributions of x and y differ by a location shift of zero
# 'cannot compute exact p-value with ties' means there are two or more identical values in the dataset (so ranks are not unique) - can ignore this error (see Field 2012 book)

WTspr <- wilcox.test(nonres_spr$Female, nonres_spr$Male, paired=F) # W = 19, p-value = 0.5178
# signed rank test if set paired=T: V = 1.5, p-value = 0.1344 #V=13.5 (same pval)if do Male,Female rather than Female,Male in code

WTsum <- wilcox.test(nonres_sum$Female, nonres_sum$Male, paired=F) # W = 25, p-value = 0.2814
# V = 6, p-value = 0.1736 # V=0 (same pval) if do Male,Female rather than Female,Male in code

WTaut <- wilcox.test(nonres_aut$Female, nonres_aut$Male, paired=F) # W = 17, p-value = 0.3631
# V = 2.5, p-value = 0.0583 #V=25.5 (same pval) if do Male,Female rather than Female,Male in code

WTwin <- wilcox.test(nonres_win$Female, nonres_win$Male, paired=F) # W = 0, p-value = 0.002045
# V = 0, p-value = 0.02225 #V=28 (same pval) if do Male,Female rather than Female,Male in code

# CALCULATE EFFECT SIZES
# Function from Andy Field 2012 DSUR book:
rFromWilcox <- function(wilcoxModel, N){ # N is no. observations (not groups) so 14 for 7 territories with 7 males and 7 for females in each territory = 14
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/sqrt(N)
  cat(wilcoxModel$data.name, "Effect size, r = ", r)
}
rFromWilcox(WTspr, N=14) ##rank sum: r =  -0.1728491
##signed rank: -0.4000661 
rFromWilcox(WTsum, N=14) ##rank sum: r =  -0.2879018
##signed rank: -0.3636965
rFromWilcox(WTaut, N=14) ##rank sum: r =  -0.2430862
##signed rank: -0.5060481
rFromWilcox(WTwin, N=14 ) ##rank sum: r =  -0.824126
##signed rank: -0.6109598

