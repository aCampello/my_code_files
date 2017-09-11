### VISUALIZING DATA BEFORE STARTING ANALYSIS

library(lattice)
# view both males and females on same axes
densityplot(~VisDur.S, groups=Sex, auto.key=TRUE, data=foxdatasubset)

hist(foxdatasubset$VisDur.S)
scatterplot(SeasonID ~ VisDur.S, data=foxdatasubset, ellipse=TRUE)

# boxplots are a better graph than means-plus-confidence intervals for data that aren't normally distributed 
# and/or are skewed in distribution. Indeed, using boxplot gives you a quick check on whether data are skewed.

boxplot(foxdatasubset$VisDur.S~foxdatasubset$SeasonID, notch=T) # plot entire variable (visit duration)
boxplot(foxdatasubset$VisDur.S~foxdatasubset$PatchID) # plot vis.duration separately for each patch
par(mar=c(5,6,4,2)+0.1) #resize margins 
attach(foxdatasubset)
interaction.plot(Season, AnimalID, VisDur.S, ylim=c(0,700))

#multipanel boxplots to study interactions
bwplot(VisDur.S~Sex|SocialStatus, auto.key=TRUE, ylab=list('Frequency', cex=1.5), xlab=list('Sex',cex=1.5))
xyplot(VisDur.S~Season|Sex, type=c("p", "r"))
xyplot(VisDur.S~Season, groups=Sex, type=c("p", "r"))



library(lattice)
stripchart(foxdatasubset$VisDur.S, pch=21, cex=2, xlab='a label', cex.lab=1.5, method="jitter")
stripchart(foxdatasubset$VisDur.S~foxdatasubset$SeasonID)
stripchart(foxdatasubset$VisDur.S~foxdatasubset$SeasonID, method='stack') # stacks multiple occurrences of same value
stripchart(foxdatasubset$VisDur.S~foxdatasubset$Season, method='jitter') # causes points with the same values to be
# slightly displaced ('jittered') from each other

#plot(x,y) or plot(y~x)
plot(foxdatasubset$VisDur.S) # plots y against row number - useful for checking typos etc
plot(foxdatasubset$SeasonID, foxdatasubset$VisDur.S)
plot(foxdatasubset$VisDur.S ~ foxdatasubset$SeasonID) #looks exaclty the same

# BARPLOTS: plot the frequency (counts) of a categorical variable as a barchart
barplot(table(foxdatasubset$BeforeMidnight))
# barplot(table(x,y),beside=T) to include variables
table(foxdatasubset$SeasonID, foxdatasubset$BeforeMidnight) # produces table of frequencies of each category in the variable y

barplot(table(foxdatasubset$SeasonID, foxdatasubset$BeforeMidnight), beside=F) # stacked bar plot
barplot(table(foxdatasubset$SeasonID, foxdatasubset$BeforeMidnight), beside=T) # unstacked bar plot
barplot(table(foxdatasubset$BeforeMidnight, foxdatasubset$SeasonID), beside=T) # unstacked bar plot
legend("topleft",                                            # Add a legend to the plot  
       legend=c("Before midnight", "After midnight"),             # Text for the legend  
       fill=c("grey20", "grey90"))                # colours

barplot(table(foxdatasubset$TimeOfDay, foxdatasubset$SeasonID), beside=T) # unstacked bar plot (crepusc, day, night)
legend("topleft",                                            # Add a legend to the plot  
        legend=c("Crepuscular", "Day", "Night"),             # Text for the legend  
        fill=c("grey20", "grey70", "grey90"))                # colours


# interaction.plot(x1,x2,y)
interaction.plot(foxdatasubset$SocialStatus, foxdatasubset$Sex, foxdatasubset$VisDur.S) 
interaction.plot(foxdatasubset$Sex, foxdatasubset$SocialStatus, foxdatasubset$VisDur.S) 
# males visit longer than females
# larger status effect for females on visit duration: dom females stay longer than sub females
# sub males stay slightly longer than dom males

interaction.plot(foxdatasubset$SocialStatus, foxdatasubset$SeasonID, foxdatasubset$VisDur.S) 
# doms longer than subs in sum+aut
# subs longer than doms in spring and winter

interaction.plot(foxdatasubset$SeasonID, foxdatasubset$Sex, foxdatasubset$VisDur.S)
interaction.plot(foxdatasubset$Sex, foxdatasubset$SeasonID, foxdatasubset$VisDur.S) 
# males longer than fems in all seasons apart from winter

interaction.plot(foxdatasubset$BeforeMidnight, foxdatasubset$SeasonID, foxdatasubset$VisDur.S) 
# longer visits before midnight in all seasons, but less of a difference in winter

interaction.plot(foxdatasubset$OtherFoxesPresent, foxdatasubset$BeforeMidnight, foxdatasubset$VisDur.S) 
#shared visits longer than unshared, but particularly before midnight
# could be that visits are longer anyway, just allowing >chance of encountering another fox

interaction.plot(foxdatasubset$OtherFoxesPresent, foxdatasubset$SeasonID, foxdatasubset$VisDur.S) 
#shared visits are longer than unshared, especially in spring
# could be that visits are longer anyway, just allowing >chance of encountering another fox

interaction.plot(foxdatasubset$OtherFoxesPresent, foxdatasubset$Sex, foxdatasubset$VisDur.S)
# during shared visits, males stay longer than females

interaction.plot(foxdatasubset$OtherFoxesPresent, foxdatasubset$SocialStatus, foxdatasubset$VisDur.S) 
# status does not affect duration of shared visits

interaction.plot(foxdatasubset$Nsharers, foxdatasubset$SeasonID, foxdatasubset$VisDur.S) 
# this is prob just an artefact, since foxes are more likely to encounter more different foxes during longer visits
# can't assume that when more foxes are present, foxes stay longer

interaction.plot(foxdatasubset$SeasonID, foxdatasubset$Nsharers, foxdatasubset$VisDur.S) 
# no clear pattern - was hoping that visits would be shorter as more foxes present, but longer visits allow more time for
# other foxes to appear at the same patch....
### DONT BOTHER TESTING EFFECT OF NUMBER OF SHARERS

interaction.plot(foxdatasubset$OtherSpeciesPresent, foxdatasubset$PatchID, foxdatasubset$VisDur.S)
# clear effect of patchID on pattern of visit duration differences when other species are present/not
interaction.plot(foxdatasubset$OtherFoxesPresent, foxdatasubset$PatchID, foxdatasubset$VisDur.S)
# as above
## suggests should include level 1 explanatory variables (patch attributes: quality!)
                                               
interaction.plot(foxdatasubset$SampleSeasonYear, foxdatasubset$SeasonID, foxdatasubset$VisDur.S)
# season effect on visdur differed between years: so year should be included in the model

pairs(foxdatasubset$VisDur.S~foxdatasubset$Sex+foxdatasubset$SocialStatus, panel=panel.smooth)
coplot(foxdatasubset$VisDur.S~foxdatasubset$Sex|foxdatasubset$SocialStatus)
xyplot(foxdatasubset$VisDur.S~foxdatasubset$SeasonID, groups=foxdatasubset$Sex, type=c("r", "p"),  method='jitter', pch=16, cex=2)

### REORDER THE FACTOR 'SEASONYR' ## NB if x (the variable to reorder) is not a factor, use levels(factor(x))
# show levels of factor, in order - reorder season-year
print(levels(foxdatasubset$SeasonYr))  
foxdatasubset$SeasonYrNumb <- factor(foxdatasubset$SeasonYr,levels(foxdatasubset$SeasonYr)[c(5,1,7,3,6,2,8,4)]) # numbers refer to order in print() list
print(levels(foxdatasubset$SeasonYrNumb)) # show levels of re-ordered factor, in order
# re-order sex
print(levels(foxdatasubset$Sex))
foxdatasubset$SeasonYrNumb <- factor(foxdatasubset$Sex,levels(foxdatasubset$Sex)[c(2,1,3)])

#######################



