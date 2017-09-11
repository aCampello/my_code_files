
#===============================
#  Fisher's omnibus test 
#===============================

# To combine P-values from Manly/Bejder test in SOCPROG for the test of pref/avoided associations

# Omnibus tests = a test of differences among means, e.g. ANOVA, 
# that can detect deviations from normality due to skew or kurtosis (arching e.g. the height of the peak in a histogram/density plot)

rm(list=ls()) #to clear the R workspace
dev.off()

setwd("E:/Statistics 2016/SNA/Social diffs & Fishers Om Test")

library(plyr)
library(ggplot2)
library(metap) # package with various methods to combine p-values
citation("metap") # see how to cite the packages
citation() # to cite R itself

# import dataset
mydata <- read.csv(file = "Table of social differentiations for R 240416.csv", header = TRUE) #import dataset
str(mydata)

mydata$Season <- factor(mydata$Season,levels(mydata$Season)[c(2,3,4,1,5)]) #change order

# convert season to a factor for easier labelling
# mydata$seasonfac<-as.factor(mydata$Season)

# Plot between-territory variation in CV between seasons:
boxplot(mydata$CV.likelihood~mydata$Season, main="Mean social differentiation (CVobs)")

# redraw boxplot with only 4 seasons, all without cubs # BEST
mydataNC<- subset(mydata, Season != "SummerC") # take subset excluding summer with cubs
mydataNC$Season<- factor(mydataNC$Season,levels(mydataNC$Season)[c(1,3,4,5)]) # remove summerC from levels of season
mydataNC$SeasonNEW <- revalue(mydataNC$Season, c("Spring"="Spring", "SummerNC"="Summer", 
                                                 "Autumn" = "Autumn", "Winter"="Winter")) # Remove "NC" from summer label
dev.off()
boxplot(mydataNC$CV.likelihood~mydataNC$SeasonNEW, main="Mean social differentiation (CVobs)", ylab="CV") # BEST

dev.print(jpeg, "Rplot_Boxplot of CV between seasons.jpeg", res=800, height=6, width=8, units="in")

# plot separate lines for each territory to see if trends similar in diff territories
ggplot(mydataNC, aes(x = as.numeric(Season), y = CV.likelihood, colour = factor(Territory))) + 
  geom_point(size=3) + geom_line(size=1) +
  scale_colour_manual(values=c("yellow", "cyan", "red", "dodgerblue", "blue", "darkorange1", "darkmagenta"),
                      name="Territory",
                      labels=c("1", "2", "3", 
                               "4", "5", "6", "7")) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab("\nSeason") + ylab("CV\n") +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# Looks confusing so boxplot prob better to show high variation in spring+summer

#-------------------------------------------------------------------------------------------

# take subset of data omitting NAs (where assoc matrix was permuted so could not test for pref/av assocs)
subs<- subset(mydata, CV.permuted.P != "") # to allow means for spring and summer to be calculated

# take further subsets for each season
sub.spring<- subset(subs, Season == "Spring") 
sub.summerNC<- subset(subs, Season == "SummerNC") # adults only
sub.summerC<- subset(subs, Season == "SummerC") # including cubs
sub.autumn<- subset(subs, Season == "Autumn") 
sub.winter<- subset(subs, Season == "Winter") 

####################

#### Combine p-values using Fisher's Omnibus Test (aka. the 'Omnibus F-Test' / 'Fisher's Test')

# To combine p-values by the sum of logs method, also known as Fisher's method:
# a chi-squared test with 2k df, where k = sample size
# template code for Fisher's omnibus test: sumlog(p) ### where p = a vector of p-values

# First create vectors of p-values for each season
sspr <-c(sub.spring$CV.permuted.P)
ssumNC <-c(sub.summerNC$CV.permuted.P)
ssumC <-c(sub.summerC$CV.permuted.P)
saut <-c(sub.autumn$CV.permuted.P)
swint <-c(sub.winter$CV.permuted.P)


# Visualise data by plotting the ascending p-values contained in a vector
par(mfrow= c(2, 3)) # prepare a 2x2 panel for plotting
schweder(sspr, xlab = "Rank of p", ylab = "p", main= "Spring") # plots graph suggested by Schweder and Spjotvol to display a collection of p-values
# REF = Schweder, T and Spjotvoll, E. Plots of P-values to evaluate many tests simultaneously. Biometrika, 69:493-502, 1982.
schweder(ssumNC, xlab = "Rank of p", ylab = "p", main= "SummerNC")
schweder(ssumC, xlab = "Rank of p", ylab = "p", main= "SummerC")
schweder(saut, xlab = "Rank of p", ylab = "p", main= "Autumn")
schweder(swint, xlab = "Rank of p", ylab = "p", main= "Winter")


# combine p-values (using Fisher's method)
combp.spr <-sumlog(sspr) # will give error saying 'some studies omitted' if any p-values are zero - change to 0.0001 etc.
combp.sumNC <-sumlog(ssumNC)
combp.sumC <-sumlog(ssumC)
combp.aut <-sumlog(saut)
combp.wint <-sumlog(swint)

# print the results 
print(combp.spr)
print(combp.sumNC)
print(combp.sumC)
print(combp.aut)
print(combp.wint)

# combine the results into a table
sp <-cbind(c(combp.spr))
suNC<-cbind(c(combp.sumNC))
suC<-cbind(c(combp.sumC))
au<-cbind(c(combp.aut))
wi<-cbind(c(combp.wint))
allseasons<-data.frame(sp, suNC, suC, au, wi)
allseasons # can only copy first 3 rows to paste into Excel, otherwise it'll all paste into the same cell!


# To combine p-values using Stoffer's method - command sumz() - for when you want to add 'weights' for p-values from different studies
# not relevant for me, but used in Jacoby's thesis. Darren Croft was his supervisor but recommended I did Fisher's test, so I will.
sumz(sspr)
sumz(ssumNC)
sumz(ssumC)
sumz(saut)
sumz(swint)

###########################################################################################
###########################################################################################
###########################################################################################


# Do same for 'permute groups within samples' p-values for mean, CV and SD (short term, long term and gregariousness)
# take  subsets for each season
sub.spring<- subset(mydata, Season == "Spring") 
sub.summerNC<- subset(mydata, Season == "SummerNC")
sub.autumn<- subset(mydata, Season == "Autumn") 
sub.winter<- subset(mydata, Season == "Winter") 

# First create vectors of p-values for each season
sspr<-c(sub.spring$Mean.p)
ssumNC<-c(sub.summerNC$Mean.p)
saut<-c(sub.autumn$Mean.p)
swint<-c(sub.winter$Mean.p)

# combine p-values (using Fisher's method)
combp.spr<-sumlog(sspr) # will give error saying 'some studies omitted' if any p-values are zero - change to 0.0001 etc.
combp.sumNC<-sumlog(ssumNC)
combp.aut<-sumlog(saut)
combp.wint<-sumlog(swint)

# print the results 
print(combp.spr)
print(combp.sumNC)
print(combp.aut)
print(combp.wint)

# combine the results into a table
sp<-cbind(c(combp.spr))
suNC<-cbind(c(combp.sumNC))
au<-cbind(c(combp.aut))
wi<-cbind(c(combp.wint))
allseasonsMEAN<-data.frame(sp, suNC, au, wi)

#============ Do same for CV_p =============#

# First create vectors of p-values for each season
sspr<-c(sub.spring$CV.p)
ssumNC<-c(sub.summerNC$CV.p)
saut<-c(sub.autumn$CV.p)
swint<-c(sub.winter$CV.p)

# combine p-values (using Fisher's method)
combp.spr<-sumlog(sspr) # will give error saying 'some studies omitted' if any p-values are zero - change to 0.0001 etc.
combp.sumNC<-sumlog(ssumNC)
combp.aut<-sumlog(saut)
combp.wint<-sumlog(swint)

# combine the results into a table
sp<-cbind(c(combp.spr))
suNC<-cbind(c(combp.sumNC))
au<-cbind(c(combp.aut))
wi<-cbind(c(combp.wint))
allseasonsCV<-data.frame(sp, suNC, au, wi)


#============ Do same for SD_p =============#

# First create vectors of p-values for each season
sspr<-c(sub.spring$SD.p)
ssumNC<-c(sub.summerNC$SD.p)
saut<-c(sub.autumn$SD.p)
swint<-c(sub.winter$SD.p)

# combine p-values (using Fisher's method)
combp.spr<-sumlog(sspr) # will give error saying 'some studies omitted' if any p-values are zero - change to 0.0001 etc.
combp.sumNC<-sumlog(ssumNC)
combp.aut<-sumlog(saut)
combp.wint<-sumlog(swint)

# combine the results into a table
sp<-cbind(c(combp.spr))
suNC<-cbind(c(combp.sumNC))
au<-cbind(c(combp.aut))
wi<-cbind(c(combp.wint))
allseasonsSD<-data.frame(sp, suNC, au, wi)

#######################################################################################

# Summarise data for plotting trends... (just exploring)

sumsocdiff <- ddply(subs, c("Season"),
                  summarise,
                  mean.assocs.dyad=mean(Mean.N.associations.per.dyad),
                  SD.assocs.dyad=sd(Mean.N.associations.per.dyad),
                  mean.CVlik=mean(CV.likelihood),
                  SD.CVlik=sd(CV.likelihood),
                  mean.rlik=mean(r.likelihood),
                  SD.rlik=sd(r.likelihood),
                  mean.CVpermP=mean(CV.permuted.P),
                  sd.CVperm=sd(CV.permuted.P),
                  N.networks=length(Season))
sumsocdiff


plot(sumsocdiff$Season,sumsocdiff$mean.CVlik, main="Mean social differentiation (S)") # clear seasonal diffs

plot(sumsocdiff$Season,sumsocdiff$mean.rlik, main="Mean correlation coefficient (r)") # clear seasonal diffs

plot(sumsocdiff$Season,sumsocdiff$mean.CVpermP, main="Mean significance of network deviation from random (P-value)")

plot(sumsocdiff$Season,sumsocdiff$N.networks, main="Number of networks") # clear seasonal diffs

###############################

# plot territories as separate lines - need to make territory a factor:
mydata$territoryfac<-as.factor(mydata$Territory) #if I plot mydata and not the subset, the lines are not connected across NA values
subs$territoryfac<-as.factor(subs$Territory) # subs - data are considered continuous when NAs are omitted, so lines are all connected

plot_colours <- c("red","darkorange1", "forestgreen", "dodgerblue", "blue", "purple", "black")

myplot<-ggplot(subs, aes(x = as.numeric(Season), y = CV.permuted.P, colour = territoryfac)) + 
  geom_point(size=4) + geom_line(size=1) +
    scale_colour_manual(values=plot_colours,
                        name="Territory",
                        labels=c("1", "2", "3", 
                                 "4", "5", "6", "7")) +
  theme_bw(base_size = 20, base_family = "") +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) +
  xlab("\nSeason") + ylab("Group size\n") +
  xlab("\nSeason") + ylab("p-value\n") 

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), 
               axis.line = element_line(colour = "black")) #to make axes darker


###################################

#### plot n. identifications and associations etc to see if sample size affects power and p values

# plot relationship between P value and number of identifications in survey
par(cex=1.25) #make plot area bigger
lm1 <- lm(subs$CV.permuted.P~subs$Nidentifications) # fit linear model to relationship between P value and number of identifications in survey
plot(subs$CV.permuted.P~subs$Nidentifications, xlab="N identifications", ylab="Bejder test p-value") # scatter plot 
abline(lm1) # add line of best fit (regression line)
summary(lm1) # relationship is not significant (p-value on bottom row)

# plot relationship between P value and number of associations per dyad
lm2 <- lm(subs$CV.permuted.P~subs$Mean.N.associations.per.dyad) # fit linear model 
summary(lm2) # relationship not significant
lm2.p<-lmp(lm2) # extract p value from the model (using the function below)
plot(subs$CV.permuted.P~subs$Mean.N.associations.per.dyad, xlab="Mean N associations per dyad", ylab="Bejder test p-value",
     main=paste("Adjusted R-sq =",format(summary(lm2)$adj.r.squared, digits=3),"\n", paste("P = ",format(lm2.p, digits=3)))) # scatter plot 
abline(lm2, col="red") # add regression line


# plot relationship between P value and number of associations per node
lm3 <- lm(subs$CV.permuted.P~subs$Mean.N.associations.per.node) # fit linear model
plot(subs$CV.permuted.P~subs$Mean.N.associations.per.node, xlab="Mean N associations per node", ylab="Bejder test p-value") # scatter plot 
abline(lm3) # add regression line
summary(lm3) # relationship not significant

str(subs) 
# plot relationship between power and number of identifications in survey
lm4 <- lm(subs$r.likelihood~subs$Nidentifications) # fit linear model to relationship between P value and number of identifications in survey
plot(subs$r.likelihood~subs$Nidentifications, xlab="N identifications", ylab="Correlation coefficient r (power)") # scatter plot 
abline(lm4) # add line of best fit (regression line)
summary(lm4) # relationship is not significant (p-value on bottom row)


# plot relationship between power and number of associations per dyad
lm5 <- lm(subs$r.likelihood~subs$Mean.N.associations.per.dyad) # fit linear model 
summary(lm5) # relationship is significant!!
lm5.p<-lmp(lm5) # extract p value from the model (using the function below)
plot(subs$r.likelihood~subs$Mean.N.associations.per.dyad, xlab="Mean N associations per dyad", ylab="Correlation coefficient r (power)",
     main=paste("Adjusted R-sq =",format(summary(lm5)$adj.r.squared, digits=3),"\n", paste("P = ",format(lm5.p, digits=3)))) 
          # draw scatter plot 
abline(lm5, col="red") # add regression line

# plot relationship between power and number of associations per node
lm6 <- lm(subs$r.likelihood~subs$Mean.N.associations.per.node) # fit linear model
summary(lm6) # relationship is significant!!
lm6.p<-lmp(lm6) # extract p value from the model (using the function below)
plot(subs$r.likelihood~subs$Mean.N.associations.per.node, xlab="Mean N associations per node", ylab="Correlation coefficient r (power)",
     main=paste("Adjusted R-sq =",format(summary(lm6)$adj.r.squared, digits=2),"\n", paste("P = ",format(lm6.p, digits=3)))) 
      # scatter plot 
abline(lm6, col="red") # add regression line

########## this last plot (lm6) could indicate to increase power i need to filter the data further somehow, to exclude nodes seen associated <5 times

#=====================================================================================
#### PLOT LAST 2 PLOTS ON PANEL:
par(mfrow=c(2,1))
# plot relationship between power and number of associations per node
plot(subs$r.likelihood~subs$Mean.N.associations.per.node, xlab="Mean associations per node", ylab="r")
legend("bottomright", bty="n", legend=paste(" R-sq =",format(summary(lm6)$adj.r.squared, digits=3),"\n", paste("p = ",format(lm6.p, digits=1))))
abline(lm6, col="red") # add regression line
# plot relationship between power and number of associations per dyad
plot(subs$r.likelihood~subs$Mean.N.associations.per.dyad, xlab="Mean associations per dyad", ylab="r")
abline(lm5, col="red") # add regression line
legend("bottomright", bty="n", legend=paste(" R-sq =",format(summary(lm5)$adj.r.squared, digits=3),"\n", paste("p = ",format(lm5.p, digits=1))))

dev.print(jpeg, "Rplot_Effect of sample size on power.jpeg", res=800, height=10, width=7, units="in") # SAVE AS JPEG
#=====================================================================================



# plot relationship between power and soc differentiation (CV)
lm7 <- lm(subs$r.likelihood~subs$CV.likelihood) # fit linear model
summary(lm7) # relationship not significant - more soc differentiated networks are not more powerful
lm7.p<-lmp(lm7) # extract p value from the model (using the function below)
plot(subs$r.likelihood~subs$CV.likelihood, xlab="Social differentiation (CV)", ylab="Correlation coefficient r (power)",
     main=paste("Adjusted R-sq =",format(summary(lm7)$adj.r.squared, digits=2),"\n", paste("P = ",format(lm7.p, digits=3)))) # scatter plot 
abline(lm7) # add regression line


##############################
# Function to extract the overall ANOVA p-value out of a linear model object
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
#######
names(lm8) # list terms in the model

# plot relationship between power and P
lm8 <- lm(subs$CV.permuted.P~subs$r.likelihood) # fit linear model 
summary(lm8) # relationship not significant
lm8.p<-lmp(lm8) # extract p value from the model (using the function below)
plot(subs$CV.permuted.P~subs$r.likelihood, xlab="Correlation coefficient r (power)", ylab="Bejder test p-value",
     main=paste("Adjusted R-sq =",format(summary(lm8)$adj.r.squared, digits=2),"\n", paste("P = ",format(lm8.p, digits=3)))) # scatter plot 
abline(lm8, col="red") # add regression line


# to add p value to top corner of plot:
legend("topright", bty="n", legend=paste(" R-squared =",format(summary(lm8)$adj.r.squared, digits=4),"\n", paste("P = ",format(lm8.p, digits=4))))
# or as a plot title, add this to the plot() command:
, main=paste("Adjusted R-sq =",format(summary(lm8)$adj.r.squared, digits=2),"\n", paste("P = ",format(lm8.p, digits=3)))




