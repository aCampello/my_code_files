#SETTING UP:

setwd("E:/Statistics 2016/Methods chapter - stats and plots/Methods - TimeIntervals_Mean_Median")
mydata <- read.table(file = "TimeIntsR.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
View(mydata)

library(plyr) #package needed to group data for summary stats
library(ggplot2)

#Take subset of data that excludes unknown social status (!= means not-equal-to, == means equal-to)
data3 <- subset(mydata,Social.status != "Unknown")

#check order seasons are displaying in
levels(data3$Season)

#reorder Seasons ########After this I can refer to mydata$season3 as just Season3 if want to order seasons in this way
mydata$Season3 <- factor(mydata$Season, levels=c("Spring", "Summer", "Autumn ", "Winter"))

#reorder Sex as Male-Female instead of the default Female-Male
mydata$Sex <- factor(mydata$Sex, levels=c("Male", "Female"))

#Update the subset of data that does not include foxes with social status=unknown 
data3 <- subset(mydata,Social.status != "Unknown")

################################

#TO DRAW BOX PLOTS ON A PANEL

#Facet_Grid command to plot this subset as separate boxplots for sex and status, as a panel (is automatically a 2x2 panel based on number of variables)
ggplot(data3,aes(x=Season3,y=ti.threshold.s,colour=Sex,linetype=Social.status)) +
  scale_colour_manual(values = c("blue","red")) + #set males to blue and females to red 
  xlab(" ") + ylab("Mean threshold time interval (min)\n") +
  geom_boxplot() + theme_bw(base_size = 18, base_family = "") + facet_grid(Social.status~Sex)


################################

#TO DRAW LINE PLOTS

###Re-run the ti.sumtable code to get means omitting foxes with social status=unknown AND CALCULATE STANDARD ERROR FOR ERROR BARS
cdata <- ddply(data3, c("Sex", "Social.status", "Season3"), summarise, #Season3 refers to ordered levels
               N    = length(ti.threshold.s),
               mean = mean(ti.threshold.s),
               sd   = sd(ti.threshold.s),
               se   = sd / sqrt(N))   #make summary table 

#Re-convert season to numeric
cdata$num <- as.numeric(cdata$Season3)

#Plot means as points joined by lines ...because we refer to the numeric form of Season, called 'num'
ggplot(cdata,aes(x=num,y=mean,colour=Sex,linetype=Social.status)) + geom_point() + theme_bw(base_size = 18, base_family = "") + facet_wrap(~ Sex) + geom_line(size=1) +
  scale_colour_manual(values = c("blue","red")) + #set males to blue and females to red
  xlab(" ") + ylab("Mean threshold time interval (min)\n") + #adding "\n" after axis title adds more space between the label and the axis
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 

####cUSTOMISING THE GRAPH - LABELS, SCALE ETC
#Save the core code for the plot as new object boxplotcode to simplify commands:
boxplotcode<-ggplot(cdata,aes(x=num,y=mean,colour=Sex,linetype=Social.status)) + geom_point(size=3) + 
  geom_errorbar(data=cdata, mapping=aes(x=num, ymin=mean-se, ymax=mean+se), width=0.2, size=0.5, color="black") +
  theme_bw(base_size = 18, base_family = "") + facet_wrap(~ Social.status) + geom_line(size=1)

#Then add labels to the boxplotcode:
boxplotcode + xlab(" ") + ylab("Mean threshold time interval (min)\n") +
  scale_colour_manual(values = c("blue","red")) + #set males to blue and females to red
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter")) 
  #had to change scale_x_discrete to scale_x_continous to make x axis the right length!

#=================================================

#### MODELS

# Does sex or social status affect time interval threshold? 
mydata <- read.table(file = "TimeIntsR.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
View(mydata)

data3 <- subset(mydata,Social.status != "Unknown")
str(data3) #check for factors
data3$relev.sex <- relevel(factor(data3$Sex),"Male")
data3$relev.season<-relevel(factor(data3$Season), "Spring")

#run a General Linear Model (fixed effects model / multiple regression)
summary(fit<-lm(ti.threshold.s ~ relev.sex + Social.status + relev.season, data = data3))
summary(fit)


#### NEED MIXED MODELS TO ACCOUNT FOR REPEATED MEASURES OF INDIVIDUAL FOXES
library(lme4)

# run linear mixed effects model on effect of sex on threshold
summary(fit2 <- lmer(ti.threshold.s ~(1|relev.sex), data = data3, REML=FALSE))

# linear mixed effects model on effects of sex and status
summary(fit3 <- lmer(ti.threshold.s ~relev.season +(1|relev.sex), data = data3, REML=FALSE))

#NORMALITY PLOT and HISTOGRAM to determine if data are normal.
hist(mydata$ti.threshold.s) - negative skew, not normal
qqnorm(mydata$ti.threshold.s)
qqline(mydata$ti.threshold.s) #ADD LINE TO NORMALITY PLOT

#=======================

#### EFFECT OF SEASON ON time interval threshold

# LMM to test effect of season
#reorder Seasons ########After this I can refer to mydata$season3 as just Season3 if want to order seasons in this way
mydata$seasonORD <- factor(mydata$Season, levels=c("Spring", "Summer", "Autumn ", "Winter"))

# make spring the reference category so model results easier to interpret
mydata$relev.season <-relevel(factor(mydata$Season), "Spring")

# run model
mod <- lmer(ti.threshold.s ~ seasonORD + (1|Fox.name), REML=F, data=mydata)

#null model
mod0 <- lmer(ti.threshold.s ~ 1 + (1|Fox.name), REML=F, data=mydata)

anova(mod, mod0) # x2(3)=14.79, p=0.002005**

#model checking
hist(resid(mod))
plot(mod)
plot(fitted(mod), residuals(mod))
lines(lowess(fitted(mod), residuals(mod))) # not amazing but prob OK... (hist looks good)

# save coefficients
a <- coef(summary(mod))

# post hoc to see which seasons differ significantly
a <- pairs(lsmeans::lsmeans(mod, ~seasonORD))
b <- data.frame(summary(a))
c <- data.frame(summary((lsmeans::lsmeans(mod, ~seasonORD))))


# show number of foxes in each season
library(plyr)
a <- ddply(mydata, "seasonORD", summarise, 
           N=length(unique(Animal.ID)))

# get overall N foxes (as some seen in multiple seasons)
length(unique(mydata$Animal.ID))



