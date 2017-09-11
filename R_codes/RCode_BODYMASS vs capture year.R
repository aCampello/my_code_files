#### Has fox body mass in Bristol increased over the past 30 years?
# Based on the first adult body weight of each fox when they were not pregnant or infected with mange.

# import data
bodymassdata <-read.csv("First adult weight by year weighted no preg or mange 160117.csv", header=T)
str(bodymassdata)

# take separate subsets for males and females, excluding captures before 1985
allBMs <- subset(bodymassdata, CaptureYear>1984)
maleBMs <- subset(bodymassdata, Sex=="Male" & CaptureYear>1984)
femaleBMs <- subset(bodymassdata, Sex=="Female" & CaptureYear>1984) # also excl capture year <1984

# get sample sizes
length(maleBMs$FoxID) #285
length(femaleBMs$FoxID) #304

# check distributions
hist(maleBMs$Weight)
hist(femaleBMs$Weight)

# general linear models
lm(Weight~CaptureYear, data=subset(allBMs, Sex=="Male"))
   
malemod <- lm(Weight~CaptureYear, data=subset(allBMs, Sex=="Male")) # not sig 
summary(malemod)
hist(resid(malemod))
malemod0 <- lm(Weight~1, data=subset(allBMs, Sex=="Male")) 
lmtest::lrtest(malemod0, malemod) # x2(1)=0.032, p=0.859

femalemod<- lm(Weight~CaptureYear, data=subset(allBMs, Sex=="Female")) # sig!
summary(femalemod)
hist(resid(femalemod))
femalemod0 <- lm(Weight~1, data=subset(allBMs, Sex=="Female")) 
lmtest::lrtest(femalemod0, femalemod) # x2(1)=2.578, p=0.108

# plot M & F on same axes
plot(allBMs$CaptureYear, allBMs$Weight, col=allBMs$Sex) # m=red, f=black
abline(malemod, col="red")
abline(femalemod, col="black")

# plot M + F separately
plot(maleBMs$CaptureYear, maleBMs$Weight, col="black")
lines(smooth.spline(maleBMs$CaptureYear, maleBMs$Weight))
plot(femaleBMs$CaptureYear, femaleBMs$Weight, add=T) 
lines(smooth.spline(femaleBMs$CaptureYear, femaleBMs$Weight), col="red")

# correlation test
cor.test(maleBMs$CaptureYear, maleBMs$Weight, exact = T, conf.level = 0.95) 
cor.test(femaleBMs$CaptureYear, femaleBMs$Weight, exact = T, conf.level = 0.95) 
# p-value shows likelihood that the observed correlation occured by chance, so if have strong correlation 
# but non-sig p-val then corr is prob due to chance/error/data sampling and not really extant in the pop.



#### PLOTTING ####
# Plot mean body mass for males and females per year, with SE error bars


library(plyr)

# mean weight per year and sex - MALES
malemeanweights <- ddply(maleBMs, c("CaptureYear"), summarise,
                     meanWeight = mean(Weight),
                     N = length(FoxID))
# mean annual sample size per sex
min(malemeanweights$N)


# mean weight per year and sex - FEMALES
femalemeanweights <- ddply(femaleBMs, c("CaptureYear"), summarise,
                         meanWeight = mean(Weight),
                         N = length(FoxID))
# mean annual sample size per sex
min(femalemeanweights$N)


# mean weight per year and sex - ALL FOXES
meanweights <- ddply(allBMs, c("Sex", "CaptureYear"), summarise,
                            N = length(FoxID),
                            meanWeight = mean(Weight),
                            sd = sd(Weight),
                            se = sd / sqrt(N))

library(ggplot2)

ggplot(meanweights,aes(x=CaptureYear,y=meanWeight,shape=Sex, col=Sex)) + 
  geom_point(size=3, position=position_dodge(0.5)) +
  geom_errorbar(aes(ymin=meanweights$meanWeight-se, ymax=meanweights$meanWeight+se),
                size=1, position=position_dodge(0.5)) +
  scale_colour_manual(values=c('black','grey60')) +
  theme_bw() + # must put theme_bw FIRST or it won't work
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA),
        legend.position = c(0.93,0.945),
        panel.border = element_rect(linetype = "solid", colour = "black", size=1.1)) +
  xlab("\nYear of capture") + ylab("Body mass (kg)\n") +
  scale_y_continuous(breaks=c(1:10)) +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) +
  geom_smooth(method=lm, lty="dotted", size=1,se=F) #lm linear trend line
  geom_smooth(lty="dotted", size=1,se=F) # loess smoothing trend line
  
# Save
dev.print(jpeg, "Rplot_MF mean adult body mass by capture year with SE error bars.jpeg", res=700, height=15, width=20, units="cm") # save as jpeg
dev.print(jpeg, "Rplot_MF mean adult body mass by capture year with SE error bars_lm trendline.jpeg", res=700, height=15, width=20, units="cm") # save as jpeg
dev.print(jpeg, "Rplot_MF mean adult body mass by capture year with SE error bars_lm trendline+lm SE ribbon.jpeg", res=700, height=15, width=20, units="cm") # save as jpeg
dev.print(jpeg, "Rplot_MF mean adult body mass by capture year with SE error bars_loess trendline.jpeg", res=700, height=15, width=20, units="cm") # save as jpeg


################################

# IGNORE:
# plot with linear models - SE ribbon looks far too narrow and LMs weren't significant anyway
ggplot(allBMs,aes(x=CaptureYear,y=Weight,shape=Sex, col=Sex)) + 
  geom_smooth(method=lm, size=1, se=TRUE) + # se=true adds a ribbon for the SE
  geom_point(size=2.5, position=position_dodge(0.5)) + 
  scale_colour_manual(name="Sex", values=c('black','grey60')) +
  scale_shape_manual(values=c(16, 1)) +
  theme_bw() + # must put theme_bw FIRST or it won't work
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        panel.border = element_rect(linetype = "solid", colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(colour="white")) +
  xlab("") + ylab("Body mass (kg)\n") +
  scale_y_continuous(breaks=c(1:10))

# Save
dev.print(jpeg, "Rplot_MF adult body mass by capture year with LM lines & SE ribbons.jpeg", res=700, height=15, width=20, units="cm") # save as jpeg
