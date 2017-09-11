#############################################################################################
# LOOKING AT DAILY PATCH VISIT FREQUENCY
#############################################################################################

# Q: are there sex, status, season and patch effects on daily patch visit frequency?

## 1. summarise visit length and frequency by individual by day
visits.day <- ddply(foxdatasubset, c("Name", "AnimalID", "SocialStatus", "Sex", "Season", 
                                    "SampleSeasonYear", "SeasonYr", "PatchID", "PatchQuality", 
                                    "TRANScDate"), 
                                    summarise,
                                    sumvisdur=sum(VisDur.S),
                                    nvisits=length(VisDur.S))
hist(visits.day$nvisits)
visits.day$log_nvisits<-log(visits.day$nvisits) # log transformation
hist(visits.day$log_nvisits) # still not normal
visits.day$sqrt_nvisits<-sqrt(visits.day$nvisits) # square root transformation should be used for count data
hist(visits.day$sqrt_nvisits) # still not normal


attach(visits.day)
dailyvisitfreq<-data.frame(as.numeric(Sex), as.numeric(SocialStatus), as.numeric(Season), as.numeric(SampleSeasonYear), as.numeric(PatchQuality), as.numeric(nvisits))
detach(visits.day)
str(dailyvisitfreq)

chart.Correlation(dailyvisitfreq, 
                  method="spearman", # Pearson is default but Spearman is more appropriate for non-normal data
                  histogram=TRUE,
                  pch=16)

par(mfrow=c(2,2)) 
boxplot(visits.day$nvisits~visits.day$Sex, main="Daily patch visit frequency by sex")
boxplot(visits.day$nvisits~visits.day$SocialStatus, main="Daily patch visit frequency by status")
boxplot(visits.day$nvisits~visits.day$Season, main="Daily patch visit frequency by season")
boxplot(visits.day$nvisits~visits.day$PatchQuality, main="Daily patch visit frequency by patch quality")
boxplot(visits.day$nvisits~visits.day$SampleSeasonYear, main="Daily patch visit frequency by survey-year")
boxplot(visits.day$nvisits~visits.day$SeasonYr, main="Daily patch visit frequency by survey-year")
boxplot(visits.day$nvisits~visits.day$SampleSeasonYear*visits.day$Season, main="Daily patch visit frequency by survey-year")
# Autumn was quite different between 2013 + 2014

boxplot(visits.day$sumvisdur~visits.day$Sex, main="Total patch residence time (s) per day by sex")
boxplot(visits.day$sumvisdur~visits.day$SocialStatus, main="Total patch residence time (s) per day by sex and status")
boxplot(visits.day$sumvisdur~visits.day$Sex*visits.day$SocialStatus, main="Total patch residence time (s) per day by sex")
boxplot(visits.day$sumvisdur~visits.day$Season, main="Total patch residence time (s) per day by season")
boxplot(visits.day$sumvisdur~visits.day$PatchQuality, main="Total patch residence time (s) per day by patch quality")
boxplot(visits.day$sumvisdur~visits.day$SampleSeasonYear, main="Total patch residence time(s) per day by survey-year")
boxplot(visits.day$sumvisdur~visits.day$SeasonYr, main="Total patch residence time (s) per day by season")
boxplot(visits.day$sumvisdur~visits.day$SampleSeasonYear*visits.day$Season, main="Total patch residence time(s) per day by survey-year")
dev.off()

### 5.1.1 Null single level model of daily visit frequency
visits.SLnull1<- lm(nvisits ~ 1, data = visits.day)

# Null multilevel model of visit duration with only random effects (of Patch ID) = an 'unconditional model'
visits.MLnull1 <- lmer(nvisits ~ 1 + (1 | PatchID),  data = visits.day, REML = FALSE)

#compare fit of single and multilevel model (must put most complex model first)
anova(visits.MLnull1, visits.SLnull1)

visits.MLnull2 <- lmer(nvisits ~ 1 + (1 | PatchID)+ (1|AnimalID),  data = visits.day, REML = FALSE)
anova(visits.MLnull2, visits.MLnull1)

par(mfrow=c(2,2))
qqp(resid(visits.MLnull2))
plot(fitdist(residuals(SLnull1),"norm")) # not normal
plot(fitdist(residuals(visits.MLnull2),"norm"))  # NORMAL?!!!!

par(mfrow=c(2,2)) 
hist(resid(visits.MLnull2))
scatter.smooth(residuals(visits.MLnull2)
               ~fitted(visits.MLnull2)) # Scatter plot of residuals with a line-of-best-fit: should be random, but mine are 
qqnorm(residuals(visits.MLnull2)) # QQNormality plot (my data are skewed)
qqline(residuals(visits.MLnull2)) # Add line to QQplot
descdist(residuals(visits.MLnull2), boot = 1000)



# GLMM using poisson, as data are counts
head(visits.day)
visfreqmod<-glmer(nvisits ~ Season + (1|AnimalID) + (1|PatchID), family="poisson"(link="log"), 
             data=visits.day, verbose=F)

visfreqmod<-glmer(nvisits ~ Sex + SocialStatus + Sex:SocialStatus + Season + PatchQuality + SampleSeasonYear +
               (1|AnimalID) + (1|PatchID), family="poisson"(link="log"), data=visits.day, verbose=F)


summary(visfreqmod)
plot(fitdist(residuals(visfreqmod),"norm")) 
descdist(residuals(visfreqmod), boot = 1000) # lognormal??

