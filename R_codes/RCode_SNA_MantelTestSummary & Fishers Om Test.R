rm(list=ls()) #to clear the R workspace
dev.off()

setwd("E:/Statistics 2016/SNA/Mantel tests to compare seasons & Fishers Om Test")

library(plyr)
library(metap) # package with various methods to combine p-values

# import dataset
mydata <- read.csv(file = "Mantel test results.csv", header = TRUE) #import dataset


# shorten variable names for easier coding:
mydata$tempsep<-(mydata$Temporal.separation.in.survey.sequence)
mydata$zp<-(mydata$Mantel.Z.test.p.value)
mydata$rp<-(mydata$Dietz.R.test..ranking..p.value)
mydata$z.corr<-(mydata$Mantel.Z.test.matrix.correlation)
mydata$r.corr<-(mydata$Dietz.R.test.matrix.RANK.correlation)

# summarise data for plotting trends... (just exploring)
str(mydata)
mantel.means <- ddply(mydata, c("Territory", 
                                "Season.Correlation.Tested", 
                                "tempsep", 
                                "RunFishersTest",
                                "N.individuals",
                                "Individuals"),
                                  summarise,
                                  mean.zp=mean(zp),
                                  SD.zp=sd(zp),
                                  SE.zp=SD.zp/length(zp), # I hope this is right!
                                  mean.rp=mean(rp),
                                  SD.rp=sd(rp),
                                  SE.rp=SD.rp/length(rp), # I hope this is right!
                                  z.corr=mean(z.corr),
                                  r.corr=mean(r.corr))

########################################################

# plotting to visualize/explore

mantel.means2 <- ddply(mydata, "tempsep", 
                      summarise,
                      mean.zp=mean(zp),
                      SD.zp=sd(zp),
                      mean.rp=mean(rp),
                      SD.rp=sd(rp),
                      z.corr=mean(z.corr),
                      r.corr=mean(r.corr))

hist(mantel.means2$mean.zp)
hist(mantel.means$mean.rp)
hist(mantel.means$z.corr)
hist(mantel.means$r.corr)
# log
hist(log(mantel.means$mean.zp))
hist(log(mantel.means$mean.rp))
hist(log(mantel.means$z.corr))
hist(log(mantel.means$r.corr))

########################################

# ATTEMPTING TO MODEL....

library(lme4)
nullmodel<-lmer(mean.rp ~ (1|Territory), REML=FALSE, data=mantel.means)
summary(nullmodel)
lmer(mean.rp ~ as.factor(tempsep) + (1|Territory), data=mantel.means)
lm(y~x)
lm1 <- lm(mean.rp ~ as.factor(tempsep) +
            N.individuals +
            Territory,
            data=mantel.means) # fit linear model
summary(lm1)
plot(lm1)

plot(mantel.means$mean.rp ~ mantel.means$tempsep)
plot(mean(mantel.means$mean.rp) ~ as.factor(mantel.means$tempsep))
mean(mantel.means$mean.rp)
abline(lm1)

###################################################

# FISHERS TO COMBINE P-VALUES TO TEST OVERALL BETWEEN-SEASON EFFECTS (AS IN CROFT 2008 BOOK P.146)
str(mantel.means)

# THESIS CORRECTIONS
# Take subset without duplicated seasons for TimeBetweenSeasons=1
## originally I combined p-values from Mantel tests between e.g. SP-SU, SU-AU & AU-WI, but Fishers 
## test assumes each combined p-value is independent, so using summer in two p-values violates this 
## assumption. So now I only combine p-vals from two pairs, e.g. SP-SU & AU-WI, so each season is only used once. 

mantel.means.sub <- subset(mantel.means, RunFishersTest=="Y")

# take subsets for each TERRITORY and TEMPSEP
  sg1.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 1)
  sg1.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 1) 
  sg1.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 1) 
  sg2.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 2)
  sg2.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 2) 
  sg2.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 2)
  sg3.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 3)
  sg3.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 3) 
  sg3.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 3)
  sg4.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 4)
  sg4.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 4)
  sg4.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 4) 
  sg5.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 5)
  sg5.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 5) 
  sg5.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 5) 
  sg6.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 6)
  sg6.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 6) 
  sg6.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 6) 
  sg7.t1<- subset(mantel.means.sub, tempsep == 1 & Territory == 7)
  sg7.t2<- subset(mantel.means.sub, tempsep == 2 & Territory == 7)
  sg7.t3<- subset(mantel.means.sub, tempsep == 3 & Territory == 7) 

# take subsets for each TEMPSEP (all territories grouped)
  all.t1<- subset(mantel.means.sub, tempsep == 1) 
  all.t2<- subset(mantel.means.sub, tempsep == 2) 
  all.t3<- subset(mantel.means.sub, tempsep == 3) 

# make vectors of P-values
  sg1.t1.p <- c(sg1.t1$mean.rp)
  sg1.t2.p <- c(sg1.t2$mean.rp)
  sg1.t3.p <- c(sg1.t3$mean.rp)
  sg2.t1.p <- c(sg2.t1$mean.rp)
  sg2.t2.p <- c(sg2.t2$mean.rp)
  sg2.t3.p <- c(sg2.t3$mean.rp)
  sg3.t1.p <- c(sg3.t1$mean.rp)
  sg3.t2.p <- c(sg3.t2$mean.rp)
  sg3.t3.p <- c(sg3.t3$mean.rp)
  sg4.t1.p <- c(sg4.t1$mean.rp)
  sg4.t2.p <- c(sg4.t2$mean.rp)
  sg4.t3.p <- c(sg4.t3$mean.rp)
  sg5.t1.p <- c(sg5.t1$mean.rp)
  sg5.t2.p <- c(sg5.t2$mean.rp)
  sg5.t3.p <- c(sg5.t3$mean.rp)
  sg6.t1.p <- c(sg6.t1$mean.rp)
  sg6.t2.p <- c(sg6.t2$mean.rp)
  sg6.t3.p <- c(sg6.t3$mean.rp)
  sg7.t1.p <- c(sg7.t1$mean.rp)
  sg7.t2.p <- c(sg7.t2$mean.rp)
  sg7.t3.p <- c(sg7.t3$mean.rp)
  
  all.t1.p <- c(all.t1$mean.rp)
  all.t2.p <- c(all.t2$mean.rp)
  all.t3.p <- c(all.t3$mean.rp)


# Fishers om test within territories & for all surveys grouped by tempsep
# will give error saying 'some studies omitted' if any p-values are zero - change to 0.0001 etc.
combp.sg1.t1<-sumlog(sg1.t1.p) 
combp.sg1.t2<-sumlog(sg1.t2.p) 
combp.sg2.t1<-sumlog(sg2.t1.p) 
combp.sg2.t2<-sumlog(sg2.t2.p) 
combp.sg3.t1<-sumlog(sg3.t1.p) 
combp.sg3.t2<-sumlog(sg3.t2.p)
combp.sg4.t1<-sumlog(sg4.t1.p) 
combp.sg4.t2<-sumlog(sg4.t2.p)
combp.sg5.t1<-sumlog(sg5.t1.p) 
combp.sg5.t2<-sumlog(sg5.t2.p)
combp.sg6.t1<-sumlog(sg6.t1.p) 
combp.sg6.t2<-sumlog(sg6.t2.p)
combp.sg7.t1<-sumlog(sg7.t1.p) 
combp.sg7.t2<-sumlog(sg7.t2.p)
    
combp.all.t1<-sumlog(all.t1.p)
combp.all.t2<-sumlog(all.t2.p)
combp.all.t3<-sumlog(all.t3.p)

# save results in a vector ready to combine into a data frame (this also prints the results)
    SG1T1.fish<-cbind(c(print(combp.sg1.t1)))
    SG1T2.fish<-cbind(c(print(combp.sg1.t2)))
    SG2T1.fish<-cbind(c(print(combp.sg2.t1)))
    SG2T2.fish<-cbind(c(print(combp.sg2.t2)))
    SG3T1.fish<-cbind(c(print(combp.sg3.t1)))
    SG3T2.fish<-cbind(c(print(combp.sg3.t2)))
    SG4T1.fish<-cbind(c(print(combp.sg4.t1)))
    SG4T2.fish<-cbind(c(print(combp.sg4.t2)))
    SG5T1.fish<-cbind(c(print(combp.sg5.t1)))
    SG5T2.fish<-cbind(c(print(combp.sg5.t2)))
    SG6T1.fish<-cbind(c(print(combp.sg6.t1)))
    SG6T2.fish<-cbind(c(print(combp.sg6.t2)))
    SG7T1.fish<-cbind(c(print(combp.sg7.t1)))
    SG7T2.fish<-cbind(c(print(combp.sg7.t2)))
    
    ALLT1.fish<-cbind(c(print(combp.all.t1)))
    ALLT2.fish<-cbind(c(print(combp.all.t2)))
    ALLT3.fish<-cbind(c(print(combp.all.t3)))


# Fisher's Om test for all territories AFTER first doing Fisher's Om test within each territory for tempsep=1 and =2
# extract p-values from fisher's test results
v1.1<-combp.sg1.t1$p # vector containing the SG1 combined P value for the 3 mantel tests between seasons tempsep=1 apart
v1.2<-combp.sg1.t2$p # vector containing the SG1 combined P value for the 2 mantel tests between seasons tempsep=2 apart
v2.1<-combp.sg2.t1$p
v2.2<-combp.sg2.t2$p
v3.1<-combp.sg3.t1$p
v3.2<-combp.sg3.t2$p
v4.1<-combp.sg4.t1$p
v4.2<-combp.sg4.t2$p
v5.1<-combp.sg5.t1$p
v5.2<-combp.sg5.t2$p
v6.1<-combp.sg6.t1$p
v6.2<-combp.sg6.t2$p
v7.1<-combp.sg7.t1$p
v7.2<-combp.sg7.t2$p
    
# Make 2 vectors containing all 7 territories' combined p-values for tempsamp=1 and tempsamp=2
v.all.t1<- c(v1.1, v2.1, v3.1, v4.1, v5.1, v6.1, v7.1)
v.all.t2<- c(v1.2, v2.2, v3.2, v4.2, v5.2, v6.2, v7.2)

# Do Fisher's Om Test on each vector
combp.v.all.t1<-sumlog(v.all.t1)
combp.v.all.t2<-sumlog(v.all.t2)

# print results and save
ALLT1.megafish<-cbind(c(print(combp.v.all.t1)))
ALLT2.megafish<-cbind(c(print(combp.v.all.t2)))
 
 
# Combine Fisher's Om Test results into a table
ultimate.fishies <- data.frame(SG1T1.fish, SG1T2.fish, SG2T1.fish, SG2T2.fish,
                            SG3T1.fish, SG3T2.fish, SG4T1.fish, SG4T2.fish,
                            SG5T1.fish, SG5T2.fish, SG6T1.fish, SG6T2.fish,
                            SG7T1.fish, SG7T2.fish, ALLT1.fish, ALLT2.fish,
                            ALLT3.fish, ALLT1.megafish, ALLT2.megafish)

# coerce into numeric in preparation for export as csv file:
ultimate.fishies.df <- data.frame(lapply(ultimate.fishies, as.character), stringsAsFactors=FALSE)
# export as csv file:
write.csv(ultimate.fishies.df, file="ultimate.fishies.csv", row.names = FALSE)

# Combine T3s into a table
T3s <-data.frame(sg1.t3.p, sg2.t3.p, sg3.t3.p, sg4.t3.p, sg5.t3.p, sg6.t3.p, sg7.t3.p)
d <- data.frame(ALLT1.megafish, ALLT2.megafish)

# coerce into numeric in preparation for export as csv file:
T3s.df <- data.frame(lapply(T3s, as.character), stringsAsFactors=FALSE)
d.df <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
# export as csv file:
write.csv(T3s.df, file="T3s.csv", row.names = FALSE)
write.csv(d.df, file="d.csv", row.names = FALSE)
