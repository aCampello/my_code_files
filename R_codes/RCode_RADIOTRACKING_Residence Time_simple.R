###Residence Time Method###

library(adehabitatLT)
library(lattice)

mydata = read.table("2332141193A1.txt", header=TRUE)
attach(mydata) 
mydata

date2<-data.frame(mydata$Day, mydata$Month, mydata$Year)
date3<-paste(date2[,3], date2[,2], date2[,1], sep="-")
date4<-as.POSIXct(date3, tz= "GMT") 
time1<-paste(mydata$Time)
datetime<-cbind(paste(date4, time1, sep=" "))
datetime1<-as.POSIXct(datetime, tz= "GMT")
as.factor(datetime1)
xy<-mydata[,c("X","Y")] 
foxid<-mydata$FoxID
fit1<-as.ltraj(xy,date=datetime1,id=foxid,burst=foxid,typeII=TRUE)

dev.off()

#Show the distances between successive relocations as a function of date
plotltr(fit1)
fit1
#Plots route between relocations and highlights possible patches
plot(fit1)


###########################
#This bit won't work, don't know why, but doesn't seem to be needed for the analysis...
###########################
#Focus on relocations in a specified time period, this time between 8pm on 30/6/92 and 3am on 1/7/92
ltr <- gdltraj(fit1, as.POSIXct(" 1992-06-30 20:00:00", tz="GMT"), as.POSIXct("1992-07-01 03:00:00", tz="GMT"))
#Plots this period only, and identifies possible patches, measures them
plot(ltr)
###########################

#lmin = min no. observations per segment, i.e. if fixes taken every 5min, and Lmin=6, each patch will be used for min 30mins.
#kmax = max no. segments


###Radius 250m, 8.5hour maxt###

restime<-residenceTime(fit1, radius=250, maxt=8.5, addinfo = TRUE, units = "hours")

res <- residenceTime(restime, radius=250, maxt=8.5, units="hour")
plot(res)
res <- residenceTime(restime, radius=250, maxt=8.5, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.250", Kmax=13, Lmin=6)
res2 
chooseseg(res2)
(pa <- findpath(res2, 5))

#plots patches in seperate segments on seperate plots
plot(pa, perani=FALSE)

#Superimpose all patches/segments on the same plot
plot(pa, perani=TRUE)


dev.off()

###Radius 200m, 8hour maxt###

restime<-residenceTime(fit1, radius=200, maxt=8, addinfo = TRUE, units = "hours")

res <- residenceTime(restime, radius=200, maxt=8, units="hour")
plot(res)
res <- residenceTime(restime, radius=200, maxt=8, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.200", Kmax=20, Lmin=3)
res2 
chooseseg(res2)
(pa <- findpath(res2, 7))
plot(pa, perani=TRUE)




###Radius 150m, 8.5hour maxt###

restime<-residenceTime(fit1, radius=150, maxt=8.5, addinfo = TRUE, units = "hours")

res <- residenceTime(restime, radius=150, maxt=8.5, units="hour")
plot(res)
res <- residenceTime(restime, radius=150, maxt=8.5, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.150", Kmax=12, Lmin=6)
res2 
chooseseg(res2)
(pa <- findpath(res2,5))
plot(pa, perani=TRUE)
pa

###Radius 100m, 8.5hour maxt###

restime<-residenceTime(fit1, radius=100, maxt=8.5, addinfo = TRUE, units = "hours")

res <- residenceTime(restime, radius=100, maxt=8.5, units="hour")
plot(res)
res <- residenceTime(restime, radius=100, maxt=8.5, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.100", Kmax=13, Lmin=6)
res2 
chooseseg(res2)
(pa <- findpath(res2, 5))
plot(pa, perani=TRUE)



###Radius 50m, 8.5hours maxt, min 30mins in patch ###

restime<-residenceTime(fit1, radius=50, maxt=8.5, addinfo = TRUE, units = "hours")
res <- residenceTime(restime, radius = 50, maxt=8.5, units="hour")
plot(res)
res <- residenceTime(restime, radius = 50, maxt=8.5, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.50", Kmax=13, Lmin=6)
res2 
chooseseg(res2)
pa <- findpath(res2, 5)
plot(pa, perani=FALSE)
plot(pa, perani=TRUE)

###Radius 25m, 8.5hours maxt, min 30mins in patch ###

restime<-residenceTime(fit1, radius=25, maxt=8.5, addinfo = TRUE, units = "hours")
res <- residenceTime(restime, radius = 25, maxt=8.5, units="hour")
plot(res)
res <- residenceTime(restime, radius = 25, maxt=8.5, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.25", Kmax=13, Lmin=6)
res2 
chooseseg(res2)
pa <- findpath(res2, 5)
plot(pa, perani=FALSE)
plot(pa, perani=TRUE)


###Radius 12.5m, 8hours maxt, min 20mins in patch ###

restime<-residenceTime(fit1, radius=12.5, maxt=8, addinfo = TRUE, units = "hours")
res <- residenceTime(restime, radius = 12.5, maxt=8, units="hour")
plot(res)
res <- residenceTime(restime, radius = 12.5, maxt=8, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.12.5", Kmax=20, Lmin=4)
res2 
chooseseg(res2)
pa <- findpath(res2, 9)
pa
plot(pa, perani=FALSE)
plot(pa, perani=TRUE)

mydata

###Radius 5m, 8hours maxt, min 20mins in patch ###

restime<-residenceTime(fit1, radius=12, maxt=8, addinfo = TRUE, units = "hours")
res <- residenceTime(restime, radius = 12, maxt=8, units="hour")
plot(res)
res <- residenceTime(restime, radius = 12, maxt=8, addinfo = TRUE, units="hour")
res
res2 <- lavielle(res, which="RT.12", Kmax=10, Lmin=1)
res2 
chooseseg(res2)
pa <- findpath(res2, 5)
pa
plot(pa, perani=FALSE)
plot(pa, perani=TRUE)
