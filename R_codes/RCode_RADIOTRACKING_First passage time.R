library(adehabitatLT)
library(lattice)

#Load and attach data (text file)
mydata = read.table("2332activeallnightssummer1990.txt", header=TRUE)
attach(mydata) 
mydata

#paste the date columns
date2<-data.frame(mydata$Day, mydata$Month, mydata$Year)

#combine the date columns in correct order YYYY-mm-dd
date3<-paste(date2[,3], date2[,2], date2[,1], sep="-")

#make date into POSIXct format
date4<-as.POSIXct(date3, tz= "GMT") 

time1<-paste(mydata$Time)

#paste date and time in 1 column
datetime<-cbind(paste(date4, time1, sep=" "))

#paste date and time as POSIXct format and store object as 'datetime1'
datetime1<-as.POSIXct(datetime, tz= "GMT")

#specify that datetime1 is a nominal factor
as.factor(datetime1)

#combine XY data
xy<-mydata[,c("X","Y")] 

#Label the unique ID for each burst / datapoint
foxid<-mydata$FoxID

#make object of ltraj
lt<-as.ltraj(xy,date=datetime1,id=foxid,burst=foxid,typeII=TRUE)


#First Passage Time R-code = fpt(lt, radii=300, units = c("seconds", "hours", "days")) 
#=fpt for 300 radii of unspecified size.

#calculate FPT of radii between 25-500m, increasing at a scale of 10m and store as object 'i'
i<-fpt(lt, seq(25, 500, by=10), units = "hours")
#calculate the variance of the log FPT - peaks indicate the preferred spatial scale of the pathway
toto1 <- varlogfpt(i, graph = TRUE)
toto1
attr(toto1, "radii")


#calculate FPT of radii between 25-150m and store as object 'i'
i<-fpt(lt, seq(25, 150), units = "hours")
#calculate the variance of the log FPT - peaks indicate the preferred spatial scale of the pathway
toto1 <- varlogfpt(i, graph = TRUE)
toto1
attr(toto1, "radii")

#Plot the FPT for a radius of 60m, as indicated by the varlogfpt plot
plot(i, scale = 60, warn = TRUE)


#show the meanFPT for each animal (rows) and rach radius (column).
toto <- meanfpt(i, graph = TRUE)
toto
attr(toto, "radii")

