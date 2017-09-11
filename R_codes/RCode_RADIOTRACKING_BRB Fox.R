library(adehabitatHR)
require(adehabitatHR)
library(adehabitatLT)
library(lattice)

mydata = read.table("2332N1Act.txt", header=TRUE)
attach(mydata) 
mydata

date2<-data.frame(mydata$Day, mydata$Month, mydata$Year)
date3<-paste(date2[,3], date2[,2], date2[,1], sep="-")
date4<-as.POSIXct(date3, tz= "GMT") 
time1<-paste(mydata$Time)
datetime<-cbind(paste(date4, time1, sep=" "))
datetime1<-as.POSIXct(datetime, tz= "GMT")
datetime1
as.factor(datetime1)

xy<-mydata[,c("X","Y")] 
xy
foxid<-mydata$FoxID

fit1 <- as.ltraj(xy = mydata[,c("X","Y")], date = datetime1,
                id = "Fox2332.N1", typeII = TRUE,
                infolocs = mydata[8:9])
fit1
head(mydata)
## The trajectory:
plot(fit1)


## Estimate the diffusion component for each habitat type
## Using the plug-in method

vv <- BRB.D(fit1, Tmax = 8*3600, Lmin = 5, habitat = NULL, activity = "Activity")
vv

## Note that the values are given here as m^2/s, whereas
## they are given as m^2/min in Benhamou (2011). The
## values in m^2 per min are:
vv[[1]][,2]*60
vv

# Intensity Distribution:

id <- BRB(fit1, D=vv, Tmax=8*3600, Lmin=5, hmin=5, type="ID", radius=15,
	maxt = 15*60, filtershort = FALSE, habitat = NULL, activity = "Activity",
	grid = 300, extent=0.05, b=FALSE, same4all=FALSE, tau=60, boundary=NULL)

# filtershort must be FALSE so the animal is not assumed to be resting when successive 
#     fixes have the same coordinates. 


# tau = interpolated time interval, i.e. if fixes taken every 5 mins, can fill in the 
# gaps between fixes with estimated locations every 1 minute, so tau would be 60 seconds


## Recursion Distribution:
rd <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, hmin=5, type = "RD",
          radius = 15, maxt = 15*60, filtershort=FALSE, activity = "Activity",
          grid = 300, extent=0.05, , same4all=FALSE, b=FALSE, tau=60)

## Utilisation Distribution:
ud <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, type="UD", activity = "Activity",
          hmin=5, radius = 15, maxt = 15*60, filtershort=FALSE,
          same4all=FALSE, b=FALSE, tau=60, grid = 300, extent=0.05)


par(mfrow = c(2,2), mar=c(0,0,2,0))
image(getvolumeUD(id))
title("ID")
image(getvolumeUD(rd))
title("RD")
image(getvolumeUD(ud))
title("UD")

###################################################################################

# Radius is by default 3*hmin, unless otherwise specified.

## Intensity Distribution with radius 30m:

id <- BRB(fit1, D=vv, Tmax=8*3600, Lmin=5, hmin=10, type="ID",
	maxt = 15*60, filtershort = FALSE, habitat = NULL, activity = "Activity",
	grid = 300, extent=0.05, b=FALSE, same4all=FALSE, tau=60, boundary=NULL)

## Recursion Distribution with radius 30m:
rd <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, hmin=10, type = "RD",
          maxt = 15*60, filtershort=FALSE, activity = "Activity",
          grid = 300, extent=0.05, , same4all=FALSE, b=FALSE, tau=60)

## Utilisation Distribution with radius 30m:
ud <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, type="UD", activity = "Activity",
          hmin=10, maxt = 15*60, filtershort=FALSE,
          same4all=FALSE, b=FALSE, tau=60, grid = 300, extent=0.05)


par(mfrow = c(2,2), mar=c(0,0,2,0))
image(getvolumeUD(id))
title("ID")
image(getvolumeUD(rd))
title("RD")
image(getvolumeUD(ud))
title("UD")

#####################################################################################


## Intensity Distribution with radius 60m:

id <- BRB(fit1, D=vv, Tmax=8*3600, Lmin=5, hmin=20, type="ID",
	maxt = 15*60, filtershort = FALSE, habitat = NULL, activity = "Activity",
	grid = 300, extent=0.05, b=FALSE, same4all=FALSE, tau=60, boundary=NULL)

## Recursion Distribution with radius 30m:
rd <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, hmin=20, type = "RD",
          maxt = 15*60, filtershort=FALSE, activity = "Activity",
          grid = 300, extent=0.05, , same4all=FALSE, b=FALSE, tau=60)

## Utilisation Distribution with radius 30m:
ud <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, type="UD", activity = "Activity",
          hmin=20, maxt = 15*60, filtershort=FALSE,
          same4all=FALSE, b=FALSE, tau=60, grid = 300, extent=0.05)


par(mfrow = c(2,2), mar=c(0,0,2,0))
image(getvolumeUD(id))
title("ID")
image(getvolumeUD(rd))
title("RD")
image(getvolumeUD(ud))
title("UD")

#####################################################################################


## Intensity Distribution with radius 45m:

id <- BRB(fit1, D=vv, Tmax=8*3600, Lmin=5, hmin=15, type="ID",
	maxt = 15*60, filtershort = FALSE, habitat = NULL, activity = "Activity",
	grid = 300, extent=0.05, b=FALSE, same4all=FALSE, tau=60, boundary=NULL)

## Recursion Distribution with radius 30m:
rd <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, hmin=15, type = "RD",
          maxt = 15*60, filtershort=FALSE, activity = "Activity",
          grid = 300, extent=0.05, , same4all=FALSE, b=FALSE, tau=60)

## Utilisation Distribution with radius 30m:
ud <- BRB(fit1, D = vv, Tmax = 8*3600, Lmin = 5, type="UD", activity = "Activity",
          hmin=15, maxt = 15*60, filtershort=FALSE,
          same4all=FALSE, b=FALSE, tau=60, grid = 300, extent=0.05)


par(mfrow = c(2,2), mar=c(0,0,2,0))
image(getvolumeUD(id))
title("ID")
image(getvolumeUD(rd))
title("RD")
image(getvolumeUD(ud))
title("UD")
image(id)