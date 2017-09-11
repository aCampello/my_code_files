head(capreochiz)
head(mydata)
head(capreochiz)
## Create an object of class "ltraj"
cap <- as.ltraj(xy = capreochiz[,c("x","y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE,
                infolocs = capreochiz[,4:8])
fit1 <- as.ltraj(xy = mydata[,c("X","Y")], date = datetime1,
                id = "Fox", typeII = TRUE,
                infolocs = mydata[,8:9])

cap
cap2 <- removeinfo(fit1)
cap2

infolocs(fit1)
head(mydata)



#######################################################################

data(buffalo)
act
## The trajectory:
buffalo
## The habitat map:
fit1$Time
fit1
mydata$Activity
infolocs(buffalo$traj)
infolocs(cap2)
## Show the dataset
plot(buffalo$traj, spixdf = buffalo$habitat)

## Estimate the diffusion component for each habitat type
## Using the plug-in method
vveg <- BRB.D(buffalo$traj, Tmax = 180*60, Lmin = 50,
habitat = buffalo$habitat, activ = "act")
vveg
## Note that the values are given here as m^2/s, whereas
## they are given as m^2/min in Benhamou (2011). The
## values in m^2 per min are:
vveg[[1]][,2]*60
## Approximately the same values, with slight differences due to
## differences in the way the program of Benhamou (2011) and the present
## one deal with the relocations occurring on the boundary between two
## different habitat types
## Note that an alternative estimation of the Diffusion coefficient
## could be found using maximum likelihood
vv2 <- BRB.likD(buffalo$traj, Tmax = 180*60, Lmin = 50,
habitat = buffalo$habitat, activ = "act")
vv2
vv[[1]][,2]*60
##