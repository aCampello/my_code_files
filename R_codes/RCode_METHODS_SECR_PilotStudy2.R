#demo data that comes with secr help file:
head(captXY)
head(trapXY)
demotraps <- read.traps(data = trapXY)
demoCHxy  <- make.capthist (captXY, demotraps, fmt = "XY")
demoCHxy            ## print method for capthist
plot(demoCHxy) 


#load my captures and trapfile data for session 1 and 2 - XY format
captures<-read.table(file = "SECRcaptures_XYformat.txt", sep='\t', header = TRUE)
trapfilemulti<-read.table(file = "trapfile.txt", sep='\t', header = T) #load camera trap coordiantes
head(captures)
str(captures)#check everything looks right
head(trapfilemulti) #check everything looks right


traps <- read.traps(data = trapfilemulti) #make a 'traps' object from the trap file
CHxy  <- make.capthist (captures, traps, fmt = "XY") #make a capthist object from the captures file

summary(CHxy)            ## print method for capthist (capture matrix)
plot(CHxy) #plot the capthist object
?detector


## fit & print null (constant parameter) model
summary(  secr0<-(secr.fit(CHxy))) #quite slow!

## compare fit of null model with learned-response model for g0
secrb <- secr.fit (CHxy, model = g0~b)
AIC (secr0, secrb)
