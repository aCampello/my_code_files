
### How i drew the species accumulation curves based on pilot study 2 in thesis chapter 2 (general methods)

# set working directory
setwd("E:/Statistics 2016/Methods chapter - stats and plots/Methods - PILOT STUDIES/SpeciesAccumCurves_PilotStudy2")

# load previous work into workspace
load("E:/Statistics 2016/Methods chapter - stats and plots/Methods - PILOT STUDIES/SpeciesAccumCurves_PilotStudy2/.RData")

library(vegan)

mydata80D <- read.table(file = "CaptureHists80days_notransients.txt", sep = "\t", header = TRUE) #import dataset
mydataSITES <- read.table(file = "CaptureHists80days_RowsAsSites.txt", sep = "\t", header = TRUE) #import dataset
mydataDaySite <- read.table(file = "CaptureHistsDayandStation_NoTransients.txt", sep = "\t", header = TRUE) #import dataset

head(mydata80D)
str(mydata80D)
str(mydataDaySite)

sp1.80D <- specaccum(mydata80D, "random", perm=5000)
sp2.80D <- specaccum(mydata80D, "rarefaction")

sp1.DaySite <- specaccum(mydataDaySite)
sp2.DaySite <- specaccum(mydataDaySite, "random", perm=5000)

sp1.SITES <- specaccum(mydataSITES)
sp2.SITES <- specaccum(mydataSITES, "random", permutations=500) #can only specify permutations if use method=random
sp3.SITES <- specaccum(mydataSITES, "rarefaction") #supposed to be for if rows are individuals instead of sites... curve doesnt make much sense
summary(sp2.SITES)
summary(sp2.DaySite)

#PLOT SPECIES ACC CURVE WITH SHADED CONFIDENCE INTERVALS - DAYS
plot(sp1.80D, ci.type="line", col="red", lwd=2, ci.lty=0, ci.col="lightblue", add=F,
     xlab="Survey days", ylab="Number of individuals", main="Species accumulation curve for pilot study 2")
#ADD LINES TO SHOW 
lines(sp1.80D, col="red")
boxplot(sp2.80D, col="yellow", add=TRUE, pch="+") #DONT USE BOXPLOTS -LINES LOOK CLEARER


#PLOT SPECIES ACC CURVE WITH SHADED CONFIDENCE INTERVALS - SITES
plot(sp2.SITES, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", ylim=c(0,16), xlim=c(0.5,6.5),
     xlab="Number of camera sites", ylab="Number of individuals", main="Species accumulation curve for pilot study 2")
# in B&W: Individual accumulation curve for pilot study 2 (500 perms)
plot(sp2.SITES, ci.type="line", col="black", lwd=1.5, ci.lty=3, ci.col="black", add=F, 
     ylim=c(0,16), xlim=c(0.5,6.5), xlab=expression(italic("N")~"camera sites"), ylab=expression(italic("N")~"individuals detected"), 
     cex.lab=1.2)

#ADD LINES TO SHOW 
lines(sp2.SITES, col="black") # dont add
boxplot(sp2.SITES, col="grey70", add=TRUE, pch=19) 
boxplot(sp2.SITES, col="grey70", add=TRUE, pch="+") # dont add

# save as jpeg
dev.print(jpeg, "Rplot_Specaccum boxplots 500perms.jpeg", res=800, height=15, width=20, units="cm") # save as jpeg




#PLOT SPECIES ACC CURVE WITH SHADED CONFIDENCE INTERVALS - SITES EACH DAY (= CAMERA DAYS)
plot(sp2.DaySite, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", add=F, ylim=c(0,14),
     xlab="Camera days", ylab="Number of individuals", main="Species accumulation curve for pilot study 2")
#ADD LINES TO SHOW 
lines(sp2.DaySite, col="blue")
boxplot(sp2.DaySite, col="yellow", add=TRUE, pch="+") #DONT USE BOXPLOTS -LINES LOOK CLEARER





#PLOT SPECIES ACC CURVE WITH SHADED CONFIDENCE INTERVALS - SITES EACH DAY (= CAMERA DAYS)
sp1.Raref <- specaccum(mydataDaySite, "rarefaction", permutations = 5000)
summary(sp1.Raref)
plot(sp1.Raref, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", 
     xlab="Camera days", ylab="Number of individuals", main="Species accumulation curve for pilot study 2")
#ADD LINES TO SHOW 
lines(sp2.DaySite, col="blue")
boxplot(sp2.DaySite, col="yellow", add=TRUE, pch="+")


#PLOT SPECIES ACCUM CURVE FOR BOTH CAMERA DAYS AND SURVEY DAYS ON SAME PANEL
par(mfrow=c(2,1), mar=c(4, 5, 2, 2))#it goes c(bottom, left, top, right) 
plot(sp1.80D, ci.type="poly", col="black", lwd=2, ci.lty=3, ci.col="white", add=F, ylim=c(0,14), xlab="Survey days", ylab="Number of individuals", main="Individual accumulation curves for pilot study 2 (random, 5000 permutations)", cex.main=0.9)
plot(sp2.DaySite, ci.type="poly", col="black", lwd=2, ci.lty=3, ci.col="white", add=F, ylim=c(0,14), xlab="Camera days", ylab="Number of individuals")



######TO DO RAREFACTION ANALYSIS - NEED MATRIX OF INDIVIDUALS (COLUMNS) AND TREATMENTS AS ROWS ####LOOKS RUBBISH AND NOT SURE HOW GOOD WRITER IS
# (E.G. SITE - THESE ARE THE SEPARATE LINES/LEGEND ENTRIES)
source("http://www.jennajacobs.org/R/rarefaction.txt")
emend<-read.csv("http://www.jennajacobs.org/R/EMEND.csv", row.names=1)
rarefsites<-read.csv("rare_sites.csv", row.names=1)

#same as 
mydataSITES
emend.rare<-rarefaction(emend, col=F) # I'm a big fan of B&W
D80.rare<-rarefaction(mydataSITES, col=T)
str(D80.rare)
plot(D80.rare.[,1] ~ D80.rare.[,3])
head(D80.rare$richness) # a matrix of the mean richness at each subsample
head(D80.rare$SE)  # a matrix of the iterative SE of the mean richness at each subsample
head(D80.rare$subsample) # the subsample sizes used to find the means

####################################################################




library(lattice)
?contourplot
contourplot(fit ~SITES~)
