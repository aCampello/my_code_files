transientTIs <- read.table(file = "TransientTIs.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
str(transientTIs)
plot(transientTIs$Seconds.Between, main="Threshold inter-record time intervals for 47 transient foxes across all seasons", xlab="Record number", ylab="Theshold time interval (s)", cex.lab=1.1)

OrchidSpring <- read.table(file = "OrchidSpring.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
str(OrchidSpring)
plot(OrchidSpring$Seconds.Between, ylim=c(0,6000), main="Spring", xlab="Record number", ylab="Theshold time interval (s)", cex.lab=1.1)

OrchidSummer <- read.table(file = "OrchidSummer.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
str(OrchidSummer)
plot(OrchidSummer$Seconds.Between, ylim=c(0,6000), main="Summer", xlab="Record number", ylab="Theshold time interval (s)", cex.lab=1.1)

OrchidAutumn <- read.table(file = "OrchidAutumn.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
str(OrchidAutumn)
plot(OrchidAutumn$Seconds.Between, ylim=c(0,6000), main="Autumn", xlab="Record number", ylab="Theshold time interval (s)", cex.lab=1.1)

OrchidWinter <- read.table(file = "OrchidWinter.txt", sep='\t', header = TRUE) #import dataset from R (ensure seasons are a list
str(OrchidWinter)
plot(OrchidWinter$Seconds.Between, ylim=c(0,6000), main="Winter", xlab="Record number", ylab="Theshold time interval (s)", cex.lab=1.1)

par(mfrow=c(2,2))


# 140617
# Trying to plot histograms of Time Intervals to see if that shows a clear breakpoint
setwd("G:/Bristol - PhD Camera Trapping Study/Statistics 2016/Methods chapter - stats and plots/Methods - TimeIntervals_Mean_Median")
mydata <- read.csv("TIme intervals from Q_TimeIntervalsGraph Excel sheet_to test plots in R.csv")

# density plot (as data are sorted ascending by SecondsBetween)
plot(mydata$SecondsBetween, xlab="Record number", ylab = "Time interval (s)")
# doesn't show much

# histogram (frequency plot)
hist(mydata$SecondsBetween, xlim=c(0,500), breaks=10000, xlab="Time interval (s)")
# had to keep decreasing the x-axis limits to show anything of interest - scale too large
# Plus this just shows the mean but over a huge scale 
# So the way I did it in my thesis was the best way
# (phew - I had forgotten my thought process!)
