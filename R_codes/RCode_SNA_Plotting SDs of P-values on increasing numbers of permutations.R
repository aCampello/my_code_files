rm(list=ls()) #to clear the R workspace
dev.off()

setwd("~/Analysis/Social network analysis")
setwd("E:/Statistics 2016/Social network analysis_synced with uni up to 180416 12noon/Finding correct N permutations for pref-av assocs")

mydata <- read.csv(file = "Testing permutations to stabilise P.csv", header = TRUE) #import dataset
str(mydata)

library(plyr) # to do calculations on data
library(ggplot2)

summaryP <- ddply(mydata, c("Sample", "Permutations"), 
                    summarise,
                    meanP=mean(P),
                    SD=sd(P))
summaryP

dev.off()
#par() command adjusts plot features using mar=c(bottom, left, top, right) 
# default is par(mar=c(5, 4, 4, 2) + 0.1)
par(mar=c(5,4,4,10 + 0.1))

#specify colours
plot_colours <- c("red","darkorange1", "forestgreen", "blue", "purple", "black") 

#plot SD
myplot<-ggplot(summaryP, aes(x = Permutations, y = SD, colour = Sample)) + 
  geom_point(size=4) + geom_line(size=1) +  
  scale_x_continuous(breaks=c(1000,2000,5000,10000), labels=c("1000", "2000", "5000", "10,000"))+
  scale_colour_manual(values=plot_colours,
                      name="Sample",
                      labels=c("T1 Winter", "T2 Summer", "T3 Autumn", 
                               "T3 Summer", "T4 Spring", "T6 Winter")) +
  theme_bw(base_size = 18, base_family = "") +
  xlab("\nNumber of permutations") + ylab("SD of p-value\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), 
               axis.line = element_line(colour = "black")) #to make axes darker
# to remove border completely: panel.border = element_blank()

# save in high res
dev.print(jpeg, "Increasing permutations to stabilise P smallertext.jpeg", res=800, height=6, width=10.5, units="in")



#plot mean (looks crap due to scale difference of SG3 autumn)
myplot<-ggplot(summaryP, aes(x = Permutations, y = meanP, colour = Sample)) + 
  geom_point(size=4) + geom_line(size=1) + 
  geom_jitter(data=summaryP, mapping=aes(x = Permutations, y = meanP, colour = Sample), size=1)+
  scale_x_continuous(breaks=c(1000,2000,5000,10000), labels=c("1000", "2000", "5000", "10,000"))+
  scale_colour_manual(values=plot_colours,
                      name="Sample",
                      labels=c("SG1 Winter", "SG2 Summer", "SG3 Autumn", 
                               "SG3 Summer", "SG4 Spring", "SG6 Winter")) +
  theme_bw(base_size = 20, base_family = "") +
  xlab("\nNumber of permutations") + ylab("Mean P-value\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), 
               axis.line = element_line(colour = "black")) #to make axes darker
