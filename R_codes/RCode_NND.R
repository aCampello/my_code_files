rm(list=ls()) #to clear the R workspace
dev.off()

setwd("~/Analysis/NNDs")

# import dataset
mydata <- read.csv(file = "NNDs_BodyLength_OnlyRecsWithaNND.csv", header = TRUE) #import dataset

library(plyr) # to do calculations on data #ddply does not auto-exclude NAs
library(ggplot2)

str(mydata)

mydata.subs<-subset(mydata, SocialStatus!="Unknown" & Sex!="Unknown" & AgeClassID==1) #remove unknowns, subadults and cubs

NNDsum.means<- ddply(mydata.subs, c("SeasonID", "Sex", "SocialStatus", "ShortCode"), 
               summarise,
               Ndist=length(Dist),
               meanNND=mean(Dist)) # ensure there are no 'na's in the list of distances

boxplot(Dist~Sex+SocialStatus, mydata.subs)

spr<-subset(NNDsum.means, SeasonID==1)
sum<-subset(NNDsum.means, SeasonID==2)
aut<-subset(NNDsum.means, SeasonID==3)
win<-subset(NNDsum.means, SeasonID==4)

par(mfrow=c(2,2))
test<-ggplot(mydata.subs, aes(x=factor(SocialStatus), y=Dist, fill=Sex)) + geom_boxplot() 
test + facet_grid(Sex~SocialStatus)
test + facet_wrap(~SeasonID)

ggspr<-ggplot(spr, aes(x=factor(SocialStatus), y=meanNND, fill=Sex)) + geom_boxplot() + ggtitle("Spring\n") + #now run the customising code below
ggsum<-ggplot(sum, aes(x=factor(SocialStatus), y=meanNND, fill=Sex)) + geom_boxplot() + ggtitle("Summer\n") +
ggaut<-ggplot(aut, aes(x=factor(SocialStatus), y=meanNND, fill=Sex)) + geom_boxplot() + ggtitle("Autumn\n") +
ggwint<-ggplot(win, aes(x=factor(SocialStatus), y=meanNND, fill=Sex)) + geom_boxplot() + ggtitle("Winter\n") +

  scale_y_continuous(limits=c(0, 4), ylab("NND category\n")) +
  scale_fill_discrete(name="Sex") +
  xlab("\nSocial status")  +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))