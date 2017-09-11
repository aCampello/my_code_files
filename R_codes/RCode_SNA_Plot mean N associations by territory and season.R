setwd("~/Analysis/Social network analysis/SOCPROG/Associations per node")
library(ggplot2)
library(plyr) 

mydata <- read.csv(file = "Mean associations territory and season.csv", header = TRUE) #import dataset
str(mydata)
mydata$T <- as.factor(mydata$Territory) #c
mydata$assoc.dyad<-mydata$Mean.associations.per.dyad
mydata$assoc.node<-mydata$Mean.associations.per.node

#plot change in group size over time in R
myplot<-  ggplot(mydata, aes(x = SeasonID, y = assoc.node, colour = T)) +
          geom_line(size=1) + geom_point(size=5) +
          scale_x_continuous(breaks=c(1,2,3,4), labels = c("Spring", "Summer", "Autumn", "Winter")) +
          scale_fill_hue(name="Origin") + # legend title
          theme_bw(base_size = 18, base_family = "") +
          xlab("") + ylab("Mean associations per node\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))


####
# boxplot for mean assocs by NODE (mean of means so not ideal but ok for presentation - 
# LATER DO THIS FROM WHOLE DATASET AND MODEL...
)
sum <- ddply(mydata, c("SeasonID"), summarise,
                          N = length(assoc.node),
                          mean = mean(assoc.node),
                          sd   = sd(assoc.node),
                          se   = sd / sqrt(N))   #make summary table 

myplot<-ggplot(sum, aes(x = SeasonID, y = mean)) +
        geom_line(size=1) + geom_point(size=5) +
        scale_x_continuous(breaks=c(1,2,3,4), labels = c("Spring", "Summer", "Autumn", "Winter")) +
        geom_errorbar(data=sum, mapping=aes(x=SeasonID, ymin=mean-se, ymax=mean+se), width=0.2, size=0.5, color="black") +
        scale_fill_hue(name="Origin") + # legend title
        theme_bw(base_size = 18, base_family = "") +
        xlab("") + ylab("Mean associations per node\n")
#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
## saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "Rplot_Seasonal mean N assocs per node with SE bars_SDs were huge.jpeg", res=800, height=6, width=10.5, units="in")

###########

# and per dyad...

sumd <- ddply(mydata, c("SeasonID"), summarise,
             N = length(assoc.dyad),
             mean = mean(assoc.dyad),
             sd   = sd(assoc.dyad),
             se   = sd / sqrt(N))   #make summary table 

myplot<-ggplot(sumd, aes(x = SeasonID, y = mean)) +
  geom_line(size=1) + geom_point(size=5) +
  scale_x_continuous(breaks=c(1,2,3,4), labels = c("Spring", "Summer", "Autumn", "Winter")) +
  scale_y_continuous(limits=c(0,11), breaks=c(0,2,4,6,8,10)) +
  geom_errorbar(data=sumd, mapping=aes(x=SeasonID, ymin=mean-se, ymax=mean+se), width=0.2, size=0.5, color="black") +
  scale_fill_hue(name="Origin") + # legend title
  theme_bw(base_size = 18, base_family = "") +
  xlab("") + ylab("Mean associations per dyad\n")
#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

# draw above in high resolution for publications
## saves most recently drawn plot as the following file name in the current working directory
dev.print(jpeg, "Rplot_Seasonal mean N assocs per dyad with SE bars_SDs were huge.jpeg", res=800, height=6, width=10.5, units="in")
