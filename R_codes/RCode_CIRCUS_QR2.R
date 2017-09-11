rm(list=ls()) #to clear the R workspace
dev.off()

setwd("~/Circus welfare report/Questionnaire R2")
mydata <- read.csv(file = "QR2_Results_R_csv_nozeros_blankbases.csv", header = TRUE) #import dataset

library(plyr) # to do calculations on data #ddply does not auto-exclude NAs
library(lsr)
library(ggplot2)

str(mydata) # see NAs are shown
mydata.na<-na.omit(mydata) # create new dataframe without NAs
str(mydata.na) # no more NAs

#Can also remove NAs in a ddply function but cant use 'summary' at same time so a bit limited...:
#Summary by question and category with min, max, median, quartiles, N
# excluding NAs
a<-ddply(mydata, c("q", "categ"), 
         function(x) {
           x <- na.omit(x$percent)
           y <- summary(x)
         })
------------------------------
  
  # box plots of whole dataset (excluding NAs)
  
#take subsets for clearer separate plots
sub.beh<-subset(mydata.na, ques.categ== "Behaviour") # subset of just the behaviour questions...
sub.env<-subset(mydata.na, ques.categ== "Environment")
sub.man<-subset(mydata.na, ques.categ== "Management")
sub.hand<-subset(mydata.na, ques.categ== "Handling and training")
sub.trav<-subset(mydata.na, ques.categ== "Travel")

# Plotting boxplots from whote dataset (minus NAs)
# boxplots show upper and lowe quartiles in the boxes and error bars are the 95% CIs
# CANT PLOT BOXPLOTS FROM MEANS!!! NEED ALL DATAPOINTS FOR DISTRIBUTIONS!!!!! IDIOT.

beh <- ggplot(sub.beh, aes(x=factor(q), y=percent, fill=categ))
myplot.beh<- beh + geom_boxplot() +
  scale_fill_discrete(name="Participant\ncategory") +
  ggtitle("Behaviour\n") +
  xlab("\nQuestion") + ylab("% agreement\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))

env <- ggplot(sub.env, aes(x=factor(q), y=percent, fill=categ))
myplot.env<- env + geom_boxplot() +
  scale_fill_discrete(name="Participant\ncategory") +
  ggtitle("Environment\n") +
  xlab("\nQuestion") + ylab("% agreement\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))

man <- ggplot(sub.man, aes(x=factor(q), y=percent, fill=categ))
myplot.man<- man + geom_boxplot() +
  scale_fill_discrete(name="Participant\ncategory") +
  ggtitle("Management\n") +
  xlab("\nQuestion") + ylab("% agreement\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))

hand <- ggplot(sub.hand, aes(x=factor(q), y=percent, fill=categ))
myplot.hand<- hand + geom_boxplot() +
  scale_fill_discrete(name="Participant\ncategory") +
  ggtitle("Handling and training\n") +
  xlab("\nQuestion") + ylab("% agreement\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))

trav <- ggplot(sub.trav, aes(x=factor(q), y=percent, fill=categ))
myplot.trav<- trav + geom_boxplot() +
  scale_fill_discrete(name="Participant\ncategory") +
  ggtitle("Transport and travel\n") +
  xlab("\nQuestion") + ylab("% agreement\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))


coord_flip() # option to flip axes and have x on the y # looks horrible!

#draw the above graph without grid in background
myplot.trav + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black")) #to make axes darker

#To export in high res (png is best qual)
#1
png("Transport&travel_Q36-42_98responses.png", width = 10, height = 6, units = 'in', res = 1000)
#2
plot(x, y) # Make plot (above) AFTER SPECIFYING FILE LOCATION
#3
dev.off() # THIS IS THE BIT THAT SAVES IT

-------------------------------------------------------------------
### STATISTICS ###
  
# show counts of responses to each question within each participant category
billy.mean<-aggregate(percent~q+categ, data=mydata.na, mean) #works better!
billy.sd<-aggregate(percent~q+categ, data=mydata.na, sd) # SD - massive numbers so use SE instead:

#to calc SE with aggregate, need to define the function for SE first:
se <- function(x) {
  sd(x)/sqrt(length(x))
} 

# then can write the aggregate code:
billy.se<-aggregate(percent~q+categ, data=mydata.na, se) # SE


# bind mean and SE into same data frame
billy.bind <- cbind(billy.mean, billy.se[,3], billy.sd[,3]) #[,3] means use only the third column

questions<- ddply(mydata.na, c("categ", "q", "ques.categ", "ques.point"), 
                  summarise,
                  N=length(categ))

billy.bind.ques<- cbind(billy.bind, questions) #bind all columns first to check alignment
billy.bind.ques<- cbind(billy.bind, questions[,3], questions[,4], questions[,5]) # it's OK. Now just bind required columns

# DONE
---------------------------------------------
# Preparing data frame of means etc to plot graphs of means
data<-billy.bind.ques
names(data) #view column names
data$ques.categ<-(data$"questions[, 3]")  #renaming columns
data$ques.desc<-(data$"questions[, 4]")  #renaming columns
data$N<-(data$"questions[, 5]")       #renaming columns
data$se<-(data$"billy.se[, 3]")
data$sd<-(data$"billy.sd[, 3]")
str(data)

# take subsets to plot each type of question separately
meanse.beh<-subset(data, ques.categ== "Behaviour") # subset of just the behaviour questions...
meanse.env<-subset(data, ques.categ== "Environment")
meanse.man<-subset(data, ques.categ== "Management")
meanse.hand<-subset(data, ques.categ== "Handling and training")
meanse.trav<-subset(data, ques.categ== "Travel")

# Plot bar charts with standard errors of the mean
behbar<-ggplot(meanse.beh, aes(x = factor(q), y = percent, fill=categ)) 
envbar<-ggplot(meanse.env, aes(x = factor(q), y = percent, fill=categ)) 
manbar<-ggplot(meanse.man, aes(x = factor(q), y = percent, fill=categ)) 
handbar<-ggplot(meanse.hand, aes(x = factor(q), y = percent, fill=categ)) 
travbar<-ggplot(meanse.trav, aes(x = factor(q), y = percent, fill=categ)) 

# plot trans& travel as bar plot with SEs as an example. 
# Question needs to be a factor (i.e. factor(q) for the x-axis scale to
# show all the question numbers and not just every other one)
barplot.trav<-travbar + geom_bar(position=position_dodge(),stat="identity") +
  geom_errorbar(aes(ymin=percent-se, ymax=percent+se),
                width=.2,
                position=position_dodge(.9)) +
  scale_fill_discrete(name="Participant\ncategory") +
  ggtitle("Transport and travel\n") + 
  xlab("\nQuestion") + ylab("% agreement\n") +
  theme(axis.text.x  = element_text(angle=0, vjust=0.5, size=12, 
                                    face="bold", color = "black"),
        axis.text.y = element_text(size=12,color = "black"),
        axis.title.x = element_text(size=13),
        axis.title.y = element_text(size=13),
        legend.title=element_text(size=12),
        plot.title = element_text(face="bold"))

barplot.trav + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black")) #to make axes darker

#To export in high res (png is best qual)
#1
png("Transport&travel_Q36-42_98responsesBAR.png", width = 10, height = 6, units = 'in', res = 1000)
#2
plot(x, y) # Make plot (above) AFTER SPECIFYING FILE LOCATION
#3
dev.off() # THIS IS THE BIT THAT SAVES IT

----------------------------------------
  
### MODELS TO TEST DIFFS BETWEEN CATEGORIES FOR EACH OF THE 42 QUESTIONS

# For each question, is percentage agreement predicted by participant category?
# = Is there a difference between the mean scores of each category.
# e.g. Exam score between schools
# reponse (FACTOR): percent
# predictor (TREATMENT): categ

#1. Are the means equal? (one-way ANOVA)
#2. If not, which means are unequal and by how much? (Tukeys test)


# Take a subset for Q1
sub.q1<-subset(mydata.na, q==1)
boxplot(percent~categ, data=sub.q1) # viualise diffs in aggreement for question #1 only
aov.q1<-aov(percent~categ, data=sub.q1) # anova
summary(aov.q1) # view the results of the ANOVA: DO THE GROUP MEANS VARY? Y/N
TukeyHSD(aov.q1) # posthoc comparisons between means (IF ANOVA SHOWS MEANS NOT EQUAL)

# TAKE ALL THE SUBSETS
sub.q1<-subset(mydata.na, q==1)
sub.q2<-subset(mydata.na, q==2)
sub.q3<-subset(mydata.na, q==3)
sub.q4<-subset(mydata.na, q==4)
sub.q5<-subset(mydata.na, q==5)
sub.q6<-subset(mydata.na, q==6)
sub.q7<-subset(mydata.na, q==7)
sub.q8<-subset(mydata.na, q==8)
sub.q9<-subset(mydata.na, q==9)
sub.q10<-subset(mydata.na, q==10)
sub.q11<-subset(mydata.na, q==11)
sub.q12<-subset(mydata.na, q==12)
sub.q13<-subset(mydata.na, q==13)
sub.q14<-subset(mydata.na, q==14)
sub.q15<-subset(mydata.na, q==15)
sub.q16<-subset(mydata.na, q==16)
sub.q17<-subset(mydata.na, q==17)
sub.q18<-subset(mydata.na, q==18)
sub.q19<-subset(mydata.na, q==19)
sub.q20<-subset(mydata.na, q==20)
sub.q21<-subset(mydata.na, q==21)
sub.q22<-subset(mydata.na, q==22)
sub.q23<-subset(mydata.na, q==23)
sub.q24<-subset(mydata.na, q==24)
sub.q25<-subset(mydata.na, q==25)
sub.q26<-subset(mydata.na, q==26)
sub.q27<-subset(mydata.na, q==27)
sub.q28<-subset(mydata.na, q==28)
sub.q29<-subset(mydata.na, q==29)
sub.q30<-subset(mydata.na, q==30)
sub.q31<-subset(mydata.na, q==31)
sub.q32<-subset(mydata.na, q==32)
sub.q33<-subset(mydata.na, q==33)
sub.q34<-subset(mydata.na, q==34)
sub.q35<-subset(mydata.na, q==35)
sub.q36<-subset(mydata.na, q==36)
sub.q37<-subset(mydata.na, q==37)
sub.q38<-subset(mydata.na, q==38)
sub.q39<-subset(mydata.na, q==39)
sub.q40<-subset(mydata.na, q==40)
sub.q41<-subset(mydata.na, q==41)
sub.q42<-subset(mydata.na, q==42)

# RUN ALL THE ANOVAS
aov.q1<-aov(percent~categ, data=sub.q1) 
aov.q2<-aov(percent~categ, data=sub.q2) 
aov.q3<-aov(percent~categ, data=sub.q3) 
aov.q4<-aov(percent~categ, data=sub.q4) 
aov.q5<-aov(percent~categ, data=sub.q5) 
aov.q6<-aov(percent~categ, data=sub.q6) 
aov.q7<-aov(percent~categ, data=sub.q7) 
aov.q8<-aov(percent~categ, data=sub.q8) 
aov.q9<-aov(percent~categ, data=sub.q9)
aov.q10<-aov(percent~categ, data=sub.q10)
aov.q11<-aov(percent~categ, data=sub.q11) 
aov.q12<-aov(percent~categ, data=sub.q12) 
aov.q13<-aov(percent~categ, data=sub.q13) 
aov.q14<-aov(percent~categ, data=sub.q14) 
aov.q15<-aov(percent~categ, data=sub.q15) 
aov.q16<-aov(percent~categ, data=sub.q16) 
aov.q17<-aov(percent~categ, data=sub.q17) 
aov.q18<-aov(percent~categ, data=sub.q18) 
aov.q19<-aov(percent~categ, data=sub.q19)
aov.q20<-aov(percent~categ, data=sub.q20)
aov.q21<-aov(percent~categ, data=sub.q21) 
aov.q22<-aov(percent~categ, data=sub.q22) 
aov.q23<-aov(percent~categ, data=sub.q23) 
aov.q24<-aov(percent~categ, data=sub.q24) 
aov.q25<-aov(percent~categ, data=sub.q25) 
aov.q26<-aov(percent~categ, data=sub.q26) 
aov.q27<-aov(percent~categ, data=sub.q27) 
aov.q28<-aov(percent~categ, data=sub.q28) 
aov.q29<-aov(percent~categ, data=sub.q29)
aov.q30<-aov(percent~categ, data=sub.q30)
aov.q31<-aov(percent~categ, data=sub.q31) 
aov.q32<-aov(percent~categ, data=sub.q32) 
aov.q33<-aov(percent~categ, data=sub.q33) 
aov.q34<-aov(percent~categ, data=sub.q34) 
aov.q35<-aov(percent~categ, data=sub.q35) 
aov.q36<-aov(percent~categ, data=sub.q36) 
aov.q37<-aov(percent~categ, data=sub.q37) 
aov.q38<-aov(percent~categ, data=sub.q38) 
aov.q39<-aov(percent~categ, data=sub.q39)
aov.q40<-aov(percent~categ, data=sub.q40)
aov.q41<-aov(percent~categ, data=sub.q41)
aov.q42<-aov(percent~categ, data=sub.q42)

summary(aov.q39) # view the results of the ANOVA
TukeyHSD(aov.q39) 

summary(aov.q42) # view the results of the ANOVA
TukeyHSD(aov.q42) #posthoc comparisons between means (if there were sig diffs)

se(sub.q21$percent)
se(sub.q22$percent)
se(sub.q23$percent)

------------
  # check model fit
  par(mfrow=c(2,2))
plot(mod1) # 4 plots - assumptions of linear models:

#1. Independance of each data points
#2. Correct distribution of the residuals
#3. Correct specification of the variance structure
#4. Linear relationship between the response and the linear predictor

# Interpret these graphs; straight lines and equal variances are good
par(mfrow=c(1,1)) #to return to one pane panel