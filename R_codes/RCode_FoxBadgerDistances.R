
#### FOX RESPONSES TO BADGER PRESENCE, MEASURED AS APPROX FOX-BADGER DISTANCES ####

# Distance was recorded as a category: 0-1m, 1-2, 2-4m and >4m. This is ORDINAL data, so analyse using package ORDINAL.

# Load in the data
mydata <- read.csv(file.choose()) # select CSV file with distances

# Subset data to exclude photos where distance was not clear
datasub <- subset(mydata, FoxBadgerNND_ID!=6)

# make Distance into an ordered factor (category ordered by increasing distance)
datasub$distanceORD <- ordered(datasub$FoxBadgerNND_ID) 

# Change reference category for Sex from female to male (as default order is alphabetical)
datasub$Sex <- relevel(datasub$Sex, ref=2)

# Delete unnecessary columns from dataset
datasub$Image.name <- NULL
datasub$Territory <- NULL
datasub$cDateTime <- NULL

# rough plot to view proportion of photos of each sex assigned each distance categ.
plot(distanceORD~Sex, data=datasub) # male column is wider, poss due to larger sample size?
   

### STATISTICS:

### Test whether males take more risks than females (i.e. have shorter fox-badger distances)
# Distances are ordered/ordinal, so need to run an ORDINAL REGRESSION MODEL:

## Run simple tests first:
# Kruskal Wallis Test One Way Anova by Ranks 
kruskal.test(datasub$FoxBadgerNND_ID ~ datasub$Sex) # where y is numeric and x is a factor
# independent 2-group Mann-Whitney U Test 
wilcox.test(datasub$FoxBadgerNND_ID ~ datasub$Sex) # where y is numeric and x is numeric OR a binary factor
# same p-values

# load R package to test ordinal data
library(ordinal) 

# Fit model with response=distance and predictor=sex)
sexmodel <- clm(distanceORD ~ Sex, data=datasub)

# view model summary - coefficients, p-value...
summary(sexmodel) # females keep greater distances from badgers

# make null model with no predictor variable
nullsexmodel <- clm(distanceORD ~ 1, data=datasub) 

# compare the full and null models to see if there is a significant effect of sex on distance (this just compares whether adding sex to the model helps explain any of the variation in distance)
anova(sexmodel, nullsexmodel) # sex effect is significant: quote the likelihood ratio statistic ("LR.stat"), df and p-value

### Use model to predict proportion of time (inferred from proportion of photos) that males/females are seen at different distances from badgers (i.e. % time males :

# tell R to make predictions for sex
predframe <- data.frame(Sex=levels(datasub$Sex)) 
# predict the % photos of males that will be in each distance category and same for females
preds <- predict(sexmodel, newdata=predframe, type="prob", interval= T)
# fit = prediction, lwr = lower confidence interval, upr = upper confidence interval

## Combine predicted values and 95% CIs into single data frame
f <-data.frame(preds$fit)
f$sex<-c("M", "F") #add sex column
# stack rows into columns using gather in 'tidyr'
# syntax: gather(dataframe, "new col name1", "new col name2", which columns to stack into rows) - 1:4 means columns 1 to 4 - but stacks all columns by default.
fpreds <- gather(f, "Dist", "pred", 1:4) 

# get confidence intervals
LCI <-data.frame(preds$lwr)
LCI$sex<-c("M", "F") # add sex column
lwrtab <- gather(LCI, "Dist", "lwr", 1:4)

UCI <-data.frame(preds$upr)
UCI$sex<-c("M", "F") # add sex column
uprtab <- gather(UCI, "Dist", "upr", 1:4)

# combine
library(dplyr)
a <- dplyr::left_join(fpreds,lwrtab, by=c("sex","Dist"))
predtable <- dplyr::left_join(a,uprtab,by=c("sex","Dist"))
predtable$sex <- relevel(factor(predtable$sex), ref=2)

## Plot the predictions
library(ggplot2)
ggplot(predtable, aes(x=Dist,y=pred, fill=sex)) + 
    geom_bar(stat="identity", position=position_dodge(0.9)) +
  geom_errorbar(aes(ymax=lwr, ymin=upr), size=1, width=0.2, 
                position=position_dodge(0.9)) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  scale_fill_manual(name="Sex", values = c("royalblue3","firebrick1")) +
  xlab("\nDistance") + ylab("Proportion of fox-badger encounters\n") +
  scale_x_discrete(labels=c("0-1m", "1-2m", "2-4m", ">4m")) 

dev.print(jpeg, "Rplot_Predicted fox-badger distances by sex with confidence interval bars.jpeg", res=700, height=6, width=10.5, units="in") # save as jpeg


