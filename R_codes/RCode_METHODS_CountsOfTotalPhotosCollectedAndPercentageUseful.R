
setwd("E:/Statistics 2016/Methods chapter - stats and plots/Methods - Percentage photos that were useful_from folder counts")

mydata <- read.table(file = "Percentages of useful photos from all collected_NEW.txt", sep='\t', header = TRUE) 
#import dataset: weekly counts of AllPhotos and AniPhotos from 4 random surveys in each season (spr, sum, aut and wint)
#for stations checked fortnightly, e.g. over xmas, numbers for the missing week were estimated from the following week 
# (i.e. =countsFromFollowingWeek/2)

str(mydata) #view data structure

library(plyr)
sumtable <- ddply(mydata, "Season", summarise,
                  Total.Nphotoscollected=sum(N.AllPhotos),
                  Total.NAniphotoscollected=sum(N.AniPhotos),
                  mean.percentage.useful=mean(Percentage.of.photos.useful),
                  mean.Nphotoscollected=mean(N.AllPhotos),
                  mean.NAniphotoscollected=mean(N.AniPhotos),
                  n.obs=length(Percentage.of.photos.useful)) #group by season ONLY
sumtable #view summary table

# Total photos collected from these surveys:
TotalObservedNPhotos <- sum(sumtable$Total.Nphotoscollected) 

sumtable2 <- ddply(mydata, c("Season", "Social.group"), summarise,
                  Total.Nphotoscollected=sum(N.AllPhotos),
                  mean.percent.useful=mean(Percentage.of.photos.useful),
                  mean.Nphotoscollected=mean(N.AllPhotos),
                  n.obs=length(Percentage.of.photos.useful)) #group by group AND season

# mean total number of photos collected per survey (i.e. in one survey in one season) 
meanNphotosCollected.season <- tapply(sumtable2$Total.Nphotoscollected, sumtable2$Season, mean) 


