# 12/07/16

#=========================#
# Event sequence analysis - Just testing the method & did not use in thesis
#=========================#
setwd("E:/Statistics 2016/Patch visitation")
# import without converting to factor
visitdata <- read.csv("MultilevelModels_PatchResTimes_NoCubs28surveys.CSV", header=T, stringsAsFactors = FALSE)
str(visitdata)
# convert datetime from character to POSIXct
visitdata$datetime <-as.POSIXct(paste(visitdata$VisitStart),format="%d/%m/%Y %H:%M:%S", tz="UTC")
visitdata$datetimeEND <-as.POSIXct(paste(visitdata$VisitEnd),format="%d/%m/%Y %H:%M:%S", tz="UTC")

# Take subset of only days feeding days (when food provided at patch)
visitdata_FD <- subset(visitdata, FeedingDay=="Yes")
str(visitdata_FD)

# Subset just the key columns for sequence analysis
foxvisdata <-data.frame(AnimalID=visitdata_FD$AnimalID, VisitStart=visitdata_FD$datetime, StationCode=visitdata_FD$StationCode)
head(foxvisdata)

# Import dataset with time of first feed on each feeding day at each patch
firstfeedtime <- read.csv("FirstFeedingTime_eachday_28surveys.csv", header=T, stringsAsFactors = FALSE)
# convert datetime from character to POSIXct
firstfeedtime$datetime <-as.POSIXct(paste(firstfeedtime$FirstOfTRANScDateTime),format="%d/%m/%Y %H:%M:%S", tz="UTC")

# Subset key columns of feeding data adding variable for 'ShortCode' called 'Food'
feeddata <- data.frame(AnimalID=1000, VisitStart=firstfeedtime$datetime, StationCode=firstfeedtime$StationCode)

# Add feeding times to fox visit data
eventdata <- rbind(feeddata, foxvisdata)
# Order data by ShortCode
eventdata <- eventdata[order(eventdata$AnimalID), ]
# Convert datetimes to numbers
eventdata$DTnum <- as.numeric(eventdata$VisitStart)


# Create sequence object
install.packages("TraMineR")
library(TraMineR)
myseq <- seqecreate(data=eventdata, id = eventdata$AnimalID,
                    timestamp = eventdata$DTnum,
                    event = eventdata$StationCode)

# find most frequent subsequence - THIS CRASHES R OR TAKES AGES AND WONT LET YOU STOP IT.
mysubseq <- seqefsub(myseq, pMinSupport = 0.05) 
mysubseq[1:15]

# plot subsequences
plot(mysubseq[1:15], col = "cyan", ylab = "Frequency", xlab = "Subsequences", cex = 1.5)













