
#### CHAPTER 5: TABLE OF N CORE FOXES IN EACH TERRITORY/SEASON ####

# As used for network centrality models (strength, eig, CC) - 51 core foxes only.

# Load attributes
attribs<-read.csv(file="Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T)
str(attribs)

# Mark pref/avoided as did not use these in models
# Networks with no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
# Mark in column: 1 = Yes, 0 = No pref/av assocs.
attribs$prefavoided <- ifelse(attribs$Territory==2 & attribs$SeasonID==2, "0", NA)
attribs$prefavoided <- ifelse(attribs$Territory==4 & attribs$SeasonID==1 & is.na(attribs$prefavoided),  "0", attribs$prefavoided)
attribs$prefavoided <- ifelse(attribs$Territory==4 & attribs$SeasonID==2 & is.na(attribs$prefavoided),  "0", attribs$prefavoided)
attribs$prefavoided <- ifelse(attribs$Territory==5 & attribs$SeasonID==4 & is.na(attribs$prefavoided),  "0", attribs$prefavoided)
attribs$prefavoided <-  ifelse(is.na(attribs$prefavoided), "1", "0")

# make copy and add sex*status column so can use tidyr function later
attribs_copy <- attribs
attribs_copy$sexstat <- interaction(attribs_copy$Sex, attribs_copy$SocialStatus)

library(plyr)

# check only 51 core foxes (must remove pref/avoided or get 53 due to Jade & Copper in T4 winter)
a <- ddply(subset(attribs_copy, prefavoided==1), "Core", summarise, 
           N = length(unique(ShortCode)))

# with subadults and adults grouped together
a <- ddply(subset(attribs_copy, prefavoided==1 & Core==1), 
           c("Territory", "SeasonID", "sexstat"),
           summarise, 
           N = length(unique(ShortCode)))

# Change rows to columns for reporting
library(tidyr)
# syntax is: newdata <- tidyr::spread(mydata, predictor, response) 
tidyA <- tidyr::spread(a, sexstat, N)

######

# with subadults in separate rows
b <- ddply(subset(attribs_copy, prefavoided==1 & Core==1), 
           c("Territory", "SeasonID", "sexstat", "AgeClass"),
           summarise, 
           N = length(unique(ShortCode)))

# Change rows to columns for reporting
tidyB <- tidyr::spread(b, sexstat, N)

#################################

# Calc number of foxes seen >=5 days in each survey (including surveys with no pref/avoied assocs)
a <- ddply(subset(attribs_copy, ns5==1), c("Territory", "SeasonID"), summarise, 
           N = length(unique(ShortCode)))
a

# get sex ratio
a <- ddply(subset(attribs_copy, ns5==1), c("Territory", "SeasonID", "Sex"), summarise, 
           N = length(unique(ShortCode)))
tidyC <- tidyr::spread(a, Sex, N)
