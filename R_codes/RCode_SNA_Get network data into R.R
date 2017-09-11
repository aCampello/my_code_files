rm(list=ls()) # Clear all objects from workspace


#========================================================================================#
#       HOW TO GET NETWORK DATA INTO R
#========================================================================================#
# Codes here are from asnipe manual and supp mat from: Farine & Whitehead (2015). "Constructing, 
# conducting, and understanding social network analysis". Journal of Animal Ecology.

#========================================================================================#
#       Asnipe example data
#========================================================================================#
library(asnipe)
# Either create the data manually using the following code, or import the data from the R files that 
# came with the package (below)

# Example data in "individuals" format - a list of individuals, one per group per line
individuals <- data.frame(ID=c("C695905","H300253","H300253",
                               "H300283","H839876","F464557","H300296","H300253",
                               "F464557","H300296","C695905","H300283","H839876"),
                          GROUP=c(1,1,2,2,2,3,3,4,5,5,6,6,6))
## create a time column:
individuals <- cbind(individuals,
                     DAY=c(1,1,1,1,1,2,2,2,3,3,3,3,3))

# Ecample data in "groups" format 
### This is similar to how I have my daya in dyadic format in SOCPROG, But think better to use
# linear mode (data_format="individuals") with a group ID in R as not sure how R would deal with the 
# self-associations, ie. IDs = '1,1' or '2,2' and not just '1' or just '2' like below:
groups <- list(G1=c("C695905","H300253"),
               G2=c("H300253","H300283","H839876"),
               G3=c("F464557","H300296"),
               G4=c("H300253"),                  # <- self-association/group of size n=1
               G5=c("F464557","H300296"),
               G6=c("C695905","H300283","H839876"))
str(groups) # in group 1 (G1) there are 2 individuals, individual "C695905" and individual "H300253"

## create a time variable
days <- c(1,1,2,2,3,3)

### Import the above data from the asnipe R package folders:
data("group_by_individual")   # gbi matrix (named 'gbi') - individuals format
data("individuals")           # table of attribute data (named 'inds')
data("times")                 # vector of times - middle timepoint of each group (named 'times')

colnames(gbi) <- inds$RING.NUMBER # fix names in gbi with 'ring number' from the list of attributes


#========================================================================================#
#    (1)   IMPORT MY DATA as a list of animalIDs and  - BEST
#========================================================================================#
### Import data as list of associations from Camera Base, and convert to a gbi in asnipe:
setwd("G:/Statistics 2016/SNA_synced with uni 180416noon/Trying sna in R 20-21.04.16") # home
setwd("E:/Statistics 2016/SNA_synced with uni 180416noon/Trying sna in R 20-21.04.16") # uni

# DATA: From all seasons and territories, EXCL. cubs and patches not studied in all seasons (except territory 7 as needed to make it up to 4 patches/season)
datacsv <- read.csv(file = "Assocs_R_gbiformat_NoCubs_StandardPatches_noduplicates.CSV", header = T, stringsAsFactors=FALSE) # stringsasfactors stores text as text rather than factors that 'can cause problems' acc. to D. Farine
str(datacsv) # chr means character (stringsAsFactors=F means factors are imported as characters instead)

datacsv <-datacsv[order(datacsv$id), ] # order by (focal) animal ID - so can rename columns/rows in subsequent networks, matrices and arrays

# 1. Save DAY (TRANScDate) as a date format:
datacsv$Date <- strptime(datacsv$DAY,format="%d/%m/%Y") 

# 2. THEN NEED TO change the day variable to 'number of days since the first day' - this is needed or else 
# get_network within locatons etc won't work as will get the error 'length of locations will not equal 
# number of groups'
datacsv$daynum <- as.numeric(difftime(datacsv$Date,min(datacsv$Date),units="days")) + 1

datacsv$seasonyearEL<-factor(datacsv$SampleSeasonCode)
library(plyr)
datacsv$year <- revalue(datacsv$seasonyearEL,       c("Summer2013L"="1",
                                                      "Autumn2013E"="1", 
                                                      "Autumn2013L"="1",
                                                      "Winter2013E"="1",
                                                      "Winter2013L"="1",
                                                      "Spring2014E"="1",
                                                      "Spring2014L"="1",
                                                      "Summer2014E"="2",
                                                      "Summer2014L"="2",
                                                      "Autumn2014E"="2",
                                                      "Autumn2014L"="2",
                                                      "Winter2014E"="2",
                                                      "Winter2014L"="2",
                                                      "Spring2015E"="2",
                                                      "Spring2015L"="2"))

datacsv$EL <- revalue(datacsv$seasonyearEL,         c("Summer2013L"="L",
                                                      "Autumn2013E"="E", 
                                                      "Autumn2013L"="L",
                                                      "Winter2013E"="E",
                                                      "Winter2013L"="L",
                                                      "Spring2014E"="E",
                                                      "Spring2014L"="L",
                                                      "Summer2014E"="E",
                                                      "Summer2014L"="L",
                                                      "Autumn2014E"="E", 
                                                      "Autumn2014L"="L",
                                                      "Winter2014E"="E",
                                                      "Winter2014L"="L",
                                                      "Spring2015E"="E",
                                                      "Spring2015L"="L"))
# Change factors back to characters:
datacsv$seasonyearEL<-as.character(datacsv$SampleSeasonCode)
datacsv$year<-as.character(datacsv$year)
datacsv$EL<-as.character(datacsv$EL)


# Attributes (excluding cubs): need to make subsets for each network and filter by DaysSeen
attribs<-read.csv(file="Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T)
attribs <-attribs[order(attribs$id), ] # order by animal ID for renaming columns and rows of subsequent networks/matrices/arrays

# Add ShortCode column to associations data
### to the unfiltered data:
datacsv$ShortCode<-attribs[match(datacsv$id, attribs$id),2] # 2 means use info from column 2 in attribs to populate the new column in mydataNS5

# Remove foxes seen on <5 days per survey from attribute data
attribsNS5<-subset(attribs, DaysSeen>4)

# Remove duplicate rows (for diff seasons) for foxes in attribute data
unique_attribs<- subset(attribsNS5, !duplicated(ShortCode))

# List only foxes seen before or after midnight
attr_B4 <- subset(attribs, DaysSeen>4 & BeforeMidnight==1)
attr_Af <- subset(attribs, DaysSeen>4 & BeforeMidnight!=1)


# List only foxes considered resident in each territory (=seen on >50% survey days with >1 true assocs and not known to be resident in another territory) 
attr_Core <- subset(attribs, DaysSeen>19 & NTrueAssocs>1 & ResidentInAnotherTerritory!=1)

# As above but before midnight
attr_Core_B4 <- subset(attribs, DaysSeen>19 & NTrueAssocs>1 & ResidentInAnotherTerritory!=1 & BeforeMidnight==1)

# As above but after midnight
attr_Core_Af <- subset(attribs, DaysSeen>19 & NTrueAssocs>1 & ResidentInAnotherTerritory!=1 & BeforeMidnight==0)



#============================================================================================
###  SKIP THIS SECTION
#============================================================================================

# TO ADD NO. RECS PER SEASON AND TERRITORY TO ATTRIBUTES TABLE AND EXPORT:


#--- A) Calculate number of records per fox in each territory and season

attach(datacsv)
simpledf<-data.frame(id, ShortCode, SeasonID, Territory)
detach(datacsv)

library(plyr)
NrecsPerSurvey<-ddply(simpledf, c("id", "ShortCode", "SeasonID", "Territory"), summarise, Nrecs=length(id))
# Add number of records to attributes table 
attribs<-merge(x=attribs, y=NrecsPerSurvey[ , c("id", "SeasonID", "Territory", "Nrecs")], by=c("id", "SeasonID", "Territory"), all.x=TRUE) # add to attribs dataframe

#  Count no.recs in all seasons per territory
a<-ddply(NrecsPerSurvey, .(id, ShortCode, Territory), mutate, totrecs = sum(Nrecs))
attribs<-merge(x=attribs, y=a[ , c("id", "SeasonID", "Territory", "totrecs")], by=c("id", "SeasonID", "Territory"), all.x=TRUE) # add to attribs dataframe
head(a)
# Use mutate to count no. unique values without grouping: counts no. diff territories visited per season for each fox
b<-ddply(NrecsPerSurvey, .(id, ShortCode, SeasonID), mutate, Nterritories = length(unique(Territory)))
attribs<-merge(x=attribs, y=b[ , c("id", "SeasonID", "Territory", "Nterritories")], by=c("id", "SeasonID", "Territory"), all.x=TRUE) # add to attribs dataframe

# Flag if seen in multiple territories
attribs$OneTerritory <- ifelse(attribs$Nterritories >1, "0", "1") # mark as '1' if fox was only seen in 1 territory during season, otherwise '0'
head(attribs)



#--- B) RANK NUMBER OF RECORDS IN EACH TERRITORY (for if seen in >1 territory per season)

library(dplyr) 
ranks <-  attribs %>% arrange (id, ShortCode, SeasonID, Territory) %>% # ensures 'ranks' table is in same order as 'attribs'
  group_by(id, SeasonID) %>% 
  mutate(rank = rank(-Nrecs))    # use '-Nrecs' to rank highest number as rank=1, or 'Nrecs' to rank lowest number as rank=1
ranks # view top few rows to check
print.data.frame(ranks) # view all rows
attribs$rank <-ranks$rank # add ranks to attributes table
attribs<-merge(x=attribs, y=ranks[ , c("id", "SeasonID", "Territory", "rank")], by=c("id", "SeasonID", "Territory"), all.x=TRUE) # add to attribs dataframe

# Repeat excluding Territory 6
attribs.T6 <- subset(attribs, Territory!=6)
ranks.T6 <-  attribs.T6 %>% arrange (id, ShortCode, SeasonID, Territory) %>% # ensures 'ranks' table is in same order as 'attribs'
  group_by(id, SeasonID) %>% 
  mutate(rankT6 = rank(-Nrecs)) 
attribs.T6<-merge(x=attribs.T6, y=ranks.T6[ , c("id", "SeasonID", "Territory", "rankT6")], by=c("id", "SeasonID", "Territory"), all.x=TRUE) # add to attributes.T6 dataframe

# Export data as CSV file in current working directory
write.csv(attribs, file="Number of records per fox in each territory and season from R.csv", row.names = FALSE) 



#--- C) ADD RANKS TO ASSOCIATION DATA FRAME - for subsetting:

# 1. merge datacsv (associations) and attribute data frames
datacsv_ranks <- merge(x=datacsv, y=attribs[ , c("id", "rank")], by = "id", all.x=TRUE) 

# 2. sort rows by day number
datacsv_ranks <-datacsv_ranks[order(datacsv_ranks$daynum), ]

# 3. Repeat for T6
datacsv_ranks.T6 <- merge(x=datacsv_ranks, y=attribs.T6[ , c("id", "rank.T6")], by = "id", all.x=TRUE) 
datacsv_ranks.T6 <-datacsv_ranks.T6[order(datacsv_ranks.T6$daynum), ] # sort by day number
str(datacsv_ranks.T6)



#########################################################################################################################

# SUBSET ASSOCIATION DATA FOR NETWORK ANALYSIS

# Remove foxes seen on <5 days per survey 
mydataNS5<-subset(datacsv, DaysSeen.focalfox>4 & DaysSeen.sharerfox>4)


#########################################################################################################################

# FINAL DATA TO USE FOR NETWORK ANALYSIS: Foxes seen in one territory per season on min 5 days, EXCLUDING territory 6

mydataNS5
attribsNS5



#========================================================================================#
#   (2)    TAKE SUBSETS FOR EACH SEASON & YEAR
#========================================================================================#

T1spr_B4 <-subset(mydataNS5, SeasonID==1 & Territory==1 & BeforeMidnight==1) 
T1sum_B4 <-subset(mydataNS5, SeasonID==2 & Territory==1 & BeforeMidnight==1)
T1aut_B4 <-subset(mydataNS5, SeasonID==3 & Territory==1 & BeforeMidnight==1)
T1win_B4 <-subset(mydataNS5, SeasonID==4 & Territory==1 & BeforeMidnight==1)

T1spr_attr_B4 <-subset(attr_B4, SeasonID==1 & Territory==1)
T1sum_attr_B4 <-subset(attr_B4, SeasonID==2 & Territory==1)
T1aut_attr_B4 <-subset(attr_B4, SeasonID==3 & Territory==1)
T1win_attr_B4 <-subset(attr_B4, SeasonID==4 & Territory==1)

T1spr_Af <-subset(datacsv, SeasonID==1 & Territory==1 & BeforeMidnight==0)
T1sum_Af <-subset(datacsv, SeasonID==2 & Territory==1 & BeforeMidnight==0)
T1aut_Af <-subset(datacsv, SeasonID==3 & Territory==1 & BeforeMidnight==0)
T1win_Af <-subset(datacsv, SeasonID==4 & Territory==1 & BeforeMidnight==0)

#-------------------------------------------------------------------------------
# Tried splitting networks into before/after midnight but too few assocs were recorded after midnight 
# so data could not be randomised and networks that differ so much in size cannot be reliably compared.
#-------------------------------------------------------------------------------

T1_spr <-subset(mydataNS5, SeasonID==1 & Territory==1)
T1_sum <-subset(mydataNS5, SeasonID==2 & Territory==1)
T1_aut <-subset(mydataNS5, SeasonID==3 & Territory==1)
T1_win <-subset(mydataNS5, SeasonID==4 & Territory==1)

T1_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==1)
T1_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==1)
T1_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==1)
T1_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==1)

T2_spr <-subset(mydataNS5, SeasonID==1 & Territory==2)
T2_sum <-subset(mydataNS5, SeasonID==2 & Territory==2)
T2_aut <-subset(mydataNS5, SeasonID==3 & Territory==2)
T2_win <-subset(mydataNS5, SeasonID==4 & Territory==2)

T2_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==2)
T2_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==2)
T2_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==2)
T2_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==2)

T3_spr <-subset(mydataNS5, SeasonID==1 & Territory==3)
T3_sum <-subset(mydataNS5, SeasonID==2 & Territory==3)
T3_aut <-subset(mydataNS5, SeasonID==3 & Territory==3)
T3_win <-subset(mydataNS5, SeasonID==4 & Territory==3)

T3_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==3)
T3_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==3)
T3_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==3)
T3_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==3)

T4_spr <-subset(mydataNS5, SeasonID==1 & Territory==1)
T4_sum <-subset(mydataNS5, SeasonID==2 & Territory==1)
T4_aut <-subset(mydataNS5, SeasonID==3 & Territory==1)
T4_win <-subset(mydataNS5, SeasonID==4 & Territory==1)

T4_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==4)
T4_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==4)
T4_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==4)
T4_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==4)

T5_spr <-subset(mydataNS5, SeasonID==1 & Territory==5)
T5_sum <-subset(mydataNS5, SeasonID==2 & Territory==5)
T5_aut <-subset(mydataNS5, SeasonID==3 & Territory==5)
T5_win <-subset(mydataNS5, SeasonID==4 & Territory==5)

T5_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==5)
T5_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==5)
T5_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==5)
T5_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==5)

T6_spr <-subset(mydataNS5, SeasonID==1 & Territory==6)
T6_sum <-subset(mydataNS5, SeasonID==2 & Territory==6)
T6_aut <-subset(mydataNS5, SeasonID==3 & Territory==6)
T6_win <-subset(mydataNS5, SeasonID==4 & Territory==6)

T6_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==6)
T6_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==6)
T6_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==6)
T6_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==6)

T7_spr <-subset(mydataNS5, SeasonID==1 & Territory==7)
T7_sum <-subset(mydataNS5, SeasonID==2 & Territory==7)
T7_aut <-subset(mydataNS5, SeasonID==3 & Territory==7)
T7_win <-subset(mydataNS5, SeasonID==4 & Territory==7)

T7_spr_attribs <-subset(attribsNS5, SeasonID==1 & Territory==7)
T7_sum_attribs <-subset(attribsNS5, SeasonID==2 & Territory==7)
T7_aut_attribs <-subset(attribsNS5, SeasonID==3 & Territory==7)
T7_win_attribs <-subset(attribsNS5, SeasonID==4 & Territory==7)


#========================================================================================#
#       CREATE A GROUP-BY-INDIVIDUAL (GBI) MATRIX - SKIP as I'm using a sampling periods array
#========================================================================================#
### 1 column per individual & 1 row per group (association) across whole study 
### 0 or 1 in each cell shows which which foxes were in each group.

# Using Asnipe example data in 'Individuals' format:
gbi <- get_group_by_individual(individuals,
                               data_format="individuals") # here IDs are ring numbers - i could use ShortCode...

# Using Asnipe example data: 'Groups' format:
gbi <- get_group_by_individual(groups,
                               data_format="groups")


#========================================================================================#
#    (3)   MAKE 8 SAMPLING PERIODS ARRAYS (Like in SOCPROG) - one per season/year
#========================================================================================#
### Creates a stack of association matrices (one per day per territory) = a K x N x N matrix 
# containing 1 or 0 in each cell: binary for whether or not the pair interacted
# so the SP object is the size of the days * number of locations (so number of groups = number of layers = number of days*territories = 160*7 = 1120)

T1_spr_SP <- get_sampling_periods(association_data=T1_spr[,c(1,2)], # columns 1 and 2 contain ID and group/observation
                                  association_times=T1_spr$daynum,  # time units are days (formatted as 'number of timeunits since the time unit when the first group was recorded')
                                  sampling_period=1,                   # time interval to use as sampling period (=over which to group data), here = 1 day (=same units as time field)
                                  data_format="individuals", 
                                  identities=NULL)
dim(T1_spr_SP) # view dimensions/size of SP
T1_spr_SP[1,,] # view association matrix for the first day and territory combination

T1_sum_SP <- get_sampling_periods(association_data=T1_sum[,c(1,2)], 
                                  association_times=T1_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T1_aut_SP <- get_sampling_periods(association_data=T1_aut[,c(1,2)], 
                                  association_times=T1_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T1_win_SP <- get_sampling_periods(association_data=T1_win[,c(1,2)], 
                                  association_times=T1_win$daynum,   
                                  sampling_period=1, data_format="individuals")
#--
T2_spr_SP <- get_sampling_periods(association_data=T2_spr[,c(1,2)], 
                                  association_times=T2_spr$daynum,   
                                  sampling_period=1, data_format="individuals")

T2_sum_SP <- get_sampling_periods(association_data=T2_sum[,c(1,2)], 
                                  association_times=T2_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T2_aut_SP <- get_sampling_periods(association_data=T2_aut[,c(1,2)], 
                                  association_times=T2_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T2_win_SP <- get_sampling_periods(association_data=T2_win[,c(1,2)], 
                                  association_times=T2_win$daynum,   
                                  sampling_period=1, data_format="individuals")
#--
T3_spr_SP <- get_sampling_periods(association_data=T3_spr[,c(1,2)], 
                                  association_times=T3_spr$daynum,   
                                  sampling_period=1, data_format="individuals")

T3_sum_SP <- get_sampling_periods(association_data=T3_sum[,c(1,2)], 
                                  association_times=T3_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T3_aut_SP <- get_sampling_periods(association_data=T3_aut[,c(1,2)], 
                                  association_times=T3_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T3_win_SP <- get_sampling_periods(association_data=T3_win[,c(1,2)], 
                                  association_times=T3_win$daynum,   
                                  sampling_period=1, data_format="individuals")
#--
T4_spr_SP <- get_sampling_periods(association_data=T4_spr[,c(1,2)], 
                                  association_times=T4_spr$daynum,   
                                  sampling_period=1, data_format="individuals")

T4_sum_SP <- get_sampling_periods(association_data=T4_sum[,c(1,2)], 
                                  association_times=T4_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T4_aut_SP <- get_sampling_periods(association_data=T4_aut[,c(1,2)], 
                                  association_times=T4_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T4_win_SP <- get_sampling_periods(association_data=T4_win[,c(1,2)], 
                                  association_times=T4_win$daynum,   
                                  sampling_period=1, data_format="individuals")
#--
T5_spr_SP <- get_sampling_periods(association_data=T5_spr[,c(1,2)], 
                                  association_times=T5_spr$daynum,   
                                  sampling_period=1, data_format="individuals")

T5_sum_SP <- get_sampling_periods(association_data=T5_sum[,c(1,2)], 
                                  association_times=T5_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T5_aut_SP <- get_sampling_periods(association_data=T5_aut[,c(1,2)], 
                                  association_times=T5_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T5_win_SP <- get_sampling_periods(association_data=T5_win[,c(1,2)], 
                                  association_times=T5_win$daynum,   
                                  sampling_period=1, data_format="individuals")
#--
T6_spr_SP <- get_sampling_periods(association_data=T6_spr[,c(1,2)], 
                                  association_times=T6_spr$daynum,   
                                  sampling_period=1, data_format="individuals")

T6_sum_SP <- get_sampling_periods(association_data=T6_sum[,c(1,2)], 
                                  association_times=T6_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T6_aut_SP <- get_sampling_periods(association_data=T6_aut[,c(1,2)], 
                                  association_times=T6_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T6_win_SP <- get_sampling_periods(association_data=T6_win[,c(1,2)], 
                                  association_times=T6_win$daynum,   
                                  sampling_period=1, data_format="individuals")
#--
T7_spr_SP <- get_sampling_periods(association_data=T7_spr[,c(1,2)], 
                                  association_times=T7_spr$daynum,   
                                  sampling_period=1, data_format="individuals")

T7_sum_SP <- get_sampling_periods(association_data=T7_sum[,c(1,2)], 
                                  association_times=T7_sum$daynum,   
                                  sampling_period=1, data_format="individuals")

T7_aut_SP <- get_sampling_periods(association_data=T7_aut[,c(1,2)], 
                                  association_times=T7_aut$daynum,   
                                  sampling_period=1, data_format="individuals")

T7_win_SP <- get_sampling_periods(association_data=T7_win[,c(1,2)], 
                                  association_times=T7_win$daynum,   
                                  sampling_period=1, data_format="individuals")

#========================================================================================#
#   (4)    CREATE NETWORK (ASSOCIATION MATRIX) FROM SAMPLING PERIODS ARRAY
#========================================================================================#


T1_spr_net <- get_network(T1_spr_SP, data_format="SP", association_index="SRI") # might take a while
T1_sum_net <- get_network(T1_sum_SP, data_format="SP", association_index="SRI")
T1_aut_net <- get_network(T1_aut_SP, data_format="SP", association_index="SRI")
T1_win_net <- get_network(T1_win_SP, data_format="SP", association_index="SRI")

isolates(T1_aut_net, diag=FALSE) # List isolates (as row/column number) - sna package

T2_spr_net <- get_network(T2_spr_SP, data_format="SP", association_index="SRI") 
T2_sum_net <- get_network(T2_sum_SP, data_format="SP", association_index="SRI")
T2_aut_net <- get_network(T2_aut_SP, data_format="SP", association_index="SRI")
T2_win_net <- get_network(T2_win_SP, data_format="SP", association_index="SRI")

T3_spr_net <- get_network(T3_spr_SP, data_format="SP", association_index="SRI") 
T3_sum_net <- get_network(T3_sum_SP, data_format="SP", association_index="SRI")
T3_aut_net <- get_network(T3_aut_SP, data_format="SP", association_index="SRI")
T3_win_net <- get_network(T3_win_SP, data_format="SP", association_index="SRI")

T4_spr_net <- get_network(T4_spr_SP, data_format="SP", association_index="SRI") 
T4_sum_net <- get_network(T4_sum_SP, data_format="SP", association_index="SRI")
T4_aut_net <- get_network(T4_aut_SP, data_format="SP", association_index="SRI")
T4_win_net <- get_network(T4_win_SP, data_format="SP", association_index="SRI")

T5_spr_net <- get_network(T5_spr_SP, data_format="SP", association_index="SRI") 
T5_sum_net <- get_network(T5_sum_SP, data_format="SP", association_index="SRI")
T5_aut_net <- get_network(T5_aut_SP, data_format="SP", association_index="SRI")
T5_win_net <- get_network(T5_win_SP, data_format="SP", association_index="SRI")

T6_spr_net <- get_network(T6_spr_SP, data_format="SP", association_index="SRI") 
T6_sum_net <- get_network(T6_sum_SP, data_format="SP", association_index="SRI")
T6_aut_net <- get_network(T6_aut_SP, data_format="SP", association_index="SRI")
T6_win_net <- get_network(T6_win_SP, data_format="SP", association_index="SRI")

T7_spr_net <- get_network(T7_spr_SP, data_format="SP", association_index="SRI") 
T7_sum_net <- get_network(T7_sum_SP, data_format="SP", association_index="SRI")
T7_aut_net <- get_network(T7_aut_SP, data_format="SP", association_index="SRI")
T7_win_net <- get_network(T7_win_SP, data_format="SP", association_index="SRI")

#========================================================================================#
#   (5)    PERMUTATIONS TO GENERATE RANDOM NETWORKS FOR HYPOTHESIS TESTING
#========================================================================================#
# Make set of random networks based on sampling periods array
## Data are already separated by day and territory in SP so don't need to include within_location
## or within_day in the network_permutation code (D Farine).

random_networks <- network_permutation(association_data=SP,
                                       data_format="SP",
                                       permutations=1000,      # Takes AGES
                                       association_matrix=net, # starting matrix (to speed things up)
                                       returns=1,              # to help if computer runs out of memory when
                                       association_index="SRI",    # running lots of permutations (see below)
                                       days=rownames(SP),
                                       within_day=TRUE)

# Note from Damien Farine on the 'returns' parameter:
# The network that is output from random_networks is a k x N x N, where k = no. permutations. 
# This output can be pretty large if you have a high N. 
# Setting 'returns' to >1 means the code will run a few permutations at a time before returning the matrix.
# So if you wanted to do 10,000 permutations (might take a long time with your data), and you set returns=10,
# you would get 1000 matrices back, each representing the state of the random network after 10 more permutations."

# 1000 permutations each with 1 return will be fine for my data as I only have small groups (- D. Farine)

#========================================================================================#
#   (6)    COMPARE NETWORK MEASURES FROM REAL NETWORK WITH 1000 RANDOM NETWORKS
#========================================================================================#
## calculate the weighted degree 
library(sna)
deg_weighted <- degree(T1_spr_net,gmode="graph", ignore.eval=FALSE) # for real network
deg_weighted_rand <- degree(random_networks, gmode="graph", g=c(1:5000), ignore.eval=FALSE) # for each permutation


## plot the distribution of permutations with the original data overlaid
hist(colMeans(deg_weighted_rand),breaks=100,
     main=paste("P = ",sum(mean(deg_weighted) < colMeans(deg_weighted_rand))/ncol(deg_weighted_rand)),
     xlab="Weighted degree", ylab="Probability")
abline(v=mean(deg_weighted), col="red")


# Eigenvector centrality
a<-evcent(T1_spr_net, gmode="graph", diag=FALSE, # graph" means undirected, as opposed to gmode="digraph"
          tmaxdev=FALSE, ignore.eval=FALSE, tol=1e-10,
          use.eigen=FALSE)
data.frame(a)

# Clustering coefficient (aka 'transivity' in igraph and sna)
gtrans(T1_spr_net, diag=FALSE, mode="graph") # only gives overall CC not per individual
detach("package:sna")


# igraph seems better:
library(igraph)
T1_spr_net_graph <- graph_from_adjacency_matrix(T1_spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
data.frame(get.edgelist(T1_spr_net_graph), round(E(T1_spr_net_graph)$weight, 3)) # list edge weights between all dyads
mean(E(T1_spr_net_graph)$weight)
simplify(T1_spr_net_graph, edge.attr.comb=list(weight=function(x) length(x)/sum(1/x)))


deg_binary.ig <- degree(T1_spr_net_graph) # binary degree
deg.ig <- graph.strength(T1_spr_net_graph) # weighted degree
cc<-transitivity(T1_spr_net_graph, type = "weighted") # weighted clustering coefficient using the Barratt 2004 method
eigc<-eigen_centrality(T1_spr_net_graph, scale = FALSE)$vector # eig cent (scale=T would rescale results between 0-1. Can't compare networks then as would depend on no. individs)
$vector # need this to view the centrality measures



#========================================================================================#
#    (7)   MODELLING ATTRIBUTE EFFECTS ON NETWORK MEASURES - SEE OTHER FILES
#========================================================================================#

# LMM in R - as responses are continuous, not counts!
library(lme4)
model_deg_real <- lmer(degree ~ sex*status*season + (1|id) + (1|Territory) + (1|early), data=net)
model_deg_rand <- lmer(degree ~ sex*status*season + (1|id) + (1|Territory) + (1|early), data=random_networks)
# To run this, need to calculate the degree, strength etc from the network and then save it as an
# attribute. Then split data into seasons, remove non-core foxes...


