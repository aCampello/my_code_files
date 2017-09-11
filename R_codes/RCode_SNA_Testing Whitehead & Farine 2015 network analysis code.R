
# This code is for all network analyses run for chapter 5 in thesis


rm(list=ls()) 

#=======================================================#
#=======================================================#
##  NETWORK ANALYSIS: MODELLING CENTRALITY MEASURES
#=======================================================#
#=======================================================#

# LOAD IN AND PREPARE DATA
setwd("G:/Statistics 2016/SNA/SNA in R") # Seagate

# Load associations
datacsv <- read.csv(file = "Assocs_R_gbiformat_NoCubs_StandardPatches_noduplicates.csv", header = T, stringsAsFactors=FALSE) # stringsasfactors stores text as text rather than factors that 'can cause problems' acc. to D. Farine
datacsv <-datacsv[order(datacsv$id), ] # order by (focal) animal ID - so can rename columns/rows in subsequent networks, matrices and arrays
datacsv$Date <- strptime(datacsv$DAY,format="%d/%m/%Y") 
datacsv$daynum <- as.numeric(difftime(datacsv$Date,min(datacsv$Date),units="days"))+1

# Load attributes
attribs<-read.csv(file="Attribs_R_NoCubs_StandardPatches_CoreResidentsMarked.csv", header=T)
attribs <-attribs[order(attribs$id), ] # order by animal ID
attribs$capturerate <- attribs$DaysSeen/40 # calculate capture rate as proportion out of total possible N days seen (per survey)

# Networks with no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
# Mark in column: 1 = Yes, 0 = No pref/av assocs.
attribs$prefavoided <- ifelse(attribs$Territory==2 & attribs$SeasonID==2, "0", NA)
attribs$prefavoided <- ifelse(attribs$Territory==4 & attribs$SeasonID==1 & is.na(attribs$prefavoided),  "0", attribs$prefavoided)
attribs$prefavoided <- ifelse(attribs$Territory==4 & attribs$SeasonID==2 & is.na(attribs$prefavoided),  "0", attribs$prefavoided)
attribs$prefavoided <- ifelse(attribs$Territory==5 & attribs$SeasonID==4 & is.na(attribs$prefavoided),  "0", attribs$prefavoided)
attribs$prefavoided <-  ifelse(is.na(attribs$prefavoided), "1", "0")

# Remove poorly-sampled foxes (seen on <5 days) 
mydataNS5 <- subset(datacsv, DaysSeen.focalfox>4 & DaysSeen.sharerfox>4)
attr <- attribs[!duplicated(attribs[1:3]),] # lists unique combinations of columns 1-3 (to remove duplicated rows for before/after midnight))
attr_unique <- attr
# Make separate datasets for before and after midnight
mydataNS5_B4 <- subset(mydataNS5, BeforeMidnight==1)
mydataNS5_Af <- subset(mydataNS5, BeforeMidnight==0)
attr_B4 <- subset(attribs, DaysSeen>4 & BeforeMidnight==1)
attr_Af <- subset(attribs, DaysSeen>4 & BeforeMidnight==0)


#============================================================
#   SUBSETTING FOR EACH TERRITORY & SEASON
#============================================================

# T1 assocs
T1spr <-subset(mydataNS5, SeasonID==1 & Territory==1) # sp ALL
T1sum <-subset(mydataNS5, SeasonID==2 & Territory==1) # su ALL
T1aut <-subset(mydataNS5, SeasonID==3 & Territory==1) # au ALL
T1win <-subset(mydataNS5, SeasonID==4 & Territory==1) # wi ALL

# T2 assocs
T2spr <-subset(mydataNS5, SeasonID==1 & Territory==2) # sp ALL
T2sum <-subset(mydataNS5, SeasonID==2 & Territory==2) # su ALL
T2aut <-subset(mydataNS5, SeasonID==3 & Territory==2) # au ALL
T2win <-subset(mydataNS5, SeasonID==4 & Territory==2) # wi ALL

# T3 assocs
T3spr <-subset(mydataNS5, SeasonID==1 & Territory==3) # sp ALL
T3sum <-subset(mydataNS5, SeasonID==2 & Territory==3) # su ALL
T3aut <-subset(mydataNS5, SeasonID==3 & Territory==3) # au ALL
T3win <-subset(mydataNS5, SeasonID==4 & Territory==3) # wi ALL

# T4 assocs
T4spr <-subset(mydataNS5, SeasonID==1 & Territory==4) # sp ALL
T4sum <-subset(mydataNS5, SeasonID==2 & Territory==4) # su ALL
T4aut <-subset(mydataNS5, SeasonID==3 & Territory==4) # au ALL
T4win <-subset(mydataNS5, SeasonID==4 & Territory==4) # wi ALL

# T5 assocs
T5spr <-subset(mydataNS5, SeasonID==1 & Territory==5) # sp ALL
T5sum <-subset(mydataNS5, SeasonID==2 & Territory==5) # su ALL
T5aut <-subset(mydataNS5, SeasonID==3 & Territory==5) # au ALL
T5win <-subset(mydataNS5, SeasonID==4 & Territory==5) # wi ALL

# T6 assocs
T6spr <-subset(mydataNS5, SeasonID==1 & Territory==6) # sp ALL
T6sum <-subset(mydataNS5, SeasonID==2 & Territory==6) # su ALL
T6aut <-subset(mydataNS5, SeasonID==3 & Territory==6) # au ALL
T6win <-subset(mydataNS5, SeasonID==4 & Territory==6) # wi ALL

# T7 assocs
T7spr <-subset(mydataNS5, SeasonID==1 & Territory==7) # sp ALL
T7sum <-subset(mydataNS5, SeasonID==2 & Territory==7) # su ALL
T7aut <-subset(mydataNS5, SeasonID==3 & Territory==7) # au ALL
T7win <-subset(mydataNS5, SeasonID==4 & Territory==7) # wi ALL

# Save attributes
T1spr_attr <- subset(attr, Territory==1 & SeasonID==1 & DaysSeen>4) 
T1sum_attr <- subset(attr, Territory==1 & SeasonID==2 & DaysSeen>4) 
T1aut_attr <- subset(attr, Territory==1 & SeasonID==3 & DaysSeen>4) 
T1win_attr <- subset(attr, Territory==1 & SeasonID==4 & DaysSeen>4) 

T2spr_attr <- subset(attr, Territory==2 & SeasonID==1 & DaysSeen>4) 
T2sum_attr <- subset(attr, Territory==2 & SeasonID==2 & DaysSeen>4) 
T2aut_attr <- subset(attr, Territory==2 & SeasonID==3 & DaysSeen>4) 
T2win_attr <- subset(attr, Territory==2 & SeasonID==4 & DaysSeen>4)

T3spr_attr <- subset(attr, Territory==3 & SeasonID==1 & DaysSeen>4) 
T3sum_attr <- subset(attr, Territory==3 & SeasonID==2 & DaysSeen>4) 
T3aut_attr <- subset(attr, Territory==3 & SeasonID==3 & DaysSeen>4) 
T3win_attr <- subset(attr, Territory==3 & SeasonID==4 & DaysSeen>4)

T4spr_attr <- subset(attr, Territory==4 & SeasonID==1 & DaysSeen>4) 
T4sum_attr <- subset(attr, Territory==4 & SeasonID==2 & DaysSeen>4) 
T4aut_attr <- subset(attr, Territory==4 & SeasonID==3 & DaysSeen>4) 
T4win_attr <- subset(attr, Territory==4 & SeasonID==4 & DaysSeen>4)

T5spr_attr <- subset(attr, Territory==5 & SeasonID==1 & DaysSeen>4) 
T5sum_attr <- subset(attr, Territory==5 & SeasonID==2 & DaysSeen>4) 
T5aut_attr <- subset(attr, Territory==5 & SeasonID==3 & DaysSeen>4) 
T5win_attr <- subset(attr, Territory==5 & SeasonID==4 & DaysSeen>4)

T6spr_attr <- subset(attr, Territory==6 & SeasonID==1 & DaysSeen>4) 
T6sum_attr <- subset(attr, Territory==6 & SeasonID==2 & DaysSeen>4) 
T6aut_attr <- subset(attr, Territory==6 & SeasonID==3 & DaysSeen>4) 
T6win_attr <- subset(attr, Territory==6 & SeasonID==4 & DaysSeen>4)

T7spr_attr <- subset(attr, Territory==7 & SeasonID==1 & DaysSeen>4) 
T7sum_attr <- subset(attr, Territory==7 & SeasonID==2 & DaysSeen>4) 
T7aut_attr <- subset(attr, Territory==7 & SeasonID==3 & DaysSeen>4) 
T7win_attr <- subset(attr, Territory==7 & SeasonID==4 & DaysSeen>4)

#============================================================
#   SUBSETTING BEFORE & AFTER (for each territory & season)
#============================================================

# T1 assocs:
T1sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==1) # spring B4
T1sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==1) # Af
T1sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==1) # summer B4
T1sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==1) # Af
T1autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==1) # autumn B4
T1autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==1) # Af
T1winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==1) # winter B4
T1winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==1) # Af

# T2 assocs:
T2sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==2) # spring B4
T2sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==2) # Af
T2sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==2) # summer B4
T2sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==2) # Af # empty - only self-associations, no true assocs at all
T2autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==2) # autumn B4
T2autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==2) # Af
T2winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==2) # winter B4
T2winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==2) # Af

# T3 assocs:
T3sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==3) # spring B4
T3sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==3) # Af
T3sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==3) # summer B4
T3sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==3) # Af
T3autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==3) # autumn B4
T3autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==3) # Af
T3winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==3) # winter B4
T3winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==3) # Af

# T4 assocs:
T4sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==4) # spring B4
T4sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==4) # Af
T4sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==4) # summer B4
T4sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==4) # Af
T4autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==4) # autumn B4
T4autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==4) # Af
T4winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==4) # winter B4
T4winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==4) # Af

# T5 assocs:
T5sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==5) # spring B4
T5sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==5) # Af
T5sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==5) # summer B4
T5sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==5) # Af
T5autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==5) # autumn B4
T5autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==5) # Af
T5winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==5) # winter B4
T5winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==5) # Af

# T6 assocs:
T6sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==6) # spring B4
T6sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==6) # spr Af
T6sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==6) # summer B4
T6sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==6) # sum Af
T6autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==6) # autumn B4
T6autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==6) # aut Af
T6winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==6) # winter B4
T6winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==6) # win Af

# T7 assocs:
T7sprB4 <-subset(mydataNS5_B4, SeasonID==1 & Territory==7) # spring B4
T7sprAf <-subset(mydataNS5_Af, SeasonID==1 & Territory==7) # spr Af
T7sumB4 <-subset(mydataNS5_B4, SeasonID==2 & Territory==7) # summer B4
T7sumAf <-subset(mydataNS5_Af, SeasonID==2 & Territory==7) # sum Af
T7autB4 <-subset(mydataNS5_B4, SeasonID==3 & Territory==7) # autumn B4
T7autAf <-subset(mydataNS5_Af, SeasonID==3 & Territory==7) # aut Af
T7winB4 <-subset(mydataNS5_B4, SeasonID==4 & Territory==7) # winter B4
T7winAf <-subset(mydataNS5_Af, SeasonID==4 & Territory==7) # win Af

#------

# Attribs: to rename matrix rows
T1sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==1) # spring B4
T1sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==1) # Af
T1sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==1) # summer B4
T1sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==1) # Af
T1autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==1) # autumn B4
T1autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==1) # Af
T1winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==1) # winter B4
T1winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==1) # Af

T2sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==2) # spring B4
T2sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==2) # Af
T2sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==2) # summer B4
T2sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==2) # Af
T2autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==2) # autumn B4
T2autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==2) # Af
T2winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==2) # winter B4
T2winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==2) # Af

T3sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==3) # spring B4
T3sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==3) # Af
T3sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==3) # summer B4
T3sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==3) # Af
T3autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==3) # autumn B4
T3autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==3) # Af
T3winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==3) # winter B4
T3winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==3) # Af

T4sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==4) # spring B4
T4sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==4) # Af
T4sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==4) # summer B4
T4sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==4) # Af
T4autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==4) # autumn B4
T4autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==4) # Af
T4winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==4) # winter B4
T4winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==4) # Af

T5sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==5) # spring B4
T5sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==5) # Af
T5sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==5) # summer B4
T5sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==5) # Af
T5autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==5) # autumn B4
T5autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==5) # Af
T5winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==5) # winter B4
T5winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==5) # Af

T6sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==6) # spring B4
T6sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==6) # Af
T6sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==6) # summer B4
T6sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==6) # Af
T6autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==6) # autumn B4
T6autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==6) # Af
T6winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==6) # winter B4
T6winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==6) # Af

T7sprB4_attr <-subset(attr_B4, SeasonID==1 & Territory==7) # spring B4
T7sprAf_attr <-subset(attr_Af, SeasonID==1 & Territory==7) # Af
T7sumB4_attr <-subset(attr_B4, SeasonID==2 & Territory==7) # summer B4
T7sumAf_attr <-subset(attr_Af, SeasonID==2 & Territory==7) # Af
T7autB4_attr <-subset(attr_B4, SeasonID==3 & Territory==7) # autumn B4
T7autAf_attr <-subset(attr_Af, SeasonID==3 & Territory==7) # Af
T7winB4_attr <-subset(attr_B4, SeasonID==4 & Territory==7) # winter B4
T7winAf_attr <-subset(attr_Af, SeasonID==4 & Territory==7) # Af

#============================================================
# MAKE SAMPLING PERIOD ARRAYS
#============================================================
library(asnipe)

#===== BEFORE/AFTER MIDNIGHT

# Territory 1:
T1sprB4_SP <- get_sampling_periods(association_data= T1sprB4[,c(1,2)], association_times= T1sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T1sprAf_SP <- get_sampling_periods(association_data= T1sprAf[,c(1,2)], association_times= T1sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T1sumB4_SP <- get_sampling_periods(association_data= T1sumB4[,c(1,2)], association_times= T1sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T1sumAf_SP <- get_sampling_periods(association_data= T1sumAf[,c(1,2)], association_times= T1sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af
T1autB4_SP <- get_sampling_periods(association_data= T1autB4[,c(1,2)], association_times= T1autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T1autAf_SP <- get_sampling_periods(association_data= T1autAf[,c(1,2)], association_times= T1autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T1winB4_SP <- get_sampling_periods(association_data= T1winB4[,c(1,2)], association_times= T1winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T1winAf_SP <- get_sampling_periods(association_data= T1winAf[,c(1,2)], association_times= T1winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4

# Territory 2:
T2sprB4_SP <- get_sampling_periods(association_data= T2sprB4[,c(1,2)], association_times= T2sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T2sprAf_SP <- get_sampling_periods(association_data= T2sprAf[,c(1,2)], association_times= T2sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T2sumB4_SP <- get_sampling_periods(association_data= T2sumB4[,c(1,2)], association_times= T2sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T2sumAf_SP <- get_sampling_periods(association_data= T2sumAf[,c(1,2)], association_times= T2sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af # empty - no true assocs
T2autB4_SP <- get_sampling_periods(association_data= T2autB4[,c(1,2)], association_times= T2autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T2autAf_SP <- get_sampling_periods(association_data= T2autAf[,c(1,2)], association_times= T2autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T2winB4_SP <- get_sampling_periods(association_data= T2winB4[,c(1,2)], association_times= T2winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T2winAf_SP <- get_sampling_periods(association_data= T2winAf[,c(1,2)], association_times= T2winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4

# Territory 3:
T3sprB4_SP <- get_sampling_periods(association_data= T3sprB4[,c(1,2)], association_times= T3sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T3sprAf_SP <- get_sampling_periods(association_data= T3sprAf[,c(1,2)], association_times= T3sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T3sumB4_SP <- get_sampling_periods(association_data= T3sumB4[,c(1,2)], association_times= T3sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T3sumAf_SP <- get_sampling_periods(association_data= T3sumAf[,c(1,2)], association_times= T3sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af
T3autB4_SP <- get_sampling_periods(association_data= T3autB4[,c(1,2)], association_times= T3autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T3autAf_SP <- get_sampling_periods(association_data= T3autAf[,c(1,2)], association_times= T3autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T3winB4_SP <- get_sampling_periods(association_data= T3winB4[,c(1,2)], association_times= T3winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T3winAf_SP <- get_sampling_periods(association_data= T3winAf[,c(1,2)], association_times= T3winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4

# Territory 4:
T4sprB4_SP <- get_sampling_periods(association_data= T4sprB4[,c(1,2)], association_times= T4sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T4sprAf_SP <- get_sampling_periods(association_data= T4sprAf[,c(1,2)], association_times= T4sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T4sumB4_SP <- get_sampling_periods(association_data= T4sumB4[,c(1,2)], association_times= T4sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T4sumAf_SP <- get_sampling_periods(association_data= T4sumAf[,c(1,2)], association_times= T4sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af
T4autB4_SP <- get_sampling_periods(association_data= T4autB4[,c(1,2)], association_times= T4autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T4autAf_SP <- get_sampling_periods(association_data= T4autAf[,c(1,2)], association_times= T4autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T4winB4_SP <- get_sampling_periods(association_data= T4winB4[,c(1,2)], association_times= T4winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T4winAf_SP <- get_sampling_periods(association_data= T4winAf[,c(1,2)], association_times= T4winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4

# Territory 5:
T5sprB4_SP <- get_sampling_periods(association_data= T5sprB4[,c(1,2)], association_times= T5sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T5sprAf_SP <- get_sampling_periods(association_data= T5sprAf[,c(1,2)], association_times= T5sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T5sumB4_SP <- get_sampling_periods(association_data= T5sumB4[,c(1,2)], association_times= T5sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T5sumAf_SP <- get_sampling_periods(association_data= T5sumAf[,c(1,2)], association_times= T5sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af
T5autB4_SP <- get_sampling_periods(association_data= T5autB4[,c(1,2)], association_times= T5autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T5autAf_SP <- get_sampling_periods(association_data= T5autAf[,c(1,2)], association_times= T5autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T5winB4_SP <- get_sampling_periods(association_data= T5winB4[,c(1,2)], association_times= T5winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T5winAf_SP <- get_sampling_periods(association_data= T5winAf[,c(1,2)], association_times= T5winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4

# Territory 6
T6sprB4_SP <- get_sampling_periods(association_data= T6sprB4[,c(1,2)], association_times= T6sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T6sprAf_SP <- get_sampling_periods(association_data= T6sprAf[,c(1,2)], association_times= T6sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T6sumB4_SP <- get_sampling_periods(association_data= T6sumB4[,c(1,2)], association_times= T6sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T6sumAf_SP <- get_sampling_periods(association_data= T6sumAf[,c(1,2)], association_times= T6sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af
T6autB4_SP <- get_sampling_periods(association_data= T6autB4[,c(1,2)], association_times= T6autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T6autAf_SP <- get_sampling_periods(association_data= T6autAf[,c(1,2)], association_times= T6autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T6winB4_SP <- get_sampling_periods(association_data= T6winB4[,c(1,2)], association_times= T6winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T6winAf_SP <- get_sampling_periods(association_data= T6winAf[,c(1,2)], association_times= T6winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4

# Territory 7
T7sprB4_SP <- get_sampling_periods(association_data= T7sprB4[,c(1,2)], association_times= T7sprB4$daynum, sampling_period= 1, data_format= "individuals") # spring B4
T7sprAf_SP <- get_sampling_periods(association_data= T7sprAf[,c(1,2)], association_times= T7sprAf$daynum, sampling_period= 1, data_format= "individuals") # spring Af
T7sumB4_SP <- get_sampling_periods(association_data= T7sumB4[,c(1,2)], association_times= T7sumB4$daynum, sampling_period= 1, data_format= "individuals") # summer B4
T7sumAf_SP <- get_sampling_periods(association_data= T7sumAf[,c(1,2)], association_times= T7sumAf$daynum, sampling_period= 1, data_format= "individuals") # summer Af
T7autB4_SP <- get_sampling_periods(association_data= T7autB4[,c(1,2)], association_times= T7autB4$daynum, sampling_period= 1, data_format= "individuals") # autumn B4
T7autAf_SP <- get_sampling_periods(association_data= T7autAf[,c(1,2)], association_times= T7autAf$daynum, sampling_period= 1, data_format= "individuals") # autumn Af
T7winB4_SP <- get_sampling_periods(association_data= T7winB4[,c(1,2)], association_times= T7winB4$daynum, sampling_period= 1, data_format= "individuals") # winter B4
T7winAf_SP <- get_sampling_periods(association_data= T7winAf[,c(1,2)], association_times= T7winAf$daynum, sampling_period= 1, data_format= "individuals") # winter B4



#===== WHOLE DAYS ONLY

# Territory 1
T1spr_SP <- get_sampling_periods(association_data= T1spr[,c(1,2)], association_times= T1spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T1sum_SP <- get_sampling_periods(association_data= T1sum[,c(1,2)], association_times= T1sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T1aut_SP <- get_sampling_periods(association_data= T1aut[,c(1,2)], association_times= T1aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T1win_SP <- get_sampling_periods(association_data= T1win[,c(1,2)], association_times= T1win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

# Territory 2
T2spr_SP <- get_sampling_periods(association_data= T2spr[,c(1,2)], association_times= T2spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T2sum_SP <- get_sampling_periods(association_data= T2sum[,c(1,2)], association_times= T2sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T2aut_SP <- get_sampling_periods(association_data= T2aut[,c(1,2)], association_times= T2aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T2win_SP <- get_sampling_periods(association_data= T2win[,c(1,2)], association_times= T2win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

# Territory 3
T3spr_SP <- get_sampling_periods(association_data= T3spr[,c(1,2)], association_times= T3spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T3sum_SP <- get_sampling_periods(association_data= T3sum[,c(1,2)], association_times= T3sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T3aut_SP <- get_sampling_periods(association_data= T3aut[,c(1,2)], association_times= T3aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T3win_SP <- get_sampling_periods(association_data= T3win[,c(1,2)], association_times= T3win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

# Territory 4
T4spr_SP <- get_sampling_periods(association_data= T4spr[,c(1,2)], association_times= T4spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T4sum_SP <- get_sampling_periods(association_data= T4sum[,c(1,2)], association_times= T4sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T4aut_SP <- get_sampling_periods(association_data= T4aut[,c(1,2)], association_times= T4aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T4win_SP <- get_sampling_periods(association_data= T4win[,c(1,2)], association_times= T4win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

# Territory 5
T5spr_SP <- get_sampling_periods(association_data= T5spr[,c(1,2)], association_times= T5spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T5sum_SP <- get_sampling_periods(association_data= T5sum[,c(1,2)], association_times= T5sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T5aut_SP <- get_sampling_periods(association_data= T5aut[,c(1,2)], association_times= T5aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T5win_SP <- get_sampling_periods(association_data= T5win[,c(1,2)], association_times= T5win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

# Territory 6
T6spr_SP <- get_sampling_periods(association_data= T6spr[,c(1,2)], association_times= T6spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T6sum_SP <- get_sampling_periods(association_data= T6sum[,c(1,2)], association_times= T6sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T6aut_SP <- get_sampling_periods(association_data= T6aut[,c(1,2)], association_times= T6aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T6win_SP <- get_sampling_periods(association_data= T6win[,c(1,2)], association_times= T6win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

# Territory 7
T7spr_SP <- get_sampling_periods(association_data= T7spr[,c(1,2)], association_times= T7spr$daynum, sampling_period= 1, data_format= "individuals") # spring ALL
T7sum_SP <- get_sampling_periods(association_data= T7sum[,c(1,2)], association_times= T7sum$daynum, sampling_period= 1, data_format= "individuals") # summer ALL
T7aut_SP <- get_sampling_periods(association_data= T7aut[,c(1,2)], association_times= T7aut$daynum, sampling_period= 1, data_format= "individuals") # autumn ALL
T7win_SP <- get_sampling_periods(association_data= T7win[,c(1,2)], association_times= T7win$daynum, sampling_period= 1, data_format= "individuals") # winter ALL

#========================================#
# CREATE NETWORKS (ASSOCIATION MATRICES)
#========================================#

#=== BEFORE/AFTER MIDNIGHT

# Territory 1
T1sprB4_net <- get_network(T1sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T1sprAf_net <- get_network(T1sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T1sumB4_net <- get_network(T1sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T1sumAf_net <- get_network(T1sumAf_SP, data_format="SP", association_index="SRI") # summer Af
T1autB4_net <- get_network(T1autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T1autAf_net <- get_network(T1autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T1winB4_net <- get_network(T1winB4_SP, data_format="SP", association_index="SRI") # winter B4
T1winAf_net <- get_network(T1winAf_SP, data_format="SP", association_index="SRI") # winter Af

# Territory 2
T2sprB4_net <- get_network(T2sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T2sprAf_net <- get_network(T2sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T2sumB4_net <- get_network(T2sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T2sumAf_net <- get_network(T2sumAf_SP, data_format="SP", association_index="SRI") # summer Af # empty - no true assocs
T2autB4_net <- get_network(T2autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T2autAf_net <- get_network(T2autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T2winB4_net <- get_network(T2winB4_SP, data_format="SP", association_index="SRI") # winter B4
T2winAf_net <- get_network(T2winAf_SP, data_format="SP", association_index="SRI") # winter Af

# Territory 3
T3sprB4_net <- get_network(T3sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T3sprAf_net <- get_network(T3sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T3sumB4_net <- get_network(T3sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T3sumAf_net <- get_network(T3sumAf_SP, data_format="SP", association_index="SRI") # summer Af
T3autB4_net <- get_network(T3autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T3autAf_net <- get_network(T3autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T3winB4_net <- get_network(T3winB4_SP, data_format="SP", association_index="SRI") # winter B4
T3winAf_net <- get_network(T3winAf_SP, data_format="SP", association_index="SRI") # winter Af

# Territory 4
T4sprB4_net <- get_network(T4sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T4sprAf_net <- get_network(T4sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T4sumB4_net <- get_network(T4sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T4sumAf_net <- get_network(T4sumAf_SP, data_format="SP", association_index="SRI") # summer Af
T4autB4_net <- get_network(T4autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T4autAf_net <- get_network(T4autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T4winB4_net <- get_network(T4winB4_SP, data_format="SP", association_index="SRI") # winter B4
T4winAf_net <- get_network(T4winAf_SP, data_format="SP", association_index="SRI") # winter Af

# Territory 5
T5sprB4_net <- get_network(T5sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T5sprAf_net <- get_network(T5sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T5sumB4_net <- get_network(T5sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T5sumAf_net <- get_network(T5sumAf_SP, data_format="SP", association_index="SRI") # summer Af
T5autB4_net <- get_network(T5autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T5autAf_net <- get_network(T5autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T5winB4_net <- get_network(T5winB4_SP, data_format="SP", association_index="SRI") # winter B4
T5winAf_net <- get_network(T5winAf_SP, data_format="SP", association_index="SRI") # winter Af

# Territory 6
T6sprB4_net <- get_network(T6sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T6sprAf_net <- get_network(T6sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T6sumB4_net <- get_network(T6sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T6sumAf_net <- get_network(T6sumAf_SP, data_format="SP", association_index="SRI") # summer Af
T6autB4_net <- get_network(T6autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T6autAf_net <- get_network(T6autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T6winB4_net <- get_network(T6winB4_SP, data_format="SP", association_index="SRI") # winter B4
T6winAf_net <- get_network(T6winAf_SP, data_format="SP", association_index="SRI") # winter Af

# Territory 7
T7sprB4_net <- get_network(T7sprB4_SP, data_format="SP", association_index="SRI") # spring B4
T7sprAf_net <- get_network(T7sprAf_SP, data_format="SP", association_index="SRI") # spring Af
T7sumB4_net <- get_network(T7sumB4_SP, data_format="SP", association_index="SRI") # summer B4
T7sumAf_net <- get_network(T7sumAf_SP, data_format="SP", association_index="SRI") # summer Af
T7autB4_net <- get_network(T7autB4_SP, data_format="SP", association_index="SRI") # autumn B4
T7autAf_net <- get_network(T7autAf_SP, data_format="SP", association_index="SRI") # autumn Af
T7winB4_net <- get_network(T7winB4_SP, data_format="SP", association_index="SRI") # winter B4
T7winAf_net <- get_network(T7winAf_SP, data_format="SP", association_index="SRI") # winter Af


#=== WHOLE DAYS ONLY

# Territory 1
T1spr_net <- get_network(T1spr_SP, data_format="SP", association_index="SRI") # spr ALL
T1sum_net <- get_network(T1sum_SP, data_format="SP", association_index="SRI") # sum ALL
T1aut_net <- get_network(T1aut_SP, data_format="SP", association_index="SRI") # aut ALL
T1win_net <- get_network(T1win_SP, data_format="SP", association_index="SRI") # win ALL

# Territory 2
T2spr_net <- get_network(T2spr_SP, data_format="SP", association_index="SRI") # spr ALL
T2sum_net <- get_network(T2sum_SP, data_format="SP", association_index="SRI") # sum ALL
T2aut_net <- get_network(T2aut_SP, data_format="SP", association_index="SRI") # aut ALL
T2win_net <- get_network(T2win_SP, data_format="SP", association_index="SRI") # win ALL

# Territory 3
T3spr_net <- get_network(T3spr_SP, data_format="SP", association_index="SRI") # spr ALL
T3sum_net <- get_network(T3sum_SP, data_format="SP", association_index="SRI") # sum ALL
T3aut_net <- get_network(T3aut_SP, data_format="SP", association_index="SRI") # aut ALL
T3win_net <- get_network(T3win_SP, data_format="SP", association_index="SRI") # win ALL

# Territory 4
T4spr_net <- get_network(T4spr_SP, data_format="SP", association_index="SRI") # spr ALL
T4sum_net <- get_network(T4sum_SP, data_format="SP", association_index="SRI") # sum ALL
T4aut_net <- get_network(T4aut_SP, data_format="SP", association_index="SRI") # aut ALL
T4win_net <- get_network(T4win_SP, data_format="SP", association_index="SRI") # win ALL

# Territory 5
T5spr_net <- get_network(T5spr_SP, data_format="SP", association_index="SRI") # spr ALL
T5sum_net <- get_network(T5sum_SP, data_format="SP", association_index="SRI") # sum ALL
T5aut_net <- get_network(T5aut_SP, data_format="SP", association_index="SRI") # aut ALL
T5win_net <- get_network(T5win_SP, data_format="SP", association_index="SRI") # win ALL

# Territory 6
T6spr_net <- get_network(T6spr_SP, data_format="SP", association_index="SRI") # spr ALL
T6sum_net <- get_network(T6sum_SP, data_format="SP", association_index="SRI") # sum ALL
T6aut_net <- get_network(T6aut_SP, data_format="SP", association_index="SRI") # aut ALL
T6win_net <- get_network(T6win_SP, data_format="SP", association_index="SRI") # win ALL

# Territory 7
T7spr_net <- get_network(T7spr_SP, data_format="SP", association_index="SRI") # spr ALL
T7sum_net <- get_network(T7sum_SP, data_format="SP", association_index="SRI") # sum ALL
T7aut_net <- get_network(T7aut_SP, data_format="SP", association_index="SRI") # aut ALL
T7win_net <- get_network(T7win_SP, data_format="SP", association_index="SRI") # win ALL


#================================================================#
# CALCULATE NETWORK CENTRALITY - from all foxes seen on min 5 days
# But only include core foxes in the models (i.e. attr$Core==1)
#================================================================#

#=== CHOSEN CENTRALITY MEASURES: All are uncorrelated and meaningful (well strength~eig are semi correlated but I only checked T1winB4 and several studies have used both these measures)

# STRENGTH: sna
# EIGENVECTOR CENTRALITY: sna
# CC: tnet 
# CLOSENESS: tnet # Normalise by score/sum(scores) 


# To calculate centrality from real networks - store in attributes table
#=====================================================================================

# STRENGTH
# Q: Is individual gregariousness influenced by sex, status, season or food availability?

# Subset attributes and order by territory/season/beforemidnight/id
TAllattr_B4Af <- subset(attribs, DaysSeen>4)
TAllattr_B4Af <- TAllattr_B4Af[order(TAllattr_B4Af$Territory, TAllattr_B4Af$SeasonID, TAllattr_B4Af$BeforeMidnight, TAllattr_B4Af$id), ] # order by animal ID

# Save factors
TAllattr_B4Af$fSeason <- factor(TAllattr_B4Af$SeasonID)
TAllattr_B4Af$fTerritory <- factor(TAllattr_B4Af$Territory)
TAllattr_B4Af$fBeforeMidnight <- factor(TAllattr_B4Af$BeforeMidnight)
TAllattr_B4Af$Core <- factor(TAllattr_B4Af$Core)

# Calculate median no. days seen (only out of interest)
datacsv_noDate <- datacsv # make copy of data and remove Date column as ddply doesn't like the POSIXlt data-type 
datacsv_noDate$Date <- NULL
library(plyr)
DaysSeenTable <- ddply(datacsv_noDate, c("id", "Territory", "SeasonID"), 
                       summarise,
                       DaysSeen = length(unique(DAY)))
DaysSeenTable[1:50,]
mean(DaysSeenTable$DaysSeen) # 18.25 days
median(DaysSeenTable$DaysSeen) # 10 days

# Mark foxes in 'TAllattr_B4Af' seen above the 20-day sighting threshold (min 20 days on territory) - higher 
# than mean and median but better to be consistent with non-resident definition:
TAllattr_B4Af$DaysSeen20 <- ifelse(TAllattr_B4Af$DaysSeen<20, 0, 1) 

#------

detach(package:tnet)
detach(package:igraph)
detach(package:qgraph)
library(sna)

# Strength not normalised - USE
TAllattr_B4Af$strength <-       c(degree(T1sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1sumAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T1winB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2sumAf_net, gmode="graph", ignore.eval=F, rescale=F),# empty - no true assocs
                                  degree(T2sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T2winB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3sumAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T3winB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4sumAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T4winB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5sumAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T5winB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6sumAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T6winB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7sprAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7sprB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7sumAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7sumB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7autAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7autB4_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7winAf_net, gmode="graph", ignore.eval=F, rescale=F),
                                  degree(T7winB4_net, gmode="graph", ignore.eval=F, rescale=F))

#-----------------------------------------------------------------------------------------------------------------------------------------
# Strength (not normalised)- save as data frame with network size to include as fixed effect
strength.df <-              rbind(data.frame(strength=a<-degree(T1sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T1winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), # empty - no true assocs
                                  data.frame(strength=a<-degree(T2sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T2winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
                                  data.frame(strength=a<-degree(T3sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T3winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T4winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T5winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T6winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7sprAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7sprB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7sumAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7sumB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7autAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7autB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7winAf_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
                                  data.frame(strength=a<-degree(T7winB4_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)))
#-----------------------------------------------------------------------------------------------------------------------------------------
# Strength normalised by dividing by sum(scores) - don't use
TAllattr_B4Af$strength.norm <-  c(degree(T1sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1sumAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T1winB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2sumAf_net, gmode="graph", ignore.eval=F, rescale=T),# empty - no true assocs
                                  degree(T2sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T2winB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3sumAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T3winB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4sumAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T4winB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5sumAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T5winB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6sumAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T6winB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7sprAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7sprB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7sumAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7sumB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7autAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7autB4_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7winAf_net, gmode="graph", ignore.eval=F, rescale=T),
                                  degree(T7winB4_net, gmode="graph", ignore.eval=F, rescale=T))
#-------------------------------------------------------------------------------------------------------------------

# Strength normalised by dividing by n-1 - DO NOT USE
TAllattr_B4Af$strength.normN1 <-c((a<-degree(T1sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T1winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),# empty - no true assocs
                                  (a<-degree(T2sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T2winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T3winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T4winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T5winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T6winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7sprAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7sprB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7sumAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7sumB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7autAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7autB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7winAf_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)),
                                  (a<-degree(T7winB4_net, gmode="graph", ignore.eval=F, rescale=F)/(length(a)-1)))

# STRENGTH - WHOLE DAY - save as DF with nfoxes to show network size - same order/length as TAllattr_wd
strengthWD_df <-  rbind(
  data.frame(strength=a<-degree(T1spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T1sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T1aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T1win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T2spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T2sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T2aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T2win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T3spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),  
  data.frame(strength=a<-degree(T3sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T3aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T3win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T4spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),   
  data.frame(strength=a<-degree(T4sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),  
  data.frame(strength=a<-degree(T4aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T4win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T5spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T5sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T5aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T5win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T6spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)), 
  data.frame(strength=a<-degree(T6sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T6aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T6win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T7spr_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T7sum_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T7aut_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a)),
  data.frame(strength=a<-degree(T7win_net, gmode="graph", ignore.eval=F, rescale=F), nfoxes=length(a))) 


#=====================================================================================

# EIGENVECTOR CENTRALITY: normalised between 0-1 by score/sum(scores) (rescale=T)

# Q: Is individual network position influenced by food availability, sex or social status
# and does this vary between seasons?

# BEFORE / AFTER MIDNIGHT: DON'T USE - WANT WHOLE DAY ONLY -----------------------
TAllattr_B4Af$eig.norm <- c(evcent(T1sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1sumAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T1winB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2sumAf_net, gmode="graph", use.eigen = T, rescale=T), # empty - no true assocs
                            evcent(T2sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T2winB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3sumAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T3winB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4sumAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T4winB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5sumAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T5winB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6sumAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T6winB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7sprAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7sprB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7sumAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7sumB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7autAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7autB4_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7winAf_net, gmode="graph", use.eigen = T, rescale=T),
                            evcent(T7winB4_net, gmode="graph", use.eigen = T, rescale=T))

#---------------------------------------------------

#== WHOLE DAY ONLY - USE

# Subset attributes
TAllattr_wd <- subset(attr, DaysSeen>4) # attr contains one row per fox per season/territory (ignore BeforeMidnight column)
TAllattr_wd <-TAllattr_wd[order(TAllattr_wd$Territory, TAllattr_wd$SeasonID, TAllattr_wd$id), ] # IMPORTANT to order by Territory, Season and ID

# Save factors
TAllattr_wd$fTerritory <- factor(TAllattr_wd$Territory)
TAllattr_wd$fSeason <- factor(TAllattr_wd$SeasonID)
TAllattr_wd$Core <- factor(TAllattr_wd$Core)

detach(package:tnet)
detach(package:qgraph)
library(sna)

# Eigenvector centrality - whole day
TAllattr_wd$eig <-  c(evcent(T1spr_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T1sum_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T1aut_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T1win_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T2spr_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T2sum_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T2aut_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T2win_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T3spr_net, gmode="graph", use.eigen=T, rescale=F),  
                      evcent(T3sum_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T3aut_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T3win_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T4spr_net, gmode="graph", use.eigen=T, rescale=F),   
                      evcent(T4sum_net, gmode="graph", use.eigen=T, rescale=F),  
                      evcent(T4aut_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T4win_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T5spr_net, gmode="graph", use.eigen=T, rescale=F),  
                      evcent(T5sum_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T5aut_net, gmode="graph", use.eigen=T, rescale=F), 
                      evcent(T5win_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T6spr_net, gmode="graph", use.eigen=T, rescale=F),  
                      evcent(T6sum_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T6aut_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T6win_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T7spr_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T7sum_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T7aut_net, gmode="graph", use.eigen=T, rescale=F),
                      evcent(T7win_net, gmode="graph", use.eigen=T, rescale=F))
# NORMALISED 0-1
TAllattr_wd$eig.norm <-  c(evcent(T1spr_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T1sum_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T1aut_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T1win_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T2spr_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T2sum_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T2aut_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T2win_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T3spr_net, gmode="graph", use.eigen=T, rescale=T),  
                           evcent(T3sum_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T3aut_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T3win_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T4spr_net, gmode="graph", use.eigen=T, rescale=T),   
                           evcent(T4sum_net, gmode="graph", use.eigen=T, rescale=T),  
                           evcent(T4aut_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T4win_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T5spr_net, gmode="graph", use.eigen=T, rescale=T),  
                           evcent(T5sum_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T5aut_net, gmode="graph", use.eigen=T, rescale=T), 
                           evcent(T5win_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T6spr_net, gmode="graph", use.eigen=T, rescale=T),  
                           evcent(T6sum_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T6aut_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T6win_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T7spr_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T7sum_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T7aut_net, gmode="graph", use.eigen=T, rescale=T),
                           evcent(T7win_net, gmode="graph", use.eigen=T, rescale=T))
#----------------------------------------

### CLOSENESS

########## DECIDED NOT TO USE CLOSENESS AS V.SIMILAR TO EIGENVECTOR #############

detach(package:sna)
library(tnet) 

# Closeness in main component only (distance between components in infinite and my largest 2nd component was a pair anyway) 

# not normalised:
TAll_closeness <- rbind(
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1spr_net, result = "graph")))$closeness), id=rownames(a), Territory=1, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1sum_net, result = "graph")))$closeness), id=rownames(a), Territory=1, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1aut_net, result = "graph")))$closeness), id=rownames(a), Territory=1, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1win_net, result = "graph")))$closeness), id=rownames(a), Territory=1, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2spr_net, result = "graph")))$closeness), id=rownames(a), Territory=2, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2sum_net, result = "graph")))$closeness), id=rownames(a), Territory=2, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2aut_net, result = "graph")))$closeness), id=rownames(a), Territory=2, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2win_net, result = "graph")))$closeness), id=rownames(a), Territory=2, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3spr_net, result = "graph")))$closeness), id=rownames(a), Territory=3, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3sum_net, result = "graph")))$closeness), id=rownames(a), Territory=3, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3aut_net, result = "graph")))$closeness), id=rownames(a), Territory=3, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3win_net, result = "graph")))$closeness), id=rownames(a), Territory=3, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T4spr_net, result = "graph"))))$closeness), id=rownames(a), Territory=4, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T4sum_net, result = "graph"))))$closeness), id=rownames(a), Territory=4, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T4aut_net, result = "graph")))$closeness), id=rownames(a), Territory=4, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T4win_net, result = "graph")))$closeness), id=rownames(a), Territory=4, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5spr_net, result = "graph")))$closeness), id=rownames(a), Territory=5, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5sum_net, result = "graph")))$closeness), id=rownames(a), Territory=5, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5aut_net, result = "graph")))$closeness), id=rownames(a), Territory=5, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5win_net, result = "graph")))$closeness), id=rownames(a), Territory=5, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6spr_net, result = "graph"))))$closeness), id=rownames(a), Territory=6, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6sum_net, result = "graph"))))$closeness), id=rownames(a), Territory=6, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6aut_net, result = "graph"))))$closeness), id=rownames(a), Territory=6, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6win_net, result = "graph"))))$closeness), id=rownames(a), Territory=6, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7spr_net, result = "graph"))))$closeness), id=rownames(a), Territory=7, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7sum_net, result = "graph"))))$closeness), id=rownames(a), Territory=7, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7aut_net, result = "graph"))))$closeness), id=rownames(a), Territory=7, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7win_net, result = "graph"))))$closeness), id=rownames(a), Territory=7, SeasonID=4, netsize=nrow(a)))

# normalised 0-1 by score/sum(scores)
TAll_closeness.norm <- rbind(
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1spr_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=1, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1sum_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=1, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1aut_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=1, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T1win_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=1, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2spr_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=2, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2sum_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=2, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2aut_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=2, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T2win_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=2, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3spr_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=3, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3sum_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=3, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3aut_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=3, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T3win_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=3, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T4spr_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=4, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T4sum_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=4, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T4aut_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=4, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T4win_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=4, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5spr_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=5, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5sum_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=5, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5aut_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=5, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(a<-sna::component.largest(T5win_net, result = "graph")))$closeness)/sum(clo), id=rownames(a), Territory=5, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6spr_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=6, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6sum_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=6, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6aut_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=6, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T6win_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=6, SeasonID=4, netsize=nrow(a)),
  
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7spr_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=7, SeasonID=1, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7sum_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=7, SeasonID=2, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7aut_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=7, SeasonID=3, netsize=nrow(a)),
  data.frame(closeness.norm=(clo<-data.frame(closeness_w(sna::as.edgelist.sna(a<-sna::component.largest(T7win_net, result = "graph"))))$closeness)/sum(clo), id=rownames(a), Territory=7, SeasonID=4, netsize=nrow(a)))

#---------------

# CLUSTERING COEFFICIENT 
# normalised within the function

detach(package:tnet)
library(qgraph)

TAll_CC <- rbind(
  data.frame(cc=clustOnnela(a<-T1spr_net)$clustOnnela,  id=rownames(a), Territory=1, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T1sum_net)$clustOnnela,  id=rownames(a), Territory=1, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T1aut_net)$clustOnnela,  id=rownames(a), Territory=1, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T1win_net)$clustOnnela,  id=rownames(a), Territory=1, SeasonID=4, netsize=nrow(a)),
  
  data.frame(cc=clustOnnela(a<-T2spr_net)$clustOnnela,  id=rownames(a), Territory=2, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T2sum_net)$clustOnnela,  id=rownames(a), Territory=2, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T2aut_net)$clustOnnela,  id=rownames(a), Territory=2, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T2win_net)$clustOnnela,  id=rownames(a), Territory=2, SeasonID=4, netsize=nrow(a)),
  
  data.frame(cc=clustOnnela(a<-T3spr_net)$clustOnnela,  id=rownames(a), Territory=3, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T3sum_net)$clustOnnela,  id=rownames(a), Territory=3, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T3aut_net)$clustOnnela,  id=rownames(a), Territory=3, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T3win_net)$clustOnnela,  id=rownames(a), Territory=3, SeasonID=4, netsize=nrow(a)),
  
  data.frame(cc=clustOnnela(a<-T4spr_net)$clustOnnela,  id=rownames(a), Territory=4, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T4sum_net)$clustOnnela,  id=rownames(a), Territory=4, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T4aut_net)$clustOnnela,  id=rownames(a), Territory=4, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T4win_net)$clustOnnela,  id=rownames(a), Territory=4, SeasonID=4, netsize=nrow(a)),
  
  data.frame(cc=clustOnnela(a<-T5spr_net)$clustOnnela,  id=rownames(a), Territory=5, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T5sum_net)$clustOnnela,  id=rownames(a), Territory=5, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T5aut_net)$clustOnnela,  id=rownames(a), Territory=5, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T5win_net)$clustOnnela,  id=rownames(a), Territory=5, SeasonID=4, netsize=nrow(a)),
  
  data.frame(cc=clustOnnela(a<-T6spr_net)$clustOnnela,  id=rownames(a), Territory=6, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T6sum_net)$clustOnnela,  id=rownames(a), Territory=6, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T6aut_net)$clustOnnela,  id=rownames(a), Territory=6, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T6win_net)$clustOnnela,  id=rownames(a), Territory=6, SeasonID=4, netsize=nrow(a)),
  
  data.frame(cc=clustOnnela(a<-T7spr_net)$clustOnnela,  id=rownames(a), Territory=7, SeasonID=1, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T7sum_net)$clustOnnela,  id=rownames(a), Territory=7, SeasonID=2, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T7aut_net)$clustOnnela,  id=rownames(a), Territory=7, SeasonID=3, netsize=nrow(a)),
  data.frame(cc=clustOnnela(a<-T7win_net)$clustOnnela,  id=rownames(a), Territory=7, SeasonID=4, netsize=nrow(a)))

#------------------------------------------------------------------------------------------------------

#== ORGANISE DATA

# Strength before and after midnight (unstandardised and standardised, but use unstandardised in models 
str(TAllattr_B4Af) # eig.norm and strength.norm = normalised by score/sum(scores). Strength normNl = normalised by score/(N-1)
str(strength.df) # contains netwk sizes, need to incl. in model as fixef
TAllattr_B4Af$nfoxes <- strength.df$nfoxes # copy over nfoxes (OK to do without using 'merge' as both dataframes are in same order)

# Whole day: eigenvector and closeness normalised by: score/sum(scores)
str(TAllattr_wd)            # eig & eig.norm
str(TAll_closeness)         # closeness
str(TAll_closeness.norm)    # closeness normalised 0-1
str(TAll_CC)                # clustering coefficient (normalised within the Onnella method function)
strengthWD_df               # strength for WHOLE DAY saved in dataframe with Nfoxes in network (same length and order as TAllattr_wd)

# 1. Merge strengthWD_df and TAllattr_wd as same length and order
strengtheig_WD <- data.frame(TAllattr_wd, strengthWD_df)
# And same for both closeness dataframes:
clo_WD <- data.frame(TAll_closeness, TAll_closeness.norm)
# delete duplicated columns:
clo_WD$id.1 <- NULL
clo_WD$Territory.1 <- NULL
clo_WD$SeasonID.1 <- NULL
clo_WD$netsize.1 <- NULL

# 2. Add closeness scores to main attributes table
TAllattr <- merge(x=strengtheig_WD, y=clo_WD[ , c("id", "SeasonID", "Territory", "closeness", "closeness.norm")], 
                  by = c("id", "SeasonID", "Territory"), all.x=TRUE)

# ...And cc
centrality <- merge(x=TAllattr, y=TAll_CC[ , c("id", "SeasonID", "Territory", "cc")], 
                    by = c("id", "SeasonID", "Territory"), all.x=TRUE)
centrality$cc[centrality$cc=='NaN'] <- NA # replace 'NaN' in cc column with NA as 'NaN' is not recognised by R

# 'Centrality' contains eigenvector, closeness and clustering coefficient
#  Strength is before/after midnight so saved in a different data frame


# 3. Label isolates and pairs using NAs in closeness column:
centrality$maincomp <- ifelse(centrality$closeness.norm=="NA", "0", 1)

# Eigenvector centrality should be zero for isolates and pairs (but for some foxes it's just the opposite sign to the connected foxes, eg. positive rather than negative)
# Convert eig of isolate/pair foxes to zero
centrality$eig <-ifelse(centrality$closeness.norm=="NA", "0", abs(centrality$eig))
centrality$eig.norm <-ifelse(centrality$closeness.norm=="NA", "0", abs(centrality$eig.norm))
centrality$eig.norm[is.na(centrality$eig.norm)] <- 0 # replace NAs with zero (previous line of code didn't put zero, just NA)

str(centrality)
centrality_core <- subset(centrality, prefavoided==1 & SocialStatus!="NA" & Core==1)
str(centrality_core)

#=====================================================================================================================================================#
#=====================================================================================================================================================#
# GENERATE STACKS OF 2000 RANDOM NETWORKS    # From data-stream permutations  # creates perm*N*N stack of matrices (perm rows, N columns and N layers) 
#=====================================================================================================================================================#
#=====================================================================================================================================================#
library(asnipe)

#== BEFORE/ AFTER MIDNIGHT:

# Territory 1
T1sprB4_rand <- network_permutation(association_data=T1sprB4_SP, association_matrix=T1sprB4_net, days=rownames(T1sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1sprAf_rand <- network_permutation(association_data=T1sprAf_SP, association_matrix=T1sprAf_net, days=rownames(T1sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1sumB4_rand <- network_permutation(association_data=T1sumB4_SP, association_matrix=T1sumB4_net, days=rownames(T1sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1sumAf_rand <- network_permutation(association_data=T1sumAf_SP, association_matrix=T1sumAf_net, days=rownames(T1sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1autB4_rand <- network_permutation(association_data=T1autB4_SP, association_matrix=T1autB4_net, days=rownames(T1autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1autAf_rand <- network_permutation(association_data=T1autAf_SP, association_matrix=T1autAf_net, days=rownames(T1autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1winB4_rand <- network_permutation(association_data=T1winB4_SP, association_matrix=T1winB4_net, days=rownames(T1winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1winAf_rand <- network_permutation(association_data=T1winAf_SP, association_matrix=T1winAf_net, days=rownames(T1winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 2
T2sprB4_rand <- network_permutation(association_data=T2sprB4_SP, association_matrix=T2sprB4_net, days=rownames(T2sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2sprAf_rand <- network_permutation(association_data=T2sprAf_SP, association_matrix=T2sprAf_net, days=rownames(T2sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2sumB4_rand <- network_permutation(association_data=T2sumB4_SP, association_matrix=T2sumB4_net, days=rownames(T2sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Can't permute as there were no true assocs after midnight in T2 summer
# T2sumAf_rand <- network_permutation(association_data=T2sumAf_SP, association_matrix=T2sumAf_net, days=rownames(T2sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

T2autB4_rand <- network_permutation(association_data=T2autB4_SP, association_matrix=T2autB4_net, days=rownames(T2autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2autAf_rand <- network_permutation(association_data=T2autAf_SP, association_matrix=T2autAf_net, days=rownames(T2autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2winB4_rand <- network_permutation(association_data=T2winB4_SP, association_matrix=T2winB4_net, days=rownames(T2winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2winAf_rand <- network_permutation(association_data=T2winAf_SP, association_matrix=T2winAf_net, days=rownames(T2winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 3
T3sprB4_rand <- network_permutation(association_data=T3sprB4_SP, association_matrix=T3sprB4_net, days=rownames(T3sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3sprAf_rand <- network_permutation(association_data=T3sprAf_SP, association_matrix=T3sprAf_net, days=rownames(T3sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3sumB4_rand <- network_permutation(association_data=T3sumB4_SP, association_matrix=T3sumB4_net, days=rownames(T3sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3sumAf_rand <- network_permutation(association_data=T3sumAf_SP, association_matrix=T3sumAf_net, days=rownames(T3sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3autB4_rand <- network_permutation(association_data=T3autB4_SP, association_matrix=T3autB4_net, days=rownames(T3autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3autAf_rand <- network_permutation(association_data=T3autAf_SP, association_matrix=T3autAf_net, days=rownames(T3autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3winB4_rand <- network_permutation(association_data=T3winB4_SP, association_matrix=T3winB4_net, days=rownames(T3winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3winAf_rand <- network_permutation(association_data=T3winAf_SP, association_matrix=T3winAf_net, days=rownames(T3winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 4
T4sprB4_rand <- network_permutation(association_data=T4sprB4_SP, association_matrix=T4sprB4_net, days=rownames(T4sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4sprAf_rand <- network_permutation(association_data=T4sprAf_SP, association_matrix=T4sprAf_net, days=rownames(T4sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4sumB4_rand <- network_permutation(association_data=T4sumB4_SP, association_matrix=T4sumB4_net, days=rownames(T4sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4sumAf_rand <- network_permutation(association_data=T4sumAf_SP, association_matrix=T4sumAf_net, days=rownames(T4sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4autB4_rand <- network_permutation(association_data=T4autB4_SP, association_matrix=T4autB4_net, days=rownames(T4autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4autAf_rand <- network_permutation(association_data=T4autAf_SP, association_matrix=T4autAf_net, days=rownames(T4autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4winB4_rand <- network_permutation(association_data=T4winB4_SP, association_matrix=T4winB4_net, days=rownames(T4winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4winAf_rand <- network_permutation(association_data=T4winAf_SP, association_matrix=T4winAf_net, days=rownames(T4winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 5
T5sprB4_rand <- network_permutation(association_data=T5sprB4_SP, association_matrix=T5sprB4_net, days=rownames(T5sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5sprAf_rand <- network_permutation(association_data=T5sprAf_SP, association_matrix=T5sprAf_net, days=rownames(T5sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5sumB4_rand <- network_permutation(association_data=T5sumB4_SP, association_matrix=T5sumB4_net, days=rownames(T5sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5sumAf_rand <- network_permutation(association_data=T5sumAf_SP, association_matrix=T5sumAf_net, days=rownames(T5sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5autB4_rand <- network_permutation(association_data=T5autB4_SP, association_matrix=T5autB4_net, days=rownames(T5autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5autAf_rand <- network_permutation(association_data=T5autAf_SP, association_matrix=T5autAf_net, days=rownames(T5autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5winB4_rand <- network_permutation(association_data=T5winB4_SP, association_matrix=T5winB4_net, days=rownames(T5winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5winAf_rand <- network_permutation(association_data=T5winAf_SP, association_matrix=T5winAf_net, days=rownames(T5winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 6
T6sprB4_rand <- network_permutation(association_data=T6sprB4_SP, association_matrix=T6sprB4_net, days=rownames(T6sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6sprAf_rand <- network_permutation(association_data=T6sprAf_SP, association_matrix=T6sprAf_net, days=rownames(T6sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6sumB4_rand <- network_permutation(association_data=T6sumB4_SP, association_matrix=T6sumB4_net, days=rownames(T6sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6sumAf_rand <- network_permutation(association_data=T6sumAf_SP, association_matrix=T6sumAf_net, days=rownames(T6sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6autB4_rand <- network_permutation(association_data=T6autB4_SP, association_matrix=T6autB4_net, days=rownames(T6autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6autAf_rand <- network_permutation(association_data=T6autAf_SP, association_matrix=T6autAf_net, days=rownames(T6autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6winB4_rand <- network_permutation(association_data=T6winB4_SP, association_matrix=T6winB4_net, days=rownames(T6winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6winAf_rand <- network_permutation(association_data=T6winAf_SP, association_matrix=T6winAf_net, days=rownames(T6winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 7
T7sprB4_rand <- network_permutation(association_data=T7sprB4_SP, association_matrix=T7sprB4_net, days=rownames(T7sprB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7sprAf_rand <- network_permutation(association_data=T7sprAf_SP, association_matrix=T7sprAf_net, days=rownames(T7sprAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7sumB4_rand <- network_permutation(association_data=T7sumB4_SP, association_matrix=T7sumB4_net, days=rownames(T7sumB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7sumAf_rand <- network_permutation(association_data=T7sumAf_SP, association_matrix=T7sumAf_net, days=rownames(T7sumAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7autB4_rand <- network_permutation(association_data=T7autB4_SP, association_matrix=T7autB4_net, days=rownames(T7autB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7autAf_rand <- network_permutation(association_data=T7autAf_SP, association_matrix=T7autAf_net, days=rownames(T7autAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7winB4_rand <- network_permutation(association_data=T7winB4_SP, association_matrix=T7winB4_net, days=rownames(T7winB4_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7winAf_rand <- network_permutation(association_data=T7winAf_SP, association_matrix=T7winAf_net, days=rownames(T7winAf_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")



#== WHOLE DAY ONLY

# Territory 1
T1spr_rand <- network_permutation(association_data=T1spr_SP, association_matrix=T1spr_net, days=rownames(T1spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1sum_rand <- network_permutation(association_data=T1sum_SP, association_matrix=T1sum_net, days=rownames(T1sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1aut_rand <- network_permutation(association_data=T1aut_SP, association_matrix=T1aut_net, days=rownames(T1aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T1win_rand <- network_permutation(association_data=T1win_SP, association_matrix=T1win_net, days=rownames(T1win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 2
T2spr_rand <- network_permutation(association_data=T2spr_SP, association_matrix=T2spr_net, days=rownames(T2spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2sum_rand <- network_permutation(association_data=T2sum_SP, association_matrix=T2sum_net, days=rownames(T2sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2aut_rand <- network_permutation(association_data=T2aut_SP, association_matrix=T2aut_net, days=rownames(T2aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T2win_rand <- network_permutation(association_data=T2win_SP, association_matrix=T2win_net, days=rownames(T2win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 3
T3spr_rand <- network_permutation(association_data=T3spr_SP, association_matrix=T3spr_net, days=rownames(T3spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3sum_rand <- network_permutation(association_data=T3sum_SP, association_matrix=T3sum_net, days=rownames(T3sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3aut_rand <- network_permutation(association_data=T3aut_SP, association_matrix=T3aut_net, days=rownames(T3aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T3win_rand <- network_permutation(association_data=T3win_SP, association_matrix=T3win_net, days=rownames(T3win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 4
T4spr_rand <- network_permutation(association_data=T4spr_SP, association_matrix=T4spr_net, days=rownames(T4spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4sum_rand <- network_permutation(association_data=T4sum_SP, association_matrix=T4sum_net, days=rownames(T4sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4aut_rand <- network_permutation(association_data=T4aut_SP, association_matrix=T4aut_net, days=rownames(T4aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T4win_rand <- network_permutation(association_data=T4win_SP, association_matrix=T4win_net, days=rownames(T4win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 5
T5spr_rand <- network_permutation(association_data=T5spr_SP, association_matrix=T5spr_net, days=rownames(T5spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5sum_rand <- network_permutation(association_data=T5sum_SP, association_matrix=T5sum_net, days=rownames(T5sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5aut_rand <- network_permutation(association_data=T5aut_SP, association_matrix=T5aut_net, days=rownames(T5aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T5win_rand <- network_permutation(association_data=T5win_SP, association_matrix=T5win_net, days=rownames(T5win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 6
T6spr_rand <- network_permutation(association_data=T6spr_SP, association_matrix=T6spr_net, days=rownames(T6spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6sum_rand <- network_permutation(association_data=T6sum_SP, association_matrix=T6sum_net, days=rownames(T6sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6aut_rand <- network_permutation(association_data=T6aut_SP, association_matrix=T6aut_net, days=rownames(T6aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T6win_rand <- network_permutation(association_data=T6win_SP, association_matrix=T6win_net, days=rownames(T6win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

# Territory 7
T7spr_rand <- network_permutation(association_data=T7spr_SP, association_matrix=T7spr_net, days=rownames(T7spr_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7sum_rand <- network_permutation(association_data=T7sum_SP, association_matrix=T7sum_net, days=rownames(T7sum_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7aut_rand <- network_permutation(association_data=T7aut_SP, association_matrix=T7aut_net, days=rownames(T7aut_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")
T7win_rand <- network_permutation(association_data=T7win_SP, association_matrix=T7win_net, days=rownames(T7win_SP), within_day=TRUE, permutations=20000, returns=10, data_format="SP")

detach(package:asnipe)

#====================================================================================================#
# CALCULATE CENTRALITY FROM STACKS OF RANDOM NETWORKS - and store in a matrix ready to fit models to
#====================================================================================================#

# STRENGTH & EIGENVECTOR CENTRALITY - package:sna can calc from stacked matrices in one line of code
detach(package:qgraph)
detach(package:tnet)
detach(package:igraph)
library(sna)


# STRENGTH: BEFORE / AFTER MIDNIGHT

# Calculate strength and store in an N*perm matrix:
T1sprB4_rand_strength <- degree(T1sprB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T1sumB4_rand_strength <- degree(T1sumB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T1autB4_rand_strength <- degree(T1autB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T1winB4_rand_strength <- degree(T1winB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T1sprAf_rand_strength <- degree(T1sprAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T1sumAf_rand_strength <- degree(T1sumAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T1autAf_rand_strength <- degree(T1autAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T1winAf_rand_strength <- degree(T1winAf_rand, gmode="graph", rescale=F,g=c(1:2000))

T2sprB4_rand_strength <- degree(T2sprB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T2sumB4_rand_strength <- degree(T2sumB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T2autB4_rand_strength <- degree(T2autB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T2winB4_rand_strength <- degree(T2winB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T2sprAf_rand_strength <- degree(T2sprAf_rand, gmode="graph", rescale=F,g=c(1:2000))
# T2sumAf - no true assocs after midnight so could not permute
T2autAf_rand_strength <- degree(T2autAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T2winAf_rand_strength <- degree(T2winAf_rand, gmode="graph", rescale=F,g=c(1:2000))

T3sprB4_rand_strength <- degree(T3sprB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T3sumB4_rand_strength <- degree(T3sumB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T3autB4_rand_strength <- degree(T3autB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T3winB4_rand_strength <- degree(T3winB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T3sprAf_rand_strength <- degree(T3sprAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T3sumAf_rand_strength <- degree(T3sumAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T3autAf_rand_strength <- degree(T3autAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T3winAf_rand_strength <- degree(T3winAf_rand, gmode="graph", rescale=F,g=c(1:2000))

T4sprB4_rand_strength <- degree(T4sprB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T4sumB4_rand_strength <- degree(T4sumB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T4autB4_rand_strength <- degree(T4autB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T4winB4_rand_strength <- degree(T4winB4_rand, gmode="graph", rescale=F,g=c(1:2000))
T4sprAf_rand_strength <- degree(T4sprAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T4sumAf_rand_strength <- degree(T4sumAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T4autAf_rand_strength <- degree(T4autAf_rand, gmode="graph", rescale=F,g=c(1:2000))
T4winAf_rand_strength <- degree(T4winAf_rand, gmode="graph", rescale=F,g=c(1:2000))

T5sprB4_rand_strength <- degree(T5sprB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T5sumB4_rand_strength <- degree(T5sumB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T5autB4_rand_strength <- degree(T5autB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T5winB4_rand_strength <- degree(T5winB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T5sprAf_rand_strength <- degree(T5sprAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T5sumAf_rand_strength <- degree(T5sumAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T5autAf_rand_strength <- degree(T5autAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T5winAf_rand_strength <- degree(T5winAf_rand, gmode="graph", rescale=F, g=c(1:2000))

T6sprB4_rand_strength <- degree(T6sprB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T6sumB4_rand_strength <- degree(T6sumB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T6autB4_rand_strength <- degree(T6autB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T6winB4_rand_strength <- degree(T6winB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T6sprAf_rand_strength <- degree(T6sprAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T6sumAf_rand_strength <- degree(T6sumAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T6autAf_rand_strength <- degree(T6autAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T6winAf_rand_strength <- degree(T6winAf_rand, gmode="graph", rescale=F, g=c(1:2000))

T7sprB4_rand_strength <- degree(T7sprB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T7sumB4_rand_strength <- degree(T7sumB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T7autB4_rand_strength <- degree(T7autB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T7winB4_rand_strength <- degree(T7winB4_rand, gmode="graph", rescale=F, g=c(1:2000))
T7sprAf_rand_strength <- degree(T7sprAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T7sumAf_rand_strength <- degree(T7sumAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T7autAf_rand_strength <- degree(T7autAf_rand, gmode="graph", rescale=F, g=c(1:2000))
T7winAf_rand_strength <- degree(T7winAf_rand, gmode="graph", rescale=F, g=c(1:2000))

# RENAME ROWS: Before midnight:
rownames(T1sprB4_rand_strength) <- T1sprB4_attr$id
rownames(T1sumB4_rand_strength) <- T1sumB4_attr$id
rownames(T1autB4_rand_strength) <- T1autB4_attr$id
rownames(T1winB4_rand_strength) <- T1winB4_attr$id

rownames(T2sprB4_rand_strength) <- T2sprB4_attr$id
rownames(T2sumB4_rand_strength) <- T2sumB4_attr$id
rownames(T2autB4_rand_strength) <- T2autB4_attr$id
rownames(T2winB4_rand_strength) <- T2winB4_attr$id

rownames(T3sprB4_rand_strength) <- T3sprB4_attr$id
rownames(T3sumB4_rand_strength) <- T3sumB4_attr$id
rownames(T3autB4_rand_strength) <- T3autB4_attr$id
rownames(T3winB4_rand_strength) <- T3winB4_attr$id

rownames(T4sprB4_rand_strength) <- T4sprB4_attr$id
rownames(T4sumB4_rand_strength) <- T4sumB4_attr$id
rownames(T4autB4_rand_strength) <- T4autB4_attr$id
rownames(T4winB4_rand_strength) <- T4winB4_attr$id

rownames(T5sprB4_rand_strength) <- T5sprB4_attr$id
rownames(T5sumB4_rand_strength) <- T5sumB4_attr$id
rownames(T5autB4_rand_strength) <- T5autB4_attr$id
rownames(T5winB4_rand_strength) <- T5winB4_attr$id

rownames(T6sprB4_rand_strength) <- T6sprB4_attr$id
rownames(T6sumB4_rand_strength) <- T6sumB4_attr$id
rownames(T6autB4_rand_strength) <- T6autB4_attr$id
rownames(T6winB4_rand_strength) <- T6winB4_attr$id

rownames(T7sprB4_rand_strength) <- T7sprB4_attr$id
rownames(T7sumB4_rand_strength) <- T7sumB4_attr$id
rownames(T7autB4_rand_strength) <- T7autB4_attr$id
rownames(T7winB4_rand_strength) <- T7winB4_attr$id

# RENAME ROWS: After midnight:
rownames(T1sprAf_rand_strength) <- T1sprAf_attr$id
rownames(T1sumAf_rand_strength) <- T1sumAf_attr$id
rownames(T1autAf_rand_strength) <- T1autAf_attr$id
rownames(T1winAf_rand_strength) <- T1winAf_attr$id

rownames(T2sprAf_rand_strength) <- T2sprAf_attr$id
# T2sumAf_rand_strength - does not exist (no true assocs so couldn't permute 
rownames(T2autAf_rand_strength) <- T2autAf_attr$id
rownames(T2winAf_rand_strength) <- T2winAf_attr$id

rownames(T3sprAf_rand_strength) <- T3sprAf_attr$id
rownames(T3sumAf_rand_strength) <- T3sumAf_attr$id
rownames(T3autAf_rand_strength) <- T3autAf_attr$id
rownames(T3winAf_rand_strength) <- T3winAf_attr$id

rownames(T4sprAf_rand_strength) <- T4sprAf_attr$id
rownames(T4sumAf_rand_strength) <- T4sumAf_attr$id
rownames(T4autAf_rand_strength) <- T4autAf_attr$id
rownames(T4winAf_rand_strength) <- T4winAf_attr$id

rownames(T5sprAf_rand_strength) <- T5sprAf_attr$id
rownames(T5sumAf_rand_strength) <- T5sumAf_attr$id
rownames(T5autAf_rand_strength) <- T5autAf_attr$id
rownames(T5winAf_rand_strength) <- T5winAf_attr$id

rownames(T6sprAf_rand_strength) <- T6sprAf_attr$id
rownames(T6sumAf_rand_strength) <- T6sumAf_attr$id
rownames(T6autAf_rand_strength) <- T6autAf_attr$id
rownames(T6winAf_rand_strength) <- T6winAf_attr$id

rownames(T7sprAf_rand_strength) <- T7sprAf_attr$id
rownames(T7sumAf_rand_strength) <- T7sumAf_attr$id
rownames(T7autAf_rand_strength) <- T7autAf_attr$id
rownames(T7winAf_rand_strength) <- T7winAf_attr$id


#--------------------------------------------------------------------

# WHOLE DAY 
detach(package:qgraph)
library(sna)
# Calculate strength and store in an N*perm matrix:
T1spr_rand_strength <- degree(T1spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T1sum_rand_strength <- degree(T1sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T1aut_rand_strength <- degree(T1aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T1win_rand_strength <- degree(T1win_rand, gmode="graph", g=c(1:2000), rescale=F) 

T2spr_rand_strength <- degree(T2spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T2sum_rand_strength <- degree(T2sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T2aut_rand_strength <- degree(T2aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T2win_rand_strength <- degree(T2win_rand, gmode="graph", g=c(1:2000), rescale=F) 

T3spr_rand_strength <- degree(T3spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T3sum_rand_strength <- degree(T3sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T3aut_rand_strength <- degree(T3aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T3win_rand_strength <- degree(T3win_rand, gmode="graph", g=c(1:2000), rescale=F) 

T4spr_rand_strength <- degree(T4spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T4sum_rand_strength <- degree(T4sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T4aut_rand_strength <- degree(T4aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T4win_rand_strength <- degree(T4win_rand, gmode="graph", g=c(1:2000), rescale=F) 

T5spr_rand_strength <- degree(T5spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T5sum_rand_strength <- degree(T5sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T5aut_rand_strength <- degree(T5aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T5win_rand_strength <- degree(T5win_rand, gmode="graph", g=c(1:2000), rescale=F) 

T6spr_rand_strength <- degree(T6spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T6sum_rand_strength <- degree(T6sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T6aut_rand_strength <- degree(T6aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T6win_rand_strength <- degree(T6win_rand, gmode="graph", g=c(1:2000), rescale=F) 

T7spr_rand_strength <- degree(T7spr_rand, gmode="graph", g=c(1:2000), rescale=F) 
T7sum_rand_strength <- degree(T7sum_rand, gmode="graph", g=c(1:2000), rescale=F) 
T7aut_rand_strength <- degree(T7aut_rand, gmode="graph", g=c(1:2000), rescale=F) 
T7win_rand_strength <- degree(T7win_rand, gmode="graph", g=c(1:2000), rescale=F) 

# Rename rows
rownames(T1spr_rand_strength) <- T1spr_attr$id
rownames(T1sum_rand_strength) <- T1sum_attr$id
rownames(T1aut_rand_strength) <- T1aut_attr$id
rownames(T1win_rand_strength) <- T1win_attr$id

rownames(T2spr_rand_strength) <- T2spr_attr$id
rownames(T2sum_rand_strength) <- T2sum_attr$id
rownames(T2aut_rand_strength) <- T2aut_attr$id
rownames(T2win_rand_strength) <- T2win_attr$id

rownames(T3spr_rand_strength) <- T3spr_attr$id
rownames(T3sum_rand_strength) <- T3sum_attr$id
rownames(T3aut_rand_strength) <- T3aut_attr$id
rownames(T3win_rand_strength) <- T3win_attr$id

rownames(T4spr_rand_strength) <- T4spr_attr$id
rownames(T4sum_rand_strength) <- T4sum_attr$id
rownames(T4aut_rand_strength) <- T4aut_attr$id
rownames(T4win_rand_strength) <- T4win_attr$id

rownames(T5spr_rand_strength) <- T5spr_attr$id
rownames(T5sum_rand_strength) <- T5sum_attr$id
rownames(T5aut_rand_strength) <- T5aut_attr$id
rownames(T5win_rand_strength) <- T5win_attr$id

rownames(T6spr_rand_strength) <- T6spr_attr$id
rownames(T6sum_rand_strength) <- T6sum_attr$id
rownames(T6aut_rand_strength) <- T6aut_attr$id
rownames(T6win_rand_strength) <- T6win_attr$id

rownames(T7spr_rand_strength) <- T7spr_attr$id
rownames(T7sum_rand_strength) <- T7sum_attr$id
rownames(T7aut_rand_strength) <- T7aut_attr$id
rownames(T7win_rand_strength) <- T7win_attr$id
#--------------------------------------------------------------------------------


# EIGENVECTOR CENTRALITY (whole day) - NOT NORMALISED
# Calculate and store in N*perm matrix:
T1spr_rand_eig <- evcent(T1spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T1sum_rand_eig <- evcent(T1sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T1aut_rand_eig <- evcent(T1aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T1win_rand_eig <- evcent(T1win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

T2spr_rand_eig <- evcent(T2spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T2sum_rand_eig <- evcent(T2sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T2aut_rand_eig <- evcent(T2aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T2win_rand_eig <- evcent(T2win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

T3spr_rand_eig <- evcent(T3spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T3sum_rand_eig <- evcent(T3sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T3aut_rand_eig <- evcent(T3aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T3win_rand_eig <- evcent(T3win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

T4spr_rand_eig <- evcent(T4spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T4sum_rand_eig <- evcent(T4sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T4aut_rand_eig <- evcent(T4aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T4win_rand_eig <- evcent(T4win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

T5spr_rand_eig <- evcent(T5spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T5sum_rand_eig <- evcent(T5sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T5aut_rand_eig <- evcent(T5aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T5win_rand_eig <- evcent(T5win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

T6spr_rand_eig <- evcent(T6spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T6sum_rand_eig <- evcent(T6sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T6aut_rand_eig <- evcent(T6aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T6win_rand_eig <- evcent(T6win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

T7spr_rand_eig <- evcent(T7spr_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T7sum_rand_eig <- evcent(T7sum_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T7aut_rand_eig <- evcent(T7aut_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)
T7win_rand_eig <- evcent(T7win_rand, gmode="graph", g=c(1:2000),use.eigen=T, rescale=F)

# Rename rows
rownames(T1spr_rand_eig) <- T1spr_attr$id
rownames(T1sum_rand_eig) <- T1sum_attr$id
rownames(T1aut_rand_eig) <- T1aut_attr$id
rownames(T1win_rand_eig) <- T1win_attr$id

rownames(T2spr_rand_eig) <- T2spr_attr$id
rownames(T2sum_rand_eig) <- T2sum_attr$id
rownames(T2aut_rand_eig) <- T2aut_attr$id
rownames(T2win_rand_eig) <- T2win_attr$id

rownames(T3spr_rand_eig) <- T3spr_attr$id
rownames(T3sum_rand_eig) <- T3sum_attr$id
rownames(T3aut_rand_eig) <- T3aut_attr$id
rownames(T3win_rand_eig) <- T3win_attr$id

rownames(T4spr_rand_eig) <- T4spr_attr$id
rownames(T4sum_rand_eig) <- T4sum_attr$id
rownames(T4aut_rand_eig) <- T4aut_attr$id
rownames(T4win_rand_eig) <- T4win_attr$id

rownames(T5spr_rand_eig) <- T5spr_attr$id
rownames(T5sum_rand_eig) <- T5sum_attr$id
rownames(T5aut_rand_eig) <- T5aut_attr$id
rownames(T5win_rand_eig) <- T5win_attr$id

rownames(T6spr_rand_eig) <- T6spr_attr$id
rownames(T6sum_rand_eig) <- T6sum_attr$id
rownames(T6aut_rand_eig) <- T6aut_attr$id
rownames(T6win_rand_eig) <- T6win_attr$id

rownames(T7spr_rand_eig) <- T7spr_attr$id
rownames(T7sum_rand_eig) <- T7sum_attr$id
rownames(T7aut_rand_eig) <- T7aut_attr$id
rownames(T7win_rand_eig) <- T7win_attr$id

#-------------------------------------------------------------------------------

### CLOSENESS 

########## DECIDED NOT TO USE CLOSENESS AS V.SIMILAR TO EIGENVECTOR #############

# tnet doesn't accept stacked matrices so use loops to compute and store closeness from each random network (quite slow)
# Matrix rows are renamed as AnimalID before each set of loops
# used sna::component.largest to remove isolates and pairs so closeness is only calculated for foxes in main component

detach(package:sna)
library(tnet)

# Set up progress bar as tnet is quite slow:
perm <- 2000
pb <- txtProgressBar(0,2000,0, style=3)


#-----------------------------------------------
#-----------------------------------------------

### OLD CODE TO CALC FOR WHOLE NETWORK INCL. ISOLATES AND PAIRS. LATER DECIDED TO CALC CLOSENESS IN MAIN COMPONENT ONLY (SEE BELOW).
# LOOPS FOR EACH NETWORK:
att <- T1spr_attr
net <- T1spr_rand
N <-nrow(att) # Number of individuals
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(closeness_randmat) <- att$id # rename rows as AnimalID
for (i in 1:2000) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- data.frame(closeness_w(net_rand, gconly=F))$closeness #gconly=F means don't just calc closeness within the main component
  clo <- a/sum(a) # divide by sum to standardise to between 0-1
  b <- clo[1:N] # check length of a matches N foxes
  b[is.na(b)] <- 0 # replace zeros with NAs (sometimes closeness doesn't calc for fox in last row if they had no true associations)
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T1spr_rand_closeness <- closeness_randmat

#-----------------------------------------------
#-----------------------------------------------

### MORE OLD CODE TO CALC NORMALISED CLOSENESS BY DIVIDING BY SUM SCORES IN EACH NETWORK 
# - as didnt make sense when filtering out non-core foxes, and data collected in same way anyway.
att <- T1spr_attr # specify attributes
net <- T1spr_rand # specify (random) network
i=1
dimnames(net)[[2]] <- att$id  # rename rows (second dimension)
dimnames(net)[[3]] <- att$id  # rename columns (3rd dimension)
N <-nrow(att)  # Number of individuals
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  # make N-by-perm matrix filled with NAs
rownames(closeness_randmat) <- att$id  # rename rows as AnimalID

for (i in 1:2000) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- sna::component.largest(net[i,,], result = "graph") # returns the network without isolates and pairs (i.e. the largest component)
  # net[i,,] selects the corresponding random matrix i from the network stack ## need commas as is 3d matrix
  clo <- data.frame(closeness_w(net_rand))$closeness # gconly=T is default and only calculates closeness in the main component (traditional closeness)
  df <- data.frame(id=rownames(net_rand), closeness=clo/sum(clo))       # label closeness scores by AnimalID
  tempvec <- vector(mode="numeric", length=N)              # make empty vector of length N
  names(tempvec) <- att$id                                 # name vector items as AnimalID   
  a <- data.frame(id=names(tempvec), tempvec)              # convert vector to data frame
  b <- df[match(a$id, df$id),2]                            # Add closeness scores - NAs generated automatically for isolates and pairs
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T1spr_rand_closeness <- closeness_randmat

#-----------------------------------------------
#-----------------------------------------------


### CALC CLOSENESS IN MAIN COMPONENT ONLY (USE):

# LOOPS FOR EACH NETWORK:
att <- T1spr_attr # specify attributes
net <- T1spr_rand # specify (random) network
i=1
dimnames(net)[[2]] <- att$id  # rename rows (second dimension)
dimnames(net)[[3]] <- att$id  # rename columns (3rd dimension)
N <-nrow(att)  # Number of individuals
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  # make N-by-perm matrix filled with NAs
rownames(closeness_randmat) <- att$id  # rename rows as AnimalID

for (i in 1:2000) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- sna::component.largest(net[i,,], result = "graph") # returns the network without isolates and pairs (i.e. the largest component)
  # net[i,,] selects the corresponding random matrix i from the network stack ## need commas as is 3d matrix
  clo <- data.frame(closeness_w(net_rand))$closeness # gconly=T is default and only calculates closeness in the main component (traditional closeness)
  df <- data.frame(id=rownames(net_rand), closeness=clo)       # label closeness scores by AnimalID
  tempvec <- vector(mode="numeric", length=N)              # make empty vector of length N
  names(tempvec) <- att$id                                 # name vector items as AnimalID   
  a <- data.frame(id=names(tempvec), tempvec)              # convert vector to data frame
  b <- df[match(a$id, df$id),2]                            # Add closeness scores - NAs generated automatically for isolates and pairs
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T1spr_rand_closeness <- closeness_randmat
#
#
att <- T1sum_attr
net <- T1sum_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)     
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]                            
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T1sum_rand_closeness <- closeness_randmat
#
#
att <- T1aut_attr
net <- T1aut_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)     
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]   
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T1aut_rand_closeness <- closeness_randmat
#
#
att <- T1win_attr
net <- T1win_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)         
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]   
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T1win_rand_closeness <- closeness_randmat
#
#####
#
att <- T2spr_attr
net <- T2spr_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]   
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T2spr_rand_closeness <- closeness_randmat
#
#
att <- T2sum_attr 
net <- T2sum_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id 
N <-nrow(att) 
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(closeness_randmat) <- att$id

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T2sum_rand_closeness <- closeness_randmat
#
#
att <- T2aut_attr
net <- T2aut_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T2aut_rand_closeness <- closeness_randmat
#
#
att <- T2win_attr
net <- T2win_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T2win_rand_closeness <- closeness_randmat
#
#####
#
att <- T3spr_attr
net <- T3spr_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)    
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]   
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T3spr_rand_closeness <- closeness_randmat
#
#
att <- T3sum_attr
net <- T3sum_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)  
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]   
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T3sum_rand_closeness <- closeness_randmat
#
#
att <- T3aut_attr
net <- T3aut_rand  # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T3aut_rand_closeness <- closeness_randmat
#
#
att <- T3win_attr
net <- T3win_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T3win_rand_closeness <- closeness_randmat
#
#####
#
att <- T4spr_attr # matrix contains 4 foxes so must convert network-> edgelist ->tnet object within loop:
net <- T4spr_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id 
N <-nrow(att) 
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(closeness_randmat) <- att$id

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T4spr_rand_closeness <- closeness_randmat
#
#
att <- T4sum_attr # matrix contains 4 foxes so must convert network-> edgelist ->tnet object within loop:
net <- T4sum_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id 
N <-nrow(att) 
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(closeness_randmat) <- att$id

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs           
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T4sum_rand_closeness <- closeness_randmat
#
#
att <- T4aut_attr
net <- T4aut_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)        
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T4aut_rand_closeness <- closeness_randmat
#
#
att <- T4win_attr
net <- T4win_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T4win_rand_closeness <- closeness_randmat
#
#####
#
att <- T5spr_attr # matrix contains 4 foxes so must convert network-> edgelist ->tnet object within loop:
net <- T5spr_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id 
N <-nrow(att) 
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(closeness_randmat) <- att$id

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net_LC as net_rand names are not Animal IDs    
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T5spr_rand_closeness <- closeness_randmat
#
#
att <- T5sum_attr
net <- T5sum_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T5sum_rand_closeness <- closeness_randmat
#
#
att <- T5aut_attr
net <- T5aut_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)         
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T5aut_rand_closeness <- closeness_randmat
#
#
att <- T5win_attr
net <- T5win_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)        
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T5win_rand_closeness <- closeness_randmat
#
#####
#
att <- T6spr_attr
net <- T6spr_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T6spr_rand_closeness <- closeness_randmat
#
#
att <- T6sum_attr
net <- T6sum_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)         
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T6sum_rand_closeness <- closeness_randmat
#
#
att <- T6aut_attr
net <- T6aut_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)         
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T6aut_rand_closeness <- closeness_randmat
#
#
att <- T6win_attr
net <- T6win_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) { 
  net_rand <- sna::component.largest(net[i,,], result = "graph") 
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_rand), closeness=clo)         
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T6win_rand_closeness <- closeness_randmat
#
#####
#
att <- T7spr_attr # matrix contains 4 foxes so must convert network-> edgelist ->tnet object within loop:
net <- T7spr_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id 
N <-nrow(att) 
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(closeness_randmat) <- att$id

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs     
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T7spr_rand_closeness <- closeness_randmat
#
#
att <- T7sum_attr # matrix contains 4 foxes so must convert network-> edgelist ->tnet object within loop:
net <- T7sum_rand
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id 
N <-nrow(att) 
closeness_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(closeness_randmat) <- att$id

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs  
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2]
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T7sum_rand_closeness <- closeness_randmat
#
#
att <- T7aut_attr
net <- T7aut_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T7aut_rand_closeness <- closeness_randmat
#
#
att <- T7win_attr
net <- T7win_rand # some random networks contain 4 foxes so must convert network-> edgelist ->tnet object within loop:
dimnames(net)[[2]] <- att$id  
dimnames(net)[[3]] <- att$id  
N <-nrow(att)  
closeness_randmat <- matrix(NA, nrow=N, ncol=2000)  
rownames(closeness_randmat) <- att$id  

for (i in 1:2000) {  
  net_LC <- sna::component.largest(net[i,,], result = "graph") 
  net.EL <- sna::as.edgelist.sna(net_LC) # convert matrix to edgelist in sna
  net_rand <- as.tnet(net.EL, type="weighted one-mode tnet") # convert edgelist to tnet object
  clo <- data.frame(closeness_w(net_rand))$closeness 
  df <- data.frame(id=rownames(net_LC), closeness=clo)    # net.LC as net_rand names are not Animal IDs      
  tempvec <- vector(mode="numeric", length=N)              
  names(tempvec) <- att$id                                 
  a <- data.frame(id=names(tempvec), tempvec)              
  b <- df[match(a$id, df$id),2] 
  closeness_randmat[,i] <- b
  setTxtProgressBar(pb, i)
}
T7win_rand_closeness <- closeness_randmat



#==============================================================================================================#

#==============================================================================================================#

#==============================================================================================================#

### WEIGHTED CLUSTERING COEFFICIENT from random network stack - qgraph Onnela 2005 method

detach(package:sna)
detach(package:tnet)
library(qgraph)

net=T1spr_rand
att <- T1spr_attr
N <-nrow(att) 
cc_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { 
  net_rand <- net[i,,]
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T1spr_cc_randmat <- cc_randmat 
#
###
#####
###
#
net <- T1sum_rand
att <- T1sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T1sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T1aut_rand
att <- T1aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T1aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T1win_rand
att <- T1win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T1win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks

#
###
#####
###
#
net <- T2spr_rand
att <- T2spr_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T2spr_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T2sum_rand
att <- T2sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T2sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T2aut_rand
att <- T2aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T2aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T2win_rand
att <- T2win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T2win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T3spr_rand
att <- T3spr_attr
N <-nrow(att) 
cc_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { 
  net_rand <- net[i,,]
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T3spr_cc_randmat <- cc_randmat 
#
###
#####
###
#
net <- T3sum_rand
att <- T3sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T3sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T3aut_rand
att <- T3aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T3aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T3win_rand
att <- T3win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T3win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks

#
###
#####
###
#
net <- T4spr_rand
att <- T4spr_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T4spr_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T4sum_rand
att <- T4sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T4sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T4aut_rand
att <- T4aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T4aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T4win_rand
att <- T4win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T4win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T5spr_rand
att <- T5spr_attr
N <-nrow(att) 
cc_randmat <- matrix(NA, nrow=N, ncol=2000) 
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { 
  net_rand <- net[i,,]
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T5spr_cc_randmat <- cc_randmat 
#
###
#####
###
#
net <- T5sum_rand
att <- T5sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T5sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T5aut_rand
att <- T5aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T5aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T5win_rand
att <- T5win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T5win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T6spr_rand
att <- T6spr_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T6spr_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T6sum_rand
att <- T6sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T6sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T6aut_rand
att <- T6aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T6aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T6win_rand
att <- T6win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T6win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T7spr_rand
att <- T7spr_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T7spr_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T7sum_rand
att <- T7sum_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T7sum_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T7aut_rand
att <- T7aut_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T7aut_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks
#
###
#####
###
#
net <- T7win_rand
att <- T7win_attr
N <-nrow(att) # Number of individuals
cc_randmat <- matrix(NA, nrow=N, ncol=2000) # make N-by-perm matrix filled with NAs
rownames(cc_randmat) <- att$id
for (i in c(1:2000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  net_rand <- net[i,,] # select the corresponding random matrix X from the network stack ## need commas as is 3d matrix
  a <- clustOnnela(net_rand)$clustOnnela
  cc_randmat[,i] <- a
}
T7win_cc_randmat <- cc_randmat # save as the right name for future use / so can do other networks




#==========================================================================================================================#
#==========================================================================================================================#
#==========================================================================================================================#

#                    RUN MODELS ON RANDOM NETWORKS TO GET RANDOM COEFFICIENT ESTIMATES 
#                           (see file 'RCode_SNA_Models for network centrality')

#==========================================================================================================================#
#==========================================================================================================================#
#==========================================================================================================================#

## FINAL DATA FRAMES FOR CENTRALITY MODELS (REAL DATA)
# Strength: 'TAllattr_B4Af' data frame (as has before and after midnight). Response field is: strength (not normalised as is measure of proportion of time spent social and I'm interested in the real values)
# Eigenvector, closeness & CC: 'centrality' data frame. Response fields are: eig.norm, closeness.norm, cc

# Remove surveys where foxes associated randomly and exclude foxes with unknown social status (no sex=NA as all those were seen <5 days so already excluded)
strength_centrality_core <- subset(TAllattr_B4Af, prefavoided==1 & SocialStatus!="NA" & Core==1)

## MAKE NEW ATTRIBS TABLE FOR WHEN USE LOOP TO CALCULATE MODEL COEFFICIENTS FROM RANDOM NETWORKS
# When using rbind or merge it's best to convert factors to characters or can get errors/warnings:

# attributes data for Before/After Midnight (with 2 rows per fox/territory/season combo)
attribs_char <- attribs
attribs_char[sapply(attribs_char, is.factor)] <- lapply(attribs_char[sapply(attribs_char, is.factor)], as.character)
attribs_char$Core <- as.character(attribs_char$Core)
str(attribs_char)

# attributes data for WHOLE DAYs (1 row per fox/territory/season combo)
attr <- attribs[!duplicated(attribs[1:3]),] # lists unique combinations of columns 1-3 (to remove duplicated rows for before/after midnight))
attr_unique <- attr
attr_unique[sapply(attr_unique, is.factor)] <- lapply(attr_unique[sapply(attr_unique, is.factor)], as.character)
attr_unique$Core <- as.character(attr_unique$Core)

detach(package:tnet)
detach(package:qgraph)
detach(package:sna)
library(lmerTest)
library(lme4)

#======================================
#   STRENGTH: BEFORE/AFTER MIDNIGHT
#======================================

Q: What is effect of food availability and individual and environmental attributes on gregariousness?

#== Variables to include in model:
# Rand: 
individ 
territory # explained no variance but kept in anyway as included in all other models. Individ:territory interaction made no difference to model fit/coefs as all core individs were members of ONLY ONE TERRITORY per season
# Fixed:
BeforeMidnight
SocialStatus
Sex
Season

# ORIGINALLY I ALSO FIT MODELS WITH DAYS SEEN AND NFOXES TO 'CONTROL' FOR SIGHTING FREQ AND NUMBER OF AVAILABLE PARTNERS
# This appeared to mask all but the most significant effects 
# Have not seen sighting frequency included in models in any other papers (prob for this reason, and because assoc indices are meant to allow for diffs in sighting freq)
# Found that nfoxes had sig effect but only due to the large network size for territory 6 in aut + wint (both n=21) compared to other networks that were all 4-14 foxes.

# To keep it simple I will use a Spearman's correlation test (for non-parametric data) to confirm why I excluded nfoxes from the model 
# 1. correlation test with all data:
cor.test(as.integer(strength_centrality_core$strength), strength_centrality_core$nfoxes,
         alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # significant (0.045)

# 2. correlation test excluding territory 6 in autumn and winter:
strength_centrality_core$outlier <- ifelse(strength_centrality_core$Territory==6 & strength_centrality_core$SeasonID==3, 1, 0)
strength_centrality_core$outlier <- ifelse(strength_centrality_core$Territory==6 & strength_centrality_core$SeasonID==4, 1, strength_centrality_core$outlier)
a <- subset(strength_centrality_core, outlier==0)
cor.test(as.integer(a$strength), a$nfoxes, alternative="two.sided", method = "spearm", exact = T, conf.level = 0.95) # no longer significant (0.165)

#-----------------------------
# CHECK DISTRIBUTION OF DATA
#-----------------------------
hist(strength_centrality_core$strength)
hist(log10(strength_centrality_core$strength)) # log-transformation makes data approximate a normal distrib, suggesting they do follow a lognormal distribution
# But I have several zero values and log(0) or log10(0) = infinity, so need to add 1 to each value before transformation:
hist(log10(strength_centrality_core$strength+1)) 

library(fitdistrplus)
# Cullen & Frey graph:
descdist(strength_centrality_core$strength, discrete = FALSE, boot=5000)  # discrete=F specifies a continuous response
# For some distributions (normal, uniform, logistic, exponential), there is only one possible value for 
# skewness and kurtosis (e.g. for a normal distrib, skewness = 0 and kurtosis = 3), and the distrib is thus
# represented by a point on the plot. Lognotmal & gamma have wider range of values shown as lines. 
# Beta has greatest spread of possible values.

# for my data, the boostrapped values lie in beta but beta distributed values must be between 0-1, as they're expected to be proportions.
# Bootstrapped values follow trajectory of lognormal and gamma lines, and weibull is also similar to lnorm and gamma
# so my data could be normal, lognormal, gamma, weibull or beta

# Plot data against normal, logorm, gamma and weibull distributions
fit.norm <- fitdist(strength_centrality_core$strength, "norm")
fit.lnorm <- fitdist(strength_centrality_core$strength+1, "lnorm")
fit.gam <- fitdist(strength_centrality_core$strength+1, "gamma") # GAMMA wont work unless add 1 to data as log(0) or log10(0) = infinite
fit.weibull <- fitdist(strength_centrality_core$strength+1, "weibull") # weibull can't take zeros so added 1 to y

# Plots in descending order of crappiness (from high crappiness to low(er) crappiness)
plot(fit.norm)
plot(fit.weibull)
plot(fit.gam)
plot(fit.lnorm) # lnorm best 

# ALTERNATIVE PLOTS USING MASS & car PACKAGES
require(MASS)
require(car)
qqp((strength_centrality_core$strength), "norm", main="normal")
qqp(strength_centrality_core$strength, "lnorm", main="lognormal") 
gamma <- fitdistr(strength_centrality_core$strength+1, "gamma", start=NULL)
qqp(strength_centrality_core$strength+1, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma") # gamma is poor 
# lognormal is best (but not perfect)

#--------
# MODELS
#--------
# lognormal LMM fit = LMM with log-transformed response (need to add 1 to all first as log(0) or log10(0) = infinity) 
# Can use log or log10, result is same but log10 easier to interpret as log10(1)=0, log10(10)=2, log10(100)=3 etc.
strengthb4afmod_full  <- lmer(log10(strength+1) ~ fBeforeMidnight + Sex*fSeason + SocialStatus*fSeason +
                                (1|ShortCode) + (1|fTerritory),
                              data=strength_centrality_core, REML=F,
                              verbose=T,
                              control=lmerControl(optimizer="bobyqa",
                                                  optCtrl = list(maxfun=1000000),
                                                  check.conv.grad=.makeCC("warning",0.05)))
#= Check residuals:
plot(fitted(strengthb4afmod_full), resid(strengthb4afmod_full))   
lines(smooth.spline(fitted(strengthb4afmod_full), resid(strengthb4afmod_full))) 
hist(resid(strengthb4afmod_full), prob=T) # prob=T to plot as probability (so can overlay a smoothing line)
lines(density(resid(strengthb4afmod_full))) # residuals approx normal with v.little heteroscedasticity (3 outliers near zero)
lattice::dotplot(ranef(strengthb4afmod_full, condVar=TRUE))   # Pluto seems to be outlier
summary(strengthb4afmod_full) # get random effect variances and SDs


# COMPARE REAL WITH RANDOM DATA
#------------------------------

# set up progress bar
perm <- 2000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrix to store the coefficient estimate from each randomised matrix
Intercept_perm <-rep(NA,perm)
fBeforeMidnight1_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm)
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
fSeason2.SocialStatusSub_perm <-rep(NA,perm)
fSeason3.SocialStatusSub_perm <-rep(NA,perm)
fSeason4.SocialStatusSub_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  input_strength <- rbind(
    # Before midnight:
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength=T1sprB4_rand_strength[,i], SeasonID="1", Territory="1", BeforeMidnight="1", id=row.names(T1sprB4_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength=T1sumB4_rand_strength[,i], SeasonID="2", Territory="1", BeforeMidnight="1", id=row.names(T1sumB4_rand_strength)), 
    data.frame(strength=T1autB4_rand_strength[,i], SeasonID="3", Territory="1", BeforeMidnight="1", id=row.names(T1autB4_rand_strength)),
    data.frame(strength=T1winB4_rand_strength[,i], SeasonID="4", Territory="1", BeforeMidnight="1", id=row.names(T1winB4_rand_strength)),
    data.frame(strength= T2sprB4_rand_strength[,i], SeasonID="1", Territory="2", BeforeMidnight="1", id=row.names(T2sprB4_rand_strength)),
    # data.frame(strength= T2sumB4_rand_strength[,i], SeasonID="2", Territory="2", BeforeMidnight="1", id=row.names(T2sumB4_rand_strength)), # no prefavoided
    data.frame(strength= T2autB4_rand_strength[,i], SeasonID="3", Territory="2", BeforeMidnight="1", id=row.names(T2autB4_rand_strength)),
    data.frame(strength= T2winB4_rand_strength[,i], SeasonID="4", Territory="2", BeforeMidnight="1", id=row.names(T2winB4_rand_strength)),
    data.frame(strength= T3sprB4_rand_strength[,i], SeasonID="1", Territory="3", BeforeMidnight="1", id=row.names(T3sprB4_rand_strength)),
    data.frame(strength= T3sumB4_rand_strength[,i], SeasonID="2", Territory="3", BeforeMidnight="1", id=row.names(T3sumB4_rand_strength)),
    data.frame(strength= T3autB4_rand_strength[,i], SeasonID="3", Territory="3", BeforeMidnight="1", id=row.names(T3autB4_rand_strength)),
    data.frame(strength= T3winB4_rand_strength[,i], SeasonID="4", Territory="3", BeforeMidnight="1", id=row.names(T3winB4_rand_strength)),
    # data.frame(strength= T4sprB4_rand_strength[,i], SeasonID="1", Territory="4", BeforeMidnight="1", id=row.names(T4sprB4_rand_strength)), # no prefavoided
    # data.frame(strength= T4sumB4_rand_strength[,i], SeasonID="2", Territory="4", BeforeMidnight="1", id=row.names(T4sumB4_rand_strength)), # no prefavoided
    data.frame(strength= T4autB4_rand_strength[,i], SeasonID="3", Territory="4", BeforeMidnight="1", id=row.names(T4autB4_rand_strength)),
    data.frame(strength= T4winB4_rand_strength[,i], SeasonID="4", Territory="4", BeforeMidnight="1", id=row.names(T4winB4_rand_strength)),
    data.frame(strength= T5sprB4_rand_strength[,i], SeasonID="1", Territory="5", BeforeMidnight="1", id=row.names(T5sprB4_rand_strength)),
    data.frame(strength= T5sumB4_rand_strength[,i], SeasonID="2", Territory="5", BeforeMidnight="1", id=row.names(T5sumB4_rand_strength)),
    data.frame(strength= T5autB4_rand_strength[,i], SeasonID="3", Territory="5", BeforeMidnight="1", id=row.names(T5autB4_rand_strength)),
    # data.frame(strength= T5winB4_rand_strength[,i], SeasonID="4", Territory="5", BeforeMidnight="1", id=row.names(T5winB4_rand_strength)), # no prefavoided
    data.frame(strength= T6sprB4_rand_strength[,i], SeasonID="1", Territory="6", BeforeMidnight="1", id=row.names(T6sprB4_rand_strength)), 
    data.frame(strength= T6sumB4_rand_strength[,i], SeasonID="2", Territory="6", BeforeMidnight="1", id=row.names(T6sumB4_rand_strength)), 
    data.frame(strength= T6autB4_rand_strength[,i], SeasonID="3", Territory="6", BeforeMidnight="1", id=row.names(T6autB4_rand_strength)), 
    data.frame(strength= T6winB4_rand_strength[,i], SeasonID="4", Territory="6", BeforeMidnight="1", id=row.names(T6winB4_rand_strength)),
    data.frame(strength= T7sprB4_rand_strength[,i], SeasonID="1", Territory="7", BeforeMidnight="1", id=row.names(T7sprB4_rand_strength)),
    data.frame(strength= T7sumB4_rand_strength[,i], SeasonID="2", Territory="7", BeforeMidnight="1", id=row.names(T7sumB4_rand_strength)), 
    data.frame(strength= T7autB4_rand_strength[,i], SeasonID="3", Territory="7", BeforeMidnight="1", id=row.names(T7autB4_rand_strength)),
    data.frame(strength= T7winB4_rand_strength[,i], SeasonID="4", Territory="7", BeforeMidnight="1", id=row.names(T7winB4_rand_strength)),
    # After midnight:
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength= T1sprAf_rand_strength[,i], SeasonID="1", Territory="1", BeforeMidnight="0", id=row.names(T1sprAf_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength= T1sumAf_rand_strength[,i], SeasonID="2", Territory="1", BeforeMidnight="0", id=row.names(T1sumAf_rand_strength)), 
    data.frame(strength= T1autAf_rand_strength[,i], SeasonID="3", Territory="1", BeforeMidnight="0", id=row.names(T1autAf_rand_strength)),
    data.frame(strength= T1winAf_rand_strength[,i], SeasonID="4", Territory="1", BeforeMidnight="0", id=row.names(T1winAf_rand_strength)),
    data.frame(strength= T2sprAf_rand_strength[,i], SeasonID="1", Territory="2", BeforeMidnight="0", id=row.names(T2sprAf_rand_strength)),
    # data.frame(strength= T2sumAf_rand_strength[,i], SeasonID="2", Territory="2", BeforeMidnight="0", id=row.names(T2sumAf_rand_strength)), # couldnt permute as  no true assocs in this network
    data.frame(strength= T2autAf_rand_strength[,i], SeasonID="3", Territory="2", BeforeMidnight="0", id=row.names(T2autAf_rand_strength)), 
    data.frame(strength= T2winAf_rand_strength[,i], SeasonID="4", Territory="2", BeforeMidnight="0", id=row.names(T2winAf_rand_strength)),
    data.frame(strength= T3sprAf_rand_strength[,i], SeasonID="1", Territory="3", BeforeMidnight="0", id=row.names(T3sprAf_rand_strength)),
    data.frame(strength= T3sumAf_rand_strength[,i], SeasonID="2", Territory="3", BeforeMidnight="0", id=row.names(T3sumAf_rand_strength)),
    data.frame(strength= T3autAf_rand_strength[,i], SeasonID="3", Territory="3", BeforeMidnight="0", id=row.names(T3autAf_rand_strength)),
    data.frame(strength= T3winAf_rand_strength[,i], SeasonID="4", Territory="3", BeforeMidnight="0", id=row.names(T3winAf_rand_strength)),
    # data.frame(strength= T4sprAf_rand_strength[,i], SeasonID="1", Territory="4", BeforeMidnight="0", id=row.names(T4sprAf_rand_strength)), # no prefavoided
    # data.frame(strength= T4sumAf_rand_strength[,i], SeasonID="2", Territory="4", BeforeMidnight="0", id=row.names(T4sumAf_rand_strength)), # no prefavoided
    data.frame(strength= T4autAf_rand_strength[,i], SeasonID="3", Territory="4", BeforeMidnight="0", id=row.names(T4autAf_rand_strength)),
    data.frame(strength= T4winAf_rand_strength[,i], SeasonID="4", Territory="4", BeforeMidnight="0", id=row.names(T4winAf_rand_strength)),
    data.frame(strength= T5sprAf_rand_strength[,i], SeasonID="1", Territory="5", BeforeMidnight="0", id=row.names(T5sprAf_rand_strength)),
    data.frame(strength= T5sumAf_rand_strength[,i], SeasonID="2", Territory="5", BeforeMidnight="0", id=row.names(T5sumAf_rand_strength)),
    data.frame(strength= T5autAf_rand_strength[,i], SeasonID="3", Territory="5", BeforeMidnight="0", id=row.names(T5autAf_rand_strength)),
    # data.frame(strength= T5winAf_rand_strength[,i], SeasonID="4", Territory="5", BeforeMidnight="0", id=row.names(T5winAf_rand_strength)), # no prefavoided
    data.frame(strength= T6sprAf_rand_strength[,i], SeasonID="1", Territory="6", BeforeMidnight="0", id=row.names(T6sprAf_rand_strength)), 
    data.frame(strength= T6sumAf_rand_strength[,i], SeasonID="2", Territory="6", BeforeMidnight="0", id=row.names(T6sumAf_rand_strength)), 
    data.frame(strength= T6autAf_rand_strength[,i], SeasonID="3", Territory="6", BeforeMidnight="0", id=row.names(T6autAf_rand_strength)), 
    data.frame(strength= T6winAf_rand_strength[,i], SeasonID="4", Territory="6", BeforeMidnight="0", id=row.names(T6winAf_rand_strength)),
    data.frame(strength= T7sprAf_rand_strength[,i], SeasonID="1", Territory="7", BeforeMidnight="0", id=row.names(T7sprAf_rand_strength)),
    data.frame(strength= T7sumAf_rand_strength[,i], SeasonID="2", Territory="7", BeforeMidnight="0", id=row.names(T7sumAf_rand_strength)), 
    data.frame(strength= T7autAf_rand_strength[,i], SeasonID="3", Territory="7", BeforeMidnight="0", id=row.names(T7autAf_rand_strength)),
    data.frame(strength= T7winAf_rand_strength[,i], SeasonID="4", Territory="7", BeforeMidnight="0", id=row.names(T7winAf_rand_strength)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_strength$id <- as.character(input_strength$id)
  input_strength$SeasonID <- as.character(input_strength$SeasonID)
  input_strength$Territory <- as.character(input_strength$Territory)
  input_strength$BeforeMidnight <- as.character(input_strength$BeforeMidnight)
  # Add attributes to input_strength FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_strength$ShortCode <- attribs_char[match(input_strength$id, attribs_char$id),4] 
  input_strength$Sex <- attribs_char[match(input_strength$id, attribs_char$id),5] 
  input_strength$SocialStatus <- attribs_char[match(input_strength$id, attribs_char$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_strength, y=attribs_char[ , c("id", "SeasonID", "Territory", "BeforeMidnight", "Core")], 
              by = c("id", "SeasonID", "Territory", "BeforeMidnight"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$Sex <- factor(df$Sex)
  df$fBeforeMidnight <- factor(df$BeforeMidnight)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <-lmer(log10(strength+1) ~ fBeforeMidnight + Sex*fSeason + SocialStatus*fSeason +
                     (1|ShortCode) + (1|fTerritory), REML=F, 
                   data=subset(df, SocialStatus!="NA" & Core==1),
                   control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
  # get the coefficient estimate and save it
  a <- coef(summary(model_tmp))
  Intercept_perm[i] <-a[1,1]
  fBeforeMidnight1_perm[i] <-a[2,1]
  SexM_perm[i] <-a[3,1]
  fSeason2_perm[i] <-a[4,1]
  fSeason3_perm[i] <-a[5,1]
  fSeason4_perm[i] <-a[6,1]
  SocialStatusSub_perm[i] <-a[7,1]
  SexM.fSeason2_perm[i] <-a[8,1]
  SexM.fSeason3_perm[i] <-a[9,1]
  SexM.fSeason4_perm[i] <-a[10,1]
  fSeason2.SocialStatusSub_perm[i] <-a[11,1]
  fSeason3.SocialStatusSub_perm[i] <-a[12,1]
  fSeason4.SocialStatusSub_perm[i] <-a[13,1]
  # update prog bar
  setTxtProgressBar(pb, i)
}

### Calc P-values as proportion of coefficient estimates from randomised matrices that are greater than the coefficient
# 1. Coefficients from the observed (real) data:
realmod <- coef(summary(strengthb4afmod_full))
Intercept_obs <- realmod[1,1]
fBeforeMidnight1_obs <- realmod[2,1]
SexM_obs <- realmod[3,1]
fSeason2_obs <- realmod[4,1]
fSeason3_obs <- realmod[5,1]
fSeason4_obs <- realmod[6,1]
SocialStatusSub_obs <- realmod[7,1]
SexM.fSeason2_obs <- realmod[8,1]
SexM.fSeason3_obs <- realmod[9,1]
SexM.fSeason4_obs <- realmod[10,1]
fSeason2.SocialStatusSub_obs <- realmod[11,1]
fSeason3.SocialStatusSub_obs <- realmod[12,1]
fSeason4.SocialStatusSub_obs <- realmod[13,1]

# 2. Calculate p-values
Intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
fBeforeMidnight1_Pvalue <- sum(fBeforeMidnight1_perm>fBeforeMidnight1_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
fSeason2.SocialStatusSub_Pvalue <- sum(fSeason2.SocialStatusSub_perm>fSeason2.SocialStatusSub_obs)/2000
fSeason3.SocialStatusSub_Pvalue <- sum(fSeason3.SocialStatusSub_perm>fSeason3.SocialStatusSub_obs)/2000
fSeason4.SocialStatusSub_Pvalue <- sum(fSeason4.SocialStatusSub_perm>fSeason4.SocialStatusSub_obs)/2000

# 3. Save in table
strength_b4afmod_prands <- rbind(intercept_Pvalue,
                                 fBeforeMidnight1_Pvalue,
                                 SexM_Pvalue,
                                 fSeason2_Pvalue,
                                 fSeason3_Pvalue,
                                 fSeason4_Pvalue,
                                 SocialStatusSub_Pvalue,
                                 SexM.fSeason2_Pvalue,
                                 SexM.fSeason3_Pvalue,
                                 SexM.fSeason4_Pvalue,
                                 fSeason2.SocialStatusSub_Pvalue,
                                 fSeason3.SocialStatusSub_Pvalue,
                                 fSeason4.SocialStatusSub_Pvalue) 

# 4. Plot histogram of random coefficients with red line for real
hist(fBeforeMidnight1_b4afperm,breaks=100, main=paste("P = ", fBeforeMidnight1_b4afPvalue),
     xlab="Effect of time of day on strength", ylab="Probability") 
abline(v=fBeforeMidnight1_b4afobs, col="red")
dev.print(jpeg, "Rplot_Hist BeforeMidnight-strength Prand.jpeg", res=700, height=15, width=15, units="cm") # save as jpeg

#== Difference between before and after midnight is not significantly diff from random, so re-fit model to strength calculated across whole days:



# WHOLE DAY STRENGTH (as BeforeMidnight not significant)
#-------------------------------------------------------

# DATA:
str(centrality_core) # whole day data (same dataset used for eig, closeness and CC models)

# check distribution
hist(centrality_core$strength) # approx normal??
hist(log10(centrality_core$strength)) # log10 transformation makes it left-skewed (don't need to add one now as no core foxes have zero strength - as criteria is >2 true assocs and it was only AfterMidnight networks that contained only self-associations for core foxes)
library(fitdistrplus)
descdist(centrality_core$strength, discrete = FALSE, boot=5000) # distrib is close to normal or uniform (square)
# Plot data against normal
fit.norm <- fitdist(centrality_core$strength, "norm")
plot(fit.norm) # looks OK (apart from bump at zero)
require(MASS)
require(car)
qqp((centrality_core$strength), "norm", main="normal") # just about normal
# Core strength from whole days is approx normal so fit LMM (don't need log10)

# Full model 
strengthWDmod_full  <- lmer(strength ~ Sex*fSeason + SocialStatus*fSeason + (1|fTerritory) + (1|ShortCode),
                            data=centrality_core, REML=F,
                            control=lmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=1000000),
                                                check.conv.grad=.makeCC("warning",0.05)))
# Model without status*season interaction
strengthWDmod_red  <- lmer(strength ~ Sex*fSeason + SocialStatus + (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))
#= Check residuals:
plot(fitted(strengthWDmod_full), resid(strengthWDmod_full))   
lines(smooth.spline(fitted(strengthWDmod_full), resid(strengthWDmod_full))) 
hist(resid(strengthWDmod_full), prob=T) # prob=T to plot as probability (so can overlay a smoothing line)
lines(density(resid(strengthWDmod_full))) # residuals approx normal with v.little heteroscedasticity (3 outliers near zero)
lattice::dotplot(ranef(strengthWDmod_full, condVar=TRUE))   # Pluto seems to be outlier

summary(strengthWDmod_red) # get random effect variances and SDs


# Compare observed and random model coefficients: strength ~ sex*season + status*season
#------------------------------------------------

# set up progress bar
perm <- 2000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrices to store the coefficient estimates from random data
Intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm)
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
fSeason2.SocialStatusSub_perm <-rep(NA,perm)
fSeason3.SocialStatusSub_perm <-rep(NA,perm)
fSeason4.SocialStatusSub_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  input_strength <- rbind(
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength=T1spr_rand_strength[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength=T1sum_rand_strength[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_strength)), 
    data.frame(strength=T1aut_rand_strength[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_strength)),
    data.frame(strength=T1win_rand_strength[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_strength)),
    data.frame(strength= T2spr_rand_strength[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_strength)),
    # data.frame(strength= T2sum_rand_strength[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_strength)), # no prefavoided
    data.frame(strength= T2aut_rand_strength[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_strength)),
    data.frame(strength= T2win_rand_strength[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_strength)),
    data.frame(strength= T3spr_rand_strength[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_strength)),
    data.frame(strength= T3sum_rand_strength[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_strength)),
    data.frame(strength= T3aut_rand_strength[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_strength)),
    data.frame(strength= T3win_rand_strength[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_strength)),
    # data.frame(strength= T4spr_rand_strength[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_strength)), # no prefavoided
    # data.frame(strength= T4sum_rand_strength[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_strength)), # no prefavoided
    data.frame(strength= T4aut_rand_strength[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_strength)),
    data.frame(strength= T4win_rand_strength[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_strength)),
    data.frame(strength= T5spr_rand_strength[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_strength)),
    data.frame(strength= T5sum_rand_strength[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_strength)),
    data.frame(strength= T5aut_rand_strength[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_strength)),
    # data.frame(strength= T5win_rand_strength[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_strength)), # no prefavoided
    data.frame(strength= T6spr_rand_strength[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_strength)), 
    data.frame(strength= T6sum_rand_strength[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_strength)), 
    data.frame(strength= T6aut_rand_strength[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_strength)), 
    data.frame(strength= T6win_rand_strength[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_strength)),
    data.frame(strength= T7spr_rand_strength[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_strength)),
    data.frame(strength= T7sum_rand_strength[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_strength)), 
    data.frame(strength= T7aut_rand_strength[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_strength)),
    data.frame(strength= T7win_rand_strength[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_strength)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_strength$id <- as.character(input_strength$id)
  input_strength$SeasonID <- as.character(input_strength$SeasonID)
  input_strength$Territory <- as.character(input_strength$Territory)
  # Add attributes to input_strength FROM ATTR_UNIQUE DATAFRAME WITH 1 ROW PER FOX/TERRITORY/SEASON
  input_strength$ShortCode <- attr_unique[match(input_strength$id, attr_unique$id),4] 
  input_strength$Sex <- attr_unique[match(input_strength$id, attr_unique$id),5] 
  input_strength$SocialStatus <- attr_unique[match(input_strength$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_strength, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  # run the model
  model_tmp <-lmer(strength ~ Sex*fSeason + SocialStatus*fSeason + (1|ShortCode) + (1|fTerritory), REML=F, 
                   data=subset(df, SocialStatus!="NA" & Core==1),
                   control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  Intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SocialStatusSub_perm[i] <- a[6,1]
  SexM.fSeason2_perm[i] <- a[7,1]
  SexM.fSeason3_perm[i] <- a[8,1]
  SexM.fSeason4_perm[i] <- a[9,1]
  fSeason2.SocialStatusSub_perm[i] <- a[10,1]
  fSeason3.SocialStatusSub_perm[i] <- a[11,1]
  fSeason4.SocialStatusSub_perm[i] <- a[12,1]
  # update prog bar
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod <-coef(summary(strengthWDmod_full))
Intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
fSeason2_obs <- realmod[3,1]
fSeason3_obs <- realmod[4,1]
fSeason4_obs <- realmod[5,1]
SocialStatusSub_obs <- realmod[6,1]
SexM.fSeason2_obs <- realmod[7,1]
SexM.fSeason3_obs <- realmod[8,1]
SexM.fSeason4_obs <- realmod[9,1]
fSeason2.SocialStatusSub_obs <- realmod[10,1]
fSeason3.SocialStatusSub_obs <- realmod[11,1]
fSeason4.SocialStatusSub_obs <- realmod[12,1]

# calculate p-values
Intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
fSeason2.SocialStatusSub_Pvalue <- sum(fSeason2.SocialStatusSub_perm>fSeason2.SocialStatusSub_obs)/2000
fSeason3.SocialStatusSub_Pvalue <- sum(fSeason3.SocialStatusSub_perm>fSeason3.SocialStatusSub_obs)/2000
fSeason4.SocialStatusSub_Pvalue <- sum(fSeason4.SocialStatusSub_perm>fSeason4.SocialStatusSub_obs)/2000

# Save in table
strength_Prands <- rbind(intercept_Pvalue,
                         SexM_Pvalue,
                         fSeason2_Pvalue,
                         fSeason3_Pvalue,
                         fSeason4_Pvalue,
                         SocialStatusSub_Pvalue,
                         SexM.fSeason2_Pvalue,
                         SexM.fSeason3_Pvalue,
                         SexM.fSeason4_Pvalue,
                         fSeason2.SocialStatusSub_Pvalue,
                         fSeason3.SocialStatusSub_Pvalue,
                         fSeason4.SocialStatusSub_Pvalue)


# Compare observed and random model coefficients: strength ~ sex*season + status WITH POST HOC TEST
#------------------------------------------------

# set up progress bar
perm <- 2000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrices to store the coefficient estimates from random data
Intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm)
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
# for post-hoc tests - compare lsmeans contrast estimate
lsm_F12_perm <-rep(NA,perm)
lsm_F13_perm <-rep(NA,perm)
lsm_F14_perm <-rep(NA,perm)
lsm_F23_perm <-rep(NA,perm)
lsm_F24_perm <-rep(NA,perm)
lsm_F34_perm <-rep(NA,perm)
lsm_M12_perm <-rep(NA,perm)
lsm_M13_perm <-rep(NA,perm)
lsm_M14_perm <-rep(NA,perm)
lsm_M23_perm <-rep(NA,perm)
lsm_M24_perm <-rep(NA,perm)
lsm_M34_perm <-rep(NA,perm)
lsm_FMspr_perm <-rep(NA,perm)
lsm_FMsum_perm <-rep(NA,perm)
lsm_FMaut_perm <-rep(NA,perm)
lsm_FMwin_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  input_strength <- rbind(
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength=T1spr_rand_strength[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength=T1sum_rand_strength[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_strength)), 
    data.frame(strength=T1aut_rand_strength[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_strength)),
    data.frame(strength=T1win_rand_strength[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_strength)),
    data.frame(strength= T2spr_rand_strength[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_strength)),
    # data.frame(strength= T2sum_rand_strength[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_strength)), # no prefavoided
    data.frame(strength= T2aut_rand_strength[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_strength)),
    data.frame(strength= T2win_rand_strength[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_strength)),
    data.frame(strength= T3spr_rand_strength[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_strength)),
    data.frame(strength= T3sum_rand_strength[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_strength)),
    data.frame(strength= T3aut_rand_strength[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_strength)),
    data.frame(strength= T3win_rand_strength[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_strength)),
    # data.frame(strength= T4spr_rand_strength[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_strength)), # no prefavoided
    # data.frame(strength= T4sum_rand_strength[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_strength)), # no prefavoided
    data.frame(strength= T4aut_rand_strength[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_strength)),
    data.frame(strength= T4win_rand_strength[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_strength)),
    data.frame(strength= T5spr_rand_strength[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_strength)),
    data.frame(strength= T5sum_rand_strength[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_strength)),
    data.frame(strength= T5aut_rand_strength[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_strength)),
    # data.frame(strength= T5win_rand_strength[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_strength)), # no prefavoided
    data.frame(strength= T6spr_rand_strength[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_strength)), 
    data.frame(strength= T6sum_rand_strength[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_strength)), 
    data.frame(strength= T6aut_rand_strength[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_strength)), 
    data.frame(strength= T6win_rand_strength[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_strength)),
    data.frame(strength= T7spr_rand_strength[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_strength)),
    data.frame(strength= T7sum_rand_strength[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_strength)), 
    data.frame(strength= T7aut_rand_strength[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_strength)),
    data.frame(strength= T7win_rand_strength[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_strength)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_strength$id <- as.character(input_strength$id)
  input_strength$SeasonID <- as.character(input_strength$SeasonID)
  input_strength$Territory <- as.character(input_strength$Territory)
  # Add attributes to input_strength FROM ATTR_UNIQUE DATAFRAME WITH 1 ROW PER FOX/TERRITORY/SEASON
  input_strength$ShortCode <- attr_unique[match(input_strength$id, attr_unique$id),4] 
  input_strength$Sex <- attr_unique[match(input_strength$id, attr_unique$id),5] 
  input_strength$SocialStatus <- attr_unique[match(input_strength$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_strength, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  # run the model
  model_tmp <-lmer(strength ~ Sex*fSeason + SocialStatus + (1|ShortCode) + (1|fTerritory), REML=F, 
                   data=subset(df, SocialStatus!="NA" & Core==1),
                   control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning",0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  Intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SocialStatusSub_perm[i] <- a[6,1]
  SexM.fSeason2_perm[i] <- a[7,1]
  SexM.fSeason3_perm[i] <- a[8,1]
  SexM.fSeason4_perm[i] <- a[9,1]
  # Run post-hoc comparisons and save estimated difference/effect size of contrast:
  # First compare estimates between seasons for each sex
  lsm_temp<-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~ fSeason|Sex))$contrasts) 
  lsm_F12_perm[i] <- lsm_temp[1,4]
  lsm_F13_perm[i] <- lsm_temp[2,4]
  lsm_F14_perm[i] <- lsm_temp[3,4]
  lsm_F23_perm[i] <- lsm_temp[4,4]
  lsm_F24_perm[i] <- lsm_temp[5,4]
  lsm_F34_perm[i] <- lsm_temp[6,4]
  lsm_M12_perm[i] <- lsm_temp[7,3]
  lsm_M13_perm[i] <- lsm_temp[8,3]
  lsm_M14_perm[i] <- lsm_temp[9,3]
  lsm_M23_perm[i] <- lsm_temp[10,3]
  lsm_M24_perm[i] <- lsm_temp[11,3]
  lsm_M34_perm[i] <- lsm_temp[12,3]
  # compare estimates between sexes for each season
  lsm_temp2<-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~ Sex|fSeason))$contrasts)
  lsm_FMspr_perm[i] <- lsm_temp2[1,3]
  lsm_FMsum_perm[i] <- lsm_temp2[2,3]
  lsm_FMaut_perm[i] <- lsm_temp2[3,3]
  lsm_FMwin_perm[i] <- lsm_temp2[4,3]
  # update prog bar
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod <-coef(summary(strengthWDmod_red))
Intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
fSeason2_obs <- realmod[3,1]
fSeason3_obs <- realmod[4,1]
fSeason4_obs <- realmod[5,1]
SocialStatusSub_obs <- realmod[6,1]
SexM.fSeason2_obs <- realmod[7,1]
SexM.fSeason3_obs <- realmod[8,1]
SexM.fSeason4_obs <- realmod[9,1]

# real post-hoc test results
lsm_tukey1<-data.frame(summary(lsmeans::lsmeans(strengthWDmod_red, pairwise~ fSeason|Sex))$contrasts)
lsm_F12_obs <- lsm_tukey1[1,3]
lsm_F13_obs <- lsm_tukey1[2,3]
lsm_F14_obs <- lsm_tukey1[3,3]
lsm_F23_obs <- lsm_tukey1[4,3]
lsm_F24_obs <- lsm_tukey1[5,3]
lsm_F34_obs <- lsm_tukey1[6,3]
lsm_M12_obs <- lsm_tukey1[7,3]
lsm_M13_obs <- lsm_tukey1[8,3]
lsm_M14_obs <- lsm_tukey1[9,3]
lsm_M23_obs <- lsm_tukey1[10,3]
lsm_M24_obs <- lsm_tukey1[11,3]
lsm_M34_obs <- lsm_tukey1[12,3]
lsm_tukey2<-data.frame(summary(lsmeans::lsmeans(strengthWDmod_red, pairwise~ Sex|fSeason))$contrasts)
lsm_FMspr_obs <- lsm_tukey2[1,3]
lsm_FMsum_obs <- lsm_tukey2[2,3]
lsm_FMaut_obs <- lsm_tukey2[3,3]
lsm_FMwin_obs <- lsm_tukey2[4,3]

# p-values for model coefficients
Intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
# Save in table
strength_Prands <- rbind(intercept_Pvalue,
                         SexM_Pvalue,
                         fSeason2_Pvalue,
                         fSeason3_Pvalue,
                         fSeason4_Pvalue,
                         SocialStatusSub_Pvalue,
                         SexM.fSeason2_Pvalue,
                         SexM.fSeason3_Pvalue,
                         SexM.fSeason4_Pvalue)
# p-values for post hoc tests
lsm_F12_Pvalue <- sum(lsm_F12_perm>lsm_F12_obs)/2000
lsm_F13_Pvalue <- sum(lsm_F13_perm>lsm_F13_obs)/2000
lsm_F14_Pvalue <- sum(lsm_F14_perm>lsm_F14_obs)/2000
lsm_F23_Pvalue <- sum(lsm_F23_perm>lsm_F23_obs)/2000
lsm_F24_Pvalue <- sum(lsm_F24_perm>lsm_F24_obs)/2000
lsm_F34_Pvalue <- sum(lsm_F34_perm>lsm_F34_obs)/2000 # GOT THESE

lsm_M12_Pvalue <- sum(lsm_M12_perm>lsm_M12_obs)/2000
lsm_M13_Pvalue <- sum(lsm_M13_perm>lsm_M13_obs)/2000
lsm_M14_Pvalue <- sum(lsm_M14_perm>lsm_M14_obs)/2000
lsm_M23_Pvalue <- sum(lsm_M23_perm>lsm_M23_obs)/2000
lsm_M24_Pvalue <- sum(lsm_M24_perm>lsm_M24_obs)/2000
lsm_M34_Pvalue <- sum(lsm_M34_perm>lsm_M34_obs)/2000

lsm_FMspr_Pvalue <- sum(lsm_FMspr_perm>lsm_FMspr_obs)/2000 # GOT THESE
lsm_FMsum_Pvalue <- sum(lsm_FMsum_perm>lsm_FMsum_obs)/2000
lsm_FMaut_Pvalue <- sum(lsm_FMaut_perm>lsm_FMaut_obs)/2000
lsm_FMwin_Pvalue <- sum(lsm_FMwin_perm>lsm_FMwin_obs)/2000

strength_lsm_Prands <- rbind(lsm_F12_Pvalue,
                             lsm_F13_Pvalue,
                             lsm_F14_Pvalue,
                             lsm_F23_Pvalue,
                             lsm_F24_Pvalue,
                             lsm_F34_Pvalue,
                             lsm_M12_Pvalue,
                             lsm_M13_Pvalue,
                             lsm_M14_Pvalue,
                             lsm_M23_Pvalue,
                             lsm_M24_Pvalue,
                             lsm_M34_Pvalue,
                             lsm_FMspr_Pvalue,
                             lsm_FMsum_Pvalue,
                             lsm_FMaut_Pvalue,
                             lsm_FMwin_Pvalue)

hist(SocialStatusSub_perm, breaks=100, xlim=c(-0.10,0.04), main=paste("P = ", SocialStatusSub_Pvalue),
     xlab="Effect of social status (subordinate) on strength", ylab="Probability") 
abline(v=SocialStatusSub_obs, col="red")
dev.print(jpeg, "Rplot_Hist status-strength Prand.jpeg", res=700, height=15, width=15, units="cm") # save as jpeg

# Plot  status against strength
boxplot(centrality_core$strength~centrality_core$SocialStatus, col=c("cadetblue", "cadetblue1"), ylab="Strength", cex.lab=1.2)
dev.print(jpeg, "RPlot_strength dom sub.jpeg", res=700, height=5, width=4, units="in") # save above in high res

boxplot(centrality_core$strength~centrality_core$Sex*centrality_core$fSeason)

# PLOT sex*season interaction
strength_mod_lsm <- data.frame(summary(lsmeans::lsmeans(strengthWDmod_red, pairwise~Sex|fSeason))$lsmeans)
strength_mod_lsm$Season <- ordered(strength_mod_lsm$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

library(ggplot2)
# line plot
ggp <- ggplot(strength_mod_lsm, aes(x=Season, y=lsmean, group=Sex, colour=factor(Sex))) 
ggp + geom_point(size=4, aes(group=Sex), position=position_dodge(0.3)) + 
  xlab("") + ylab("Strength\n") +
  geom_errorbar(mapping=aes(x=Season, ymin=lower.CL, ymax=upper.CL),
                width=0.2, size=1, position=position_dodge(0.3)) +
  scale_colour_manual(name="Sex", values=c('royalblue3','firebrick1')) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black", size=2),
        panel.border = element_rect(linetype = "solid", colour = "black"))
# save in high res.
dev.print(jpeg, "RPlot_LMM lsmeans & CIs for strength by sex and season_LINES.jpeg", res=700, height=6, width=8, units="in") 

# box plot
centrality_core$Season <- ordered(centrality_core$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
centrality_core$Sex <- factor(centrality_core$Sex, levels(centrality_core$Sex)[c(2,1)]) # re-order sex
ggp <- ggplot(centrality_core, aes(x=Sex, y=strength, group=Sex, fill=factor(Sex))) 
ggp + geom_boxplot() +
  facet_grid(~Season) +
  scale_fill_manual(name="Sex", values=c('royalblue3','firebrick1'),guide=FALSE) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black", size=2),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.x = element_text(face="bold"),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  xlab("") + ylab("Strength")
dev.print(jpeg, "RPlot_LMM strength by sex and season_BOXPLOT.jpeg", res=800, height=4, width=6, units="in") 

##########################################################################################################
##########################################################################################################



######################################################################
##                                                                  ##
##   MODELS FOR WHOLE DAY NETWORK MEASURES: eig, closseness & CC    ##
##                                                                  ##
######################################################################

# Data frame for WHOLE DAY strength, eigenvector, closeness and clustering coefficient (CC) = 'centrality'
str(centrality)
# Take subset of core animals only, in surveys where animals associated non-randomly
centrality_core <- subset(centrality, prefavoided==1 & SocialStatus!="NA" & Core==1)
# response variables:
eig.norm
closeness.norm 
cc


#============================
##  EIGENVECTOR CENTRALITY
#============================
str(centrality_core)

# CHECK DISTRIBUTION OF DATA
#----------------------------
library(fitdistrplus)
# Cullen & Frey graph:
descdist(centrality_core$eig, discrete = FALSE, boot=5000) # distrib is uniform (square)

# Plot data against normal
fit.norm <- fitdist(centrality_core$eig, "norm")
fit.lnorm <- fitdist(centrality_core$eig, "lnorm")
fit.gam <- fitdist(centrality_core$eig, "gamma") 
plot(fit.norm)
plot(fit.lnorm)
plot(fit.gam)

require(MASS)
require(car)
qqp((centrality_core$eig), "norm", main="normal") # looks OK... a bit wiggly though
qqp((centrality_core$eig), "lnorm", main="lognormal") # bad! - stick with normal as think uniform=normal...?

#--------
# MODELS
#--------
library(lme4)
# LMM - old, with interactions: no Prands were significant
eig_mod_full  <- lmer(eig ~ Sex*fSeason + SocialStatus*fSeason +
                        (1|fTerritory) + (1|ShortCode),
                      data=centrality_core, REML=F, verbose=T,
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=1000000),
                                          check.conv.grad=.makeCC("warning",0.05)))
# new mod with no season and a sex*status interaction: no Prands were significant
eig_mod_red <- lmer(eig ~ Sex*SocialStatus +
                      (1|fTerritory) + (1|ShortCode),
                    data=centrality_core, REML=F, verbose=T,
                    control=lmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun=1000000),
                                        check.conv.grad=.makeCC("warning",0.05)))

# new mod with no interactions: no Prands were significant
eig_mod_red_noint <- lmer(eig ~ Sex + SocialStatus +
                            (1|fTerritory) + (1|ShortCode),
                          data=centrality_core, REML=F, verbose=T,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=1000000),
                                              check.conv.grad=.makeCC("warning",0.05)))

#= Check residuals of full model
plot(eig_mod_full) # OK
plot(fitted(eig_mod_full), resid(eig_mod_full))   
lines(smooth.spline(fitted(eig_mod_full), resid(eig_mod_full))) # relatively flat

# check ranefs
lattice::dotplot(ranef(eig_mod_full, condVar=TRUE)) # territory 1 and 6 quite diff to others. 

coef(summary(eig_mod_full))

#--------------------------------------------------------------
# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA
#--------------------------------------------------------------
library(sna)

# set up progress bar
perm <- 2000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrices to store the coefficient estimate from each randomised matrix
Intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm)
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
fSeason2.SocialStatusSub_perm <-rep(NA,perm)
fSeason3.SocialStatusSub_perm <-rep(NA,perm)
fSeason4.SocialStatusSub_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
  input_eig <- rbind(data.frame(eig = abs(T1spr_rand_eig[,i]), SeasonID="1", Territory="1", id=row.names(T1spr_rand_eig)), # if rownames won't work chec have run code above to rename the row names
                     data.frame(eig = abs(T1sum_rand_eig[,i]), SeasonID="2", Territory="1", id=row.names(T1sum_rand_eig)), 
                     data.frame(eig = abs(T1aut_rand_eig[,i]), SeasonID="3", Territory="1", id=row.names(T1aut_rand_eig)),
                     data.frame(eig = abs(T1win_rand_eig[,i]), SeasonID="4", Territory="1", id=row.names(T1win_rand_eig)),
                     
                     data.frame(eig = abs(T2spr_rand_eig[,i]), SeasonID="1", Territory="2", id=row.names(T2spr_rand_eig)),
                     # data.frame(eig = abs(T2sum_rand_eig[,i]), SeasonID="2", Territory="2", id=row.names(T2sum_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T2aut_rand_eig[,i]), SeasonID="3", Territory="2", id=row.names(T2aut_rand_eig)), 
                     data.frame(eig = abs(T2win_rand_eig[,i]), SeasonID="4", Territory="2", id=row.names(T2win_rand_eig)),
                     
                     data.frame(eig = abs(T3spr_rand_eig[,i]), SeasonID="1", Territory="3", id=row.names(T3spr_rand_eig)),
                     data.frame(eig = abs(T3sum_rand_eig[,i]), SeasonID="2", Territory="3", id=row.names(T3sum_rand_eig)),
                     data.frame(eig = abs(T3aut_rand_eig[,i]), SeasonID="3", Territory="3", id=row.names(T3aut_rand_eig)),
                     data.frame(eig = abs(T3win_rand_eig[,i]), SeasonID="4", Territory="3", id=row.names(T3win_rand_eig)),
                     
                     # data.frame(eig = abs(T4spr_rand_eig[,i]), SeasonID="1", Territory="4", id=row.names(T4spr_rand_eig)), # no prefavoided
                     # data.frame(eig = abs(T4sum_rand_eig[,i]), SeasonID="2", Territory="4", id=row.names(T4sum_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T4aut_rand_eig[,i]), SeasonID="3", Territory="4", id=row.names(T4aut_rand_eig)),
                     data.frame(eig = abs(T4win_rand_eig[,i]), SeasonID="4", Territory="4", id=row.names(T4win_rand_eig)),
                     
                     data.frame(eig = abs(T5spr_rand_eig[,i]), SeasonID="1", Territory="5", id=row.names(T5spr_rand_eig)),
                     data.frame(eig = abs(T5sum_rand_eig[,i]), SeasonID="2", Territory="5", id=row.names(T5sum_rand_eig)),
                     data.frame(eig = abs(T5aut_rand_eig[,i]), SeasonID="3", Territory="5", id=row.names(T5aut_rand_eig)),
                     # data.frame(eig = T5win_rand_eig[,i]), SeasonID="4", Territory="5", id=row.names(T5win_rand_eig)), # no prefavoided
                     
                     data.frame(eig = abs(T6spr_rand_eig[,i]), SeasonID="1", Territory="6", id=row.names(T6spr_rand_eig)), 
                     data.frame(eig = abs(T6sum_rand_eig[,i]), SeasonID="2", Territory="6", id=row.names(T6sum_rand_eig)), 
                     data.frame(eig = abs(T6aut_rand_eig[,i]), SeasonID="3", Territory="6", id=row.names(T6aut_rand_eig)), 
                     data.frame(eig = abs(T6win_rand_eig[,i]), SeasonID="4", Territory="6", id=row.names(T6win_rand_eig)),
                     
                     data.frame(eig = abs(T7spr_rand_eig[,i]), SeasonID="1", Territory="7", id=row.names(T7spr_rand_eig)),
                     data.frame(eig = abs(T7sum_rand_eig[,i]), SeasonID="2", Territory="7", id=row.names(T7sum_rand_eig)), 
                     data.frame(eig = abs(T7aut_rand_eig[,i]), SeasonID="3", Territory="7", id=row.names(T7aut_rand_eig)),
                     data.frame(eig = abs(T7win_rand_eig[,i]), SeasonID="4", Territory="7", id=row.names(T7win_rand_eig)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_eig$id        <- as.character(input_eig$id)
  input_eig$SeasonID  <- as.character(input_eig$SeasonID)
  input_eig$Territory <- as.character(input_eig$Territory)
  # Add attributes to input_eig FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_eig$ShortCode    <- attr_unique[match(input_eig$id, attr_unique$id),4] 
  input_eig$Sex    <- attr_unique[match(input_eig$id, attr_unique$id),5]
  input_eig$SocialStatus <- attr_unique[match(input_eig$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_eig, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$SocialStatus <- factor(df$SocialStatus)
  df$Sex <- factor(df$Sex)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <- lmer(eig ~ Sex*fSeason + SocialStatus*fSeason + (1|fTerritory) + (1|ShortCode),
                    data=subset(df, SocialStatus!="NA" & Core==1), REML=F, 
                    control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  Intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SocialStatusSub_perm[i] <- a[6,1]
  SexM.fSeason2_perm[i] <- a[7,1]
  SexM.fSeason3_perm[i] <- a[8,1]
  SexM.fSeason4_perm[i] <- a[9,1]
  fSeason2.SocialStatusSub_perm[i] <- a[10,1]
  fSeason3.SocialStatusSub_perm[i] <- a[11,1]
  fSeason4.SocialStatusSub_perm[i] <- a[12,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod <-coef(summary(eig_mod_full))
Intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
fSeason2_obs <- realmod[3,1]
fSeason3_obs <- realmod[4,1]
fSeason4_obs <- realmod[5,1]
SocialStatusSub_obs <- realmod[6,1]
SexM.fSeason2_obs <- realmod[7,1]
SexM.fSeason3_obs <- realmod[8,1]
SexM.fSeason4_obs <- realmod[9,1]
fSeason2.SocialStatusSub_obs <- realmod[10,1]
fSeason3.SocialStatusSub_obs <- realmod[11,1]
fSeason4.SocialStatusSub_obs <- realmod[12,1]

# calculate p-values
Intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
fSeason2.SocialStatusSub_Pvalue <- sum(fSeason2.SocialStatusSub_perm>fSeason2.SocialStatusSub_obs)/2000
fSeason3.SocialStatusSub_Pvalue <- sum(fSeason3.SocialStatusSub_perm>fSeason3.SocialStatusSub_obs)/2000
fSeason4.SocialStatusSub_Pvalue <- sum(fSeason4.SocialStatusSub_perm>fSeason4.SocialStatusSub_obs)/2000

# Save in table
eig_fullmod_prands <- rbind(intercept_Pvalue,
                            SexM_Pvalue,
                            fSeason2_Pvalue,
                            fSeason3_Pvalue,
                            fSeason4_Pvalue,
                            SocialStatusSub_Pvalue,
                            SexM.fSeason2_Pvalue,
                            SexM.fSeason3_Pvalue,
                            SexM.fSeason4_Pvalue,
                            fSeason2.SocialStatusSub_Pvalue,
                            fSeason3.SocialStatusSub_Pvalue,
                            fSeason4.SocialStatusSub_Pvalue) # NONE ARE SIGNIFICANT



### Also tried fitting models without season, with just sex+status and (sex*status) (-> none were significant)

# Make storage matrices
Intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)

# Run loop
for(i in 1:2000){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
  input_eig <- rbind(data.frame(eig = abs(T1spr_rand_eig[,i]), SeasonID="1", Territory="1", id=row.names(T1spr_rand_eig)), # if rownames won't work chec have run code above to rename the row names
                     data.frame(eig = abs(T1sum_rand_eig[,i]), SeasonID="2", Territory="1", id=row.names(T1sum_rand_eig)), 
                     data.frame(eig = abs(T1aut_rand_eig[,i]), SeasonID="3", Territory="1", id=row.names(T1aut_rand_eig)),
                     data.frame(eig = abs(T1win_rand_eig[,i]), SeasonID="4", Territory="1", id=row.names(T1win_rand_eig)),
                     
                     data.frame(eig = abs(T2spr_rand_eig[,i]), SeasonID="1", Territory="2", id=row.names(T2spr_rand_eig)),
                     # data.frame(eig = abs(T2sum_rand_eig[,i]), SeasonID="2", Territory="2", id=row.names(T2sum_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T2aut_rand_eig[,i]), SeasonID="3", Territory="2", id=row.names(T2aut_rand_eig)), 
                     data.frame(eig = abs(T2win_rand_eig[,i]), SeasonID="4", Territory="2", id=row.names(T2win_rand_eig)),
                     
                     data.frame(eig = abs(T3spr_rand_eig[,i]), SeasonID="1", Territory="3", id=row.names(T3spr_rand_eig)),
                     data.frame(eig = abs(T3sum_rand_eig[,i]), SeasonID="2", Territory="3", id=row.names(T3sum_rand_eig)),
                     data.frame(eig = abs(T3aut_rand_eig[,i]), SeasonID="3", Territory="3", id=row.names(T3aut_rand_eig)),
                     data.frame(eig = abs(T3win_rand_eig[,i]), SeasonID="4", Territory="3", id=row.names(T3win_rand_eig)),
                     
                     # data.frame(eig = abs(T4spr_rand_eig[,i]), SeasonID="1", Territory="4", id=row.names(T4spr_rand_eig)), # no prefavoided
                     # data.frame(eig = abs(T4sum_rand_eig[,i]), SeasonID="2", Territory="4", id=row.names(T4sum_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T4aut_rand_eig[,i]), SeasonID="3", Territory="4", id=row.names(T4aut_rand_eig)),
                     data.frame(eig = abs(T4win_rand_eig[,i]), SeasonID="4", Territory="4", id=row.names(T4win_rand_eig)),
                     
                     data.frame(eig = abs(T5spr_rand_eig[,i]), SeasonID="1", Territory="5", id=row.names(T5spr_rand_eig)),
                     data.frame(eig = abs(T5sum_rand_eig[,i]), SeasonID="2", Territory="5", id=row.names(T5sum_rand_eig)),
                     data.frame(eig = abs(T5aut_rand_eig[,i]), SeasonID="3", Territory="5", id=row.names(T5aut_rand_eig)),
                     # data.frame(eig = T5win_rand_eig[,i]), SeasonID="4", Territory="5", id=row.names(T5win_rand_eig)), # no prefavoided
                     
                     data.frame(eig = abs(T6spr_rand_eig[,i]), SeasonID="1", Territory="6", id=row.names(T6spr_rand_eig)), 
                     data.frame(eig = abs(T6sum_rand_eig[,i]), SeasonID="2", Territory="6", id=row.names(T6sum_rand_eig)), 
                     data.frame(eig = abs(T6aut_rand_eig[,i]), SeasonID="3", Territory="6", id=row.names(T6aut_rand_eig)), 
                     data.frame(eig = abs(T6win_rand_eig[,i]), SeasonID="4", Territory="6", id=row.names(T6win_rand_eig)),
                     
                     data.frame(eig = abs(T7spr_rand_eig[,i]), SeasonID="1", Territory="7", id=row.names(T7spr_rand_eig)),
                     data.frame(eig = abs(T7sum_rand_eig[,i]), SeasonID="2", Territory="7", id=row.names(T7sum_rand_eig)), 
                     data.frame(eig = abs(T7aut_rand_eig[,i]), SeasonID="3", Territory="7", id=row.names(T7aut_rand_eig)),
                     data.frame(eig = abs(T7win_rand_eig[,i]), SeasonID="4", Territory="7", id=row.names(T7win_rand_eig)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_eig$id        <- as.character(input_eig$id)
  input_eig$SeasonID  <- as.character(input_eig$SeasonID)
  input_eig$Territory <- as.character(input_eig$Territory)
  # Add attributes to input_eig FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_eig$ShortCode    <- attr_unique[match(input_eig$id, attr_unique$id),4] 
  input_eig$Sex    <- attr_unique[match(input_eig$id, attr_unique$id),5]
  input_eig$SocialStatus <- attr_unique[match(input_eig$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_eig, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$SocialStatus <- factor(df$SocialStatus)
  df$Sex <- factor(df$Sex)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <- lmer(eig ~ Sex + SocialStatus + (1|fTerritory) + (1|ShortCode),
                    data=subset(df, SocialStatus!="NA" & Core==1), REML=F, 
                    control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  Intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  SocialStatusSub_perm[i] <- a[3,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod <-coef(summary(eig_mod_red_noint))
Intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
SocialStatusSub_obs <- realmod[3,1]

# calculate p-values
Intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
# Save in table
eig_redmod_prands <- rbind(intercept_Pvalue,
                           SexM_Pvalue,
                           SocialStatusSub_Pvalue) 
# CONCLUSION: SEX AND STATUS DO NOT EXPLAIN NETWORK POSITION AS DESCRIBED BY EIGENVECTOR CENTRALITY


##########################################################################################################
##########################################################################################################


#=========================================#
##  CLOSENESS CENTRALITY (weighted)
#=========================================#

# Take subset of core animals only, in surveys where animals associated non-randomly
centrality_core <- subset(centrality, prefavoided==1 & SocialStatus!="NA" & Core==1)

# response variable: closeness (not normalised)


# CHECK DISTRIBUTION OF DATA
#-----------------------------
library(fitdistrplus)
# Cullen & Frey graph:
descdist(centrality_core$closeness, discrete = FALSE, boot=5000) # lognormal or gamma
hist(centrality_core$closeness) # right-skewed
hist(log10(centrality_core$closeness)) # log10 transformation helps a lot!

# Plot data against normal, lognormal and gamma...
fit.norm <- fitdist(centrality_core$closeness, "norm")
fit.lnorm <- fitdist(centrality_core$closeness, "lnorm") 
fit.gam <- fitdist(centrality_core$closeness, "gamma") 
plot(fit.norm) 
plot(fit.lnorm)
plot(fit.gam)
# lognormal looks best

require(MASS)
require(car)
qqp((centrality_core$closeness), "norm", main="normal") 
qqp((centrality_core$closeness), "lnorm", main="lognormal") 
gamma <- fitdistr(centrality_core$closeness, "gamma", start=NULL)
qqp(centrality_core$closeness, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma") # gamma is poor 
# lognormal looks best again

#--------
# MODELS
#--------

# Lognormal LMMs
# season interactions - no significant Prands
clo_mod_full  <- lmer(log10(closeness) ~ Sex*fSeason + SocialStatus*fSeason +
                        (1|fTerritory) + (1|ShortCode),
                      data=centrality_core, REML=F, verbose=T,
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=1000000),
                                          check.conv.grad=.makeCC("warning",0.05)))

# Lognormal LMM with sex*status interaction - no significant Prands
clo_mod_red_ssint  <- lmer(log10(closeness) ~ Sex*SocialStatus +
                             (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F, verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))

# Lognormal LMM without season - no significant Prands
clo_mod_red  <- lmer(log10(closeness) ~ Sex + SocialStatus +
                       (1|fTerritory) + (1|ShortCode),
                     data=centrality_core, REML=F, verbose=T,
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=1000000),
                                         check.conv.grad=.makeCC("warning",0.05)))

#= Check residuals:
plot(clo_mod_full) # OK
plot(fitted(clo_mod_full), resid(clo_mod_full))   
lines(smooth.spline(fitted(clo_mod_full), resid(clo_mod_full))) # GOOD!
hist(resid(clo_mod_full))
# check ranefs
lattice::dotplot(ranef(clo_mod_full, condVar=TRUE)) # short code does not account for any variation.

summary(clo_mod_full)

# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA: Sex*fSeason + SocialStatus*fSeason
#------------------------------------------------------------------------------------------------
library(sna)

# set up progress bar
perm <- 2000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrices to store the coefficient estimates from each randomised matrix
intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm) 
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
fSeason2.SocialStatusSub_perm <-rep(NA,perm)
fSeason3.SocialStatusSub_perm <-rep(NA,perm)
fSeason4.SocialStatusSub_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_clo <- rbind(data.frame(closeness = T1spr_rand_closeness[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_closeness)),
                     data.frame(closeness = T1sum_rand_closeness[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_closeness)), 
                     data.frame(closeness = T1aut_rand_closeness[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_closeness)),
                     data.frame(closeness = T1win_rand_closeness[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_closeness)),
                     
                     data.frame(closeness = T2spr_rand_closeness[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_closeness)),
                     #data.frame(closeness = T2sum_rand_closeness[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_closeness)), # no pref/avoided assocs
                     data.frame(closeness = T2aut_rand_closeness[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_closeness)), 
                     data.frame(closeness = T2win_rand_closeness[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_closeness)),
                     
                     data.frame(closeness = T3spr_rand_closeness[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_closeness)),
                     data.frame(closeness = T3sum_rand_closeness[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_closeness)),
                     data.frame(closeness = T3aut_rand_closeness[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_closeness)),
                     data.frame(closeness = T3win_rand_closeness[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_closeness)),
                     
                     #data.frame(closeness = T4spr_rand_closeness[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_closeness)), # no pref/avoided assocs
                     #data.frame(closeness = T4sum_rand_closeness[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_closeness)), # no pref/avoided assocs
                     data.frame(closeness = T4aut_rand_closeness[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_closeness)),
                     data.frame(closeness = T4win_rand_closeness[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_closeness)),
                     
                     data.frame(closeness = T5spr_rand_closeness[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_closeness)),
                     data.frame(closeness = T5sum_rand_closeness[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_closeness)),
                     data.frame(closeness = T5aut_rand_closeness[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_closeness)),
                     #data.frame(closeness = T5win_rand_closeness[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_closeness)), # no pref/avoided assocs
                     
                     data.frame(closeness = T6spr_rand_closeness[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_closeness)), 
                     data.frame(closeness = T6sum_rand_closeness[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_closeness)), 
                     data.frame(closeness = T6aut_rand_closeness[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_closeness)), 
                     data.frame(closeness = T6win_rand_closeness[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_closeness)),
                     
                     data.frame(closeness = T7spr_rand_closeness[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_closeness)),
                     data.frame(closeness = T7sum_rand_closeness[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_closeness)), 
                     data.frame(closeness = T7aut_rand_closeness[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_closeness)),
                     data.frame(closeness = T7win_rand_closeness[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_closeness)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_clo$id        <- as.character(input_clo$id)
  input_clo$SeasonID  <- as.character(input_clo$SeasonID)
  input_clo$Territory <- as.character(input_clo$Territory)
  # Add attributes to input_clo FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_clo$ShortCode    <- attr_unique[match(input_clo$id, attr_unique$id),4] 
  input_clo$Sex    <- attr_unique[match(input_clo$id, attr_unique$id),5] 
  input_clo$SocialStatus <- attr_unique[match(input_clo$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_clo, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  # run the model
  model_tmp <- lmer(log10(closeness) ~ Sex*fSeason + SocialStatus*fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SocialStatusSub_perm[i] <- a[6,1]
  SexM.fSeason2_perm[i] <- a[7,1]
  SexM.fSeason3_perm[i] <- a[8,1]
  SexM.fSeason4_perm[i] <- a[9,1]
  fSeason2.SocialStatusSub_perm[i] <- a[10,1]
  fSeason3.SocialStatusSub_perm[i] <- a[11,1]
  fSeason4.SocialStatusSub_perm[i] <- a[12,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod_clo <- coef(summary(clo_mod_full))

intercept_obs <- realmod_clo[1,1]
SexM_obs <- realmod_clo[2,1]
fSeason2_obs <- realmod_clo[3,1]
fSeason3_obs <- realmod_clo[4,1]
fSeason4_obs <- realmod_clo[5,1]
SocialStatusSub_obs <- realmod_clo[6,1]
SexM.fSeason2_obs <- realmod_clo[7,1]
SexM.fSeason3_obs <- realmod_clo[8,1]
SexM.fSeason4_obs <- realmod_clo[9,1]
fSeason2.SocialStatusSub_obs <- realmod_clo[10,1]
fSeason3.SocialStatusSub_obs <- realmod_clo[11,1]
fSeason4.SocialStatusSub_obs <- realmod_clo[12,1]

intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
fSeason2.SocialStatusSub_Pvalue <- sum(fSeason2.SocialStatusSub_perm>fSeason2.SocialStatusSub_obs)/2000
fSeason3.SocialStatusSub_Pvalue <- sum(fSeason3.SocialStatusSub_perm>fSeason3.SocialStatusSub_obs)/2000
fSeason4.SocialStatusSub_Pvalue <- sum(fSeason4.SocialStatusSub_perm>fSeason4.SocialStatusSub_obs)/2000

clo_full_prands <- rbind(intercept_Pvalue,
                         SexM_Pvalue,
                         fSeason2_Pvalue,
                         fSeason3_Pvalue,
                         fSeason4_Pvalue,
                         SocialStatusSub_Pvalue,
                         SexM.fSeason2_Pvalue,
                         SexM.fSeason3_Pvalue,
                         SexM.fSeason4_Pvalue,
                         fSeason2.SocialStatusSub_Pvalue,
                         fSeason3.SocialStatusSub_Pvalue,
                         fSeason4.SocialStatusSub_Pvalue) 

#-------------

##### Testing real vs random for simpler model without *fSeason interactions:

# Make 1D storage matrices to store the coefficient estimates from each randomised matrix
intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm) 
SocialStatusSub_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_clo <- rbind(data.frame(closeness = T1spr_rand_closeness[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_closeness)),
                     data.frame(closeness = T1sum_rand_closeness[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_closeness)), 
                     data.frame(closeness = T1aut_rand_closeness[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_closeness)),
                     data.frame(closeness = T1win_rand_closeness[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_closeness)),
                     
                     data.frame(closeness = T2spr_rand_closeness[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_closeness)),
                     #data.frame(closeness = T2sum_rand_closeness[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_closeness)), # no pref/avoided assocs
                     data.frame(closeness = T2aut_rand_closeness[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_closeness)), 
                     data.frame(closeness = T2win_rand_closeness[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_closeness)),
                     
                     data.frame(closeness = T3spr_rand_closeness[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_closeness)),
                     data.frame(closeness = T3sum_rand_closeness[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_closeness)),
                     data.frame(closeness = T3aut_rand_closeness[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_closeness)),
                     data.frame(closeness = T3win_rand_closeness[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_closeness)),
                     
                     #data.frame(closeness = T4spr_rand_closeness[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_closeness)), # no pref/avoided assocs
                     #data.frame(closeness = T4sum_rand_closeness[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_closeness)), # no pref/avoided assocs
                     data.frame(closeness = T4aut_rand_closeness[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_closeness)),
                     data.frame(closeness = T4win_rand_closeness[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_closeness)),
                     
                     data.frame(closeness = T5spr_rand_closeness[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_closeness)),
                     data.frame(closeness = T5sum_rand_closeness[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_closeness)),
                     data.frame(closeness = T5aut_rand_closeness[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_closeness)),
                     #data.frame(closeness = T5win_rand_closeness[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_closeness)), # no pref/avoided assocs
                     
                     data.frame(closeness = T6spr_rand_closeness[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_closeness)), 
                     data.frame(closeness = T6sum_rand_closeness[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_closeness)), 
                     data.frame(closeness = T6aut_rand_closeness[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_closeness)), 
                     data.frame(closeness = T6win_rand_closeness[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_closeness)),
                     
                     data.frame(closeness = T7spr_rand_closeness[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_closeness)),
                     data.frame(closeness = T7sum_rand_closeness[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_closeness)), 
                     data.frame(closeness = T7aut_rand_closeness[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_closeness)),
                     data.frame(closeness = T7win_rand_closeness[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_closeness)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_clo$id        <- as.character(input_clo$id)
  input_clo$SeasonID  <- as.character(input_clo$SeasonID)
  input_clo$Territory <- as.character(input_clo$Territory)
  # Add attributes to input_clo FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_clo$ShortCode    <- attr_unique[match(input_clo$id, attr_unique$id),4] 
  input_clo$Sex    <- attr_unique[match(input_clo$id, attr_unique$id),5] 
  input_clo$SocialStatus <- attr_unique[match(input_clo$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_clo, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  # run the model
  model_tmp <- lmer(log10(closeness) ~ Sex + SocialStatus + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  SocialStatusSub_perm[i] <- a[3,1]
  setTxtProgressBar(pb, i)
}

# Estimate from the observed (real) data
realmod_clo <- coef(summary(clo_mod_red))
intercept_obs <- realmod_clo[1,1]
SexM_obs <- realmod_clo[2,1]
SocialStatusSub_obs <- realmod_clo[3,1]

intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000

clo_red_prands <- rbind(intercept_Pvalue,
                        SexM_Pvalue,
                        SocialStatusSub_Pvalue)

##########################################################################################################
##########################################################################################################

#============================#
##  CLUSTERING COEFFICIENT
#============================#

# Take subset of core animals only, in surveys where animals associated non-randomly
centrality_core <- subset(centrality, prefavoided==1 & SocialStatus!="NA" & Core==1)

# response variable: cc


# CHECK DISTRIBUTION OF DATA
#-----------------------------
hist(centrality_core$cc)
require(MASS)
require(car)
qqp((centrality_core$cc), "norm", main="normal") # approx normal


# MODELS
#--------

# LMM sex*season and status*season
cc_mod_full  <- lmer(cc ~ Sex*fSeason + SocialStatus*fSeason + (1|fTerritory) + (1|ShortCode),
                     data=centrality_core, REML=F, verbose=T,
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=1000000),
                                         check.conv.grad=.makeCC("warning",0.05)))
cc_full_prands # Prands for observed coefficients compared to 2000 randomisations - for cc_mod_full - see code below.

# just sex*season + status (as status*season not significantly diff from random)
cc_mod_red  <- lmer(cc ~ Sex*fSeason + SocialStatus + (1|fTerritory) + (1|ShortCode),
                    data=centrality_core, REML=F, verbose=T,
                    control=lmerControl(optimizer="bobyqa",
                                        optCtrl = list(maxfun=1000000),
                                        check.conv.grad=.makeCC("warning",0.05)))
cc_red_prands # Prands for observed coefficients compared to 2000 randomisations - for cc_mod_red - see code below.

# just sex*season (as status not significantly diff from random)
cc_mod_red_nostatus  <- lmer(cc ~ Sex*fSeason + (1|fTerritory) + (1|ShortCode),
                             data=centrality_core, REML=F, verbose=T,
                             control=lmerControl(optimizer="bobyqa",
                                                 optCtrl = list(maxfun=1000000),
                                                 check.conv.grad=.makeCC("warning",0.05)))
cc_nostatus_prands # Prands for observed coefficients compared to 2000 randomisations - for cc_mod_red_nostatus - see code below.
# sex and sex(male)*season(autumn) are significant. See post-hoc tests below.

#= Check residuals of final model
plot(cc_mod_red_nostatus) # OK
plot(fitted(cc_mod_red_nostatus), resid(cc_mod_red_nostatus))   
lines(smooth.spline(fitted(cc_mod_red_nostatus), resid(cc_mod_red_nostatus))) # GOOD!
# check ranefs
lattice::dotplot(ranef(cc_mod_red_nostatus, condVar=TRUE)) # territory 1 and 6 quite diff to others. 

# SAVE MODEL COEFFICIENTS 
#---------------------------
cc_mod_coeffs <- coef(summary(cc_mod_red_nostatus))  # save as DF
ranef(cc_mod_red_nostatus) # to see order of random effects: ShortCode is first (sig01), then Territory
cc_mod_cis <- confint(cc_mod_red_nostatus)

# POST HOC Q: In which seasons does CC differ between the sexes?
#-----------------------------------------------------------------
cc_mod_contrasts <- data.frame(summary(lsmeans::lsmeans(cc_mod_red_nostatus, pairwise~Sex|fSeason))$contrasts) # test diffs between M/F in each season

# PLOT
cc_mod_lsm <- data.frame(summary(lsmeans::lsmeans(cc_mod_red_nostatus, pairwise~Sex|fSeason))$lsmeans)
cc_mod_lsm$Season <- ordered(cc_mod_lsm$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
cc_mod_lsm$Sex <- factor(cc_mod_lsm$Sex, levels(cc_mod_lsm$Sex)[c(2,1)]) # re-order sex
cc_rawdata <- centrality_core # make copy of raw data to plot in background
cc_rawdata$Sex <- factor(cc_rawdata$Sex,levels(cc_rawdata$Sex)[c(2,1)])
cc_rawdata$Season <- ordered(cc_rawdata$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

library(ggplot2)
ggp <- ggplot(cc_mod_lsm, aes(x=Season, y=lsmean, group=Sex, colour=factor(Sex))) 
ggp + geom_point(size=4, aes(group=Sex), position=position_dodge(0.3)) + 
  xlab("") + ylab("Clustering coefficient\n") +
  geom_errorbar(mapping=aes(x=Season, ymin=lower.CL, ymax=upper.CL),
                width=0.3, size=1.2, position=position_dodge(0.3)) +
  scale_colour_manual(name="Sex", values=c('royalblue3','firebrick1')) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black", size=2),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  # overlay raw data to please Innes
  geom_point(data=cc_rawdata, aes(Season, cc, colour=Sex), 
             position=position_dodge(0.3)) 
# save in high res.
dev.print(jpeg, "RPlot_LMM lsmeans & CIs for clustering coefficient by season_withrawdata.jpeg", res=700, height=6, width=8, units="in") 


# boxplot of raw data
# lines on boxplots are medians and show diff trend to means from raw data and lsmeans from model
centrality_core$Season <- ordered(centrality_core$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))
centrality_core$Sex <- factor(centrality_core$Sex, levels(centrality_core$Sex)[c(2,1)]) # re-order sex
ggp <- ggplot(centrality_core, aes(x=Sex, y=cc, group=Sex, fill=factor(Sex))) 
ggp + geom_boxplot() +
  facet_grid(~Season) +
  scale_fill_manual(name="Sex", values=c('royalblue3','firebrick1'),guide=FALSE) +
  theme_bw(base_size = 16, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black", size=2),
        panel.border = element_rect(linetype = "solid", colour = "black"),
        strip.text.x = element_text(face="bold"),
        panel.border = element_rect(linetype = "solid", colour = "black")) +
  xlab("") + ylab("Clustering coefficient")

dev.print(jpeg, "RPlot_Clustering coefficient sex-season BOXPLOTS.jpeg", res=900, height=6, width=8, units="in") 


# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA - cc_mod_full (sex*season + status*season)
#------------------------------------------------------------
perm <- 2000

# Make 1D storage matrices 
intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm) 
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatusSub_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
fSeason2.SocialStatusSub_perm <-rep(NA,perm)
fSeason3.SocialStatusSub_perm <-rep(NA,perm)
fSeason4.SocialStatusSub_perm <-rep(NA,perm)

# Run loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_CC <- rbind (data.frame(cc = T1spr_cc_randmat[,i], SeasonID="1", Territory="1", id=row.names(T1spr_cc_randmat)),
                     data.frame(cc = T1sum_cc_randmat[,i], SeasonID="2", Territory="1", id=row.names(T1sum_cc_randmat)), 
                     data.frame(cc = T1aut_cc_randmat[,i], SeasonID="3", Territory="1", id=row.names(T1aut_cc_randmat)),
                     data.frame(cc = T1win_cc_randmat[,i], SeasonID="4", Territory="1", id=row.names(T1win_cc_randmat)),
                     
                     data.frame(cc = T2spr_cc_randmat[,i], SeasonID="1", Territory="2", id=row.names(T2spr_cc_randmat)),
                     #data.frame(cc = T2sum_cc_randmat[,i], SeasonID="2", Territory="2", id=row.names(T2sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T2aut_cc_randmat[,i], SeasonID="3", Territory="2", id=row.names(T2aut_cc_randmat)), 
                     data.frame(cc = T2win_cc_randmat[,i], SeasonID="4", Territory="2", id=row.names(T2win_cc_randmat)),
                     
                     data.frame(cc = T3spr_cc_randmat[,i], SeasonID="1", Territory="3", id=row.names(T3spr_cc_randmat)),
                     data.frame(cc = T3sum_cc_randmat[,i], SeasonID="2", Territory="3", id=row.names(T3sum_cc_randmat)),
                     data.frame(cc = T3aut_cc_randmat[,i], SeasonID="3", Territory="3", id=row.names(T3aut_cc_randmat)),
                     data.frame(cc = T3win_cc_randmat[,i], SeasonID="4", Territory="3", id=row.names(T3win_cc_randmat)),
                     
                     #data.frame(cc = T4spr_cc_randmat[,i], SeasonID="1", Territory="4", id=row.names(T4spr_cc_randmat)), # no pref/avoided assocs
                     #data.frame(cc = T4sum_cc_randmat[,i], SeasonID="2", Territory="4", id=row.names(T4sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4aut_cc_randmat[,i], SeasonID="3", Territory="4", id=row.names(T4aut_cc_randmat)),
                     data.frame(cc = T4win_cc_randmat[,i], SeasonID="4", Territory="4", id=row.names(T4win_cc_randmat)),
                     
                     data.frame(cc = T5spr_cc_randmat[,i], SeasonID="1", Territory="5", id=row.names(T5spr_cc_randmat)),
                     data.frame(cc = T5sum_cc_randmat[,i], SeasonID="2", Territory="5", id=row.names(T5sum_cc_randmat)),
                     data.frame(cc = T5aut_cc_randmat[,i], SeasonID="3", Territory="5", id=row.names(T5aut_cc_randmat)),
                     #data.frame(cc = T5win_cc_randmat[,i], SeasonID="4", Territory="5", id=row.names(T5win_cc_randmat)), # no pref/avoided assocs
                     
                     data.frame(cc = T6spr_cc_randmat[,i], SeasonID="1", Territory="6", id=row.names(T6spr_cc_randmat)), 
                     data.frame(cc = T6sum_cc_randmat[,i], SeasonID="2", Territory="6", id=row.names(T6sum_cc_randmat)), 
                     data.frame(cc = T6aut_cc_randmat[,i], SeasonID="3", Territory="6", id=row.names(T6aut_cc_randmat)), 
                     data.frame(cc = T6win_cc_randmat[,i], SeasonID="4", Territory="6", id=row.names(T6win_cc_randmat)),
                     
                     data.frame(cc = T7spr_cc_randmat[,i], SeasonID="1", Territory="7", id=row.names(T7spr_cc_randmat)),
                     data.frame(cc = T7sum_cc_randmat[,i], SeasonID="2", Territory="7", id=row.names(T7sum_cc_randmat)), 
                     data.frame(cc = T7aut_cc_randmat[,i], SeasonID="3", Territory="7", id=row.names(T7aut_cc_randmat)),
                     data.frame(cc = T7win_cc_randmat[,i], SeasonID="4", Territory="7", id=row.names(T7win_cc_randmat)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_CC$id        <- as.character(input_CC$id)
  input_CC$SeasonID  <- as.character(input_CC$SeasonID)
  input_CC$Territory <- as.character(input_CC$Territory)
  # Add attributes to input_CC FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_CC$ShortCode    <- attr_unique[match(input_CC$id, attr_unique$id),4] 
  input_CC$Sex    <- attr_unique[match(input_CC$id, attr_unique$id),5] 
  input_CC$SocialStatus <- attr_unique[match(input_CC$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_CC, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  # run the model
  model_tmp <- lmer(cc ~ Sex*fSeason + SocialStatus*fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SocialStatusSub_perm[i] <- a[6,1]
  SexM.fSeason2_perm[i] <- a[7,1]
  SexM.fSeason3_perm[i] <- a[8,1]
  SexM.fSeason4_perm[i] <- a[9,1]
  fSeason2.SocialStatusSub_perm[i] <- a[10,1]
  fSeason3.SocialStatusSub_perm[i] <- a[11,1]
  fSeason4.SocialStatusSub_perm[i] <- a[12,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod <- coef(summary(cc_mod_full))
intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
fSeason2_obs <- realmod[3,1]
fSeason3_obs <- realmod[4,1]
fSeason4_obs <- realmod[5,1]
SocialStatusSub_obs <- realmod[6,1]
SexM.fSeason2_obs <- realmod[7,1]
SexM.fSeason3_obs <- realmod[8,1]
SexM.fSeason4_obs <- realmod[9,1]
fSeason2.SocialStatusSub_obs <- realmod[10,1]
fSeason3.SocialStatusSub_obs <- realmod[11,1]
fSeason4.SocialStatusSub_obs <- realmod[12,1]

intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatusSub_Pvalue <- sum(SocialStatusSub_perm>SocialStatusSub_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
fSeason2.SocialStatusSub_Pvalue <- sum(fSeason2.SocialStatusSub_perm>fSeason2.SocialStatusSub_obs)/2000
fSeason3.SocialStatusSub_Pvalue <- sum(fSeason3.SocialStatusSub_perm>fSeason3.SocialStatusSub_obs)/2000
fSeason4.SocialStatusSub_Pvalue <- sum(fSeason4.SocialStatusSub_perm>fSeason4.SocialStatusSub_obs)/2000

cc_full_prands <- rbind(intercept_Pvalue,
                        SexM_Pvalue,
                        fSeason2_Pvalue,
                        fSeason3_Pvalue,
                        fSeason4_Pvalue,
                        SocialStatusSub_Pvalue,
                        SexM.fSeason2_Pvalue,
                        SexM.fSeason3_Pvalue,
                        SexM.fSeason4_Pvalue,
                        fSeason2.SocialStatusSub_Pvalue,
                        fSeason3.SocialStatusSub_Pvalue,
                        fSeason4.SocialStatusSub_Pvalue)  
# significant interaction between sex and season?
#-------------------------------------------------
# Re-fit model with Sex*fSeason + SocialStatus:

# Make storage matrices 
intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm) 
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SocialStatus_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)

# Loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_CC <- rbind (data.frame(cc = T1spr_cc_randmat[,i], SeasonID="1", Territory="1", id=row.names(T1spr_cc_randmat)),
                     data.frame(cc = T1sum_cc_randmat[,i], SeasonID="2", Territory="1", id=row.names(T1sum_cc_randmat)), 
                     data.frame(cc = T1aut_cc_randmat[,i], SeasonID="3", Territory="1", id=row.names(T1aut_cc_randmat)),
                     data.frame(cc = T1win_cc_randmat[,i], SeasonID="4", Territory="1", id=row.names(T1win_cc_randmat)),
                     
                     data.frame(cc = T2spr_cc_randmat[,i], SeasonID="1", Territory="2", id=row.names(T2spr_cc_randmat)),
                     #data.frame(cc = T2sum_cc_randmat[,i], SeasonID="2", Territory="2", id=row.names(T2sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T2aut_cc_randmat[,i], SeasonID="3", Territory="2", id=row.names(T2aut_cc_randmat)), 
                     data.frame(cc = T2win_cc_randmat[,i], SeasonID="4", Territory="2", id=row.names(T2win_cc_randmat)),
                     
                     data.frame(cc = T3spr_cc_randmat[,i], SeasonID="1", Territory="3", id=row.names(T3spr_cc_randmat)),
                     data.frame(cc = T3sum_cc_randmat[,i], SeasonID="2", Territory="3", id=row.names(T3sum_cc_randmat)),
                     data.frame(cc = T3aut_cc_randmat[,i], SeasonID="3", Territory="3", id=row.names(T3aut_cc_randmat)),
                     data.frame(cc = T3win_cc_randmat[,i], SeasonID="4", Territory="3", id=row.names(T3win_cc_randmat)),
                     
                     #data.frame(cc = T4spr_cc_randmat[,i], SeasonID="1", Territory="4", id=row.names(T4spr_cc_randmat)), # no pref/avoided assocs
                     #data.frame(cc = T4sum_cc_randmat[,i], SeasonID="2", Territory="4", id=row.names(T4sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4aut_cc_randmat[,i], SeasonID="3", Territory="4", id=row.names(T4aut_cc_randmat)),
                     data.frame(cc = T4win_cc_randmat[,i], SeasonID="4", Territory="4", id=row.names(T4win_cc_randmat)),
                     
                     data.frame(cc = T5spr_cc_randmat[,i], SeasonID="1", Territory="5", id=row.names(T5spr_cc_randmat)),
                     data.frame(cc = T5sum_cc_randmat[,i], SeasonID="2", Territory="5", id=row.names(T5sum_cc_randmat)),
                     data.frame(cc = T5aut_cc_randmat[,i], SeasonID="3", Territory="5", id=row.names(T5aut_cc_randmat)),
                     #data.frame(cc = T5win_cc_randmat[,i], SeasonID="4", Territory="5", id=row.names(T5win_cc_randmat)), # no pref/avoided assocs
                     
                     data.frame(cc = T6spr_cc_randmat[,i], SeasonID="1", Territory="6", id=row.names(T6spr_cc_randmat)), 
                     data.frame(cc = T6sum_cc_randmat[,i], SeasonID="2", Territory="6", id=row.names(T6sum_cc_randmat)), 
                     data.frame(cc = T6aut_cc_randmat[,i], SeasonID="3", Territory="6", id=row.names(T6aut_cc_randmat)), 
                     data.frame(cc = T6win_cc_randmat[,i], SeasonID="4", Territory="6", id=row.names(T6win_cc_randmat)),
                     
                     data.frame(cc = T7spr_cc_randmat[,i], SeasonID="1", Territory="7", id=row.names(T7spr_cc_randmat)),
                     data.frame(cc = T7sum_cc_randmat[,i], SeasonID="2", Territory="7", id=row.names(T7sum_cc_randmat)), 
                     data.frame(cc = T7aut_cc_randmat[,i], SeasonID="3", Territory="7", id=row.names(T7aut_cc_randmat)),
                     data.frame(cc = T7win_cc_randmat[,i], SeasonID="4", Territory="7", id=row.names(T7win_cc_randmat)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_CC$id        <- as.character(input_CC$id)
  input_CC$SeasonID  <- as.character(input_CC$SeasonID)
  input_CC$Territory <- as.character(input_CC$Territory)
  # Add attributes to input_CC FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_CC$ShortCode    <- attr_unique[match(input_CC$id, attr_unique$id),4] 
  input_CC$Sex    <- attr_unique[match(input_CC$id, attr_unique$id),5] 
  input_CC$SocialStatus <- attr_unique[match(input_CC$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_CC, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  # run the model
  model_tmp <- lmer(cc ~ Sex*fSeason + SocialStatus + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SocialStatus_perm[i] <- a[6,1]
  SexM.fSeason2_perm[i] <- a[7,1]
  SexM.fSeason3_perm[i] <- a[8,1]
  SexM.fSeason4_perm[i] <- a[9,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
realmod <- coef(summary(cc_mod_red))
intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
fSeason2_obs <- realmod[3,1]
fSeason3_obs <- realmod[4,1]
fSeason4_obs <- realmod[5,1]
SocialStatus_obs <- realmod[6,1]
SexM.fSeason2_obs <- realmod[7,1]
SexM.fSeason3_obs <- realmod[8,1]
SexM.fSeason4_obs <- realmod[9,1]

# P-values
intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SocialStatus_Pvalue <-sum(SocialStatus_perm>SocialStatus_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000

# save in df
cc_red_prands <- a<-rbind(intercept_Pvalue,
                          SexM_Pvalue,
                          fSeason2_Pvalue,
                          fSeason3_Pvalue,
                          fSeason4_Pvalue,
                          SocialStatus_Pvalue,
                          SexM.fSeason2_Pvalue,
                          SexM.fSeason3_Pvalue,
                          SexM.fSeason4_Pvalue) # sex and sex(male)*season(autumn) are significant!

# Histogram of random coefficients with red line for real
hist(CC_seas.aov_perm,breaks=100, main=paste("P = ", sum(CC_seas.aov_perm>CC_seas.aov_obs)/2000),
     xlab="Effect of season on clustering coefficient", ylab="Probability") 
abline(v=CC_seas.aov_obs, col="red")

# significant interaction between sex and season but no sig effect of status
#-------------------------------------------------
# Re-fit model with Sex*fSeason and run post-hoc at same time

# Make storage matrices
# for model coefficients:
intercept_perm <-rep(NA,perm)
SexM_perm <-rep(NA,perm) 
fSeason2_perm <-rep(NA,perm)
fSeason3_perm <-rep(NA,perm)
fSeason4_perm <-rep(NA,perm)
SexM.fSeason2_perm <-rep(NA,perm)
SexM.fSeason3_perm <-rep(NA,perm)
SexM.fSeason4_perm <-rep(NA,perm)
# for post-hoc tests - compare lsmeans contrast estimate
lsm_F12_perm <-rep(NA,perm)
lsm_F13_perm <-rep(NA,perm)
lsm_F14_perm <-rep(NA,perm)
lsm_F23_perm <-rep(NA,perm)
lsm_F24_perm <-rep(NA,perm)
lsm_F34_perm <-rep(NA,perm)
lsm_M12_perm <-rep(NA,perm)
lsm_M13_perm <-rep(NA,perm)
lsm_M14_perm <-rep(NA,perm)
lsm_M23_perm <-rep(NA,perm)
lsm_M24_perm <-rep(NA,perm)
lsm_M34_perm <-rep(NA,perm)
lsm_FMspr_perm <-rep(NA,perm)
lsm_FMsum_perm <-rep(NA,perm)
lsm_FMaut_perm <-rep(NA,perm)
lsm_FMwin_perm <-rep(NA,perm)

# Loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_CC <- rbind (data.frame(cc = T1spr_cc_randmat[,i], SeasonID="1", Territory="1", id=row.names(T1spr_cc_randmat)),
                     data.frame(cc = T1sum_cc_randmat[,i], SeasonID="2", Territory="1", id=row.names(T1sum_cc_randmat)), 
                     data.frame(cc = T1aut_cc_randmat[,i], SeasonID="3", Territory="1", id=row.names(T1aut_cc_randmat)),
                     data.frame(cc = T1win_cc_randmat[,i], SeasonID="4", Territory="1", id=row.names(T1win_cc_randmat)),
                     
                     data.frame(cc = T2spr_cc_randmat[,i], SeasonID="1", Territory="2", id=row.names(T2spr_cc_randmat)),
                     #data.frame(cc = T2sum_cc_randmat[,i], SeasonID="2", Territory="2", id=row.names(T2sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T2aut_cc_randmat[,i], SeasonID="3", Territory="2", id=row.names(T2aut_cc_randmat)), 
                     data.frame(cc = T2win_cc_randmat[,i], SeasonID="4", Territory="2", id=row.names(T2win_cc_randmat)),
                     
                     data.frame(cc = T3spr_cc_randmat[,i], SeasonID="1", Territory="3", id=row.names(T3spr_cc_randmat)),
                     data.frame(cc = T3sum_cc_randmat[,i], SeasonID="2", Territory="3", id=row.names(T3sum_cc_randmat)),
                     data.frame(cc = T3aut_cc_randmat[,i], SeasonID="3", Territory="3", id=row.names(T3aut_cc_randmat)),
                     data.frame(cc = T3win_cc_randmat[,i], SeasonID="4", Territory="3", id=row.names(T3win_cc_randmat)),
                     
                     #data.frame(cc = T4spr_cc_randmat[,i], SeasonID="1", Territory="4", id=row.names(T4spr_cc_randmat)), # no pref/avoided assocs
                     #data.frame(cc = T4sum_cc_randmat[,i], SeasonID="2", Territory="4", id=row.names(T4sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4aut_cc_randmat[,i], SeasonID="3", Territory="4", id=row.names(T4aut_cc_randmat)),
                     data.frame(cc = T4win_cc_randmat[,i], SeasonID="4", Territory="4", id=row.names(T4win_cc_randmat)),
                     
                     data.frame(cc = T5spr_cc_randmat[,i], SeasonID="1", Territory="5", id=row.names(T5spr_cc_randmat)),
                     data.frame(cc = T5sum_cc_randmat[,i], SeasonID="2", Territory="5", id=row.names(T5sum_cc_randmat)),
                     data.frame(cc = T5aut_cc_randmat[,i], SeasonID="3", Territory="5", id=row.names(T5aut_cc_randmat)),
                     #data.frame(cc = T5win_cc_randmat[,i], SeasonID="4", Territory="5", id=row.names(T5win_cc_randmat)), # no pref/avoided assocs
                     
                     data.frame(cc = T6spr_cc_randmat[,i], SeasonID="1", Territory="6", id=row.names(T6spr_cc_randmat)), 
                     data.frame(cc = T6sum_cc_randmat[,i], SeasonID="2", Territory="6", id=row.names(T6sum_cc_randmat)), 
                     data.frame(cc = T6aut_cc_randmat[,i], SeasonID="3", Territory="6", id=row.names(T6aut_cc_randmat)), 
                     data.frame(cc = T6win_cc_randmat[,i], SeasonID="4", Territory="6", id=row.names(T6win_cc_randmat)),
                     
                     data.frame(cc = T7spr_cc_randmat[,i], SeasonID="1", Territory="7", id=row.names(T7spr_cc_randmat)),
                     data.frame(cc = T7sum_cc_randmat[,i], SeasonID="2", Territory="7", id=row.names(T7sum_cc_randmat)), 
                     data.frame(cc = T7aut_cc_randmat[,i], SeasonID="3", Territory="7", id=row.names(T7aut_cc_randmat)),
                     data.frame(cc = T7win_cc_randmat[,i], SeasonID="4", Territory="7", id=row.names(T7win_cc_randmat)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_CC$id        <- as.character(input_CC$id)
  input_CC$SeasonID  <- as.character(input_CC$SeasonID)
  input_CC$Territory <- as.character(input_CC$Territory)
  # Add attributes to input_CC FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_CC$ShortCode    <- attr_unique[match(input_CC$id, attr_unique$id),4] 
  input_CC$Sex <- attr_unique[match(input_CC$id, attr_unique$id),5]
  input_CC$SocialStatus <- attr_unique[match(input_CC$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_CC, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$Sex <- factor(df$Sex)
  df$SocialStatus <- factor(df$SocialStatus)
  # run the model
  model_tmp <- lmer(cc ~ Sex*fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  a <-coef(summary(model_tmp))
  intercept_perm[i] <- a[1,1]
  SexM_perm[i] <- a[2,1]
  fSeason2_perm[i] <- a[3,1]
  fSeason3_perm[i] <- a[4,1]
  fSeason4_perm[i] <- a[5,1]
  SexM.fSeason2_perm[i] <- a[6,1]
  SexM.fSeason3_perm[i] <- a[7,1]
  SexM.fSeason4_perm[i] <- a[8,1]
  # Run post-hoc comparisons and save estimated difference/effect size of contrast:
  # test diffs between M/F in each season
  lsm_temp <- data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~Sex|fSeason))$contrasts) 
  lsm_FMspr_perm[i] <- lsm_temp[1,3]
  lsm_FMsum_perm[i] <- lsm_temp[2,3]
  lsm_FMaut_perm[i] <- lsm_temp[3,3]
  lsm_FMwin_perm[i] <- lsm_temp[4,3]
  # compare rates between seasons for each sex
  lsm_temp2 <- data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason|Sex))$contrasts)
  lsm_F12_perm[i] <- lsm_temp2[1,3]
  lsm_F13_perm[i] <- lsm_temp2[2,3]
  lsm_F14_perm[i] <- lsm_temp2[3,3]
  lsm_F23_perm[i] <- lsm_temp2[4,3]
  lsm_F24_perm[i] <- lsm_temp2[5,3]
  lsm_F34_perm[i] <- lsm_temp2[6,3]
  lsm_M12_perm[i] <- lsm_temp2[7,3]
  lsm_M13_perm[i] <- lsm_temp2[8,3]
  lsm_M14_perm[i] <- lsm_temp2[9,3]
  lsm_M23_perm[i] <- lsm_temp2[10,3]
  lsm_M24_perm[i] <- lsm_temp2[11,3]
  lsm_M34_perm[i] <- lsm_temp2[12,3]
  setTxtProgressBar(pb, i)
}

# model coefficients from the observed (real) data
realmod <- coef(summary(cc_mod_red_nostatus))
intercept_obs <- realmod[1,1]
SexM_obs <- realmod[2,1]
fSeason2_obs <- realmod[3,1]
fSeason3_obs <- realmod[4,1]
fSeason4_obs <- realmod[5,1]
SexM.fSeason2_obs <- realmod[6,1]
SexM.fSeason3_obs <- realmod[7,1]
SexM.fSeason4_obs <- realmod[8,1]

#lsmeans estimates from observed (real) data
# compare sexes within seasons
cc_mod_contrasts <- data.frame(summary(lsmeans::lsmeans(cc_mod_red_nostatus, pairwise~Sex|fSeason))$contrasts) # test diffs between M/F in each season
lsm_FMspr_obs <- cc_mod_contrasts[1,3]
lsm_FMsum_obs <- cc_mod_contrasts[2,3]
lsm_FMaut_obs <- cc_mod_contrasts[3,3]
lsm_FMwin_obs <- cc_mod_contrasts[4,3]
# compare seasons within sexes
cc_mod_contrasts2 <- data.frame(summary(lsmeans::lsmeans(cc_mod_red_nostatus, pairwise~fSeason|Sex))$contrasts) # test diffs between M/F in each season
lsm_F12_obs  <- cc_mod_contrasts2[1,3]
lsm_F13_obs  <- cc_mod_contrasts2[2,3]
lsm_F14_obs  <- cc_mod_contrasts2[3,3]
lsm_F23_obs  <- cc_mod_contrasts2[4,3]
lsm_F24_obs  <- cc_mod_contrasts2[5,3]
lsm_F34_obs  <- cc_mod_contrasts2[6,3]
lsm_M12_obs  <- cc_mod_contrasts2[7,3]
lsm_M13_obs  <- cc_mod_contrasts2[8,3]
lsm_M14_obs  <- cc_mod_contrasts2[9,3]
lsm_M23_obs  <- cc_mod_contrasts2[10,3]
lsm_M24_obs  <- cc_mod_contrasts2[11,3]
lsm_M34_obs  <- cc_mod_contrasts2[12,3]

# model coefficient p-values
intercept_Pvalue <- sum(intercept_perm>intercept_obs)/2000
SexM_Pvalue <- sum(SexM_perm>SexM_obs)/2000
fSeason2_Pvalue <- sum(fSeason2_perm>fSeason2_obs)/2000
fSeason3_Pvalue <- sum(fSeason3_perm>fSeason3_obs)/2000
fSeason4_Pvalue <- sum(fSeason4_perm>fSeason4_obs)/2000
SexM.fSeason2_Pvalue <- sum(SexM.fSeason2_perm>SexM.fSeason2_obs)/2000
SexM.fSeason3_Pvalue <- sum(SexM.fSeason3_perm>SexM.fSeason3_obs)/2000
SexM.fSeason4_Pvalue <- sum(SexM.fSeason4_perm>SexM.fSeason4_obs)/2000
# lsmeans p-values
lsm_FMspr_Pvalue <- sum(lsm_FMspr_perm>lsm_FMspr_obs)/2000 
lsm_FMsum_Pvalue <- sum(lsm_FMsum_perm>lsm_FMsum_obs)/2000 
lsm_FMaut_Pvalue <- sum(lsm_FMaut_perm>lsm_FMaut_obs)/2000 
lsm_FMwin_Pvalue <- sum(lsm_FMwin_perm>lsm_FMwin_obs)/2000 
lsm_F12_Pvalue <- sum(lsm_F12_perm>lsm_F12_obs)/2000 
lsm_F13_Pvalue <- sum(lsm_F13_perm>lsm_F13_obs)/2000 
lsm_F14_Pvalue <- sum(lsm_F14_perm>lsm_F14_obs)/2000 
lsm_F23_Pvalue <- sum(lsm_F23_perm>lsm_F23_obs)/2000 
lsm_F24_Pvalue <- sum(lsm_F24_perm>lsm_F24_obs)/2000 
lsm_F34_Pvalue <- sum(lsm_F34_perm>lsm_F34_obs)/2000 
lsm_M12_Pvalue <- sum(lsm_M12_perm>lsm_M12_obs)/2000 
lsm_M13_Pvalue <- sum(lsm_M13_perm>lsm_M13_obs)/2000 
lsm_M14_Pvalue <- sum(lsm_M14_perm>lsm_M14_obs)/2000 
lsm_M23_Pvalue <- sum(lsm_M23_perm>lsm_M23_obs)/2000 
lsm_M24_Pvalue <- sum(lsm_M24_perm>lsm_M24_obs)/2000 
lsm_M34_Pvalue <- sum(lsm_M34_perm>lsm_M34_obs)/2000

# save in df
cc_nostatus_prands <- a<-rbind(intercept_Pvalue,
                               SexM_Pvalue,
                               fSeason2_Pvalue,
                               fSeason3_Pvalue,
                               fSeason4_Pvalue,
                               SexM.fSeason2_Pvalue,
                               SexM.fSeason3_Pvalue,
                               SexM.fSeason4_Pvalue) 

cc_lsm_Prands <- rbind(lsm_FMspr_Pvalue, 
                       lsm_FMsum_Pvalue, 
                       lsm_FMaut_Pvalue, 
                       lsm_FMwin_Pvalue,
                       lsm_F12_Pvalue,
                       lsm_F13_Pvalue,
                       lsm_F14_Pvalue, 
                       lsm_F23_Pvalue,
                       lsm_F24_Pvalue,
                       lsm_F34_Pvalue,
                       lsm_M12_Pvalue, 
                       lsm_M13_Pvalue,
                       lsm_M14_Pvalue,
                       lsm_M23_Pvalue,
                       lsm_M24_Pvalue,
                       lsm_M34_Pvalue)

# adjust p-values for multiple testing
a<-c("0.015", "0.0205", "0.0095", "0.1405") # convert two-tailed Ps to low numbers (1-p)
p.adjust(a, meth="holm") # list the adjusted p-values

# Histograms of random coefficients with red line for real
par(mfrow=c(2,2))
hist(lsm_FMspr_perm,breaks=100, main=paste("P = ", lsm_FMspr_Pvalue),
     xlab="Spring", ylab="Probability", cex.lab=1.2) 
abline(v=lsm_FMspr_obs, col="red")

hist(lsm_FMsum_perm,breaks=100, main=paste("P = ", lsm_FMsum_Pvalue),
     xlab="Summer", ylab="Probability", cex.lab=1.2)  
abline(v=lsm_FMsum_obs, col="red")

hist(lsm_FMaut_perm,breaks=100, main=paste("P = ", lsm_FMaut_Pvalue),
     xlab="Autumn", ylab="Probability", cex.lab=1.2) 
abline(v=lsm_FMaut_obs, col="red")

hist(lsm_FMwin_perm,breaks=100, main=paste("P = ", lsm_FMwin_Pvalue),
     xlab="Winter", ylab="Probability", cex.lab=1.2)  
abline(v=lsm_FMwin_obs, col="red")
dev.print(jpeg, "Rplot_Hist clustering coeff Prands for lsm contrast between F-M in each season.jpeg", res=700, height=20, width=20, units="cm") # save as jpeg

################################################################################################################################


# Repeatability of individual network position between seasons
#--------------------------------------------------------------
# Now need to compare to random:
perm<-2000
pb <- txtProgressBar(0,perm,0,style=3)

# Loop for strength:
strength_icc_perm <- rep(NA,perm)
for(i in 1:perm){ 
  input_strength <- rbind(
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(strength=T1spr_rand_strength[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_strength)), # if rownames won't work chec have run code above to rename the row names
    data.frame(strength=T1sum_rand_strength[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_strength)), 
    data.frame(strength=T1aut_rand_strength[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_strength)),
    data.frame(strength=T1win_rand_strength[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_strength)),
    data.frame(strength= T2spr_rand_strength[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_strength)),
    data.frame(strength= T2sum_rand_strength[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_strength)), # no prefavoided
    data.frame(strength= T2aut_rand_strength[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_strength)),
    data.frame(strength= T2win_rand_strength[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_strength)),
    data.frame(strength= T3spr_rand_strength[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_strength)),
    data.frame(strength= T3sum_rand_strength[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_strength)),
    data.frame(strength= T3aut_rand_strength[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_strength)),
    data.frame(strength= T3win_rand_strength[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_strength)),
    data.frame(strength= T4spr_rand_strength[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_strength)), # no prefavoided
    data.frame(strength= T4sum_rand_strength[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_strength)), # no prefavoided
    data.frame(strength= T4aut_rand_strength[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_strength)),
    data.frame(strength= T4win_rand_strength[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_strength)),
    data.frame(strength= T5spr_rand_strength[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_strength)),
    data.frame(strength= T5sum_rand_strength[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_strength)),
    data.frame(strength= T5aut_rand_strength[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_strength)),
    data.frame(strength= T5win_rand_strength[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_strength)), # no prefavoided
    data.frame(strength= T6spr_rand_strength[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_strength)), 
    data.frame(strength= T6sum_rand_strength[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_strength)), 
    data.frame(strength= T6aut_rand_strength[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_strength)), 
    data.frame(strength= T6win_rand_strength[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_strength)),
    data.frame(strength= T7spr_rand_strength[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_strength)),
    data.frame(strength= T7sum_rand_strength[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_strength)), 
    data.frame(strength= T7aut_rand_strength[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_strength)),
    data.frame(strength= T7win_rand_strength[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_strength)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_strength$id <- as.character(input_strength$id)
  input_strength$SeasonID <- as.character(input_strength$SeasonID)
  input_strength$Territory <- as.character(input_strength$Territory)
  # Add attributes to input_strength
  input_strength$ShortCode <- attr_unique[match(input_strength$id, attr_unique$id),4] 
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_strength, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  # take subset of core only
  dat <-subset(df, Core==1)
  # get ICCs
  null_tmp <- lmer(strength ~ 1 + (1|ShortCode) + (1|fTerritory), data=dat)
  a<-data.frame(print(VarCorr(null_tmp), comp=c("Variance","Std.Dev.")))
  # ICC = variance explained by shortcode (individual ID) 
  strength_icc_perm[i] <- a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance
  # update prog bar
  setTxtProgressBar(pb, i)
}

# real ICC for strength
dataaa <- subset(centrality, Core==1)
null_strength <- lmer(strength ~ 1 + (1|ShortCode) + (1|fTerritory), data=dataaa)
a<-data.frame(print(VarCorr(null_strength), comp=c("Variance","Std.Dev.")))
# ICC = variance explained by shortcode (individual ID) 
strength_icc_obs <- a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance

# calc p-value
sum(strength_icc_perm>strength_icc_obs)/2000 

# mean perm ICC
mean(strength_icc_perm)

#---------------

# Loop for eig centrality:
eig_icc_perm <- rep(NA,perm)
for(i in 1:perm){ 
  input_eig <- rbind(data.frame(eig = abs(T1spr_rand_eig[,i]), SeasonID="1", Territory="1", id=row.names(T1spr_rand_eig)), # if rownames won't work chec have run code above to rename the row names
                     data.frame(eig = abs(T1sum_rand_eig[,i]), SeasonID="2", Territory="1", id=row.names(T1sum_rand_eig)), 
                     data.frame(eig = abs(T1aut_rand_eig[,i]), SeasonID="3", Territory="1", id=row.names(T1aut_rand_eig)),
                     data.frame(eig = abs(T1win_rand_eig[,i]), SeasonID="4", Territory="1", id=row.names(T1win_rand_eig)),
                     data.frame(eig = abs(T2spr_rand_eig[,i]), SeasonID="1", Territory="2", id=row.names(T2spr_rand_eig)),
                     data.frame(eig = abs(T2sum_rand_eig[,i]), SeasonID="2", Territory="2", id=row.names(T2sum_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T2aut_rand_eig[,i]), SeasonID="3", Territory="2", id=row.names(T2aut_rand_eig)), 
                     data.frame(eig = abs(T2win_rand_eig[,i]), SeasonID="4", Territory="2", id=row.names(T2win_rand_eig)),
                     data.frame(eig = abs(T3spr_rand_eig[,i]), SeasonID="1", Territory="3", id=row.names(T3spr_rand_eig)),
                     data.frame(eig = abs(T3sum_rand_eig[,i]), SeasonID="2", Territory="3", id=row.names(T3sum_rand_eig)),
                     data.frame(eig = abs(T3aut_rand_eig[,i]), SeasonID="3", Territory="3", id=row.names(T3aut_rand_eig)),
                     data.frame(eig = abs(T3win_rand_eig[,i]), SeasonID="4", Territory="3", id=row.names(T3win_rand_eig)),
                     data.frame(eig = abs(T4spr_rand_eig[,i]), SeasonID="1", Territory="4", id=row.names(T4spr_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T4sum_rand_eig[,i]), SeasonID="2", Territory="4", id=row.names(T4sum_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T4aut_rand_eig[,i]), SeasonID="3", Territory="4", id=row.names(T4aut_rand_eig)),
                     data.frame(eig = abs(T4win_rand_eig[,i]), SeasonID="4", Territory="4", id=row.names(T4win_rand_eig)),
                     data.frame(eig = abs(T5spr_rand_eig[,i]), SeasonID="1", Territory="5", id=row.names(T5spr_rand_eig)),
                     data.frame(eig = abs(T5sum_rand_eig[,i]), SeasonID="2", Territory="5", id=row.names(T5sum_rand_eig)),
                     data.frame(eig = abs(T5aut_rand_eig[,i]), SeasonID="3", Territory="5", id=row.names(T5aut_rand_eig)),
                     data.frame(eig = abs(T5win_rand_eig[,i]), SeasonID="4", Territory="5", id=row.names(T5win_rand_eig)), # no prefavoided
                     data.frame(eig = abs(T6spr_rand_eig[,i]), SeasonID="1", Territory="6", id=row.names(T6spr_rand_eig)), 
                     data.frame(eig = abs(T6sum_rand_eig[,i]), SeasonID="2", Territory="6", id=row.names(T6sum_rand_eig)), 
                     data.frame(eig = abs(T6aut_rand_eig[,i]), SeasonID="3", Territory="6", id=row.names(T6aut_rand_eig)), 
                     data.frame(eig = abs(T6win_rand_eig[,i]), SeasonID="4", Territory="6", id=row.names(T6win_rand_eig)),
                     data.frame(eig = abs(T7spr_rand_eig[,i]), SeasonID="1", Territory="7", id=row.names(T7spr_rand_eig)),
                     data.frame(eig = abs(T7sum_rand_eig[,i]), SeasonID="2", Territory="7", id=row.names(T7sum_rand_eig)), 
                     data.frame(eig = abs(T7aut_rand_eig[,i]), SeasonID="3", Territory="7", id=row.names(T7aut_rand_eig)),
                     data.frame(eig = abs(T7win_rand_eig[,i]), SeasonID="4", Territory="7", id=row.names(T7win_rand_eig)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_eig$id        <- as.character(input_eig$id)
  input_eig$SeasonID  <- as.character(input_eig$SeasonID)
  input_eig$Territory <- as.character(input_eig$Territory)
  # Add attributes to input_eig
  input_eig$ShortCode    <- attr_unique[match(input_eig$id, attr_unique$id),4] 
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_eig, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  # take subset of core only
  dat <-subset(df, Core==1)
  # get ICCs
  null_tmp <- lmer(eig ~ 1 + (1|ShortCode) + (1|fTerritory), data=dat)
  a<-data.frame(print(VarCorr(null_tmp), comp=c("Variance","Std.Dev.")))
  # ICC = variance explained by shortcode (individual ID) 
  eig_icc_perm[i] <- a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance
  # update prog bar
  setTxtProgressBar(pb, i)
}

# real ICC for eig
dataaa <- subset(centrality, Core==1)
null_eig <- lmer(eig ~ 1 + (1|ShortCode) + (1|fTerritory), data=dataaa)
a<-data.frame(print(VarCorr(null_eig), comp=c("Variance","Std.Dev.")))
# ICC = variance explained by shortcode (individual ID) 
eig_icc_obs <- a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance

# calc p-value
sum(eig_icc_perm>eig_icc_obs)/2000 

# mean perm ICC
mean(eig_icc_perm)

#---------------
# Loop for Clustering coefficient:
cc_icc_perm <- rep(NA,perm)
for(i in 1:perm){ 
  input_CC <- rbind (data.frame(cc = T1spr_cc_randmat[,i], SeasonID="1", Territory="1", id=row.names(T1spr_cc_randmat)),
                     data.frame(cc = T1sum_cc_randmat[,i], SeasonID="2", Territory="1", id=row.names(T1sum_cc_randmat)), 
                     data.frame(cc = T1aut_cc_randmat[,i], SeasonID="3", Territory="1", id=row.names(T1aut_cc_randmat)),
                     data.frame(cc = T1win_cc_randmat[,i], SeasonID="4", Territory="1", id=row.names(T1win_cc_randmat)),
                     data.frame(cc = T2spr_cc_randmat[,i], SeasonID="1", Territory="2", id=row.names(T2spr_cc_randmat)),
                     data.frame(cc = T2sum_cc_randmat[,i], SeasonID="2", Territory="2", id=row.names(T2sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T2aut_cc_randmat[,i], SeasonID="3", Territory="2", id=row.names(T2aut_cc_randmat)), 
                     data.frame(cc = T2win_cc_randmat[,i], SeasonID="4", Territory="2", id=row.names(T2win_cc_randmat)),
                     data.frame(cc = T3spr_cc_randmat[,i], SeasonID="1", Territory="3", id=row.names(T3spr_cc_randmat)),
                     data.frame(cc = T3sum_cc_randmat[,i], SeasonID="2", Territory="3", id=row.names(T3sum_cc_randmat)),
                     data.frame(cc = T3aut_cc_randmat[,i], SeasonID="3", Territory="3", id=row.names(T3aut_cc_randmat)),
                     data.frame(cc = T3win_cc_randmat[,i], SeasonID="4", Territory="3", id=row.names(T3win_cc_randmat)),
                     data.frame(cc = T4spr_cc_randmat[,i], SeasonID="1", Territory="4", id=row.names(T4spr_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4sum_cc_randmat[,i], SeasonID="2", Territory="4", id=row.names(T4sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4aut_cc_randmat[,i], SeasonID="3", Territory="4", id=row.names(T4aut_cc_randmat)),
                     data.frame(cc = T4win_cc_randmat[,i], SeasonID="4", Territory="4", id=row.names(T4win_cc_randmat)),
                     data.frame(cc = T5spr_cc_randmat[,i], SeasonID="1", Territory="5", id=row.names(T5spr_cc_randmat)),
                     data.frame(cc = T5sum_cc_randmat[,i], SeasonID="2", Territory="5", id=row.names(T5sum_cc_randmat)),
                     data.frame(cc = T5aut_cc_randmat[,i], SeasonID="3", Territory="5", id=row.names(T5aut_cc_randmat)),
                     data.frame(cc = T5win_cc_randmat[,i], SeasonID="4", Territory="5", id=row.names(T5win_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T6spr_cc_randmat[,i], SeasonID="1", Territory="6", id=row.names(T6spr_cc_randmat)), 
                     data.frame(cc = T6sum_cc_randmat[,i], SeasonID="2", Territory="6", id=row.names(T6sum_cc_randmat)), 
                     data.frame(cc = T6aut_cc_randmat[,i], SeasonID="3", Territory="6", id=row.names(T6aut_cc_randmat)), 
                     data.frame(cc = T6win_cc_randmat[,i], SeasonID="4", Territory="6", id=row.names(T6win_cc_randmat)),
                     data.frame(cc = T7spr_cc_randmat[,i], SeasonID="1", Territory="7", id=row.names(T7spr_cc_randmat)),
                     data.frame(cc = T7sum_cc_randmat[,i], SeasonID="2", Territory="7", id=row.names(T7sum_cc_randmat)), 
                     data.frame(cc = T7aut_cc_randmat[,i], SeasonID="3", Territory="7", id=row.names(T7aut_cc_randmat)),
                     data.frame(cc = T7win_cc_randmat[,i], SeasonID="4", Territory="7", id=row.names(T7win_cc_randmat)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_CC$id        <- as.character(input_CC$id)
  input_CC$SeasonID  <- as.character(input_CC$SeasonID)
  input_CC$Territory <- as.character(input_CC$Territory)
  # Add attributes to input_CC FROM ATTRIBUTES_CHAR DATA WITH 2 ROWS PER FOX/SEASON/TERRITORY COMBO
  input_CC$ShortCode    <- attr_unique[match(input_CC$id, attr_unique$id),4] 
  input_CC$Sex <- attr_unique[match(input_CC$id, attr_unique$id),5]
  input_CC$SocialStatus <- attr_unique[match(input_CC$id, attr_unique$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_CC, y=attr_unique[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  # take subset of core only
  dat <-subset(df, Core==1)
  # get ICCs
  null_tmp <- lmer(cc ~ 1 + (1|ShortCode) + (1|fTerritory), data=dat)
  a<-data.frame(print(VarCorr(null_tmp), comp=c("Variance","Std.Dev.")))
  # ICC = variance explained by shortcode (individual ID) 
  cc_icc_perm[i] <- a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance
  # update prog bar
  setTxtProgressBar(pb, i)
}

# real ICC for cc (including all networks, not just pref/avoided)
dataaa <- subset(centrality, Core==1)
null_cc <- lmer(cc ~ 1 + (1|ShortCode) + (1|fTerritory), data=dataaa)
a<-data.frame(print(VarCorr(null_cc), comp=c("Variance","Std.Dev.")))
cc_icc_obs <- a[1,4,]/sum(a$vcov) # ShortCode variance divided by total variance

# calc p-value
sum(cc_icc_perm>cc_icc_obs)/2000 

# mean perm ICC
mean(cc_icc_perm)



#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#
## GLOBAL NETWORK MEASURES TO COMPARE BETWEEN SEASONS: DENSITY, TRANSITIVITY AND MODULARITY
#-------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------#

library(sna)

# network density: sum of tie values divided by total possible ties (so density=mean edge weight when ignore.eval=F)
density_data <-rbind(
  data.frame(Territory=1, SeasonID=1, densityW=gden(T1spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T1spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=1, SeasonID=2, densityW=gden(T1sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T1sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=1, SeasonID=3, densityW=gden(T1aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T1aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=1, SeasonID=4, densityW=gden(T1win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T1win_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=2, SeasonID=1, densityW=gden(T2spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T2spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=2, SeasonID=2, densityW=gden(T2sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T2sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=2, SeasonID=3, densityW=gden(T2aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T2aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=2, SeasonID=4, densityW=gden(T2win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T2win_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=3, SeasonID=1, densityW=gden(T3spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T3spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=3, SeasonID=2, densityW=gden(T3sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T3sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=3, SeasonID=3, densityW=gden(T3aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T3aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=3, SeasonID=4, densityW=gden(T3win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T3win_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=4, SeasonID=1, densityW=gden(T4spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T4spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=4, SeasonID=2, densityW=gden(T4sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T4sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=4, SeasonID=3, densityW=gden(T4aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T4aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=4, SeasonID=4, densityW=gden(T4win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T4win_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=5, SeasonID=1, densityW=gden(T5spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T5spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=5, SeasonID=2, densityW=gden(T5sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T5sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=5, SeasonID=3, densityW=gden(T5aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T5aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=5, SeasonID=4, densityW=gden(T5win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T5win_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=6, SeasonID=1, densityW=gden(T6spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T6spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=6, SeasonID=2, densityW=gden(T6sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T6sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=6, SeasonID=3, densityW=gden(T6aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T6aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=6, SeasonID=4, densityW=gden(T6win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T6win_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=7, SeasonID=1, densityW=gden(T7spr_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T7spr_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=7, SeasonID=2, densityW=gden(T7sum_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T7sum_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=7, SeasonID=3, densityW=gden(T7aut_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T7aut_net, g=1, mode="graph", ignore.eval = T)),
  data.frame(Territory=7, SeasonID=4, densityW=gden(T7win_net, g=1, mode="graph", ignore.eval = F), densityUW=gden(T7win_net, g=1, mode="graph", ignore.eval = T)))

# Network transitivity
transitivity_data <-rbind(
  data.frame(Territory=1, SeasonID=1, transitivity=gtrans(T1spr_net, g=1, mode="graph")),
  data.frame(Territory=1, SeasonID=2, transitivity=gtrans(T1sum_net, g=1, mode="graph")),
  data.frame(Territory=1, SeasonID=3, transitivity=gtrans(T1aut_net, g=1, mode="graph")),
  data.frame(Territory=1, SeasonID=4, transitivity=gtrans(T1win_net, g=1, mode="graph")),
  data.frame(Territory=2, SeasonID=1, transitivity=gtrans(T2spr_net, g=1, mode="graph")),
  data.frame(Territory=2, SeasonID=2, transitivity=gtrans(T2sum_net, g=1, mode="graph")),
  data.frame(Territory=2, SeasonID=3, transitivity=gtrans(T2aut_net, g=1, mode="graph")),
  data.frame(Territory=2, SeasonID=4, transitivity=gtrans(T2win_net, g=1, mode="graph")),
  data.frame(Territory=3, SeasonID=1, transitivity=gtrans(T3spr_net, g=1, mode="graph")),
  data.frame(Territory=3, SeasonID=2, transitivity=gtrans(T3sum_net, g=1, mode="graph")),
  data.frame(Territory=3, SeasonID=3, transitivity=gtrans(T3aut_net, g=1, mode="graph")),
  data.frame(Territory=3, SeasonID=4, transitivity=gtrans(T3win_net, g=1, mode="graph")),
  data.frame(Territory=4, SeasonID=1, transitivity=gtrans(T4spr_net, g=1, mode="graph")),
  data.frame(Territory=4, SeasonID=2, transitivity=gtrans(T4sum_net, g=1, mode="graph")),
  data.frame(Territory=4, SeasonID=3, transitivity=gtrans(T4aut_net, g=1, mode="graph")),
  data.frame(Territory=4, SeasonID=4, transitivity=gtrans(T4win_net, g=1, mode="graph")),
  data.frame(Territory=5, SeasonID=1, transitivity=gtrans(T5spr_net, g=1, mode="graph")),
  data.frame(Territory=5, SeasonID=2, transitivity=gtrans(T5sum_net, g=1, mode="graph")),
  data.frame(Territory=5, SeasonID=3, transitivity=gtrans(T5aut_net, g=1, mode="graph")),
  data.frame(Territory=5, SeasonID=4, transitivity=gtrans(T5win_net, g=1, mode="graph")),
  data.frame(Territory=6, SeasonID=1, transitivity=gtrans(T6spr_net, g=1, mode="graph")),
  data.frame(Territory=6, SeasonID=2, transitivity=gtrans(T6sum_net, g=1, mode="graph")),
  data.frame(Territory=6, SeasonID=3, transitivity=gtrans(T6aut_net, g=1, mode="graph")),
  data.frame(Territory=6, SeasonID=4, transitivity=gtrans(T6win_net, g=1, mode="graph")),
  data.frame(Territory=7, SeasonID=1, transitivity=gtrans(T7spr_net, g=1, mode="graph")),
  data.frame(Territory=7, SeasonID=2, transitivity=gtrans(T7sum_net, g=1, mode="graph")),
  data.frame(Territory=7, SeasonID=3, transitivity=gtrans(T7aut_net, g=1, mode="graph")),
  data.frame(Territory=7, SeasonID=4, transitivity=gtrans(T7win_net, g=1, mode="graph")))

# Network modularity - via community detection in igraph
detach(package:sna)
library(igraph)
T1spr_net_graph <- graph_from_adjacency_matrix(T1spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T1sum_net_graph <- graph_from_adjacency_matrix(T1sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T1aut_net_graph <- graph_from_adjacency_matrix(T1aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T1win_net_graph <- graph_from_adjacency_matrix(T1win_net,mode="undirected",weighted=TRUE,diag=FALSE)
T2spr_net_graph <- graph_from_adjacency_matrix(T2spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T2sum_net_graph <- graph_from_adjacency_matrix(T2sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T2aut_net_graph <- graph_from_adjacency_matrix(T2aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T2win_net_graph <- graph_from_adjacency_matrix(T2win_net,mode="undirected",weighted=TRUE,diag=FALSE)
T3spr_net_graph <- graph_from_adjacency_matrix(T3spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T3sum_net_graph <- graph_from_adjacency_matrix(T3sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T3aut_net_graph <- graph_from_adjacency_matrix(T3aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T3win_net_graph <- graph_from_adjacency_matrix(T3win_net,mode="undirected",weighted=TRUE,diag=FALSE)
T4spr_net_graph <- graph_from_adjacency_matrix(T4spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T4sum_net_graph <- graph_from_adjacency_matrix(T4sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T4aut_net_graph <- graph_from_adjacency_matrix(T4aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T4win_net_graph <- graph_from_adjacency_matrix(T4win_net,mode="undirected",weighted=TRUE,diag=FALSE)
T5spr_net_graph <- graph_from_adjacency_matrix(T5spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T5sum_net_graph <- graph_from_adjacency_matrix(T5sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T5aut_net_graph <- graph_from_adjacency_matrix(T5aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T5win_net_graph <- graph_from_adjacency_matrix(T5win_net,mode="undirected",weighted=TRUE,diag=FALSE)
T6spr_net_graph <- graph_from_adjacency_matrix(T6spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T6sum_net_graph <- graph_from_adjacency_matrix(T6sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T6aut_net_graph <- graph_from_adjacency_matrix(T6aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T6win_net_graph <- graph_from_adjacency_matrix(T6win_net,mode="undirected",weighted=TRUE,diag=FALSE)
T7spr_net_graph <- graph_from_adjacency_matrix(T7spr_net,mode="undirected",weighted=TRUE,diag=FALSE)
T7sum_net_graph <- graph_from_adjacency_matrix(T7sum_net,mode="undirected",weighted=TRUE,diag=FALSE)
T7aut_net_graph <- graph_from_adjacency_matrix(T7aut_net,mode="undirected",weighted=TRUE,diag=FALSE)
T7win_net_graph <- graph_from_adjacency_matrix(T7win_net,mode="undirected",weighted=TRUE,diag=FALSE)

# Get communities using Newman method same as in SOCPROG
T1spr_com   <- cluster_leading_eigen(T1spr_net_graph) 
T1sum_com   <- cluster_leading_eigen(T1sum_net_graph)
T1aut_com   <- cluster_leading_eigen(T1aut_net_graph)
T1win_com   <- cluster_leading_eigen(T1win_net_graph)
T2spr_com   <- cluster_leading_eigen(T2spr_net_graph) 
T2sum_com   <- cluster_leading_eigen(T2sum_net_graph)
T2aut_com   <- cluster_leading_eigen(T2aut_net_graph)
T2win_com   <- cluster_leading_eigen(T2win_net_graph)
T3spr_com   <- cluster_leading_eigen(T3spr_net_graph) 
T3sum_com   <- cluster_leading_eigen(T3sum_net_graph)
T3aut_com   <- cluster_leading_eigen(T3aut_net_graph)
T3win_com   <- cluster_leading_eigen(T3win_net_graph)
T4spr_com   <- cluster_leading_eigen(T4spr_net_graph) 
T4sum_com   <- cluster_leading_eigen(T4sum_net_graph)
T4aut_com   <- cluster_leading_eigen(T4aut_net_graph)
T4win_com   <- cluster_leading_eigen(T4win_net_graph)
T5spr_com   <- cluster_leading_eigen(T5spr_net_graph) 
T5sum_com   <- cluster_leading_eigen(T5sum_net_graph)
T5aut_com   <- cluster_leading_eigen(T5aut_net_graph)
T5win_com   <- cluster_leading_eigen(T5win_net_graph)
T6spr_com   <- cluster_leading_eigen(T6spr_net_graph) 
T6sum_com   <- cluster_leading_eigen(T6sum_net_graph)
T6aut_com   <- cluster_leading_eigen(T6aut_net_graph)
T6win_com   <- cluster_leading_eigen(T6win_net_graph)
T7spr_com   <- cluster_leading_eigen(T7spr_net_graph) 
T7sum_com   <- cluster_leading_eigen(T7sum_net_graph)
T7aut_com   <- cluster_leading_eigen(T7aut_net_graph)
T7win_com   <- cluster_leading_eigen(T7win_net_graph)

# Calc modularityQ and also N clusters standardised (0-1) for network size using Nclusters/Nfoxes, where lower N = fewer divisions, so >cohesion.
modQ_obs <- rbind(data.frame(Territory=1, SeasonID=1, modQ=modularity(T1spr_com), PA=T, Nclust=max(membership(T1spr_com))/length(membership(T1spr_com))),
                  data.frame(Territory=1, SeasonID=2, modQ=modularity(T1sum_com), PA=T, Nclust=max(membership(T1sum_com))/length(membership(T1sum_com))),
                  data.frame(Territory=1, SeasonID=3, modQ=modularity(T1aut_com), PA=T, Nclust=max(membership(T1aut_com))/length(membership(T1aut_com))),
                  data.frame(Territory=1, SeasonID=3, modQ=modularity(T1win_com), PA=T, Nclust=max(membership(T1win_com))/length(membership(T1win_com))),
                  data.frame(Territory=2, SeasonID=1, modQ=modularity(T2spr_com), PA=T, Nclust=max(membership(T2spr_com))/length(membership(T2spr_com))),
                  data.frame(Territory=2, SeasonID=2, modQ=modularity(T2sum_com), PA=F, Nclust=max(membership(T2sum_com))/length(membership(T2sum_com))),
                  data.frame(Territory=2, SeasonID=3, modQ=modularity(T2aut_com), PA=T, Nclust=max(membership(T2aut_com))/length(membership(T2aut_com))),
                  data.frame(Territory=2, SeasonID=4, modQ=modularity(T2win_com), PA=T, Nclust=max(membership(T2win_com))/length(membership(T2win_com))),
                  data.frame(Territory=3, SeasonID=1, modQ=modularity(T3spr_com), PA=T, Nclust=max(membership(T3spr_com))/length(membership(T3spr_com))),
                  data.frame(Territory=3, SeasonID=2, modQ=modularity(T3sum_com), PA=T, Nclust=max(membership(T3sum_com))/length(membership(T3sum_com))),
                  data.frame(Territory=3, SeasonID=3, modQ=modularity(T3aut_com), PA=T, Nclust=max(membership(T3aut_com))/length(membership(T3aut_com))),
                  data.frame(Territory=3, SeasonID=4, modQ=modularity(T3win_com), PA=T, Nclust=max(membership(T3win_com))/length(membership(T3win_com))),
                  data.frame(Territory=4, SeasonID=1, modQ=modularity(T4spr_com), PA=F, Nclust=max(membership(T4spr_com))/length(membership(T4spr_com))),
                  data.frame(Territory=4, SeasonID=2, modQ=modularity(T4sum_com), PA=F, Nclust=max(membership(T4sum_com))/length(membership(T4sum_com))),
                  data.frame(Territory=4, SeasonID=3, modQ=modularity(T4aut_com), PA=T, Nclust=max(membership(T4aut_com))/length(membership(T4aut_com))),
                  data.frame(Territory=4, SeasonID=4, modQ=modularity(T4win_com), PA=T, Nclust=max(membership(T4win_com))/length(membership(T4win_com))),
                  data.frame(Territory=5, SeasonID=1, modQ=modularity(T5spr_com), PA=T, Nclust=max(membership(T5spr_com))/length(membership(T5spr_com))),
                  data.frame(Territory=5, SeasonID=2, modQ=modularity(T5sum_com), PA=T, Nclust=max(membership(T5sum_com))/length(membership(T5sum_com))),
                  data.frame(Territory=5, SeasonID=3, modQ=modularity(T5aut_com), PA=T, Nclust=max(membership(T5aut_com))/length(membership(T5aut_com))),
                  data.frame(Territory=5, SeasonID=4, modQ=modularity(T5win_com), PA=F, Nclust=max(membership(T5win_com))/length(membership(T5win_com))),
                  data.frame(Territory=6, SeasonID=1, modQ=modularity(T6spr_com), PA=T, Nclust=max(membership(T6spr_com))/length(membership(T6spr_com))),
                  data.frame(Territory=6, SeasonID=2, modQ=modularity(T6sum_com), PA=T, Nclust=max(membership(T6sum_com))/length(membership(T6sum_com))),
                  data.frame(Territory=6, SeasonID=3, modQ=modularity(T6aut_com), PA=T, Nclust=max(membership(T6aut_com))/length(membership(T6aut_com))),
                  data.frame(Territory=6, SeasonID=4, modQ=modularity(T6win_com), PA=T, Nclust=max(membership(T6win_com))/length(membership(T6win_com))),
                  data.frame(Territory=7, SeasonID=1, modQ=modularity(T7spr_com), PA=T, Nclust=max(membership(T7spr_com))/length(membership(T7spr_com))),
                  data.frame(Territory=7, SeasonID=2, modQ=modularity(T7sum_com), PA=T, Nclust=max(membership(T7sum_com))/length(membership(T7sum_com))),
                  data.frame(Territory=7, SeasonID=3, modQ=modularity(T7aut_com), PA=T, Nclust=max(membership(T7aut_com))/length(membership(T7aut_com))),
                  data.frame(Territory=7, SeasonID=4, modQ=modularity(T7win_com), PA=T, Nclust=max(membership(T7win_com))/length(membership(T7win_com))))
detach(package:igraph)

# Merge transitivity and density data into same df. Both are in same order so can 'merge' the simple way:
density_data$transitivity <- transitivity_data$transitivity 
# Merge modularity with density data too
global_network_data <-data.frame(density_data, modQ_obs)
# delete duplicated columns
global_network_data$Territory.1 <- NULL
global_network_data$SeasonID.1 <- NULL
# Make factors
global_network_data$fTerritory<-factor(global_network_data $Territory)
global_network_data$fSeason<-factor(global_network_data $SeasonID)


# Plot raw data to identify potential patterns
#---------------------------------------------
par(mfrow=c(3,1))
boxplot(global_network_data$densityUW ~ global_network_data$fSeason, main="Unweighted density")
boxplot(global_network_data$densityW ~ global_network_data$fSeason, main="Weighted density")
boxplot(global_network_data$transitivity~global_network_data$fSeason, main="Transitivity")
boxplot(global_network_data$modQ~global_network_data$fSeason, main="Modularity Q")
boxplot(global_network_data$N~global_network_data$fSeason, main="N clusters")
# Unweighted and weighted density show the same trend
# Density is the opposite of modularity (and I couldn't calc modularity from random networks - igraph kept failing)
# N clusters shows little seasonal variation
# Cohesion depends on the patterning of ties as well as the volume of ties ()
# Think best measures are DENSITY (and maybe transitivity)
# wweighted density
interaction.plot(global_network_data$fSeason,global_network_data$Territory, global_network_data$densityW, main="Weighted density")
#unweighted density
interaction.plot(global_network_data$fSeason,global_network_data$Territory, global_network_data$densityUW, main="Unweighted density")
#transitivity
interaction.plot(global_network_data$fSeason,global_network_data$Territory, global_network_data$transitivity, main="Transitivity")


#### CALCULATE UNWEIGHTED DENSITY FROM RANDOM NETWORKS 
T1spr_rand_densityUW <- gden(T1spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T1sum_rand_densityUW <- gden(T1sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T1aut_rand_densityUW <- gden(T1aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T1win_rand_densityUW <- gden(T1win_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T2spr_rand_densityUW <- gden(T2spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T2sum_rand_densityUW <- gden(T2sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T2aut_rand_densityUW <- gden(T2aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T2win_rand_densityUW <- gden(T2win_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T3spr_rand_densityUW <- gden(T3spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T3sum_rand_densityUW <- gden(T3sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T3aut_rand_densityUW <- gden(T3aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T3win_rand_densityUW <- gden(T3win_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T4spr_rand_densityUW <- gden(T4spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T4sum_rand_densityUW <- gden(T4sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T4aut_rand_densityUW <- gden(T4aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T4win_rand_densityUW <- gden(T4win_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T5spr_rand_densityUW <- gden(T5spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T5sum_rand_densityUW <- gden(T5sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T5aut_rand_densityUW <- gden(T5aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T5win_rand_densityUW <- gden(T5win_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T6spr_rand_densityUW <- gden(T6spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T6sum_rand_densityUW <- gden(T6sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T6aut_rand_densityUW <- gden(T6aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T6win_rand_densityUW <- gden(T6win_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T7spr_rand_densityUW <- gden(T7spr_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T7sum_rand_densityUW <- gden(T7sum_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T7aut_rand_densityUW <- gden(T7aut_rand, g=c(1:2000), mode="graph", ignore.eval=T)
T7win_rand_densityUW <- gden(T7win_rand, g=c(1:2000), mode="graph", ignore.eval=T)

#### CALCULATE WEIGHTED DENSITY FROM RANDOM NETWORKS 
T1spr_rand_densityW <- gden(T1spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T1sum_rand_densityW <- gden(T1sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T1aut_rand_densityW <- gden(T1aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T1win_rand_densityW <- gden(T1win_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T2spr_rand_densityW <- gden(T2spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T2sum_rand_densityW <- gden(T2sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T2aut_rand_densityW <- gden(T2aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T2win_rand_densityW <- gden(T2win_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T3spr_rand_densityW <- gden(T3spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T3sum_rand_densityW <- gden(T3sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T3aut_rand_densityW <- gden(T3aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T3win_rand_densityW <- gden(T3win_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T4spr_rand_densityW <- gden(T4spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T4sum_rand_densityW <- gden(T4sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T4aut_rand_densityW <- gden(T4aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T4win_rand_densityW <- gden(T4win_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T5spr_rand_densityW <- gden(T5spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T5sum_rand_densityW <- gden(T5sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T5aut_rand_densityW <- gden(T5aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T5win_rand_densityW <- gden(T5win_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T6spr_rand_densityW <- gden(T6spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T6sum_rand_densityW <- gden(T6sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T6aut_rand_densityW <- gden(T6aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T6win_rand_densityW <- gden(T6win_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T7spr_rand_densityW <- gden(T7spr_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T7sum_rand_densityW <- gden(T7sum_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T7aut_rand_densityW <- gden(T7aut_rand, g=c(1:2000), mode="graph", ignore.eval=F)
T7win_rand_densityW <- gden(T7win_rand, g=c(1:2000), mode="graph", ignore.eval=F)

#### CALCULATE TRANSITIVIY FROM RANDOM NETWORKS
T1spr_rand_transitivity <- gtrans(T1spr_rand, g=c(1:2000), mode="graph")
T1sum_rand_transitivity <- gtrans(T1sum_rand, g=c(1:2000), mode="graph")
T1aut_rand_transitivity <- gtrans(T1aut_rand, g=c(1:2000), mode="graph")
T1win_rand_transitivity <- gtrans(T1win_rand, g=c(1:2000), mode="graph")
T2spr_rand_transitivity <- gtrans(T2spr_rand, g=c(1:2000), mode="graph")
T2sum_rand_transitivity <- gtrans(T2sum_rand, g=c(1:2000), mode="graph")
T2aut_rand_transitivity <- gtrans(T2aut_rand, g=c(1:2000), mode="graph")
T2win_rand_transitivity <- gtrans(T2win_rand, g=c(1:2000), mode="graph")
T3spr_rand_transitivity <- gtrans(T3spr_rand, g=c(1:2000), mode="graph")
T3sum_rand_transitivity <- gtrans(T3sum_rand, g=c(1:2000), mode="graph")
T3aut_rand_transitivity <- gtrans(T3aut_rand, g=c(1:2000), mode="graph")
T3win_rand_transitivity <- gtrans(T3win_rand, g=c(1:2000), mode="graph")
T4spr_rand_transitivity <- gtrans(T4spr_rand, g=c(1:2000), mode="graph")
T4sum_rand_transitivity <- gtrans(T4sum_rand, g=c(1:2000), mode="graph")
T4aut_rand_transitivity <- gtrans(T4aut_rand, g=c(1:2000), mode="graph")
T4win_rand_transitivity <- gtrans(T4win_rand, g=c(1:2000), mode="graph")
T5spr_rand_transitivity <- gtrans(T5spr_rand, g=c(1:2000), mode="graph")
T5sum_rand_transitivity <- gtrans(T5sum_rand, g=c(1:2000), mode="graph")
T5aut_rand_transitivity <- gtrans(T5aut_rand, g=c(1:2000), mode="graph")
T5win_rand_transitivity <- gtrans(T5win_rand, g=c(1:2000), mode="graph")
T6spr_rand_transitivity <- gtrans(T6spr_rand, g=c(1:2000), mode="graph")
T6sum_rand_transitivity <- gtrans(T6sum_rand, g=c(1:2000), mode="graph")
T6aut_rand_transitivity <- gtrans(T6aut_rand, g=c(1:2000), mode="graph")
T6win_rand_transitivity <- gtrans(T6win_rand, g=c(1:2000), mode="graph")
T7spr_rand_transitivity <- gtrans(T7spr_rand, g=c(1:2000), mode="graph")
T7sum_rand_transitivity <- gtrans(T7sum_rand, g=c(1:2000), mode="graph")
T7aut_rand_transitivity <- gtrans(T7aut_rand, g=c(1:2000), mode="graph")
T7win_rand_transitivity <- gtrans(T7win_rand, g=c(1:2000), mode="graph")

#=====================================#
### Modelling global network measures
#=====================================#

#---------------------------
# UN-WEIGHTED DENSITY MODEL
#---------------------------

# check distrib
library(fitdistrplus)
descdist(global_network_data$densityUW, discrete = FALSE, boot=5000) # distrib looks NORMAL
require(MASS)
require(car)
qqp((global_network_data$densityUW), "norm", main="normal") # NORMAL

# model
densityUW_mod <-lmer(densityUW ~ fSeason + (1|fTerritory), REML=F, data=global_network_data)
summary(densityUW_mod)
plot(fitted(densityUW_mod), resid(densityUW_mod))
lines(smooth.spline(fitted(densityUW_mod), resid(densityUW_mod))) 
qqnorm(resid(densityUW_mod))
qqline(resid(densityUW_mod))
hist(resid(densityUW_mod))

# NOW COMPARE TO RANDOM:
perm <-2000
pb <- txtProgressBar(0,perm,0,style=3)

densityUW_intercept_perm <- rep(NA,perm)
densityUW_sum_perm <- rep(NA,perm)
densityUW_aut_perm <- rep(NA,perm)
densityUW_win_perm <- rep(NA,perm)
densityUW_12_perm <- rep(NA,perm)
densityUW_13_perm <- rep(NA,perm)
densityUW_14_perm <- rep(NA,perm)
densityUW_23_perm <- rep(NA,perm)
densityUW_24_perm <- rep(NA,perm)
densityUW_34_perm <- rep(NA,perm)

for(i in 1:perm){ 
  input_densityUW <- rbind(
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(densityUW=T1spr_rand_densityUW[i], SeasonID="1", Territory="1"), # if rownames won't work chec have run code above to rename the row names
    data.frame(densityUW=T1sum_rand_densityUW[i], SeasonID="2", Territory="1"), 
    data.frame(densityUW=T1aut_rand_densityUW[i], SeasonID="3", Territory="1"),
    data.frame(densityUW=T1win_rand_densityUW[i], SeasonID="4", Territory="1"),
    data.frame(densityUW= T2spr_rand_densityUW[i], SeasonID="1", Territory="2"),
    data.frame(densityUW= T2sum_rand_densityUW[i], SeasonID="2", Territory="2"), # no prefavoided
    data.frame(densityUW= T2aut_rand_densityUW[i], SeasonID="3", Territory="2"),
    data.frame(densityUW= T2win_rand_densityUW[i], SeasonID="4", Territory="2"),
    data.frame(densityUW= T3spr_rand_densityUW[i], SeasonID="1", Territory="3"),
    data.frame(densityUW= T3sum_rand_densityUW[i], SeasonID="2", Territory="3"),
    data.frame(densityUW= T3aut_rand_densityUW[i], SeasonID="3", Territory="3"),
    data.frame(densityUW= T3win_rand_densityUW[i], SeasonID="4", Territory="3"),
    data.frame(densityUW= T4spr_rand_densityUW[i], SeasonID="1", Territory="4"), # no prefavoided
    data.frame(densityUW= T4sum_rand_densityUW[i], SeasonID="2", Territory="4"), # no prefavoided
    data.frame(densityUW= T4aut_rand_densityUW[i], SeasonID="3", Territory="4"),
    data.frame(densityUW= T4win_rand_densityUW[i], SeasonID="4", Territory="4"),
    data.frame(densityUW= T5spr_rand_densityUW[i], SeasonID="1", Territory="5"),
    data.frame(densityUW= T5sum_rand_densityUW[i], SeasonID="2", Territory="5"),
    data.frame(densityUW= T5aut_rand_densityUW[i], SeasonID="3", Territory="5"),
    data.frame(densityUW= T5win_rand_densityUW[i], SeasonID="4", Territory="5"), # no prefavoided
    data.frame(densityUW= T6spr_rand_densityUW[i], SeasonID="1", Territory="6"), 
    data.frame(densityUW= T6sum_rand_densityUW[i], SeasonID="2", Territory="6"), 
    data.frame(densityUW= T6aut_rand_densityUW[i], SeasonID="3", Territory="6"), 
    data.frame(densityUW= T6win_rand_densityUW[i], SeasonID="4", Territory="6"),
    data.frame(densityUW= T7spr_rand_densityUW[i], SeasonID="1", Territory="7"),
    data.frame(densityUW= T7sum_rand_densityUW[i], SeasonID="2", Territory="7"), 
    data.frame(densityUW= T7aut_rand_densityUW[i], SeasonID="3", Territory="7"),
    data.frame(densityUW= T7win_rand_densityUW[i], SeasonID="4", Territory="7"))
  # Make factors
  input_densityUW$fSeason <- factor(input_densityUW$SeasonID)
  input_densityUW$fTerritory <- factor(input_densityUW$Territory)
  # Run model
  model_tmp <- lmer(densityUW ~ fSeason + (1|fTerritory), data=input_densityUW, REML=F)
  # save coefficients
  a <- coef(summary(model_tmp))
  densityUW_intercept_perm[i] <- a[1,1]
  densityUW_sum_perm[i] <- a[2,1]
  densityUW_aut_perm[i] <- a[3,1]
  densityUW_win_perm[i] <- a[4,1]
  # run post hoc test and save estimates
  lsm_temp<-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)
  densityUW_12_perm[i] <- lsm_temp[1,2]
  densityUW_13_perm[i] <- lsm_temp[2,2]
  densityUW_14_perm[i] <- lsm_temp[3,2]
  densityUW_23_perm[i] <- lsm_temp[4,2]
  densityUW_24_perm[i] <- lsm_temp[5,2]
  densityUW_34_perm[i] <- lsm_temp[6,2]
  # update prog bar
  setTxtProgressBar(pb, i)
}
# real densityUW (including all networks, not just pref/avoided)
densityUW_mod <-lmer(densityUW ~ fSeason + (1|fTerritory), data=global_network_data, REML=F)
realmod <- coef(summary(densityUW_mod))
densityUW_intercept_obs <- realmod[1,1]
densityUW_sum_obs <- realmod[2,1]
densityUW_aut_obs <- realmod[3,1]
densityUW_win_obs <- realmod[4,1]

# real posthoc estimates
densityUW_lsm <- data.frame(summary(lsmeans::lsmeans(densityUW_mod, pairwise~fSeason))$contrasts)
densityUW_12_obs <- densityUW_lsm[1,2]
densityUW_13_obs <- densityUW_lsm[2,2]
densityUW_14_obs <- densityUW_lsm[3,2]
densityUW_23_obs <- densityUW_lsm[4,2]
densityUW_24_obs <- densityUW_lsm[5,2]
densityUW_34_obs <- densityUW_lsm[6,2]

# calc p-values
densityUW_intercept_Pvalue <- sum(densityUW_intercept_perm>densityUW_intercept_obs)/2000 
densityUW_sum_Pvalue <- sum(densityUW_sum_perm>densityUW_sum_obs)/2000 
densityUW_aut_Pvalue <- sum(densityUW_aut_perm>densityUW_aut_obs)/2000 
densityUW_win_Pvalue <- sum(densityUW_win_perm>densityUW_win_obs)/2000 
densityUW_lsm12_Pvalue <- sum(densityUW_12_perm>densityUW_12_obs)/2000
densityUW_lsm13_Pvalue <- sum(densityUW_13_perm>densityUW_13_obs)/2000
densityUW_lsm14_Pvalue <- sum(densityUW_14_perm>densityUW_14_obs)/2000
densityUW_lsm23_Pvalue <- sum(densityUW_23_perm>densityUW_23_obs)/2000
densityUW_lsm24_Pvalue <- sum(densityUW_24_perm>densityUW_24_obs)/2000
densityUW_lsm34_Pvalue <- sum(densityUW_34_perm>densityUW_34_obs)/2000

# save
densityUW_Prands <- rbind(densityUW_intercept_Pvalue,
                          densityUW_sum_Pvalue,
                          densityUW_aut_Pvalue,
                          densityUW_win_Pvalue,
                          densityUW_lsm12_Pvalue,
                          densityUW_lsm13_Pvalue,
                          densityUW_lsm14_Pvalue,
                          densityUW_lsm23_Pvalue,
                          densityUW_lsm24_Pvalue,
                          densityUW_lsm34_Pvalue) # ALL NON-SIGNIFICANT

# adjust post-hoc p-values for multiple testing
p.adjust(c(0.149,0.050,0.087,0.496,0.475,0.471), meth="holm") # ALL NON-SIGNIFICANT

#=====================================================================================

#--------------------------
# WEIGHTED DENSITY MODEL
#--------------------------

# check distrib
descdist(global_network_data$densityW, discrete = FALSE, boot=5000) # distrib looks GAMMA or poss normal/lognormal
qqp((global_network_data$densityW), "norm", main="normal")
qqp(global_network_data$densityW, "lnorm", main="lognormal") 
gamma <- fitdistr(global_network_data$densityW, "gamma", start=NULL) 
qqp(global_network_data$densityW, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma") # gamma is poor 
# GAMMA is best

# model
densityW_mod <-glmer(densityW ~ fSeason + (1|fTerritory), data=global_network_data, family=Gamma(link="log"))
summary(densityW_mod)
plot(fitted(densityW_mod), resid(densityW_mod))
lines(smooth.spline(fitted(densityW_mod), resid(densityW_mod))) 
qqnorm(resid(densityW_mod))
qqline(resid(densityW_mod))
visreg::visreg(densityW_mod)

# NOW COMPARE TO RANDOM:
perm<-2000
pb <- txtProgressBar(0,perm,0,style=3)

densityW_intercept_perm <- rep(NA,perm)
densityW_sum_perm <- rep(NA,perm)
densityW_aut_perm <- rep(NA,perm)
densityW_win_perm <- rep(NA,perm)

for(i in 1:perm){ 
  input_densityW <- rbind(
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(densityW=T1spr_rand_densityW[i], SeasonID="1", Territory="1"), # if rownames won't work chec have run code above to rename the row names
    data.frame(densityW=T1sum_rand_densityW[i], SeasonID="2", Territory="1"), 
    data.frame(densityW=T1aut_rand_densityW[i], SeasonID="3", Territory="1"),
    data.frame(densityW=T1win_rand_densityW[i], SeasonID="4", Territory="1"),
    data.frame(densityW= T2spr_rand_densityW[i], SeasonID="1", Territory="2"),
    data.frame(densityW= T2sum_rand_densityW[i], SeasonID="2", Territory="2"), # no prefavoided
    data.frame(densityW= T2aut_rand_densityW[i], SeasonID="3", Territory="2"),
    data.frame(densityW= T2win_rand_densityW[i], SeasonID="4", Territory="2"),
    data.frame(densityW= T3spr_rand_densityW[i], SeasonID="1", Territory="3"),
    data.frame(densityW= T3sum_rand_densityW[i], SeasonID="2", Territory="3"),
    data.frame(densityW= T3aut_rand_densityW[i], SeasonID="3", Territory="3"),
    data.frame(densityW= T3win_rand_densityW[i], SeasonID="4", Territory="3"),
    data.frame(densityW= T4spr_rand_densityW[i], SeasonID="1", Territory="4"), # no prefavoided
    data.frame(densityW= T4sum_rand_densityW[i], SeasonID="2", Territory="4"), # no prefavoided
    data.frame(densityW= T4aut_rand_densityW[i], SeasonID="3", Territory="4"),
    data.frame(densityW= T4win_rand_densityW[i], SeasonID="4", Territory="4"),
    data.frame(densityW= T5spr_rand_densityW[i], SeasonID="1", Territory="5"),
    data.frame(densityW= T5sum_rand_densityW[i], SeasonID="2", Territory="5"),
    data.frame(densityW= T5aut_rand_densityW[i], SeasonID="3", Territory="5"),
    data.frame(densityW= T5win_rand_densityW[i], SeasonID="4", Territory="5"), # no prefavoided
    data.frame(densityW= T6spr_rand_densityW[i], SeasonID="1", Territory="6"), 
    data.frame(densityW= T6sum_rand_densityW[i], SeasonID="2", Territory="6"), 
    data.frame(densityW= T6aut_rand_densityW[i], SeasonID="3", Territory="6"), 
    data.frame(densityW= T6win_rand_densityW[i], SeasonID="4", Territory="6"),
    data.frame(densityW= T7spr_rand_densityW[i], SeasonID="1", Territory="7"),
    data.frame(densityW= T7sum_rand_densityW[i], SeasonID="2", Territory="7"), 
    data.frame(densityW= T7aut_rand_densityW[i], SeasonID="3", Territory="7"),
    data.frame(densityW= T7win_rand_densityW[i], SeasonID="4", Territory="7"))
  # Make factors
  input_densityW$fSeason <- factor(input_densityW$SeasonID)
  input_densityW$fTerritory <- factor(input_densityW$Territory)
  # Run model
  model_tmp  <-glmer(densityW ~ fSeason + (1|fTerritory), data=input_densityW, family=Gamma(link="log"))
  # save coefficients
  a <- coef(summary(model_tmp))
  densityW_intercept_perm[i] <- a[1,1]
  densityW_sum_perm[i] <- a[2,1]
  densityW_aut_perm[i] <- a[3,1]
  densityW_win_perm[i] <- a[4,1]
  # update prog bar
  setTxtProgressBar(pb, i)
}
# real densityW (including all networks, not just pref/avoided)
densityW_mod <-glmer(densityW ~ fSeason + (1|fTerritory), data=global_network_data, family=Gamma(link="log"))
realmod <- coef(summary(densityW_mod))
densityW_intercept_obs <- realmod[1,1]
densityW_sum_obs <- realmod[2,1]
densityW_aut_obs <- realmod[3,1]
densityW_win_obs <- realmod[4,1]

# calc p-values
densityW_intercept_Pvalue <- sum(densityW_intercept_perm>densityW_intercept_obs)/2000 
densityW_sum_Pvalue <- sum(densityW_sum_perm>densityW_sum_obs)/2000 
densityW_aut_Pvalue <- sum(densityW_aut_perm>densityW_aut_obs)/2000 
densityW_win_Pvalue <- sum(densityW_win_perm>densityW_win_obs)/2000 

# save
densityW_Prands <- rbind(densityW_intercept_Pvalue,
                         densityW_sum_Pvalue,
                         densityW_aut_Pvalue,
                         densityW_win_Pvalue) # ALL NON-SIGNIFICANT

#=====================================================================================

#--------------------
# TRANSITIVITY MODEL
#--------------------

# check distrib
descdist(global_network_data$transitivity, discrete = FALSE, boot=5000) # uniform/normal
hist(global_network_data$transitivity) # uniform - TREAT AS NORMAL
# model
transitivity_mod <-lmer(transitivity~fSeason + (1|Territory), data=global_network_data, REML=F)
summary(transitivity_mod)

# NOW COMPARE TO RANDOM:
perm<-2000
pb <- txtProgressBar(0,perm,0,style=3)

trans_intercept_perm <- rep(NA,perm)
trans_sum_perm <- rep(NA,perm)
trans_aut_perm <- rep(NA,perm)
trans_win_perm <- rep(NA,perm)

for(i in 1:perm){ 
  input_trans <- rbind(
    ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
    data.frame(transitivity=T1spr_rand_transitivity[i], SeasonID="1", Territory="1"), # if rownames won't work chec have run code above to rename the row names
    data.frame(transitivity=T1sum_rand_transitivity[i], SeasonID="2", Territory="1"), 
    data.frame(transitivity=T1aut_rand_transitivity[i], SeasonID="3", Territory="1"),
    data.frame(transitivity=T1win_rand_transitivity[i], SeasonID="4", Territory="1"),
    data.frame(transitivity= T2spr_rand_transitivity[i], SeasonID="1", Territory="2"),
    data.frame(transitivity= T2sum_rand_transitivity[i], SeasonID="2", Territory="2"), # no prefavoided
    data.frame(transitivity= T2aut_rand_transitivity[i], SeasonID="3", Territory="2"),
    data.frame(transitivity= T2win_rand_transitivity[i], SeasonID="4", Territory="2"),
    data.frame(transitivity= T3spr_rand_transitivity[i], SeasonID="1", Territory="3"),
    data.frame(transitivity= T3sum_rand_transitivity[i], SeasonID="2", Territory="3"),
    data.frame(transitivity= T3aut_rand_transitivity[i], SeasonID="3", Territory="3"),
    data.frame(transitivity= T3win_rand_transitivity[i], SeasonID="4", Territory="3"),
    data.frame(transitivity= T4spr_rand_transitivity[i], SeasonID="1", Territory="4"), # no prefavoided
    data.frame(transitivity= T4sum_rand_transitivity[i], SeasonID="2", Territory="4"), # no prefavoided
    data.frame(transitivity= T4aut_rand_transitivity[i], SeasonID="3", Territory="4"),
    data.frame(transitivity= T4win_rand_transitivity[i], SeasonID="4", Territory="4"),
    data.frame(transitivity= T5spr_rand_transitivity[i], SeasonID="1", Territory="5"),
    data.frame(transitivity= T5sum_rand_transitivity[i], SeasonID="2", Territory="5"),
    data.frame(transitivity= T5aut_rand_transitivity[i], SeasonID="3", Territory="5"),
    data.frame(transitivity= T5win_rand_transitivity[i], SeasonID="4", Territory="5"), # no prefavoided
    data.frame(transitivity= T6spr_rand_transitivity[i], SeasonID="1", Territory="6"), 
    data.frame(transitivity= T6sum_rand_transitivity[i], SeasonID="2", Territory="6"), 
    data.frame(transitivity= T6aut_rand_transitivity[i], SeasonID="3", Territory="6"), 
    data.frame(transitivity= T6win_rand_transitivity[i], SeasonID="4", Territory="6"),
    data.frame(transitivity= T7spr_rand_transitivity[i], SeasonID="1", Territory="7"),
    data.frame(transitivity= T7sum_rand_transitivity[i], SeasonID="2", Territory="7"), 
    data.frame(transitivity= T7aut_rand_transitivity[i], SeasonID="3", Territory="7"),
    data.frame(transitivity= T7win_rand_transitivity[i], SeasonID="4", Territory="7"))
  # Make factors
  input_trans$fSeason <- factor(input_trans$SeasonID)
  input_trans$fTerritory <- factor(input_trans$Territory)
  # Run model
  model_tmp <- lmer(transitivity ~ fSeason + (1|fTerritory), data=input_trans, REML=F)
  # save coefficients
  a <- coef(summary(model_tmp))
  trans_intercept_perm[i] <- a[1,1]
  trans_sum_perm[i] <- a[2,1]
  trans_aut_perm[i] <- a[3,1]
  trans_win_perm[i] <- a[4,1]
  # update prog bar
  setTxtProgressBar(pb, i)
}

# real transitivity (including all networks, not just pref/avoided)
transitivity_mod <-lmer(transitivity~fSeason + (1|Territory), data=global_network_data, REML=F)
realmod <- coef(summary(transitivity_mod))
trans_intercept_obs <- realmod[1,1]
trans_sum_obs <- realmod[2,1]
trans_aut_obs <- realmod[3,1]
trans_win_obs <- realmod[4,1]

# calc p-values
trans_intercept_Pvalue <- sum(trans_intercept_perm>trans_intercept_obs)/2000 
trans_sum_Pvalue <- sum(trans_sum_perm>trans_sum_obs)/2000 
trans_aut_Pvalue <- sum(trans_aut_perm>trans_aut_obs)/2000 
trans_win_Pvalue <- sum(trans_win_perm>trans_win_obs)/2000 
# save
trans_Prands <- rbind(trans_intercept_Pvalue,
                      trans_sum_Pvalue,
                      trans_aut_Pvalue,
                      trans_win_Pvalue) # ALL NON-SIGNIFICANT

#========================================================================

# PLOTTING GLOBAL MEASURES IN EACH TERRITORY, IN SEQUENCE OF DATA COLLECTION

# import data - was easier to add sequence of data collection and years in Excel
global_network_data_ovseq <- read.csv(file = "Global net measures LONG FORMAT with years and sequence of data collection.csv", header = T) 

# convert territory to factor for legend
global_network_data_ovseq$fTerritory <- factor(global_network_data_ovseq$Territory)

# Facetted graph with all 3 measures (weighted & unweighted density and transitivity):
myplot<-ggplot(global_network_data_ovseq, aes(x = OverallSequence, y = Value, colour = fTerritory)) + 
  geom_line(size=1) + geom_point(size=3) +
  facet_grid(GlobalMeasure ~ ., scales = "free") +
  scale_colour_manual(values = c("red","darkorange", "maroon", "gray35", "blue", "forestgreen", "black"),
                      name="Territory") +
  theme_bw(base_size = 18, base_family = "") +
  scale_x_continuous(breaks=c(1:8), 
                     labels = c("Summer\n2013", "Autumn\n2013", "Winter\n2013-14", "Spring\n2014", 
                                "Summer\n2014", "Autumn\n2014", "Winter\n2014-15", "Spring\n2015")) +
  xlab("") + ylab("Value\n")

#draw the above graph without grid in background
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
# save above in high resolution for publications
dev.print(jpeg, "RPlot_Global network measures_FACET.jpeg", res=700, height=14, width=10, units="in")

#========================================================================

# Boxplots of global measures
boxplot(global_network_data_ovseq$densityUW ~ global_network_data_ovseq$Season, main="Unweighted density")
boxplot(global_network_data_ovseq$densityW ~ global_network_data_ovseq$Season, main="Weighted density")
boxplot(global_network_data_ovseq$transitivity~global_network_data_ovseq$Season, main="Transitivity")

#========================================================================