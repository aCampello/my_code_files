
#### MAKE TABLE TO SHOW INDIVIDUAL GROUP MEMBERSHIP BY DIFFERENT DEFINITIONS ####

groupmembership <- read.csv("E:/Statistics 2016/Group size/Group membership by different definitions 110816.csv", header=T, 
                       stringsAsFactors = FALSE) # MUST IMPORT AS strings as some foxes have unknown sex and status - otherwise no matter what I do R thinks there are 3 levels (M, F and '')... WHY!!


#### Mark whether group member or not by each definition ####

# spatial overlap
groupmembership$SO <-  1

# spatial overlap with sighting threshold
groupmembership$SOT <-  ifelse(groupmembership$DaysSeen>19, 1, 0)

# spatiotemporal overlap (min 1 true assoc so connected to main component and not in a pair)
groupmembership$STO <-  ifelse(groupmembership$NTrueAssocs<2 | groupmembership$Pair=="Yes", 0, 1)

# spatial overlap with sighting threshold (min 20 days) and number of social connections (min 2)
Core # already marked as ~ 



# Calc number of foxes that were group members by both SOT and STO
# (to show that although both definitions produce similar group size estimate, they include different individuals,
# to support idea that combination method is better)

# Tidy up data frame 
TIDYgroupmembership <- groupmembership

TIDYgroupmembership$SurveyID <- NULL
TIDYgroupmembership$WhyNotCore <- NULL
TIDYgroupmembership$ResidentInAnotherTerritory <- NULL
TIDYgroupmembership$Pair <- NULL

# mark comparisons between definitions as 1 if they both assigned the fox to the same group category and zero if not.
summaryGM <- ddply(groupmembership, c("SiteID", "SeasonID", "AnimalID", "ShortCode", "ShortSex", "ShortStatus", "AgeClassName", 
                               "SO", "SOT", "STO", "Core"), summarise, 
            SOT_STO = ifelse(sum(SOT, STO)==2, 1, ifelse(sum(SOT, STO)==0,1,0)),
            SOT_Core = ifelse(sum(SOT, Core)==2, 1, ifelse(sum(SOT, Core)==0,1,0)),
            STO_Core = ifelse(sum(STO, Core)==2, 1, ifelse(sum(STO, Core)==0,1,0)))

# save as csv
write.csv(summaryGM, file="Summary of group membership.csv", row.names = FALSE) # saves as csv file in current working directory



# Calc number of individuals in each season with same / diff group category by diff definitions

a <- ddply(summaryGM, c("SiteID", "SeasonID"), summarise,
           # overall group size estimates
           SO_groupsize = sum(SO),
           SOT_groupsize = sum(SOT),
           STO_groupsize = sum(STO), 
           Core_groupsize = sum(Core),
           # diff between estimates - number of foxes with same & different group membership
           Nsame_SOT_STO = sum(SOT_STO),
           Ndiff_SOT_STO = SO_groupsize-Nsame_SOT_STO,
           Nsame_SOT_Core = sum(SOT_Core),
           Ndiff_SOT_Core = SO_groupsize-Nsame_SOT_Core,
           Nsame_STO_Core = sum(STO_Core),
           Ndiff_STO_Core = SO_groupsize-Nsame_STO_Core)
          








