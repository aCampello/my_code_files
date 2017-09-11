
#=========================================================================#
# CODE TO EXTRACT EDGELIST WITH WEIGHTS OF ASSOCIATIONS BETWEEN EACH DYAD #
#=========================================================================#

# Was going to use this to compare dyadic edge weights before and after midnight, but can't check it
# using data stream permutations so probably not worth continuing with.

# 1. Make edgelist to extract dyadic edge weights (from network matrix 'T1sprB4_net'):
library(tnet)
T1sprB4_edgelist <- data.frame(as.tnet(T1sprB4_net), Territory=1, SeasonID=1, BeforeMidnight=1) ###

# 2. Shorten object names so don't have to change all code 
edgelist <- T1sprB4_edgelist 
att <- T1sprB4_attr <- subset(attr_B4, SeasonID==1 & Territory==1)

# 3. Change id to proper AnimalID
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]

# 4. List dyad ordered minID-maxID
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")

# 7. Rename edgelist
T1sprB4_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1sprAf_edgelist <- data.frame(as.tnet(T1sprAf_net), Territory=1, SeasonID=1, BeforeMidnight=0) ###
edgelist <- T1sprAf_edgelist 
att <- T1sprAf_attr <- subset(attr_Af, SeasonID==1 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")

# Rename edgelist
T1sprAf_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1sumB4_edgelist <- data.frame(as.tnet(T1sumB4_net), Territory=1, SeasonID=2, BeforeMidnight=1) ###
edgelist <- T1sumB4_edgelist 
att <- T1sumB4_attr <- subset(attr_B4, SeasonID==2 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")
# Rename edgelist
T1sumB4_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1sumAf_edgelist <- data.frame(as.tnet(T1sumAf_net), Territory=1, SeasonID=2, BeforeMidnight=0) ###
edgelist <- T1sumAf_edgelist 
att <- T1sumAf_attr <-  subset(attr_Af, SeasonID==2 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")
# Rename edgelist
T1sumAf_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1autB4_edgelist <- data.frame(as.tnet(T1autB4_net), Territory=1, SeasonID=3, BeforeMidnight=1) ###
edgelist <- T1autB4_edgelist 
att <- T1autB4_attr <- subset(attr_B4, SeasonID==3 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")
# Rename edgelist
T1autB4_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1autAf_edgelist <- data.frame(as.tnet(T1autAf_net), Territory=1, SeasonID=3, BeforeMidnight=0) ###
edgelist <- T1autAf_edgelist 
att <- T1autAf_attr <-  subset(attr_Af, SeasonID==3 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")
# Rename edgelist
T1autAf_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1winB4_edgelist <- data.frame(as.tnet(T1winB4_net), Territory=1, SeasonID=4, BeforeMidnight=1) ###
edgelist <- T1winB4_edgelist 
att <- T1winB4_attr <- subset(attr_B4, SeasonID==4 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")
# Rename edgelist
T1winB4_edgelist <- edgelist

#== END CURRENT NETWORK
#----------------------------------
#== START NEW NETWORK:

# Change name, territory, season and BeforeMidnight to make edgelist and attributes table
T1winAf_edgelist <- data.frame(as.tnet(T1winAf_net), Territory=1, SeasonID=4, BeforeMidnight=0) ###
edgelist <- T1winAf_edgelist 
att <- T1winAf_attr <-  subset(attr_Af, SeasonID==4 & Territory==1)      

# label dyad id
edgelist$foxi <- att[match(edgelist$i, a<-(1:nrow(att))), 1]
edgelist$foxj <- att[match(edgelist$j, a<-(1:nrow(att))), 1]
edgelist$dyadid <-paste(apply(data.frame(edgelist$foxi, edgelist$foxj), 1, min),
                        apply(data.frame(edgelist$foxi, edgelist$foxj), 1, max), sep="-")
# Rename edgelist
T1winAf_edgelist <- edgelist

#=====================================================


# Compile all territories, seasons and beforemidnight:
edgelist <- rbind(T1sprB4_edgelist, T1sprAf_edgelist, T1sumB4_edgelist, T1sumAf_edgelist, T1autB4_edgelist, T1autAf_edgelist, T1winB4_edgelist, T1winAf_edgelist)

# label sex and status of dyads
edgelist$dyadsextype <- ifelse(paste(attribs[match(edgelist$foxi, attribs$id), 5], 
                                     attribs[match(edgelist$foxj, attribs$id), 5], sep = "") =="FM", "MF", 
                               paste(attribs[match(edgelist$foxi, attribs$id), 5], 
                                     attribs[match(edgelist$foxj, attribs$id), 5], sep = ""))
edgelist$dyadstatustype <- ifelse(paste(attribs[match(edgelist$foxi, attribs$id), 6], 
                                        attribs[match(edgelist$foxj, attribs$id), 6], sep = "-") =="Sub-Dom", "Dom-Sub", 
                                  paste(attribs[match(edgelist$foxi, attribs$id), 6], 
                                        attribs[match(edgelist$foxj, attribs$id), 6], sep = "-"))

# Mark core animals
edgelist$id<-edgelist$foxi # first make ID refer to fox i
edgelist_core <- merge(x=edgelist, y=attribs[ , c("id", "SeasonID", "Territory", "BeforeMidnight", "Core")], 
                       by = c("id", "SeasonID", "Territory", "BeforeMidnight"), all.x=TRUE) 
edgelist_core$Corei <- edgelist_core$Core # save core as corei (as is for fox i)
edgelist_core <- data.frame(edgelist_core[,1:12],Corei=edgelist_core[,14]) # take subset without 'Core' column so can merge corej column:
# repeat for fox j (the other member of the dyad)
edgelist_core$id<-edgelist_core$foxj # first make ID refer to fox j
edgelist_core <- merge(x=edgelist_core, y=attribs[ , c("id", "SeasonID", "Territory", "BeforeMidnight", "Core")], 
                       by = c("id", "SeasonID", "Territory", "BeforeMidnight"), all.x=TRUE) 
edgelist_core$Corej <- edgelist_core$Core 
edgelist_core <- data.frame(edgelist_core[,2:13],Corej=edgelist_core[,15]) # take subset without 'Core' column 


# Remove dyads involving non-core foxes
total_edgelist <-  subset(edgelist_core, Corei==1 & Corej==1)

# Save variables as factors
total_edgelist$fTerritory <- factor(total_edgelist$Territory)
total_edgelist$fSeason <- factor(total_edgelist$SeasonID)
total_edgelist$fBeforeMidnight <- factor(total_edgelist$BeforeMidnight)
total_edgelist$fdyadID <- factor(total_edgelist$dyadid)
total_edgelist$fsextype <- factor(total_edgelist$dyadsextype)
total_edgelist$fstatustype <- factor(total_edgelist$dyadstatustype)

# Save data frame as CSV file to check in Excel
write.table(x = edgelist, file="edgelist.txt", row.names = FALSE, sep=" ")

# Plot
boxplot(w~fBeforeMidnight, data=total_edgelist)
boxplot(w~fSeason, data=total_edgelist)

# LMM to test effect of season, food (beforemidnight), sex and status on core foxes (territory 1 only)
require(lmerTest)
library(lme4)
model <- lmer(w ~ fBeforeMidnight + fSeason + fsextype + fstatustype + 
                (1|fdyadID), REML=F, data=total_edgelist) # should add (1|Territory) when add other territories
summary(model)
