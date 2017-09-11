library(asnipe)
setwd("G:/Statistics 2016/SNA_synced with uni 180416noon/Trying sna in R 20-21.04.16")

mydata <- read.csv(file = "Assocs_R_gbiformat_NoCubs_StandardPatches.csv", header = T, stringsAsFactors=FALSE)
ids<-read.csv(file="Attribs_R_AllData_StandardPatches.csv", header=T, stringsAsFactors=FALSE)

mydata$Date <- strptime(mydata$DAY,format="%d/%m/%Y") # save TRANScDate as a date format
mydata$Day <- as.numeric(difftime(mydata$Date,min(mydata$Date),units="days")) + 1 # change day variable to 'number of days since the first day'

SP <- get_sampling_periods(association_data=mydata[,c(1,2)],
                           association_times=mydata$Day, 
                           sampling_period=1,
                           data_format="individuals",
                           location=mydata$Territory,
                           within_locations=TRUE)

net <- get_network(SP, data_format="SP")

random_networks <- network_permutation(association_data=SP,
                                       data_format="SP",
                                       permutations=10,
                                       association_matrix=net,
                                       returns=1,
                                       association_index="SRI",
                                       days=rownames(SP),
                                       within_day=TRUE,
                                       classes=unique(ids[,c(1,3)])$Sex,
                                       within_class=TRUE) # see note below:

# D Farine: "You would permute within sex if you thought there might be some sex effects that are intrinsic. 
# For example, if males are territorial and females roam widely, then you might want to prevent data from 
# females and males being swapped."

# D Farine: "For the randomisation, you don't need to fix the within-location so much because the SP is 
# already disaggregated at that level (ie it already has it all separated out).