
#install asnipe package in R studio via Tools > Install Package
#load asnipe package
library(asnipe)

#import association data (K*N matrix of foxes*gathering events at all locations). Must be imported as a MATRIX
#and transposed (assume this is what the command t() does) to make it the data type 'GBI'.
#gbi = group (rows) by individual (columns) matrix, c.f. ibg = individual by group matrix.
gbi <- t(as.matrix(read.csv('SG1_Sum13.csv', header=FALSE)))

#import location data (vector/list of each gathering event's location). Must be imported as a MATRIX.
locations <- as.matrix(read.csv('locations.csv', header=FALSE))
#import days data (vector/list of each gathering event's dates. Must be imported as a MATRIX.
days <- as.matrix(read.csv('Days.csv', header=FALSE))

#Convert locations and days to a 1-dimensional vector
locations <- locations[1,]
days <- days[1,]

#check length and properties of variables
length(locations)
str(locations)
length(gbi)
str(gbi)
length(days)
str(days)

#Calculate a network from a group by individual matrix
network <- get_network(gbi)

#Performs permutations on the data and calculates network for each step
networks.rand <- network_permutation(association_data=gbi, data_format = "GBI", permutations = 1000,
                    returns=1, association_index = "SRI", association_matrix = network, identities = NULL,
                    which_identities = NULL, times = NULL, locations = locations, which_locations = locations,
                    start_time = NULL, end_time = NULL, classes = NULL, which_classes = NULL,
                    days = days, within_day = TRUE, within_location = TRUE, within_class = FALSE)

#If get error saying "Error in 1:nrow(a) : argument of length 0" this indicates there's no data for one day at one of the locations. 
#If this occurs, can avoid it by changing within_location to FALSE to allow randomisation between different locations.

#To see if network associations differ from random:
library(raster)

#Compute the coefficient of variation (expressed as a percentage) for the recorded network
obs <- cv(network)
#and for the random network, generated through permutations
rand <- apply(networks.rand,1,cv)

#See whether the coefficient of variation (%) in the real network (obs) is lower than each of the 1000 random networks generated
#(rand), in terms of True or False:
obs < rand
#show a summary - the proportion of random networks different from the observed (e.g. 0.284 means 284 out of 1000 networks). To reject
#the Null hypothesis that there is no association, obs should be greater than 97.5% (0.975 for 1000 permutations) of the random networks
# (so P<0.05)
sum(obs < rand)/1000

get_sampling_periods(gbi, association_times, sampling_period,
                     identities = NULL, location = NULL, within_locations = FALSE,
                     data_format = c("gbi", "groups", "individuals"))

