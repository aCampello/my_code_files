library(igraph)
#library(statnet)
#library(sna)
#library(asnipe)
#library(network)

# CUSTOMISED RANDOMISATION

# randomising network data and testing if effect is different from random

# initialise starting dataframe - ordered by season winter-fall, and then ID
# = attribs
xdataobs<-data.frame(id=rep(c("f00","f01","f02","f08","f09","f11","f12","f13","f14","f15","f16","f18","f19","f20","f21","f22","f23","f25","f26","ok"),4),season=c(rep("winter", 20),rep("spring",20), rep("summer",20),rep("fall", 20)),liceload=rep(sample(1:20),4), dominance=rep(sample(1:20),4), obsnetdeg=numeric(80), obsnetstr=numeric(80), obsnetev=numeric(80), obsnetcc=numeric(80))
str(xdataobs)

# read data sets, one for each season (associations)
xdata <- as.matrix(read.delim("~/./fallcontactmat.txt", row.names=1))
xfa <- xdata
xdata <- as.matrix(read.delim("~/./wintercontactmat.txt", row.names=1))
xwi <- xdata
xdata <- as.matrix(read.delim("~/./springcontactmat.txt", row.names=1))
xsp <- xdata 
xdata <- as.matrix(read.delim("~/Documents/./summercontactmat.txt", row.names=1)) 
xsu <- xdata 
                              
# create the networks for each season
netwin <- graph.adjacency(xwi, mode = "directed", weighted = TRUE, diag = FALSE)
netsp <- graph.adjacency(xsp, mode = "directed", weighted = TRUE, diag = FALSE)
netsum <- graph.adjacency(xsu, mode = "directed", weighted = TRUE, diag = FALSE)
netfall <- graph.adjacency(xfa, mode = "directed", weighted = TRUE, diag = FALSE)

# original (REAL) network measures - saved in attribute table
xdataobs$obsnetstr <- c(graph.strength(netwin, mode="all"), graph.strength(netsp, mode="all"),graph.strength(netsum, mode="all"),graph.strength(netfall, mode="all"))
xdataobs$obsnetdeg <- c(degree(netwin, mode="all"), degree(netsp, mode="all"),degree(netsum, mode="all"),degree(netfall, mode="all"))
xdataobs$obsnetev <- c(evcent(netwin)$vector, evcent(netsp)$vector,evcent(netsum)$vector,evcent(netfall)$vector)
xdataobs$obsnetcc<- c(transitivity(netwin,type="weighted"),transitivity(netsp,type="weighted"),transitivity(netsum,type="weighted"),transitivity(netfall,type="weighted"))

# set number of randomizations
perm <- 1000

# create (N by perm) matrices that will contain the new network measures (from the randomized networks)
matrwin <- matrix(ncol=ncol(xwi), nrow= perm) #she put ncol(xdata) but i think she meant ncol(xwi)..
matrsp <- matrix(ncol=ncol(xsp), nrow= perm) #ditto
matrsum <- matrix(ncol=ncol(xsu), nrow= perm) #ditto
matrfall <- matrix(ncol=ncol(xfa), nrow= perm)# #ditto

# empty vectors to contain model estimates for the effects of network measure on lice load
randest <- numeric(perm)
repest<-numeric(perm)
# empty vector for the pvalues for the full-null model comparison of season on network measure
pvals <- numeric(perm)

# set up progress bar
pb <- txtProgressBar(0,perm,0, style=3)

# run the loop - to calc degree for each set of seasonal random networks and save in new matrices
i=1
for(i in 1:perm) {
  # recalculate the network measure for each season and save the results in the results matrix
  matrwin[i, ] <- degree(rewire(netwin, with = each_edge(prob=0.05,loops=F, multiple=F)), mode="all")
  matrsp[i, ] <- degree(rewire(netsp,with = each_edge(prob=0.05,loops=F, multiple=F)), mode="all")
  matrsum[i, ] <- degree(rewire(netsum,with = each_edge(prob=0.05,loops=F, multiple=F)), mode="all")
  matrfall[i, ] <- degree(rewire(netfall,with = each_edge(prob=0.05,loops=F, multiple=F)), mode="all")
  # create a data.frame for the model to be run 
  xd <- data.frame(ID=xdataobs$id, season=xdataobs$season, liceload=xdataobs$liceload, dom=xdataobs$dominance, 
                   netw = c(matrwin[i,], matrsp[i, ],matrsum[i, ],matrfall[i, ]))
  # run the model
  tempres <- lmer(netw ~ season+liceload+dom+ (1|ID), REML=F, xd)
  # get the estimate and save it
  randest[i] <- coef(summary(tempres))[5,1]
  
  setTxtProgressBar(pb, i)
}

xd <- data.frame(ID=xdataobs$id, season=xdataobs$season, liceload=xdataobs$liceload, dom=xdataobs$dominance, 
                 netw = c(matrwin[i,], matrsp[i, ],matrsum[i, ],matrfall[i, ]))


#---------- trying code with FOX data (doesn't work yet)
# make N-by-perm matrix filled with NAs
N <-nrow(T1sprB4_attr) # Number of individuals
dwreach_randmat <- matrix(NA, nrow=N, ncol=1000) # make N-by-perm matrix filled with NAs
rownames(dwreach_randmat) <- T1sprB4_attr$ShortCode
for (col in c(N:1000)) { # for each column X (so each permuted network) (coud use col, i, j, z or any letter as long as it's consistent throughout the loop code)
  
mat_spr  <- matrix(NA, nrow= nrow(T1spr_net), ncol=1000)
mat_sum <- matrix(NA, nrow= nrow(T1sum_net), ncol=1000)
mat_aut  <- matrix(NA, nrow= nrow(T1aut_net), ncol=1000)
mat_win  <- matrix(NA, nrow= nrow(T1win_net), ncol=1000)

# empty vectors to contain model estimates for the effects of network measure on lice load
perm <- 1000
randest <- numeric(perm)
repest<-numeric(perm)
# empty vector for the pvalues for the full-null model comparison of season on network measure
pvals <- numeric(perm)
detach(package:igraph)
library(sna)
i=1
for(i in 1:1000) {
  # recalculate the network measure for each season and save the results in the results matrix
  mat_spr[,i] <- evcent(T1spr_rand, gmode="graph",use.eigen=T)
  mat_sum[,i] <- evcent(T1sum_rand, gmode="graph",use.eigen=T)
  mat_aut[,i] <- evcent(T1aut_rand, gmode="graph",use.eigen=T)
  mat_win[,i] <- evcent(T1win_rand, gmode="graph",use.eigen=T)
  # create a data.frame for the model to be run 
  xd <- data.frame(ID=tALLattr_eig$ShortCode, season=tALLattr_eig$fSeason, 
                   sex=tALLattr_eig$Sex, status=tALLattr_eig$SocialStatus,
                   netw = c(mat_spr[,i], mat_sum[,i],mat_aut[,i],mat_win[,i]))
  # run the model
  tempres <- lme4::lmer(netw ~ season+sex+status + (1|ID), REML=F, xd)
  # get the estimate and save it
  randest[i] <- coef(summary(tempres))[2,1]
  
  setTxtProgressBar(pb, i)
} # won't work as arguments imply differing number of rows: 241, 45

data.frame(netw = c(mat_spr[i,], mat_sum[i,],mat_aut[i,],mat_win[i,])) # to combine matrices
#---------------------------------------
# Back to Julie Duboscq code:

# the original model
obsmod <- lmer(obsnetdeg ~ season + liceload+dominance+(1|id), REML=F, xdataobs)
summary(obsmod)
obsmodcoef <- coef(summary(obsmod))[5,1] # coefficient on 5th row in 1st column (=liceload)

# histogram of all the estimates derived from network measure (which were based on randomized raw data)
hist(randest); abline(v=obsmodcoef, col="red", lwd=2)
# your new overall p-value telling you if the effect of season (controlling for lice load) on degree  
sum(obsmodcoef>=randest)/perm



# RANDOMISATION WITH ASNIPE PACKAGE - what I (Jo) used in the end.

#define a 4*N*N array that will hold the 4 N*N association matrices
networks<-array(0,c(20,20,4))

#store matrices in array
networks[,,1]<-xwi
networks[,,2]<-xsp
networks[,,3]<-xsu
networks[,,4]<-xfa

#calculate networks directly from array
netwin<-graph.adjacency(networks[,,1],mode="directed",diag=F,weighted=T)
netspr<-graph.adjacency(networks[,,2],mode="directed",diag=F,weighted=T)
netsum<-graph.adjacency(networks[,,3],mode="directed",diag=F,weighted=T)
netfall<-graph.adjacency(networks[,,4],mode="directed",diag=F,weighted=T)

# these networks can now be used in any package to calculate network statistics

# create matrix to store network measures
deg<-matrix(0,nrow=20,ncol=4)
# calculate network measures with igraph (with sna, no need for mode)
deg[,1]<-degree(netwin,mode="all")
deg[,2]<-degree(netsp,mode="all")
deg[,3]<-degree(netsum,mode="all")
deg[,4]<-degree(netfall,mode="all")

# calculate degree with sna
deg_weighted<-degree(networks,gmode="digraph",g=c(1,2,3,4),cmode="freeman", ignore.eval=TRUE)

# network permutation
network1_perm<-network_permutation(networks,data_format="SP", association_matrix=networks[,,1],permutations = 100)
network2_perm<-network_permutation(networks,data_format="SP", association_matrix=networks[,,2],permutations = 100)
network3_perm<-network_permutation(networks,data_format="SP", association_matrix=networks[,,3],permutations = 100)
network4_perm<-network_permutation(networks,data_format="SP", association_matrix=networks[,,4],permutations = 100)

#calculate weighted degree for each permutation
deg_weighted_perm1<-degree(network1_perm,gmode="digraph",g=c(1:100),cmode="freeman")
deg_weighted_perm2<-degree(network2_perm,gmode="digraph",g=c(1:100),cmode="freeman")
deg_weighted_perm3<-degree(network3_perm,gmode="digraph",g=c(1:100),cmode="freeman")
deg_weighted_perm4<-degree(network4_perm,gmode="digraph",g=c(1:100),cmode="freeman")

# using permutation with linear models
# build a dataset with all data in one column of dataframe 
input<-rbind(data.frame(Degree= deg_weighted[,1], Time="Winter"), data.frame(Degree=deg_weighted[,2], Time="Spring"),data.frame(Degree= deg_weighted[,3],Time="Summer"),data.frame(Degree= deg_weighted[,4],Time="Fall"))
model<-lm(Degree ~ Time, data=input)
e<-coef(summary(model))[2,1]
e_perm<-rep(NA,100)
for(i in 1:100) {
  input_perm<-rbind(data.frame(Degree=deg_weighted_perm1[,i],Time="Winter"),data.frame(Degree=deg_weighted_perm2[,i], Time="Spring"),data.frame(Degree=deg_weighted_perm3[,i],Time="Summer"),data.frame(Degree=deg_weighted_perm4[,i],Time="Fall"))
  model_tmp<-lm(Degree ~ Time,data=input_perm)
  e_perm[i]<-coef(summary(model_tmp))[2,1]
}

pvalue<-sum(e_perm>e)/100
