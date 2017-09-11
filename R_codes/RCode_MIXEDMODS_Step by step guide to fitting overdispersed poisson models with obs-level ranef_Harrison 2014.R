																#################################
																#
																#
																# 	Example Code to Demonstrate How to Quantify Overdispersion in Mixed Models
																#	and fit Observation-Level Random Effects to Cope with Overdispersion
																#
																#	xav.harrison@gmail.com
																#
																#
																#################################
	
#################################
# Load Data and Fit Candidate Model
#################################

d<-read.csv("Example Data File.csv",header=T) #download the data file in the supplementary material

library(lme4)
	
m1<-glmer(y ~ bodysize + (1|popid),family="poisson",data=d)

#################################
# Quantifying Point Estimate of Overdispersion
#################################

#Function to calculate a point estimate of overdispersion from a mixed model object
od.point<-function(modelobject){
	x<-sum(resid(modelobject,type="pearson")^2)
	rdf<-summary(modelobject)$AICtab[5]
	return(x/rdf)
}

od.point(m1)
	
#################################
# Quantifying Overdispersion Through Parametric Bootstrap
#################################

#Function to pass to parametric bootstrap function 'bootMer' that calculates the sum of squared Pearson residuals (required for 'od' function)
FUN <- function(fit) {
    #return(fixef(fit))
    x<-resid(fit,type="pearson")
    return(sum(x^2))
}	

#Function To Calculate Ratio of Model SS to Mean Parametric Bootstrap SS ('bias')
od<-function(bootobject){
	biasvals<-bootobject $t0/bootobject[2]$t
	bias<-mean(biasvals,na.rm=T)
	intervals<-quantile(biasvals,c(0.025,0.975),na.rm=T)
	dat<-c(bias,intervals)
	return(dat)
}


#Parametric bootstrap of the model - requires 'FUN' from above
library(boot) #required to inspect results

m1boot<-bootMer(m1,FUN,100)
m1boot

#Calculate Dispersion Parameter - uses "OD" function above
od(m1boot)

#################################
# Model The Data with an Observation Level Random Effect
#################################

#Create a sequence of numbers corresponding to each observation (rows of the dataframe)
obs<-seq(nrow(d))

#Fit the model
m1obs<-glmer(y ~ bodysize + (1|popid) + (1|obs),family="poisson",data=d)
m1obs #Std Dev of Obs level random effect should be ~ 0.5 in this example
