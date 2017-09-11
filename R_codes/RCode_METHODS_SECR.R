
#### SPATIALLY EXPLICIT CAPTURE-RECAPTURE (SECR) - to calc pop density from camera trapping ####

# didnt use this in thesis as did not aim to quantify pop density

library(secr)

setwd("G:/Statistics 2016/Methods chapter - stats and plots/Methods - PILOT STUDIES")

#load captures data for session 1
captures<-"captfilecov.txt"

#Make Sex and Status covariates into Character Vectors
Sex<-structure(captures$V5, class="Sex")
Status<-structure(captures$V6, class="Status")

foxCH <- read.capthist("captfilecov.txt", "trapfile.txt", detector = "count",
	fmt="trapID", covnames = Sex, Status, trapcovnames=TRUE, cutval=NULL,
	verify=TRUE, noncapt="NONE")


##Read in basic capture data as a capthist format. 
###If says 'error incomplete final line' just go to the txt file and press return 
after the last line of data, then re-save it###

foxCH <- read.capthist("captfilecov.txt", "trapfile.txt", detector = "multi",
	fmt= "trapID", covnames = "Sex", "Status", trapcovnames=NULL, cutval=NULL,
	verify=TRUE, noncapt="NONE")

summary(foxCH)


######DETECTOR TYPE "COUNT" GIVES TOTAL NO.VISITS THAT DAY BY ALL FOXES##########

######DETECTOR TYPE "MULTI" GIVES TOTAL NO.DIFFERENT FOXES DETECTED THAT DAY #####

##Read in my data with covariates to make a capthist object (capture history)

foxCH <- read.capthist("captfilecov.txt", "trapfile.txt", detector = "count", 
	fmt = "trapID", trapcovnames = NULL)

#Summary of fox capture history object

summary(foxCH)

#Verify any problems in the object foxCH

verify(foxCH, 2)

#view the covariates
covariates(foxCH)
#View summary of covariates (no. individuals in that category)
summary(covariates(foxCH))

#create object of type 'traps'
traps<-traps(foxCH)

#Map the locations of detectors (traps)
plot(traps, border = 100, label = TRUE, offset = c(6,6), add = FALSE,
hidetr = FALSE, detpar = list(), txtpar = list(), bg = "white",
gridlines = TRUE, gridspace = 100, gridcol = "grey",
markused = FALSE, markvarying = FALSE, markvertices = FALSE,
labelclusters = FALSE)

#suggest a buffer
detpar <- list(g0 = 0.1, sigma = 25)
suggest.buffer(foxCH, "halfnormal", detpar, 5)

#Create the habitat mask
mask<-make.mask(traps, buffer = 100, type = "trapbuffer")

#Plot the mask over the detector points
plot(mask, add=TRUE)
summary.capthist(foxCH)


######fit different secr models to find the best fit#########

secr0 <- secr.fit (foxCH, model = g0~1,
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)

secr.g0b <- secr.fit (foxCH, model = g0~b,
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)

secr.g0bsigma <- secr.fit (foxCH, model = list(g0~b, sigma~b),
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)

secr.g0h2 <- secr.fit (foxCH, model = list(g0~h2),
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)

secr.g0bT <- secr.fit (foxCH, model = list(g0~b+T),
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)

secr.Dgg0g <- secr.fit (foxCH, model = list(list(D~g, g0~g)),
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)


####Compare model fits using the AIC (lower is better)####

AIC(secr0, secr.g0b, secr.g0bsigma, secr.g0h2, secr.g0bT, secr.Dgg0g)


#Check that the likelihood is stable while varying the mask buffer width (rows) 
#and spacing (columns) - lower is better
mask.check(secr0)
?mask.check


###Best model is g0~h2 (2-class fitted mixture model)
####Run this model again####

secr.g0h2 <- secr.fit (foxCH, model = list(g0~h2),
    mask = mask, buffer = 100, CL = FALSE, detectfn = NULL,
    binomN = 0, start = NULL, link = list(), fixed = list(),
    timecov = 40, sessioncov = NULL, hcov = NULL, groups = NULL,
    dframe = NULL, details = list(), method = "Newton-Raphson",
    verify = TRUE, biasLimit = 0.01, trace = NULL, ncores = 1)

##D estimate shows density per hectare, with 95% c.i. lcl (lower) and ucl (upper)##


#Plot the model
plot(secr.g0h2, newdata = NULL, add = FALSE,
sigmatick = FALSE, rgr = FALSE, limits = FALSE, alpha = 0.05,
xval = 0:200, ylim = NULL, xlab = NULL, ylab = "Detection Probability")

#Show predicted values model
predict(secr.g0h2)