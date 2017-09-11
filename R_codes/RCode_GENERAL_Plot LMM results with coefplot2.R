#=================#
#  Plotting LMMs
#=================#

# For multi-panel plotting:
par(mfrow=c(2,1))

cex.before <- par("cex") # par() gives the current settings in the R environment
par(cex = 1) #save the text size as a bit bigger by default

### use sufficiently large upper margin to make sure codes all shown in the Tukey's plot (cld)
par(mar=c(5.1,4.1,4.1,2.1)) # default R margins - bottom/left/top/right
par(mai=c(1.02,0.82,0.82,0.42)) # imai = margins in inches
par()$mar # retrieve current margin settings
#---------------------------------------------------------------------------#
par("mar"=c(1, 1, 1, 1)) #### KEEP! new margins to ensure plot fits on page
#---------------------------------------------------------------------------#

# PLOTTING LMM RESULTS OF Group size ~ Season for different definitions:

snames<-c("Summer", "Autumn", "Winter") # create vector for season names
longnames <- c("(Intercept)", snames) # specify the intercept

coefplot2(lme.SO.gsmod1, main= "Regression estimates\n\n",    # first plot regression estimates
          cex.var=1.1, cex.pts=1, varnames=longnames,
          intercept=TRUE, mar=c(1,6,5,1))                

a<-cld(summary(glht(lme.SO.gsmod1, linfct=mcp(season="Tukey")))) # save Tukeys results
a # To show which comparisons were signif. diff: if all letters shown then all levels were unique

par(mar=c(2,4,10,1)) #### KEEP!! # new margins for Tukey's plot on the bottom (so it doesnt overlap the top plot

plot(a, main="Tukey's test\n\n\n\n\n",  xaxt='n', xlab="")  # plot without axes intially
xtick<-seq(1, 4, by=1)                                      # specify where to put tick marks
seasonlabels<-c("Spring", "Summer", "Autumn", "Winter")     # list of labels for the x-axis
axis(side=1, at=xtick, labels = seasonlabels)               # draw the x axis and add labels
# letters at top of graph show similar (grouped) and different levels, 
# i.e. two levels labelled 'a' were not sig. diff. from each 
# other but were diff from 'b' or 'c'

# draw above in high resolution for publications
dev.print(jpeg, "RPlot_LMM SO group size by season.jpeg", res=800, height=14, width=10, units="in")

#----------------------------

