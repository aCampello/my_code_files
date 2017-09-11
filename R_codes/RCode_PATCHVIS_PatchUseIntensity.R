###############################################################################################
# LOOKING AT PATCH DIFFERENCES IN 'UTILIZATION INTENSITY' / 'INTENSITY OF USE'
###############################################################################################

# Calculating number of VISITORS to each patch - to look at patch preference rel. to quality*Season
patchvisitors.day <- ddply(foxdatasubset, c("Patch", "PatchID", "PatchQuality", "Season", "TRANScDate"), 
                           summarise,
                           nvisitors=length(unique(AnimalID)),
                           nvisits=length(VisDur.S),
                           totvisdur=sum(VisDur.S))
head(patchvisitors)
hist(patchvisitors.day$nvisitors) # slightly skewed but approx normal
patchvisitors.day$sqrtnvisitors<-sqrt(patchvisitors.day$nvisitors) # square-root transformation is 
# appropriate for count data
hist(patchvisitors.day$sqrtnvisitors) # data now fit a normal distribution

# Visualizing trends:
boxplot(patchvisitors.day$sqrtnvisitors~patchvisitors.day$PatchQuality)
boxplot(patchvisitors.day$sqrtnvisitors~patchvisitors.day$Season)

# If I model the sqrt-transformed nvisitors, should I also plot that in graphs instead of the 
# untransformed one?
boxplot(patchvisitors.day$nvisitors~patchvisitors.day$PatchQuality)
boxplot(patchvisitors.day$nvisitors~patchvisitors.day$Season)


# MEAN & MEDIAN NUMBER OF VISITORS, VISITS AND DURATIONS PER DAY FOR DIFFERENT PATCHES AND SEASONS
summarypatchvisitors.day <- ddply(patchvisitors.day, c("Patch", "PatchID", "PatchQuality", "Season"), summarise,
                                  mean.nvisitors=mean(nvisitors),
                                  median.nvisitors=median(nvisitors),
                                  mean.nvisits=mean(nvisits),
                                  median.nvisits=median(nvisits),
                                  mean.totvisdur=mean(totvisdur),
                                  median.totvisdur=median(totvisdur))
head(summarypatchvisitors.day)