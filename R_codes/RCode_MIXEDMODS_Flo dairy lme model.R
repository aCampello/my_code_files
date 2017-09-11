library(randtoolbox)
library(fitdistrplus)
library(psych)
library(nlme)
library(lme4)
library(car)
library(MuMIn)
library(visreg)
library(arm)
library(sjPlot)
library(RLRsim)
library(lmerTest)

### Organise the data #####
length(names(Raw_r))
names(Raw_r)

# Make factors
class(Raw_r$farm)
class(Raw_r$year)
class(as.factor(Raw_r$year))

## Make numeric
for (i in c(6:14, 21:212, 217)) {
     if (is.factor(Raw_r[,i])) {
          TRUE
          Raw_r[,i] <- as.numeric(as.character(Raw_r[,i]))
     }
}

## Set up the management variables
## Normalise all the data
## Use annual data for individual farms
## Include farm as a repeated measure to account for this
## Include herd size (herdDoe) as a covariate (interaction with each varaiable) to account for differences in herd size


herdDoe <- with(Raw_r, as.vector(tapply(herdDoe, list(year, farm), median, na.rm=T)))
herdDoe[herdDoe==0] <- 50
herdDoe_ <- (herdDoe-min(herdDoe))/(max(herdDoe)-min(herdDoe))

dairy <- with(Raw_r, as.vector(tapply(incMilkVol+incCheeseL+fsMilk.l+(fsCheese.kg*cheeseL.kg), list(year, farm), sum)))
dairy_ <- (dairy-min(dairy))/(max(dairy)-min(dairy))

dairy.doe <- dairy/herdDoe
dairy.doe_ <- (dairy.doe-min(dairy.doe))/(max(dairy.doe)-min(dairy.doe))

sales <- with(Raw_r, as.vector(tapply(as.numeric(as.character(incDoe.head)) + as.numeric(as.character(incBuck.head)) + as.numeric(as.character(incJuvM.head)) + as.numeric(as.character(incJuvF.head)) + as.numeric(as.character(incKidF.head)) + as.numeric(as.character(incKidM.head))
                                      , list(year, farm), sum)))
sales_ <- (sales-min(sales))/(max(sales)-min(sales))

sales.doe <- sales/herdDoe
sales.doe_ <- (sales.doe-min(sales.doe))/(max(sales.doe)-min(sales.doe))

meat <- with(Raw_r, as.vector(tapply(incJuvM.kgTot+incJuvF.kgTot+incKidM.kgTot+incKidF.kgTot+incBuck.kgTot+incDoe.kgTot+
                                          fsJuvM.kgTot+fsJuvF.kgTot+fsKidM.kgTot+fsKidF.kgTot+fsBuck.kgTot+fsDoe.kgTot
                                     , list(year, farm), sum, na.rm=T)))
meat_ <- (meat-min(meat))/(max(meat)-min(meat))
meat.doe <- meat/herdDoe
meat.doe_ <- (meat.doe-min(meat.doe))/(max(meat.doe)-min(meat.doe))

landFeed <- as.vector(with(Raw_r, tapply(landLegume, list(year, farm), median, na.rm=T) + tapply(landArable.feed, list(year, farm), median, na.rm=T)))
landFeed_ <- (landFeed-min(landFeed))/(max(landFeed)-min(landFeed))

grazing <- with(Raw_r, as.vector(tapply(as.numeric(as.character(grazingHours))*30, list(year, farm), sum, na.rm=T)))
grazing[25] <- (grazing[26]+grazing[27])/2
grazing[28] <- (grazing[29]+grazing[30])/2
grazing_ <- (grazing-min(grazing))/(max(grazing)-min(grazing))

feedConc <- with(Raw_r, as.vector(tapply(feedConcLact.tot+feedConcDry.tot+feedConcBuck.tot+feedConcJuv.tot, list(year, farm), sum, na.rm=T)))
feedConc_ <- (feedConc-min(feedConc))/(max(feedConc)-min(feedConc))
feedConc.doe <- feedConc/herdDoe
feedConc.doe_ <- (feedConc.doe-min(feedConc.doe))/(max(feedConc.doe)-min(feedConc.doe))
feedConc2 <- with(Raw_r, as.vector(tapply(feedConcLact.tot+feedConcDry.tot, list(year, farm), sum, na.rm=T)))
feedConc2_ <- (feedConc2-min(feedConc2))/(max(feedConc2)-min(feedConc2))
feedConc.doe2 <- feedConc2/herdDoe
feedConc.doe2_ <- (feedConc.doe2-min(feedConc.doe2))/(max(feedConc.doe2)-min(feedConc.doe2))

Anth <- with(Raw_r, as.vector(tapply(Anthelmintic, list(year, farm), sum, na.rm=T)))
Anth_ <- (Anth-min(Anth))/(max(Anth)-min(Anth))

fertility <- with(Raw_r, (tapply(reproTriplets+reproTwins+reproSingles, list(year,farm), sum)
                          +  tapply(reproAbort, list(year, farm), sum))
                  /  tapply(herdDoe, list(year, farm), max, na.rm=T)
)
fertility[fertility>1] <- 1
fertility_ <- (fertility-min(fertility))/(max(fertility)-min(fertility))

reproductivity <- with(Raw_r, (tapply(reproKidsTot, list(year, farm), sum)
                               / (tapply(reproTriplets+reproTwins+reproSingles, list(year,farm), sum)+tapply(reproAbort, list(year, farm), sum))
))
reproductivity_ <- (reproductivity-min(reproductivity))/(max(reproductivity)-min(reproductivity))

fecundity <- fertility * reproductivity
fecundity_ <- (fecundity-min(fecundity))/(max(fecundity)-min(fecundity))

newBirthRate <- 1-with(Raw_r, ((tapply(reproMortLess8, list(year, farm), sum)
                                + tapply(reproMortMore8, list(year, farm), sum))
                               /tapply(reproKidsTot, list(year, farm), sum)))
newBirthRate_ <- (newBirthRate-min(newBirthRate))/(max(newBirthRate)-min(newBirthRate))

kidMort <- with(Raw_r, ((tapply(reproMortLess8, list(year, farm), sum)
                         + tapply(reproMortMore8, list(year, farm), sum))
                        /tapply(reproKidsTot, list(year, farm), sum)))
kidMort_ <- as.vector((kidMort-min(kidMort))/(max(kidMort)-min(kidMort)))

reproRate <- as.vector(fecundity * newBirthRate)
reproRate_ <- (reproRate-min(reproRate))/(max(reproRate)-min(reproRate))

loss <- with(Raw_r,
             tapply((mortDoe + mortBuck + mortJuvF + mortJuvM
                     + incDoe.head + incBuck.head + incJuvM.head + incJuvF.head + incKidF.head + incKidM.head
                     + fsDoe.head + fsBuck.head + fsJuvM.head + fsJuvF.head + fsKidF.head + fsKidM.head)
                    , list(year, farm), sum, na.rm=T)
             /tapply(herdTotal, list(year, farm), max, na.rm=T)
)
loss_ <- (loss-min(loss))/(max(loss)-min(loss))

reproLifeDoe <- as.vector(1/
                               (with(Raw_r,
                                     tapply((mortDoe + incDoe.head + fsDoe.head), list(year, farm), sum, na.rm=T))/herdDoe))
reproLifeDoe[1] <- mean(c(reproLifeDoe[2], reproLifeDoe[3]))
reproLifeDoe[25] <- mean(c(reproLifeDoe[26], reproLifeDoe[27]))
reproLifeDoe_ <- (reproLifeDoe-min(reproLifeDoe, na.rm=T))/(max(reproLifeDoe, na.rm=T)-min(reproLifeDoe, na.rm=T))

doeCullRate <- as.vector((with(Raw_r, tapply((mortDoe + incDoe.head + fsDoe.head), list(year, farm), sum, na.rm=T))/herdDoe))
doeCullRate[doeCullRate > 1] <- 1
doeCullRate_ <- (doeCullRate-min(doeCullRate))/(max(doeCullRate)-min(doeCullRate))

replRateDoe <- as.vector(loss)/reproRate
replRateDoe_ <- (replRate-min(replRate))/(max(replRate)-min(replRate))


labour <- as.vector(with(Raw_r, tapply((as.numeric(as.character(labourFamily.goat))/7*30)+ (as.numeric(as.character(labourPaid.goat))/7*30), list(year, farm), sum, na.rm=T)))
labour_ <- (labour-min(labour))/(max(labour)-min(labour))


incMeat <- with(Raw_r, as.vector(tapply(incMeat, list(year, farm), sum, na.rm=T)))
incMeat.doe <- with(Raw_r, as.vector(tapply(incMeat, list(year, farm), sum, na.rm=T)))/herdDoe
incMeat.doe_ <- (incMeat.doe-min(incMeat.doe))/(max(incMeat.doe)-min(incMeat.doe))


gm <- with(Raw_r, as.vector(tapply(incTot-costsTotal, list(year, farm), sum, na.rm=T)))
gm_ <- (gm-min(gm))/(max(gm)-min(gm))
gm.doe <- round(gm/herdDoe,0)
gm.doe_ <- (gm.doe-min(gm.doe))/(max(gm.doe)-min(gm.doe))

farm <- as.factor(as.vector(c("A","A","A","B","B","B","C","C","C","D","D","D","E","E","E","G","G","G","H","H","H","I","I","I","J","J","J","L","L","L")))
month <- as.factor(rep(c(8,9,10,11,12,1,2,3,4,5,6,7), 30))
year <- as.factor(as.vector(rep(c(1,2,3), 10)))
produce <- as.factor(as.vector(c(rep("Cheese", 3), rep("Milk", 12), rep("Cheese", 6), rep("none", 9))))

## Plot the variables to check for linear associations
pairs(data.frame(cbind(herdDoe_, grazing_, feedConc.doe_, Anth, doeCullRate_, labour_, reproRate_, kidMort_)), panel=panel.smooth)

cor(data.frame(cbind(herdDoe_, grazing_, feedConc.doe_, Anth, doeCullRate_, labour_, reproRate_, kidMort_)), method="spearman")
# Plot these and add smoothers
lo <- lowess(labour_ ~ herdDoe_) # linear relationship
plot(labour_ ~ herdDoe_)
lines(lo$y ~ lo$x)

lo <- lowess(doeCullRate_ ~ grazing_) # probably not a relationship
plot(doeCullRate_ ~ grazing_)
lines(lo$y ~ lo$x)

lo <- lowess(labour_ ~ feedConc.doe_) # probably not a relationship
plot(labour_ ~ feedConc.doe_)
lines(lo$y ~ lo$x)

lo <- lowess(kidMort_ ~ herdDoe_) # possible relationship, non-linear
plot(kidMort_ ~ herdDoe_)
lines(lo$y ~ lo$x)

lo <- lowess(kidMort_ ~ reproRate_) # linear relationship
plot(kidMort_ ~ reproRate_)
lines(lo$y ~ lo$x)


dairy.doe.mod1 <- lmer(dairy.doe_ ~ 
                            kidMort_
                       + reproRate_
                       + kidMort_:reproRate_
                       + (1|farm)
                       , na.action="na.pass"
                       , REML=F)
anova(dairy.doe.mod1)

dairy.doe.mod2 <- lmer(dairy.doe_ ~ 
                            kidMort_
                       + reproRate_
                       + (1|farm)
                       , na.action="na.pass"
                       , REML=F)
anova(dairy.doe.mod2)

dairy.doe.mod3 <- lmer(dairy.doe_ ~ 
                            reproRate_
                       + (1|farm)
                       , na.action="na.pass"
                       , REML=F)
anova(dairy.doe.mod3)
## No significant influence, therefore consider the variables under the farmers control only


dairy.doe.mod4 <- lmer(dairy.doe_ ~ 
                            herdDoe_
                       + produce
                       + grazing_
                       + feedConc.doe_
                       + Anth
                       + doeCullRate_
                       + labour_
                       + produce:feedConc.doe_
                       + produce:doeCullRate_
                       + herdDoe_:labour_
                       + (1|farm)
                       , na.action="na.pass"
                       , REML=F)
anova(dairy.doe.mod4)

dairy.doe.mod_set <- dredge(dairy.doe.mod4)
(top_dairy.doe.mod <- get.models(dairy.doe.mod_set, subset=delta<3))
model.avg(top_dairy.doe.mod)$avg.model
model.avg(top_dairy.doe.mod)$importance
model.sel(top_dairy.doe.mod)
get.models(dairy.doe.mod_set, subset = 1)[[1]]

dairy.doe.mod <- get.models(dairy.doe.mod_set, subset = 1)[[1]]
par(mfrow=c(2,2))
scatter.smooth(residuals(dairy.doe.mod)
               ~fitted(dairy.doe.mod))
hist(residuals(dairy.doe.mod))
qqnorm(residuals(dairy.doe.mod))
qqline(residuals(dairy.doe.mod))
descdist(residuals(dairy.doe.mod))
## Histogram not normal - skew

dairy.doe.mod2 <- get.models(dairy.doe.mod_set, subset = 1)[[2]]
par(mfrow=c(2,2))
scatter.smooth(residuals(dairy.doe.mod2)
               ~fitted(dairy.doe.mod2))
hist(residuals(dairy.doe.mod2))
qqnorm(residuals(dairy.doe.mod2))
qqline(residuals(dairy.doe.mod2))
descdist(residuals(dairy.doe.mod2))
## Histogram not normal - more skew

dairy.doe.mod3 <- get.models(dairy.doe.mod_set, subset = 1)[[3]]
par(mfrow=c(2,2))
scatter.smooth(residuals(dairy.doe.mod3)
               ~fitted(dairy.doe.mod3))
hist(residuals(dairy.doe.mod3))
qqnorm(residuals(dairy.doe.mod3))
qqline(residuals(dairy.doe.mod3))
descdist(residuals(dairy.doe.mod3))
## not normal

dairy.doe.mod4 <- get.models(dairy.doe.mod_set, subset = 1)[[4]]
par(mfrow=c(2,2))
scatter.smooth(residuals(dairy.doe.mod4)
               ~fitted(dairy.doe.mod4))
hist(residuals(dairy.doe.mod4))
qqnorm(residuals(dairy.doe.mod4))
qqline(residuals(dairy.doe.mod4))
descdist(residuals(dairy.doe.mod4))
# more normal

dairy.doe.mod5 <- get.models(dairy.doe.mod_set, subset = 1)[[5]]
par(mfrow=c(2,2))
scatter.smooth(residuals(dairy.doe.mod5)
               ~fitted(dairy.doe.mod5))
hist(residuals(dairy.doe.mod5))
qqnorm(residuals(dairy.doe.mod5))
qqline(residuals(dairy.doe.mod5))
descdist(residuals(dairy.doe.mod5))
# more normal still

dairy.doe.mod6 <- get.models(dairy.doe.mod_set, subset = 1)[[6]]
par(mfrow=c(2,2))
scatter.smooth(residuals(dairy.doe.mod6)
               ~fitted(dairy.doe.mod6))
hist(residuals(dairy.doe.mod6))
qqnorm(residuals(dairy.doe.mod6))
qqline(residuals(dairy.doe.mod6))
descdist(residuals(dairy.doe.mod6))