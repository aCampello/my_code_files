
### What distribution best fits my data?

require(car)
require(MASS)

# Drawing distributions in qqp:
# y axis: observations
# x axis: quantiles modeled by the distribution
# The solid red line represents a perfect distribution fit
# The dashed red lines are the confidence intervals of the perfect distribution fit
# Pick the distribution for which the largest number of observations falls between the dashed lines

qqp(duration_data_sub$duration.s, "norm", main="normal") # would require lmer with family = gaussian
qqp(duration_data_sub$duration.s, "lnorm", main="lognormal") # would require lmer with log or log10(response)

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter:
nbinom <- fitdistr(duration_data_sub$duration.s, "Negative Binomial")
qqp(duration_data_sub$duration.s, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]], main="negative binomial")

poisson <- fitdistr(duration_data_sub$duration.s, "Poisson")
qqp(duration_data_sub$duration.s, "pois", poisson$estimate, main="poisson")

gamma <- fitdistr(duration_data_sub$duration.s, "gamma", start=NULL)
qqp(duration_data_sub$duration.s, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]], main="gamma")

weibull <- fitdistr(duration_data_sub$duration.s, "weibull")
qqp(duration_data_sub$duration.s, "weibull", shape = weibull$estimate[[1]], scale = weibull$estimate[[2]], main="weibull")

expo <- fitdistr(duration_data_sub$duration.s, "exponential")
qqp(duration_data_sub$duration.s, "exp", expo$estimate, main="exponential")


# can also use:
library(fitdistrplus) # to plot model residuals
plot(fitdist(residuals(model1),"norm"))  # seems to only work with lme4 models
# indicates which distributions best fit the data

#plot cullen & frey graph to indicate influential datapoints that may be outliers
descdist(residuals(model1), boot = 1000) # boot means number of bootstrap estimates to run

# Plot data against normal, logorm, gamma and weibull distributions
fit.norm <- fitdist(strength_centrality_core$strength, "norm")
fit.lnorm <- fitdist(strength_centrality_core$strength+1, "lnorm")
fit.gam <- fitdist(strength_centrality_core$strength+1, "gamma") # GAMMA wont work unless add 1 to data as log(0) or log10(0) = infinite
fit.weibull <- fitdist(strength_centrality_core$strength+1, "weibull") # weibull can't take zeros so added 1 to y

plot(fit.norm)
plot(fit.weibull)
plot(fit.gam)
plot(fit.lnorm)
