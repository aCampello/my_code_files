#=========================================#
##  CLOSENESS CENTRALITY (weighted)
#=========================================#

# Take subset of core animals only, in surveys where animals associated non-randomly
centrality_core <- subset(centrality, prefavoided==1 & SocialStatus!="NA" & Core==1)

# response variable: closeness.norm 


#-----------------------------
# CHECK DISTRIBUTION OF DATA
#-----------------------------
library(fitdistrplus)
# Cullen & Frey graph:
descdist(centrality_core$closeness.norm, discrete = FALSE, boot=5000) # distrib uniform (square)

# Plot data against normal
fit.norm <- fitdist(centrality_core$closeness.norm, "norm")
plot(fit.norm) # looks OK
fit.lnorm <- fitdist(centrality_core$closeness.norm, "lnorm") # logging wouldn't help
plot(fit.lnorm)
require(MASS)
require(car)
qqp((centrality_core$closeness.norm), "norm", main="normal") # approx normal


#--------
# MODELS
#--------

# LMM 
clo_mod_full  <- lmer(closeness.norm ~ Sex*SocialStatus*fSeason +
                        (1|fTerritory) + (1|ShortCode),
                      data=centrality_core, REML=F, verbose=T,
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=1000000),
                                          check.conv.grad=.makeCC("warning",0.05)))
#= Check residuals:
plot(clo_mod_full) # OK
plot(fitted(clo_mod_full), resid(clo_mod_full))   
lines(smooth.spline(fitted(clo_mod_full), resid(clo_mod_full))) # GOOD!

# check ranefs
lattice::dotplot(ranef(clo_mod_full, condVar=TRUE)) # territory 1 and 6 quite diff to others. 

#-------------------------
#== Model simplification
#-------------------------
car::Anova(clo_mod_full) # Season and status are sig

# 3X 2-way interactions
clo_mod_twowayints  <- lmer(closeness.norm ~ Sex*SocialStatus + Sex*fSeason + SocialStatus*fSeason +
                              (1|fTerritory) + (1|ShortCode),
                            data=centrality_core, REML=F, verbose=T,
                            control=lmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=1000000),
                                                check.conv.grad=.makeCC("warning",0.05)))
anova(clo_mod_full, clo_mod_twowayints) # no diff without three-way so can remove
car::Anova(clo_mod_twowayints)

# without sex*seas as lowest significance
clo_mod_nosexseas  <- lmer(closeness.norm ~ Sex*SocialStatus + fSeason*SocialStatus +
                             (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F, verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))
car::Anova(clo_mod_nosexseas)
anova(clo_mod_twowayints, clo_mod_nosexseas)

# no sex-stat interaction as lowest significance
clo_mod_nosexstat  <- lmer(closeness.norm ~ Sex + SocialStatus*fSeason +
                             (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F, verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))
car::Anova(clo_mod_nosexstat)
anova(clo_mod_nosexseas, clo_mod_nosexstat)

# no interactions
clo_mod_noints  <- lmer(closeness.norm ~ Sex + SocialStatus + fSeason +
                          (1|fTerritory) + (1|ShortCode),
                        data=centrality_core, REML=F, verbose=T,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=1000000),
                                            check.conv.grad=.makeCC("warning",0.05)))
car::Anova(clo_mod_noints)
anova(clo_mod_nosexstat, clo_mod_noints)

# no sex
clo_mod_nosex  <- lmer(closeness.norm ~ SocialStatus + fSeason +
                         (1|fTerritory) + (1|ShortCode),
                       data=centrality_core, REML=F, verbose=T,
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=1000000),
                                           check.conv.grad=.makeCC("warning",0.05)))
car::Anova(clo_mod_nosex) # <- FINAL MODEL: all terms significant
anova(clo_mod_noints, clo_mod_nosex)


bbmle::ICtab(clo_mod_full,
             clo_mod_twowayints,
             clo_mod_nosexseas,
             clo_mod_nosexstat, 
             clo_mod_noints, 
             clo_mod_nosex) # nosex is best, then noints but these are all non-sig so ignore as simpler models are better


# FINAL MODEL FOR CLOSENESS (NORMALISED 0-1)
clo_mod <- lmer(closeness.norm ~ 
                  SocialStatus + 
                  fSeason +
                  (1|fTerritory) + (1|ShortCode),
                data=centrality_core, REML=F, verbose=T,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl = list(maxfun=1000000),
                                    check.conv.grad=.makeCC("warning",0.05)))

#----------------------
# FINAL MODEL CHECKING
#----------------------
plot(clo_mod)
plot(fitted(clo_mod), resid(clo_mod))
lines(smooth.spline(fitted(clo_mod), resid(clo_mod))) # ignore! just cos clusters of common values
hist(resid(clo_mod))
visreg::visreg(clo_mod) 
lattice::dotplot(ranef(clo_mod, condVar=TRUE))

# Check for overdispersion
overdisp_fun <- function(model) {
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}
overdisp_fun(clo_mod) # p-value: If <0.05, data are overdispersed. Mine=1, not overdispersed.


#---------------------------
#== SAVE MODEL COEFFICIENTS 
#---------------------------
clo_mod_coeffs <- coef(summary(clo_mod))  # save as DF
ranef(clo_mod) # to see order of random effects: ShortCode is first (sig01), then Territory
clo_mod_cis <- confint(clo_mod)
summary(clo_mod) # get variance and SD for random effects


#----------------------------------------------------------------------------------------------
#== LR TESTS TO GET X2 AND P-VALUES FOR NON-SIG TERMS BY ADDING EACH TERM TO THE MINIMAL MODEL
#----------------------------------------------------------------------------------------------

# 1. Compare minimal signif mod without each sig term to get chisq values:
# without status
clo_mod_nostatus <- lmer(closeness.norm ~ 
                           fSeason +
                           (1|fTerritory) + (1|ShortCode),
                         data=centrality_core, REML=F, verbose=T,
                         control=lmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=1000000),
                                             check.conv.grad=.makeCC("warning",0.05)))
# without season
clo_mod_noseas <- lmer(closeness.norm ~ 
                         SocialStatus +
                         (1|fTerritory) + (1|ShortCode),
                       data=centrality_core, REML=F, verbose=T,
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=1000000),
                                           check.conv.grad=.makeCC("warning",0.05)))
anova(clo_mod, clo_mod_nostatus)
anova(clo_mod, clo_mod_noseas)

# 2. Compare minimal signif mod with each non-sig term to get chisq values:
# with sex added
clo_mod_withsex <- lmer(closeness.norm ~ Sex +
                          SocialStatus +
                          fSeason +
                          (1|fTerritory) + (1|ShortCode),
                        data=centrality_core, REML=F, verbose=T,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=1000000),
                                            check.conv.grad=.makeCC("warning",0.05)))

clo_mod_withsex <- lmer(closeness.norm ~
                          SocialStatus*fSeason +
                          (1|fTerritory) + (1|ShortCode),
                        data=centrality_core, REML=F, verbose=T,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=1000000),
                                            check.conv.grad=.makeCC("warning",0.05)))
anova(clo_mod,clo_mod_withsex)
anova(clo_mod,clo_mod_withsex)



# POST HOC
#----------------------------------
clo_mod_contrasts <- rbind(data.frame(summary(lsmeans::lsmeans(clo_mod, pairwise~fSeason))$contrasts),
                           data.frame(summary(lsmeans::lsmeans(clo_mod, pairwise~SocialStatus))$contrasts))



# PLOT
#----------------------------------
clo_mod_lsm <- data.frame(summary(lsmeans::lsmeans(clo_mod, pairwise~fSeason|SocialStatus))$lsmeans)
clo_mod_lsm$Season <- ordered(clo_mod_lsm$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

library(ggplot2)
ggp <- ggplot(clo_mod_lsm, aes(x=Season, y=lsmean, group=SocialStatus, colour=factor(SocialStatus))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=SocialStatus), size=1, position=position_dodge(0.2)) + 
  geom_point(size=4, position=position_dodge(0.2)) + 
  scale_colour_manual(name="Social\nstatus", values=c('royalblue3','firebrick1')) + # rename legend
  xlab("") + ylab("Closeness centrality\n") +
  geom_errorbar(mapping=aes(x=Season, ymin=lower.CL, ymax=upper.CL),
                width=0.2, size=0.8, position=position_dodge(0.2)) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black")) 

# save above in high res.
dev.print(jpeg, "RPlot_LMM predictions for closeness season & status CIs.jpeg", 
          res=800, height=6, width=10, units="in") 




# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA
#--------------------------------------------------------------
library(sna)

# set up progress bar
perm <- 1000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrices to store the coefficient estimates from each randomised matrix
clo_status_perm    <-rep(NA,perm) 
clo_seasonSUM_perm <-rep(NA,perm)
clo_seasonAUT_perm <-rep(NA,perm)
clo_seasonWIN_perm <-rep(NA,perm)
# 2 more matrixes to store Anova p-values:
clo_status_aovP_perm    <-rep(NA,perm)
clo_seas_aovP_perm      <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_clo <- rbind(data.frame(closeness.norm = T1spr_rand_closeness[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_closeness)),
                     data.frame(closeness.norm = T1sum_rand_closeness[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_closeness)), 
                     data.frame(closeness.norm = T1aut_rand_closeness[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_closeness)),
                     data.frame(closeness.norm = T1win_rand_closeness[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_closeness)),
                     
                     data.frame(closeness.norm = T2spr_rand_closeness[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_closeness)),
                     #data.frame(closeness.norm = T2sum_rand_closeness[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_closeness)), # no pref/avoided assocs
                     data.frame(closeness.norm = T2aut_rand_closeness[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_closeness)), 
                     data.frame(closeness.norm = T2win_rand_closeness[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_closeness)),
                     
                     data.frame(closeness.norm = T3spr_rand_closeness[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_closeness)),
                     data.frame(closeness.norm = T3sum_rand_closeness[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_closeness)),
                     data.frame(closeness.norm = T3aut_rand_closeness[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_closeness)),
                     data.frame(closeness.norm = T3win_rand_closeness[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_closeness)),
                     
                     #data.frame(closeness.norm = T4spr_rand_closeness[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_closeness)), # no pref/avoided assocs
                     #data.frame(closeness.norm = T4sum_rand_closeness[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_closeness)), # no pref/avoided assocs
                     data.frame(closeness.norm = T4aut_rand_closeness[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_closeness)),
                     data.frame(closeness.norm = T4win_rand_closeness[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_closeness)),
                     
                     data.frame(closeness.norm = T5spr_rand_closeness[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_closeness)),
                     data.frame(closeness.norm = T5sum_rand_closeness[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_closeness)),
                     data.frame(closeness.norm = T5aut_rand_closeness[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_closeness)),
                     #data.frame(closeness.norm = T5win_rand_closeness[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_closeness)), # no pref/avoided assocs
                     
                     data.frame(closeness.norm = T6spr_rand_closeness[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_closeness)), 
                     data.frame(closeness.norm = T6sum_rand_closeness[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_closeness)), 
                     data.frame(closeness.norm = T6aut_rand_closeness[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_closeness)), 
                     data.frame(closeness.norm = T6win_rand_closeness[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_closeness)),
                     
                     data.frame(closeness.norm = T7spr_rand_closeness[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_closeness)),
                     data.frame(closeness.norm = T7sum_rand_closeness[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_closeness)), 
                     data.frame(closeness.norm = T7aut_rand_closeness[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_closeness)),
                     data.frame(closeness.norm = T7win_rand_closeness[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_closeness)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_clo$id        <- as.character(input_clo$id)
  input_clo$SeasonID  <- as.character(input_clo$SeasonID)
  input_clo$Territory <- as.character(input_clo$Territory)
  # Add attributes to input_clo
  input_clo$ShortCode    <- attribs_char[match(input_clo$id, attribs_char$id),4] 
  input_clo$SocialStatus <- attribs_char[match(input_clo$id, attribs_char$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_clo, y=attribs_char[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$SocialStatus <- factor(df$SocialStatus)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <- lmer(closeness.norm ~ SocialStatus +  fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  clo_status_perm[i]    <-coef(summary(model_tmp))[2,1] 
  clo_seasonSUM_perm[i] <-coef(summary(model_tmp))[3,1]
  clo_seasonAUT_perm[i] <-coef(summary(model_tmp))[4,1]
  clo_seasonWIN_perm[i] <-coef(summary(model_tmp))[5,1]
  # Get Anova p-value and save it
  clo_status_aovP_perm[i] <- data.frame(car::Anova(model_tmp))[1,3] # for status
  clo_seas_aovP_perm[i]   <- data.frame(car::Anova(model_tmp))[2,3] # for season
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
clo_status_obs    <- coef(summary(clo_mod))[2,1]
clo_seasonSUM_obs <- coef(summary(clo_mod))[3,1]
clo_seasonAUT_obs <- coef(summary(clo_mod))[4,1]
clo_seasonWIN_obs <- coef(summary(clo_mod))[5,1]

clo_status_Pvalue    <- sum(clo_status_perm>clo_status_obs)/1000 # = 0.091
clo_seasonSUM_Pvalue <- sum(clo_seasonSUM_perm>clo_seasonSUM_obs)/1000 # = 0.93
clo_seasonAUT_Pvalue <- sum(clo_seasonAUT_perm>clo_seasonAUT_obs)/1000 # = 0.643
clo_seasonWIN_Pvalue <- sum(clo_seasonWIN_perm>clo_seasonWIN_obs)/1000 # = 0.921


# ??? ALTERNATIVE - get 'coefficients' from ANOVA table (actually chisq values)
clo_status_aovP_obs <- data.frame(car::Anova(clo_mod))[1,3]
clo_seas_aovP_obs   <- data.frame(car::Anova(clo_mod))[2,3]

clo_status_aovPvalue <- sum(clo_status_aovP_perm>clo_status_aovP_obs)/1000 # = 0.49
clo_seas_aovP  <- sum( clo_seas_aovP_perm>clo_seas_aovP_obs)/1000 # = 0

# Histogram of random coefficients with red line for real
hist(clo_status_perm,breaks=100, main=paste("P = ", sum(clo_status_perm>clo_status_obs)/1000),
     xlab="Effect of status on closeness", ylab="Probability") 
abline(v=clo_status_obs, col="red")


hist(clo_seasonSUM_perm,breaks=100, main=paste("P = ", sum(clo_seasonSUM_perm>clo_seasonSUM_obs)/1000),
     xlab="Effect of status on closeness", ylab="Probability") 
abline(v=clo_seasonSUM_obs, col="red")

