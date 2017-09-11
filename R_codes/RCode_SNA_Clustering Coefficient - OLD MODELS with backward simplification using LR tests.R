
#============================#
##  CLUSTERING COEFFICIENT
#============================#

# Take subset of core animals only, in surveys where animals associated non-randomly
centrality_core <- subset(centrality, prefavoided==1 & SocialStatus!="NA" & Core==1)

# response variable: cc

#-----------------------------
# CHECK DISTRIBUTION OF DATA
#-----------------------------
hist(centrality_core$cc)
require(MASS)
require(car)
qqp((centrality_core$cc), "norm", main="normal") # approx normal


#--------
# MODELS
#--------

# LMM 
CC_mod_full  <- lmer(cc ~ Sex*SocialStatus*fSeason +
                       (1|fTerritory) + (1|ShortCode),
                     data=centrality_core, REML=F, verbose=T,
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=1000000),
                                         check.conv.grad=.makeCC("warning",0.05)))
#= Check residuals:
plot(CC_mod_full) # OK
plot(fitted(CC_mod_full), resid(CC_mod_full))   
lines(smooth.spline(fitted(CC_mod_full), resid(CC_mod_full))) # GOOD!

# check ranefs
lattice::dotplot(ranef(CC_mod_full, condVar=TRUE)) # territory 1 and 6 quite diff to others. 

#-------------------------
#== Model simplification
#-------------------------
car::Anova(CC_mod_full) # Season and status are sig
summary(CC_mod_full)
# 3X 2-way interactions
CC_mod_twowayints  <- lmer(cc ~ Sex*SocialStatus + Sex*fSeason + SocialStatus*fSeason +
                             (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F, verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_full, CC_mod_twowayints) # no diff without three-way so can remove
car::Anova(CC_mod_twowayints)

# without sex*seas as lowest significance
CC_mod_nosexseas  <- lmer(cc ~ Sex*SocialStatus + fSeason*SocialStatus +
                            (1|fTerritory) + (1|ShortCode),
                          data=centrality_core, REML=F, verbose=T,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=1000000),
                                              check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_twowayints, CC_mod_nosexseas)
car::Anova(CC_mod_nosexseas)

# no sex-stat interaction as lowest significance
CC_mod_nosexstat  <- lmer(cc ~ Sex + SocialStatus*fSeason +
                            (1|fTerritory) + (1|ShortCode),
                          data=centrality_core, REML=F, verbose=T,
                          control=lmerControl(optimizer="bobyqa",
                                              optCtrl = list(maxfun=1000000),
                                              check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_nosexseas, CC_mod_nosexstat)
car::Anova(CC_mod_nosexstat)

# no interactions
CC_mod_noints  <- lmer(cc ~ Sex + SocialStatus + fSeason +
                         (1|fTerritory) + (1|ShortCode),
                       data=centrality_core, REML=F, verbose=T,
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=1000000),
                                           check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_nosexstat, CC_mod_noints)
car::Anova(CC_mod_noints)

# no sex
CC_mod_nosex  <- lmer(cc ~ SocialStatus + fSeason +
                        (1|fTerritory) + (1|ShortCode),
                      data=centrality_core, REML=F, verbose=T,
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=1000000),
                                          check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_noints, CC_mod_nosex)
car::Anova(CC_mod_nosex) 

# no status
CC_mod_nostatus  <- lmer(cc ~ fSeason +
                           (1|fTerritory) + (1|ShortCode),
                         data=centrality_core, REML=F, verbose=T,
                         control=lmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=1000000),
                                             check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_nosex, CC_mod_nostatus)
car::Anova(CC_mod_nostatus) # <- FINAL MODEL: all terms significant

CC_mod_NULL  <- lmer(cc ~ (1|fTerritory) + (1|ShortCode),
                     data=centrality_core, REML=F, verbose=T,
                     control=lmerControl(optimizer="bobyqa",
                                         optCtrl = list(maxfun=1000000),
                                         check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod_nostatus, CC_mod_NULL)

bbmle::ICtab(CC_mod_full,
             CC_mod_twowayints,
             CC_mod_nosexseas,
             CC_mod_nosexstat, 
             CC_mod_noints, 
             CC_mod_nosex,
             CC_mod_nostatus,
             CC_mod_NULL) # Best with just season. 


# FINAL MODEL FOR CLUSTERING COEFFICIENT (NORMALISED)
CC_mod <- lmer(cc ~ 
                 fSeason +
                 (1|fTerritory) + (1|ShortCode),
               data=centrality_core, REML=F, verbose=T,
               control=lmerControl(optimizer="bobyqa",
                                   optCtrl = list(maxfun=1000000),
                                   check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod, CC_mod_NULL)

#----------------------
# FINAL MODEL CHECKING
#----------------------
plot(CC_mod)
plot(fitted(CC_mod), resid(CC_mod))
lines(smooth.spline(fitted(CC_mod), resid(CC_mod))) # ignore! just cos clusters of common values
hist(resid(CC_mod))
visreg::visreg(CC_mod) 
lattice::dotplot(ranef(CC_mod, condVar=TRUE))

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
overdisp_fun(CC_mod) # p-value: If <0.05, data are overdispersed. Mine=1, not overdispersed.


#---------------------------
#== SAVE MODEL COEFFICIENTS 
#---------------------------
CC_mod_coeffs <- coef(summary(CC_mod))  # save as DF
ranef(CC_mod) # to see order of random effects: ShortCode is first (sig01), then Territory
CC_mod_cis <- confint(CC_mod)
summary(CC_mod) # get variance and SD for random effects

car::Anova(CC_mod)
#----------------------------------------------------------------------------------------------
#== LR TESTS TO GET X2 AND P-VALUES FOR NON-SIG TERMS BY ADDING EACH TERM TO THE MINIMAL MODEL
#----------------------------------------------------------------------------------------------

# 1. Compare minimal signif mod without each sig term to get chisq values:
anova(CC_mod_nostatus, CC_mod_NULL)

# 2. Compare minimal signif mod with each non-sig term to get chisq values:
# with sex added
CC_mod_withsex <- lmer(cc ~ Sex +
                         fSeason +
                         (1|fTerritory) + (1|ShortCode),
                       data=centrality_core, REML=F, verbose=T,
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=1000000),
                                           check.conv.grad=.makeCC("warning",0.05)))
anova(CC_mod,CC_mod_withsex)




# POST HOC
#----------------------------------
CC_mod_contrasts <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)


# PLOT
#----------------------------------
CC_mod_lsm <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$lsmeans)
CC_mod_lsm$Season <- ordered(CC_mod_lsm$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

library(ggplot2)
ggp <- ggplot(CC_mod_lsm, aes(x=Season, y=lsmean)) # need to specify it's a factor to get separate colours
ggp + geom_point(size=4) + 
  xlab("") + ylab("Clustering coefficient\n") +
  geom_errorbar(mapping=aes(x=Season, ymin=lower.CL, ymax=upper.CL),
                width=0.2, size=0.8) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black", size=2),
        panel.border = element_rect(linetype = "solid", colour = "black"))

# save above in high res.
dev.print(jpeg, "RPlot_LMM lsmeans & CIs for clustering coefficient by season.jpeg", res=900, height=6, width=8, units="in") 



# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA
#--------------------------------------------------------------
library(sna)

# set up progress bar
perm <- 1000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrices to store the coefficient estimates from each randomised matrix
CC_seas.aov_perm <-rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_CC <- rbind (data.frame(cc = T1spr_cc_randmat[,i], SeasonID="1", Territory="1", id=row.names(T1spr_cc_randmat)),
                     data.frame(cc = T1sum_cc_randmat[,i], SeasonID="2", Territory="1", id=row.names(T1sum_cc_randmat)), 
                     data.frame(cc = T1aut_cc_randmat[,i], SeasonID="3", Territory="1", id=row.names(T1aut_cc_randmat)),
                     data.frame(cc = T1win_cc_randmat[,i], SeasonID="4", Territory="1", id=row.names(T1win_cc_randmat)),
                     
                     data.frame(cc = T2spr_cc_randmat[,i], SeasonID="1", Territory="2", id=row.names(T2spr_cc_randmat)),
                     #data.frame(cc = T2sum_cc_randmat[,i], SeasonID="2", Territory="2", id=row.names(T2sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T2aut_cc_randmat[,i], SeasonID="3", Territory="2", id=row.names(T2aut_cc_randmat)), 
                     data.frame(cc = T2win_cc_randmat[,i], SeasonID="4", Territory="2", id=row.names(T2win_cc_randmat)),
                     
                     data.frame(cc = T3spr_cc_randmat[,i], SeasonID="1", Territory="3", id=row.names(T3spr_cc_randmat)),
                     data.frame(cc = T3sum_cc_randmat[,i], SeasonID="2", Territory="3", id=row.names(T3sum_cc_randmat)),
                     data.frame(cc = T3aut_cc_randmat[,i], SeasonID="3", Territory="3", id=row.names(T3aut_cc_randmat)),
                     data.frame(cc = T3win_cc_randmat[,i], SeasonID="4", Territory="3", id=row.names(T3win_cc_randmat)),
                     
                     #data.frame(cc = T4spr_cc_randmat[,i], SeasonID="1", Territory="4", id=row.names(T4spr_cc_randmat)), # no pref/avoided assocs
                     #data.frame(cc = T4sum_cc_randmat[,i], SeasonID="2", Territory="4", id=row.names(T4sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4aut_cc_randmat[,i], SeasonID="3", Territory="4", id=row.names(T4aut_cc_randmat)),
                     data.frame(cc = T4win_cc_randmat[,i], SeasonID="4", Territory="4", id=row.names(T4win_cc_randmat)),
                     
                     data.frame(cc = T5spr_cc_randmat[,i], SeasonID="1", Territory="5", id=row.names(T5spr_cc_randmat)),
                     data.frame(cc = T5sum_cc_randmat[,i], SeasonID="2", Territory="5", id=row.names(T5sum_cc_randmat)),
                     data.frame(cc = T5aut_cc_randmat[,i], SeasonID="3", Territory="5", id=row.names(T5aut_cc_randmat)),
                     #data.frame(cc = T5win_cc_randmat[,i], SeasonID="4", Territory="5", id=row.names(T5win_cc_randmat)), # no pref/avoided assocs
                     
                     data.frame(cc = T6spr_cc_randmat[,i], SeasonID="1", Territory="6", id=row.names(T6spr_cc_randmat)), 
                     data.frame(cc = T6sum_cc_randmat[,i], SeasonID="2", Territory="6", id=row.names(T6sum_cc_randmat)), 
                     data.frame(cc = T6aut_cc_randmat[,i], SeasonID="3", Territory="6", id=row.names(T6aut_cc_randmat)), 
                     data.frame(cc = T6win_cc_randmat[,i], SeasonID="4", Territory="6", id=row.names(T6win_cc_randmat)),
                     
                     data.frame(cc = T7spr_cc_randmat[,i], SeasonID="1", Territory="7", id=row.names(T7spr_cc_randmat)),
                     data.frame(cc = T7sum_cc_randmat[,i], SeasonID="2", Territory="7", id=row.names(T7sum_cc_randmat)), 
                     data.frame(cc = T7aut_cc_randmat[,i], SeasonID="3", Territory="7", id=row.names(T7aut_cc_randmat)),
                     data.frame(cc = T7win_cc_randmat[,i], SeasonID="4", Territory="7", id=row.names(T7win_cc_randmat)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_CC$id        <- as.character(input_CC$id)
  input_CC$SeasonID  <- as.character(input_CC$SeasonID)
  input_CC$Territory <- as.character(input_CC$Territory)
  # Add attributes to input_CC
  input_CC$ShortCode    <- attribs_char[match(input_CC$id, attribs_char$id),4] 
  input_CC$SocialStatus <- attribs_char[match(input_CC$id, attribs_char$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_CC, y=attribs_char[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <- lmer(cc ~ fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # save the Anova chisq statistic
  CC_seas.aov_perm[i] <- data.frame(car::Anova(model_tmp))[1,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
CC_seas.aov_obs <- data.frame(car::Anova(CC_mod))[1,1]

CC_seasonSUM_Pvalue <- sum(CC_seas.aov_perm>CC_seas.aov_obs)/1000 # = 1.00


# Histogram of random coefficients with red line for real
hist(CC_seas.aov_perm,breaks=100, main=paste("P = ", sum(CC_seas.aov_perm>CC_seas.aov_obs)/1000),
     xlab="Effect of season on clustering coefficient", ylab="Probability") 
abline(v=CC_seas.aov_obs, col="red")


#========================================================================================


# Loop to compare real vs random LSMEANS

#Make 1D storage matrices to store the model coefficients
CC_seasonSUM_perm <-rep(NA,perm)
CC_seasonAUT_perm <-rep(NA,perm)
CC_seasonWIN_perm <-rep(NA,perm)

# and more to store the lsm contrast estimates
CC_lsm12_perm <-rep(NA,perm)
CC_lsm13_perm <-rep(NA,perm)
CC_lsm14_perm <-rep(NA,perm)
CC_lsm23_perm <-rep(NA,perm)
CC_lsm24_perm <-rep(NA,perm)
CC_lsm34_perm <-rep(NA,perm)

# And one to use Duboscq method for comparing p-value of full vs null model
CC_pval_modvsnull_perm <- rep(NA,perm)

# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter:
  input_CC <- rbind (data.frame(cc = T1spr_cc_randmat[,i], SeasonID="1", Territory="1", id=row.names(T1spr_cc_randmat)),
                     data.frame(cc = T1sum_cc_randmat[,i], SeasonID="2", Territory="1", id=row.names(T1sum_cc_randmat)), 
                     data.frame(cc = T1aut_cc_randmat[,i], SeasonID="3", Territory="1", id=row.names(T1aut_cc_randmat)),
                     data.frame(cc = T1win_cc_randmat[,i], SeasonID="4", Territory="1", id=row.names(T1win_cc_randmat)),
                     
                     data.frame(cc = T2spr_cc_randmat[,i], SeasonID="1", Territory="2", id=row.names(T2spr_cc_randmat)),
                     #data.frame(cc = T2sum_cc_randmat[,i], SeasonID="2", Territory="2", id=row.names(T2sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T2aut_cc_randmat[,i], SeasonID="3", Territory="2", id=row.names(T2aut_cc_randmat)), 
                     data.frame(cc = T2win_cc_randmat[,i], SeasonID="4", Territory="2", id=row.names(T2win_cc_randmat)),
                     
                     data.frame(cc = T3spr_cc_randmat[,i], SeasonID="1", Territory="3", id=row.names(T3spr_cc_randmat)),
                     data.frame(cc = T3sum_cc_randmat[,i], SeasonID="2", Territory="3", id=row.names(T3sum_cc_randmat)),
                     data.frame(cc = T3aut_cc_randmat[,i], SeasonID="3", Territory="3", id=row.names(T3aut_cc_randmat)),
                     data.frame(cc = T3win_cc_randmat[,i], SeasonID="4", Territory="3", id=row.names(T3win_cc_randmat)),
                     
                     #data.frame(cc = T4spr_cc_randmat[,i], SeasonID="1", Territory="4", id=row.names(T4spr_cc_randmat)), # no pref/avoided assocs
                     #data.frame(cc = T4sum_cc_randmat[,i], SeasonID="2", Territory="4", id=row.names(T4sum_cc_randmat)), # no pref/avoided assocs
                     data.frame(cc = T4aut_cc_randmat[,i], SeasonID="3", Territory="4", id=row.names(T4aut_cc_randmat)),
                     data.frame(cc = T4win_cc_randmat[,i], SeasonID="4", Territory="4", id=row.names(T4win_cc_randmat)),
                     
                     data.frame(cc = T5spr_cc_randmat[,i], SeasonID="1", Territory="5", id=row.names(T5spr_cc_randmat)),
                     data.frame(cc = T5sum_cc_randmat[,i], SeasonID="2", Territory="5", id=row.names(T5sum_cc_randmat)),
                     data.frame(cc = T5aut_cc_randmat[,i], SeasonID="3", Territory="5", id=row.names(T5aut_cc_randmat)),
                     #data.frame(cc = T5win_cc_randmat[,i], SeasonID="4", Territory="5", id=row.names(T5win_cc_randmat)), # no pref/avoided assocs
                     
                     data.frame(cc = T6spr_cc_randmat[,i], SeasonID="1", Territory="6", id=row.names(T6spr_cc_randmat)), 
                     data.frame(cc = T6sum_cc_randmat[,i], SeasonID="2", Territory="6", id=row.names(T6sum_cc_randmat)), 
                     data.frame(cc = T6aut_cc_randmat[,i], SeasonID="3", Territory="6", id=row.names(T6aut_cc_randmat)), 
                     data.frame(cc = T6win_cc_randmat[,i], SeasonID="4", Territory="6", id=row.names(T6win_cc_randmat)),
                     
                     data.frame(cc = T7spr_cc_randmat[,i], SeasonID="1", Territory="7", id=row.names(T7spr_cc_randmat)),
                     data.frame(cc = T7sum_cc_randmat[,i], SeasonID="2", Territory="7", id=row.names(T7sum_cc_randmat)), 
                     data.frame(cc = T7aut_cc_randmat[,i], SeasonID="3", Territory="7", id=row.names(T7aut_cc_randmat)),
                     data.frame(cc = T7win_cc_randmat[,i], SeasonID="4", Territory="7", id=row.names(T7win_cc_randmat)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_CC$id        <- as.character(input_CC$id)
  input_CC$SeasonID  <- as.character(input_CC$SeasonID)
  input_CC$Territory <- as.character(input_CC$Territory)
  # Add attributes to input_CC
  input_CC$ShortCode    <- attribs_char[match(input_CC$id, attribs_char$id),4] 
  input_CC$SocialStatus <- attribs_char[match(input_CC$id, attribs_char$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_CC, y=attribs_char[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <- lmer(cc ~ fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # save the model coefficients
  CC_seasonSUM_perm[i] <-coef(summary(model_tmp))[2,1]
  CC_seasonAUT_perm[i] <-coef(summary(model_tmp))[3,1]
  CC_seasonWIN_perm[i] <-coef(summary(model_tmp))[4,1]
  # Run post-hoc comparisons and save estimated difference/effect size of contrast:
  CC_lsm12_perm[i] <-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)[1,2]
  CC_lsm13_perm[i] <-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)[2,2]
  CC_lsm14_perm[i] <-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)[3,2]
  CC_lsm23_perm[i] <-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)[4,2]
  CC_lsm24_perm[i] <-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)[5,2]
  CC_lsm34_perm[i] <-data.frame(summary(lsmeans::lsmeans(model_tmp, pairwise~fSeason))$contrasts)[6,2]
  setTxtProgressBar(pb, i)
}


# model coefficient from the observed (real) data
CC_seasonSUM_obs <- coef(summary(CC_mod))[2,1]
CC_seasonAUT_obs <- coef(summary(CC_mod))[3,1]
CC_seasonWIN_obs <- coef(summary(CC_mod))[4,1]
# compare to random data to get p-value
CC_seasonSUM_Pvalue <- sum(CC_seasonSUM_perm>CC_seasonSUM_obs)/1000 # = 1
CC_seasonAUT_Pvalue <- sum(CC_seasonAUT_perm>CC_seasonAUT_obs)/1000 # = 0.0961
CC_seasonWIN_Pvalue <- sum(CC_seasonWIN_perm>CC_seasonWIN_obs)/1000 # = 0.972

# lsmeans from observed (real) data
CC_lsm12_obs <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)[1,2]
CC_lsm13_obs <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)[2,2]
CC_lsm14_obs <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)[3,2]
CC_lsm23_obs <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)[4,2]
CC_lsm24_obs <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)[5,2]
CC_lsm34_obs <- data.frame(summary(lsmeans::lsmeans(CC_mod, pairwise~fSeason))$contrasts)[6,2]

# compare to random data to get p-values
CC_lsm12_Pvalue <- sum(CC_lsm12_perm>CC_lsm12_obs)/1000 # = 
CC_lsm13_Pvalue <- sum(CC_lsm13_perm>CC_lsm13_obs)/1000 # = 
CC_lsm14_Pvalue <- sum(CC_lsm14_perm>CC_lsm14_obs)/1000 # = 
CC_lsm23_Pvalue <- sum(CC_lsm23_perm>CC_lsm23_obs)/1000 # = 
CC_lsm24_Pvalue <- sum(CC_lsm24_perm>CC_lsm24_obs)/1000 # = 
CC_lsm34_Pvalue <- sum(CC_lsm34_perm>CC_lsm34_obs)/1000 # =


# Histogram of random coefficients with red line for real
hist(CC_seasonSUM_perm,breaks=100, main=paste("P = ", sum(CC_seasonSUM_perm>CC_seasonSUM_obs)/1000),
     xlab="Effect of season on clustering coefficient", ylab="Probability") 
abline(v=CC_seasonSUM_obs, col="red")

# Histogram of random coefficients with red line for real
hist(CC_seasonAUT_perm,breaks=100, main=paste("P = ", sum(CC_seasonAUT_perm>CC_seasonAUT_obs)/1000),
     xlab="Effect of season on clustering coefficient", ylab="Probability") 
abline(v=CC_seasonAUT_obs, col="red")

# Histogram of random coefficients with red line for real
hist(CC_seasonWIN_perm,breaks=100, main=paste("P = ", sum(CC_seasonWIN_perm>CC_seasonWIN_obs)/1000),
     xlab="Effect of season on clustering coefficient", ylab="Probability") 
abline(v=CC_seasonWIN_obs, col="red")
