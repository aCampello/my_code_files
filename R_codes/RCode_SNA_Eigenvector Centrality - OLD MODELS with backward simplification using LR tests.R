#============================
##  EIGENVECTOR CENTRALITY
#============================


# CHECK DISTRIBUTION OF DATA
#----------------------------

library(fitdistrplus)
# Cullen & Frey graph:
descdist(centrality_core$eig.norm, discrete = FALSE, boot=5000) # distrib is normal or uniform (square)

# Plot data against normal
fit.norm <- fitdist(centrality_core$eig.norm, "norm")
plot(fit.norm) # looks OK (apart from bump at zero)
require(MASS)
require(car)
qqp((centrality_core$eig.norm), "norm", main="normal") # looks good!


#--------
# MODELS
#--------

# LMM 
eig_mod_full  <- lmer(eig.norm ~ Sex*SocialStatus*fSeason +
                        (1|fTerritory) + (1|ShortCode),
                      data=centrality_core, REML=F, verbose=T,
                      control=lmerControl(optimizer="bobyqa",
                                          optCtrl = list(maxfun=1000000),
                                          check.conv.grad=.makeCC("warning",0.05)))
#= Check residuals:
plot(eig_mod_full) # OK
plot(fitted(eig_mod_full), resid(eig_mod_full))   
lines(smooth.spline(fitted(eig_mod_full), resid(eig_mod_full))) # relatively flat

# check ranefs
lattice::dotplot(ranef(eig_mod_full, condVar=TRUE)) # territory 1 and 6 quite diff to others. 

#-------------------------
#== Model simplification
#-------------------------
car::Anova(eig_mod_full) # Season and status are sig!!

# 3X 2-way interactions
eig_mod_twowayints  <- lmer(eig.norm ~ Sex*SocialStatus + Sex*fSeason + SocialStatus*fSeason +
                              (1|fTerritory) + (1|ShortCode),
                            data=centrality_core, REML=F, verbose=T,
                            control=lmerControl(optimizer="bobyqa",
                                                optCtrl = list(maxfun=1000000),
                                                check.conv.grad=.makeCC("warning",0.05)))
anova(eig_mod_full, eig_mod_twowayints) # no diff without three-way so can remove
car::Anova(eig_mod_twowayints)

# without sex*seas as lowest significance
eig_mod_nosexseas  <- lmer(eig.norm ~ Sex*SocialStatus + fSeason*SocialStatus +
                             (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F, verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))
car::Anova(eig_mod_nosexseas)
anova(eig_mod_twowayints, eig_mod_nosexseas)

# no sex-stat interaction as lowest significance
eig_mod_nosexstat  <- lmer(eig.norm ~ Sex + SocialStatus*fSeason +
                             (1|fTerritory) + (1|ShortCode),
                           data=centrality_core, REML=F, verbose=T,
                           control=lmerControl(optimizer="bobyqa",
                                               optCtrl = list(maxfun=1000000),
                                               check.conv.grad=.makeCC("warning",0.05)))
car::Anova(eig_mod_nosexstat)
anova(eig_mod_nosexseas, eig_mod_nosexstat)

# no interactions
eig_mod_noints  <- lmer(eig.norm ~ Sex + SocialStatus + fSeason +
                          (1|fTerritory) + (1|ShortCode),
                        data=centrality_core, REML=F, verbose=T,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=1000000),
                                            check.conv.grad=.makeCC("warning",0.05)))
car::Anova(eig_mod_noints)
anova(eig_mod_nosexstat, eig_mod_noints)

# no sex
eig_mod_nosex  <- lmer(eig.norm ~ SocialStatus + fSeason +
                         (1|fTerritory) + (1|ShortCode),
                       data=centrality_core, REML=F, verbose=T,
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=1000000),
                                           check.conv.grad=.makeCC("warning",0.05)))
car::Anova(eig_mod_nosex) # <- FINAL MODEL: all terms significant
anova(eig_mod_noints, eig_mod_nosex)


bbmle::ICtab(eig_mod_full,
             eig_mod_twowayints,
             eig_mod_nosexseas,
             eig_mod_nosexstat, 
             eig_mod_noints, 
             eig_mod_nosex) # nosex is best, then noints but these are all non-sig so ignore as simpler models are better


# FINAL MODEL FOR EIGENVECTOR (NORMALISED 0-1)
eig_mod <- lmer(eig.norm ~ 
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
plot(eig_mod)
plot(fitted(eig_mod), resid(eig_mod))
lines(smooth.spline(fitted(eig_mod), resid(eig_mod))) 
hist(resid(eig_mod))
visreg::visreg(eig_mod) 
lattice::dotplot(ranef(eig_mod, condVar=TRUE))

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
overdisp_fun(eig_mod) # p-value: If <0.05, data are overdispersed. Mine=1, not overdispersed.


#---------------------------
#== SAVE MODEL COEFFICIENTS 
#---------------------------
eig_mod_coeffs <- coef(summary(eig_mod))  # save as DF
ranef(eig_mod) # to see order of random effects: ShortCode is first (sig01), then Territory
eig_mod_cis <- confint(eig_mod)
summary(eig_mod) # get variance and SD for random effects


#----------------------------------------------------------------------------------------------
#== LR TESTS TO GET X2 AND P-VALUES FOR NON-SIG TERMS BY ADDING EACH TERM TO THE MINIMAL MODEL
#----------------------------------------------------------------------------------------------

# 1. Compare minimal signif mod without each sig term to get chisq values:
# without status
eig_mod_nostatus <- lmer(eig.norm ~ 
                           fSeason +
                           (1|fTerritory) + (1|ShortCode),
                         data=centrality_core, REML=F, verbose=T,
                         control=lmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=1000000),
                                             check.conv.grad=.makeCC("warning",0.05)))
# without season
eig_mod_noseas <- lmer(eig.norm ~ 
                         SocialStatus +
                         (1|fTerritory) + (1|ShortCode),
                       data=centrality_core, REML=F, verbose=T,
                       control=lmerControl(optimizer="bobyqa",
                                           optCtrl = list(maxfun=1000000),
                                           check.conv.grad=.makeCC("warning",0.05)))
anova(eig_mod, eig_mod_nostatus)
anova(eig_mod, eig_mod_noseas)

# 2. Compare minimal signif mod with each non-sig term to get chisq values:
# with sex added
eig_mod_withsex <- lmer(eig.norm ~ Sex +
                          SocialStatus +
                          fSeason +
                          (1|fTerritory) + (1|ShortCode),
                        data=centrality_core, REML=F, verbose=T,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=1000000),
                                            check.conv.grad=.makeCC("warning",0.05)))

eig_mod_withsex <- lmer(eig.norm ~ SocialStat +
                          SocialStatus*fSeason +
                          (1|fTerritory) + (1|ShortCode),
                        data=centrality_core, REML=F, verbose=T,
                        control=lmerControl(optimizer="bobyqa",
                                            optCtrl = list(maxfun=1000000),
                                            check.conv.grad=.makeCC("warning",0.05)))
anova(eig_mod,eig_mod_withsex)
anova(eig_mod,eig_mod_withsex)

# POST HOC
eig_mod_contrasts <- data.frame(summary(lsmeans::lsmeans(eig_mod, pairwise~fSeason))$contrasts)

# PLOT
eig_mod_lsm <- data.frame(summary(lsmeans::lsmeans(eig_mod, pairwise~fSeason|SocialStatus))$lsmeans)
eig_mod_lsm$Season <- ordered(eig_mod_lsm$fSeason, levels = c(1,2,3,4), labels=c("Spring", "Summer", "Autumn", "Winter"))

library(ggplot2)
ggp <- ggplot(eig_mod_lsm, aes(x=Season, y=lsmean, group=SocialStatus, colour=factor(SocialStatus))) # need to specify it's a factor to get separate colours
ggp + geom_line(aes(group=SocialStatus), size=1, position=position_dodge(0.2)) + 
  geom_point(size=4, position=position_dodge(0.2)) + 
  scale_colour_manual(name="Social\nstatus", values=c('royalblue3','firebrick1')) + # rename legend
  xlab("") + ylab("Eigenvector centrality\n") +
  geom_errorbar(mapping=aes(x=Season, ymin=lower.CL, ymax=upper.CL),
                width=0.2, size=0.8, position=position_dodge(0.2)) +
  theme_bw(base_size = 18, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black"),
        strip.text.x = element_text(face="bold"),
        strip.background=element_blank())

# save above in high res.
dev.print(jpeg, "RPlot_LMM predictions for eig season & status CIs.jpeg", res=900, height=6, width=10, units="in") 



#--------------------------------------------------------------
# COMPARE MODEL FIT ON REAL DATA TO MODELS FIT ON RANDOM DATA
#--------------------------------------------------------------
library(sna)

# set up progress bar
perm <- 1000
pb <- txtProgressBar(0,perm,0, style=3)

# Make 1D storage matrix to store the coefficient estimate from each randomised matrix
eig_status_perm<-rep(NA,perm) # Need to save 2 coefficients so make 2 matrices
eig_seasonSUM_perm <-rep(NA,perm)
eig_seasonAUT_perm <-rep(NA,perm)
eig_seasonWIN_perm <-rep(NA,perm)


# Run the loop
for(i in 1:perm){ 
  ## Networks excluded as no pref/avoided assocs (permuted groups in samples, LT assocs only) = T2 summer, T4 spring, T4 summer, T5 winter
  input_eig <- rbind(data.frame(eig.norm = T1spr_rand_eig[,i], SeasonID="1", Territory="1", id=row.names(T1spr_rand_eig)), # if rownames won't work chec have run code above to rename the row names
                     data.frame(eig.norm = T1sum_rand_eig[,i], SeasonID="2", Territory="1", id=row.names(T1sum_rand_eig)), 
                     data.frame(eig.norm = T1aut_rand_eig[,i], SeasonID="3", Territory="1", id=row.names(T1aut_rand_eig)),
                     data.frame(eig.norm = T1win_rand_eig[,i], SeasonID="4", Territory="1", id=row.names(T1win_rand_eig)),
                     
                     data.frame(eig.norm = T2spr_rand_eig[,i], SeasonID="1", Territory="2", id=row.names(T2spr_rand_eig)),
                     # data.frame(eig.norm = T2sum_rand_eig[,i], SeasonID="2", Territory="2", id=row.names(T2sum_rand_eig)), # no prefavoided
                     data.frame(eig.norm = T2aut_rand_eig[,i], SeasonID="3", Territory="2", id=row.names(T2aut_rand_eig)), 
                     data.frame(eig.norm = T2win_rand_eig[,i], SeasonID="4", Territory="2", id=row.names(T2win_rand_eig)),
                     
                     data.frame(eig.norm = T3spr_rand_eig[,i], SeasonID="1", Territory="3", id=row.names(T3spr_rand_eig)),
                     data.frame(eig.norm = T3sum_rand_eig[,i], SeasonID="2", Territory="3", id=row.names(T3sum_rand_eig)),
                     data.frame(eig.norm = T3aut_rand_eig[,i], SeasonID="3", Territory="3", id=row.names(T3aut_rand_eig)),
                     data.frame(eig.norm = T3win_rand_eig[,i], SeasonID="4", Territory="3", id=row.names(T3win_rand_eig)),
                     
                     # data.frame(eig.norm = T4spr_rand_eig[,i], SeasonID="1", Territory="4", id=row.names(T4spr_rand_eig)), # no prefavoided
                     # data.frame(eig.norm = T4sum_rand_eig[,i], SeasonID="2", Territory="4", id=row.names(T4sum_rand_eig)), # no prefavoided
                     data.frame(eig.norm = T4aut_rand_eig[,i], SeasonID="3", Territory="4", id=row.names(T4aut_rand_eig)),
                     data.frame(eig.norm = T4win_rand_eig[,i], SeasonID="4", Territory="4", id=row.names(T4win_rand_eig)),
                     
                     data.frame(eig.norm = T5spr_rand_eig[,i], SeasonID="1", Territory="5", id=row.names(T5spr_rand_eig)),
                     data.frame(eig.norm = T5sum_rand_eig[,i], SeasonID="2", Territory="5", id=row.names(T5sum_rand_eig)),
                     data.frame(eig.norm = T5aut_rand_eig[,i], SeasonID="3", Territory="5", id=row.names(T5aut_rand_eig)),
                     # data.frame(eig.norm = T5win_rand_eig[,i], SeasonID="4", Territory="5", id=row.names(T5win_rand_eig)), # no prefavoided
                     
                     data.frame(eig.norm = T6spr_rand_eig[,i], SeasonID="1", Territory="6", id=row.names(T6spr_rand_eig)), 
                     data.frame(eig.norm = T6sum_rand_eig[,i], SeasonID="2", Territory="6", id=row.names(T6sum_rand_eig)), 
                     data.frame(eig.norm = T6aut_rand_eig[,i], SeasonID="3", Territory="6", id=row.names(T6aut_rand_eig)), 
                     data.frame(eig.norm = T6win_rand_eig[,i], SeasonID="4", Territory="6", id=row.names(T6win_rand_eig)),
                     
                     data.frame(eig.norm = T7spr_rand_eig[,i], SeasonID="1", Territory="7", id=row.names(T7spr_rand_eig)),
                     data.frame(eig.norm = T7sum_rand_eig[,i], SeasonID="2", Territory="7", id=row.names(T7sum_rand_eig)), 
                     data.frame(eig.norm = T7aut_rand_eig[,i], SeasonID="3", Territory="7", id=row.names(T7aut_rand_eig)),
                     data.frame(eig.norm = T7win_rand_eig[,i], SeasonID="4", Territory="7", id=row.names(T7win_rand_eig)))
  # Convert factors to characters (to avoid warnings when using merge)
  input_eig$id        <- as.character(input_eig$id)
  input_eig$SeasonID  <- as.character(input_eig$SeasonID)
  input_eig$Territory <- as.character(input_eig$Territory)
  # Add attributes to input_eig
  input_eig$ShortCode    <- attribs_char[match(input_eig$id, attribs_char$id),4] 
  input_eig$SocialStatus <- attribs_char[match(input_eig$id, attribs_char$id),6]
  # merge all columns from first df (x) and only certain ones from the second df (y) 
  df <- merge(x=input_eig, y=attribs_char[ , c("id", "SeasonID", "Territory", "Core")], 
              by = c("id", "SeasonID", "Territory"), all.x=TRUE) # must also contain the 'by' variable in the c("", "") section
  # now make factors
  df$ShortCode <- factor(df$ShortCode)
  df$fTerritory <- factor(df$Territory)
  df$SocialStatus <- factor(df$SocialStatus)
  df$fSeason <- factor(df$SeasonID)
  df$Core <- factor(df$Core)
  # run the model
  model_tmp <- lmer(eig.norm ~ SocialStatus +  fSeason + (1|fTerritory) + (1|ShortCode), 
                    data=subset(df, SocialStatus!="NA" & Core==1),
                    REML=F, control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=1000000), check.conv.grad=.makeCC("warning", 0.05)))
  # get the coefficient estimate and save it
  eig_status_perm[i]    <-coef(summary(model_tmp))[2,1] 
  eig_seasonSUM_perm[i] <-coef(summary(model_tmp))[3,1]
  eig_seasonAUT_perm[i] <-coef(summary(model_tmp))[4,1]
  eig_seasonWIN_perm[i] <-coef(summary(model_tmp))[5,1]
  setTxtProgressBar(pb, i)
}

# estimate from the observed (real) data
eig_status_obs    <- coef(summary(eig_mod))[2,1]
eig_seasonSUM_obs <- coef(summary(eig_mod))[3,1]
eig_seasonAUT_obs <- coef(summary(eig_mod))[4,1]
eig_seasonWIN_obs <- coef(summary(eig_mod))[5,1]

eig_status_Pvalue    <- sum(eig_status_perm>eig_status_obs)/1000
eig_seasonSUM_Pvalue <- sum(eig_seasonSUM_perm>eig_seasonSUM_obs)/1000
eig_seasonAUT_Pvalue <- sum(eig_seasonAUT_perm>eig_seasonAUT_obs)/1000
eig_seasonWIN_Pvalue <- sum(eig_seasonWIN_perm>eig_seasonWIN_obs)/1000

# ??? ALTERNATIVE - get 'coefficients' from ANOVA table (actually chisq values)
eig_status_A <- data.frame(car::Anova(model_tmp))[1,1]
eig_seas_A   <- data.frame(car::Anova(model_tmp))[2,1]


# Histogram of random coefficients with red line for real
hist(eig_status_perm,breaks=100, main=paste("P = ", sum(eig_status_perm>eig_status_obs)/1000),
     xlab="Weighted degree", ylab="Probability") 
abline(v=eig_status_obs, col="red")


hist(eig_seasonSUM_perm,breaks=100, main=paste("P = ", sum(eig_seasonSUM_perm>eig_seasonSUM_obs)/1000),
     xlab="Weighted degree", ylab="Probability") 
abline(v=eig_seasonSUM_obs, col="red")