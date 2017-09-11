
# 1. create null model (lme or lmer)
#2. create models of increasing complexity (lme or lmer)

# 3. compare all models in an anova (choose order of models entered carefully, start from null to increasing
      # complexity, i.e. anova(lme.null, lme.model1, lme.model2, lme.model3)
      # this produces chi-sq values (x2) that show whether each added parameter improves model fit

# 4. create model selection table with AICc and delta AICc, LL and K (no.parameters in model)
library(AICcmodavg) 
Cand.mods <- list("seas*ori" = lme.orimod3, "seas+ori" = lme.orimod2, "seas" = lme.orimod1, "null" = lme.orimod0)
aictab(cand.set = Cand.mods, second.ord = TRUE)
print(aictab(cand.set = Cand.mods, second.ord = TRUE),digits = 3, LL = TRUE)

# If there is a significant interaction, the main effects of variables involved in that
# interaction should not be interpreted individually because they are not meaningful alone.
 

# 5. Check model fit using hist(residuals(model)) and qqplot/qqline or qqp (needs MASS and car packages)

# 6. Do post-hoc tests with lsmeans or glht (multcomp)

# 7. Calculate effect sizes from contrasts/comparisons (in posthoc tests) from the t-value and d.f (quote as r=)
        # then can quote like "sig more strangers were recorded in winter compared to spring (Tukey's test: 
        # spring-winter: t(18) = -5.36, p < 0.001, r = 0.78"


