
#========================================
# GLMER models, excluding stratum
#========================================
source("libraries.R")
source("ownfunctions.R")
# Upper river
load("data/all_reduced_clean.Rda")
dim(all_reduced_clean)
locate.nas(all_reduced_clean)

# Lower river
load("data/all2_reduced_clean.Rda")
dim(all2_reduced_clean)
locate.nas(all2_reduced_clean)

# Bind the two datasets together
combined <- rbind(all_reduced_clean, all2_reduced_clean)
combined$snag <- factor(as.character(combined$snag))
combined$wingdyke <- factor(as.character(combined$wingdyke))
combined$riprap <- factor(as.character(combined$riprap))

# Remove some variables that we aren't going to use
combined <- combined %>% dplyr::select(-c(barcode, lcode, sdate, utm_e, utm_n, year, near_terr_name))

# Scale the numeric variables to avoid problems with model convergence. 
combined_scaled <- combined %>%
  mutate_if(is.numeric, scale) # so elegant!!!
#=============================
# ALL POOLS
#=============================

m0 <- glmer(snag ~ pct_terr + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0) # not significant

m1 <- glmer(snag ~ pct_prm_wetf + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1) # that's better

# Add pct_terr after the fact and it looks better
m2 <-  glmer(snag ~ pct_prm_wetf + pct_terr + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m2) # this is good

# Add perimeter
m3 <- glmer(snag ~ pct_prm_wetf + pct_terr + perimeter + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m3) # nope

# Add wingdyke instead
m4 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) # yep
anova(m4, m2, test = "Chisq") # yep

# Add avg_depth
m5 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + avg_depth + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m5) # marginal, let's try shoreline_density_index instead

# Add shoreline_density_index instead
m6 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + shoreline_density_index + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m6) # nope, not good. Try tot_vol

# Add tot_vol instead
m7 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + tot_vol + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m7) # nope, not good. Try near_forest_dist

# Add near_forest_dist instead
m8 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + near_forest_dist + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m8) # that helped a lot. Let's backtrack and try adding avg_depth again.

# Now add avg_depth
m9 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + near_forest_dist + avg_depth + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m9) # nah

# Try shoreline_density_index instead
m10 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + near_forest_dist + shoreline_density_index + (1 | uniq_id),
            data = combined_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m10) # no, definitely not

# Try tot_vol instead
m11 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + near_forest_dist + tot_vol + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m11) # no, not this either.

# Add near_terr_class_7
m12 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + near_forest_dist + near_terr_class_7 + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m12) # no, not this either.
Anova(m12, type = 2)
anova(m12, m8, test = "Chisq") # looks good. Add avg_depth

# *now* add avg_depth?: nope
# *now* add shoreline_density_index: nope
# *now* add tot_vol: nope
# Ok, last main effect: try max_depth
m13 <- glmer(snag ~ pct_prm_wetf + pct_terr + wingdyke + near_forest_dist + near_terr_class_7 + max_depth + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m13) # no, not this either.

# Any interactions?
summary(m12)

# interact pct_prm_wetf with pct_terr
m14 <- glmer(snag ~ pct_prm_wetf*pct_terr + wingdyke + near_forest_dist + near_terr_class_7 + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m14) # nope, bad

# interact pct_prm_wetf with wingdyke
m15 <- glmer(snag ~ pct_terr + pct_prm_wetf*wingdyke + near_forest_dist + near_terr_class_7 + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m15) # nope, still bad

# interact pct_prm_wetf with near_forest_dist
m15 <- glmer(snag ~ pct_terr + wingdyke + pct_prm_wetf*near_forest_dist + near_terr_class_7 + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m15) # that's good 

# interact pct_terr with near_terr_class_7
m16 <- glmer(snag ~ pct_terr*near_terr_class_7 + wingdyke + pct_prm_wetf*near_forest_dist + (1 | uniq_id),
             data = combined_scaled,
             family = binomial,
             control = glmerControl(optimizer = "bobyqa",
                                    optCtrl = list(maxfun=2e5)))
summary(m16) # that's also good

mfinal_all_nostratum <- m16
summary(mfinal_all_nostratum)
save(mfinal_all_nostratum, file = "data/mfinal_all_nostratum.Rda")
save(combined_scaled, file = "data/combined_scaled.Rda")
#=============================
# POOL 4
#=============================
pool4_scaled <- combined_scaled[combined_scaled$pool == 4,] %>% dplyr::select(-pool)

m0 <- glmer(snag ~ wingdyke + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# add nearest land class
m1 <- glmer(snag ~ wingdyke + near_terr_class_7 + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1)
Anova(m1, type = 2)

# add avg_depth
m2 <- glmer(snag ~ wingdyke + near_terr_class_7 + avg_depth + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) # ok good
Anova(m2, type = 2)

# add near_forest_dist
m3 <- glmer(snag ~ wingdyke + near_terr_class_7 + avg_depth + near_forest_dist + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) # still good
Anova(m3, type = 2)

# add perimeter
m4 <- glmer(snag ~ wingdyke + near_terr_class_7 + avg_depth + near_forest_dist + perimeter + (1 | uniq_id),
            data = pool4_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) # nope, stop here
Anova(m4, type = 2)

# interact wingdyke by avg_depth: not good
# interact near_terr_class_7 by avg_depth: not good
# interact wingdyke by near_forest_dist: not good
# interact wingdyke by near_terr_class_7: not good
# interact near_terr_class_7 by near_forest_dist: not good
# interact near_forest_dist by avg_depth: not good
# ok, I guess we're not gonna use any interactions for this one.

mfinal_pool4_nostratum <- m3
plot(allEffects(m3))
summary(mfinal_pool4_nostratum)

#=============================
# POOL 8
#=============================
pool8_scaled <- combined_scaled[combined_scaled$pool == 8,] %>% dplyr::select(-pool)

m0 <- glmer(snag ~ wingdyke + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# add avg_depth
m1 <- glmer(snag ~ wingdyke + avg_depth + (1 | uniq_id),
           data = pool8_scaled,
           family = binomial,
           control = glmerControl(optimizer = "bobyqa",
                                  optCtrl = list(maxfun=2e5)))
summary(m1) #looks ok
anova(m1, m0, test = "Chisq") #looks good

# add shoreline_density_index
m2 <- glmer(snag ~ wingdyke + avg_depth + shoreline_density_index + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) #marginal
anova(m2, m1, test = "Chisq") #barely good
Anova(m2, type = 2)

# try near_terr_class_7 instead
m3 <- glmer(snag ~ wingdyke + avg_depth + near_terr_class_7 + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) #marginal
Anova(m3, type = 2) #ok

# add near_forest_dist
m4 <- glmer(snag ~ wingdyke + avg_depth + near_terr_class_7 + near_forest_dist + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) #ok
Anova(m4, type = 2) #ok

# add max_depth
m5 <- glmer(snag ~ wingdyke + avg_depth + near_terr_class_7 + near_forest_dist + max_depth + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m5) # nope
Anova(m5, type = 2) # nope nope nope

# add riprap instead
m6 <- glmer(snag ~ wingdyke + avg_depth + near_terr_class_7 + near_forest_dist + riprap + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m6) # hmm
Anova(m6, type = 2) # looks good, but now I don't like avg_depth

# remove avg_depth
m7 <- glmer(snag ~ wingdyke + near_terr_class_7 + near_forest_dist + riprap + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m7) # ok
Anova(m7, type = 2) #looks better

# interact riprap by near_forest_dist
m8 <- glmer(snag ~ wingdyke + near_terr_class_7 + near_forest_dist*riprap + (1 | uniq_id),
            data = pool8_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m8) # ok
Anova(m8, type = 2) #looks ok
plot(allEffects(m8))

# interact riprap*near_terr_class_7: doesn't work
# interact wingdyke*near_forest_dist: only marginally significant
# interact near_forest_dist*near_terr_class_7: only marginally significant

mfinal_pool8_nostratum <- m8

#=============================
# POOL 13
#=============================
pool13_scaled <- combined_scaled[combined_scaled$pool == 13,] %>% dplyr::select(-pool)

# start with avg_depth
m0 <- glmer(snag ~ avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)

# weird, not sure why that's not significant. Maybe wingdyke instead?
m1 <- glmer(snag ~ wingdyke + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1) # that's better. What if we add avg_depth on top of that?

# add avg_depth
m2 <- glmer(snag ~ wingdyke + avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) #marginally significant

# add shoreline_density_index
m3 <- glmer(snag ~ wingdyke + avg_depth + shoreline_density_index + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) # not good. What if we try it without avg_depth?
anova(m3, m2, test = "Chisq") #badbad

# add shoreline_density_index but remove avg_depth
m4 <- glmer(snag ~ wingdyke + shoreline_density_index + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4) # not good at all. 

# back to model 2. Add max_depth
m5 <- glmer(snag ~ wingdyke + avg_depth + max_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m5) #whoa, very bad

# add pct_terrestrial shore instead
m6 <- glmer(snag ~ wingdyke + avg_depth + pct_terrestrial_shore + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m6) # not good

# okay, I guess we're sticking with model 2. One last thing: try interacting wingdyke with avg_depth. 
m7 <- glmer(snag ~ wingdyke*avg_depth + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m7) # not good
Anova(m7, type = 2)
Anova(m7, type = 3)
# not good

# try adding near_terr_dist
m8 <- glmer(snag ~ wingdyke + avg_depth + near_terr_dist + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m8) #ooh, that looks nice
anova(m8, m2, test = "Chisq")

# adding perimeter gives nothing interesting
# add riprap
m9 <- glmer(snag ~ wingdyke + avg_depth + near_terr_dist + riprap + (1 | uniq_id),
            data = pool13_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m9) #ooh, that looks nice
anova(m9, m8, test = "Chisq")

# add near_forest_dist: marginal
# interact riprap*near_terr_dist: nope nope
# interact riprap*wingdyke: very bad
# interact riprap*avg_depth: ok, but some marginal effects
# let's stuck with m9. 
plot(allEffects(m9))

mfinal_pool13_nostratum <- m9
summary(mfinal_pool13_nostratum)

#=============================
# POOL 26
#=============================
pool26_scaled <- combined_scaled[combined_scaled$pool == 26,] %>% dplyr::select(-pool)

# start with near_terr_class_7
m0 <- glmer(snag ~ near_terr_class_7 + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m0)
Anova(m0, type = 2) # nice

# add pct_prm_wetf
m1 <- glmer(snag ~ near_terr_class_7 + pct_prm_wetf + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m1) # nah, not good
Anova(m1, type = 2)

# add near_forest_dist instead
m2 <- glmer(snag ~ near_terr_class_7 + near_forest_dist + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m2) # okay
Anova(m2, type = 2)

# add riprap
m3 <- glmer(snag ~ near_terr_class_7 + near_forest_dist + riprap + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m3) # okay, but would be better without near_forest_dist
Anova(m3, type = 2)

# add riprap
m4 <- glmer(snag ~ near_terr_class_7 + riprap + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m4)
Anova(m4, type = 2) # much better

# add pct_terr
m5 <- glmer(snag ~ near_terr_class_7 + riprap + pct_terr + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m5)
Anova(m5, type = 2) # nope nope

# add tot_vol instead: not good
# add max_depth instead: not good
# Interact near_terr_class_7 and riprap: failed to converge
m6 <- glmer(snag ~ near_terr_class_7*riprap + (1 | uniq_id),
            data = pool26_scaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun=2e5)))
summary(m6)

mfinal_pool26_nostratum <- m4
summary(mfinal_pool26_nostratum)
plot(allEffects(mfinal_pool26_nostratum), rescale.axis = F)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
finalmodels_nostratum <- list(mfinal_all_nostratum, mfinal_pool4_nostratum, mfinal_pool8_nostratum, mfinal_pool13_nostratum, mfinal_pool26_nostratum, mfinal_poolLG_nostratum, mfinal_poolOR_nostratum)
save(finalmodels_nostratum, file = "data/finalmodels_nostratum.Rda")

datasets_nostratum <- list(combined_scaled, pool4_scaled, pool8_scaled, pool13_scaled)
names(datasets_nostratum) <- c("combined_scaled", "pool4_scaled", "pool8_scaled", "pool13_scaled")
save(datasets_nostratum, file = "data/datasets_nostratum.Rda")

# Make nice effect plots for the all pools model
plot(wd, main = "Wood by wingdyke presence", 
     xlab = "wingdyke presence (0 = no, 1 = yes)", 
     ylab = "P(wood)", ylim = c(-2, 2))
plot(ad, main = "Wood by avg. water depth",
     xlab = "Avg. water depth, scaled (sd from mean)",
     ylab = "P(wood)", ylim = c(-2, 2))
plot(nlc, main = "Wood by nearest landcover type",
     xlab = "Nearest landcover type",
     ylab = "P(wood)", ylim = c(-2, 2))
plot(rbydtf, ylim = c(-2, 2))

