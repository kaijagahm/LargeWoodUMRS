
#========================================
# cforest tree on point level for larger sample size
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
m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m0)

# add avg_depth
m1 <- glmer(snag ~ stratum + avg_depth + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m1)
plot(allEffects(m1), rescale.axis = F)

# add revetment
m2 <- glmer(snag ~ stratum + avg_depth + revetment + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m2)
Anova(m2, type = 2)
anova(m2, m1, test = "Chisq") # ok

# add nearest_land_class
m3 <- glmer(snag ~ stratum + avg_depth + revetment + nearest_land_class + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m3)
Anova(m3, type = 2)
anova(m3, m2, test = "Chisq") # good good

# add pct_perimeter_wetforest
m4 <- glmer(snag ~ stratum + avg_depth + revetment + nearest_land_class + pct_perimeter_wetforest + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m4)
Anova(m4, type = 2)
anova(m4, m3, test = "Chisq") #marginal
plot(allEffects(m4, rescale.axis = F))

# add wingdam
m5 <- glmer(snag ~ stratum + avg_depth + revetment + nearest_land_class + pct_perimeter_wetforest + wingdam + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m5)
Anova(m5, type = 2)
anova(m5, m4, test = "Chisq") #nope

# This is too complicated. Let's go back to m0, which will just show differences between the habitat strata. 
allscaled <- within(allscaled, stratum <- relevel(stratum, ref = "MCB-U"))
m0 <- glmer(snag ~ stratum + (1 | uniq_id),
            data = allscaled,
            family = binomial,
            control = glmerControl(optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)))
summary(m0)
plot(allEffects(m0),
     main = "Probability of large wood by habitat type",
     xlab = "Habitat type",
     ylab = "P(wood)",
     rescale.axis = F,
     ylim = c(0,1))

# do we want to look at depth or something else broken down by stratum, or just abandon stratum entirely?
