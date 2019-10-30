source("libraries.R")
source("ownfunctions.R")
load("data/sites_aa.Rda")
load("data/sites_aa_5m.Rda")
load("data/p8_5.Rda")
#Run the week 5 code with the new data (5m buffer)
q_p8 <- p8_5 %>% select(barcode, snag, snagyn, stratum, Area, Perimeter, avg_depth, tot_vol, avg_fetch, shoreline_density_index, pct_terr, pct_prm_wetf, pct_terr_shore_wetf, pct_terr_shore_rev, pct_prm_rev, sinuosity, NEAR_TERR_DIST.p, NEAR_FOREST_DIST.p, depth.p, pct_area_le100) %>% filter(!is.na(snag))
c_p8 <- p8_5 %>% select(barcode, snag, snagyn, stratum, stratum_name, AQUA_CODE, NEAR_TERR_CLASS_31.p, NEAR_TERR_CLASS_15.p, NEAR_TERR_CLASS_7.p, NEAR_FOREST_CLASS_31.p, NEAR_FOREST_CLASS_15.p, NEAR_FOREST_CLASS_7.p, gear.p, trib.p, wingdyke.p, riprap.p, substrt.p, wingdyke.p) %>% filter(!is.na(snag))

#prepare to run univariate logistic regression on a few of these variables
vars <- colnames(q_p8)[!colnames(q_p8) %in% c("barcode", "snag", "snagyn", "stratum")]

source("CWD_distribution/univ.logit.regression.script.R")
outputlist <- vector("list", length(vars)) 

for(i in 1:length(vars)){
  a <- makelogit(df_source = q_p8, 
                 var_of_interest = vars[[i]], 
                 cwdfactor = "snagyn", 
                 cwd = "snag", 
                 varname = vars[i])
  outputlist[[i]] <- a
}
#name the elements of the output list
names(outputlist) <- vars

#Let's get these outputs sorted by type
#initialize lists for the output objects
boxplots <- vector("list", length(vars))
densityplots <- vector("list", length(vars))
modelsummaries <- vector("list", length(vars))
predictions <- vector("list", length(vars))
logitplots <- vector("list", length(vars))

#fill the lists
for(i in 1:length(vars)){
  boxplots[[i]] <- outputlist[[i]]$boxplot
  densityplots[[i]] <- outputlist[[i]]$density_plot
  modelsummaries[[i]] <- outputlist[[i]]$model_summary
  predictions[[i]] <- outputlist[[i]]$predictions
  logitplots[[i]] <- outputlist[[i]]$logit_plot
}

# data frame for model coefficients and p values
coef.pval <- data.frame(
  index = 1:length(vars),
  var = vars,
  coef = rep(NA, length(vars)),
  pval = rep(NA, length(vars))
)
# fill data frame with coefficients and p values
for(i in 1:length(vars)){
  coef.pval$pval[i] <- outputlist[[i]]$p_value
  coef.pval$coef[i] <- coef(outputlist[[i]]$model)[2]
}

# remove the text from the p-values, convert to numeric, round to 6 sig figs
coef.pval$pval <- gsub("p = ", "", coef.pval$pval) %>% 
  as.numeric() %>% 
  signif(digits = 6)

# Add significance codes
coef.pval$sig001 <- ifelse(coef.pval$pval <= 0.001, "**", NA)
coef.pval$sig01 <- ifelse(coef.pval$pval <= 0.01, "*", NA)
coef.pval$sig05 <- ifelse(coef.pval$pval <= 0.05, ".", NA)
head(coef.pval)

# Let's examine the logit plots for the variables that were significant at a 0.05 level.
indices <- coef.pval$index[!is.na(coef.pval$sig05)]
densityplots[indices]
logitplots[indices]


# Examine the ones that were not significant at alpha = 0.05
densityplots[-indices]
logitplots[-indices]

# Save all logit plots
for(i in 1:length(logitplots)){
  ggsave(filename = paste0("figure_images/", "logit.", names(outputlist)[i], ".jpg"), 
         plot = logitplots[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}

# Save all density plots
for(i in 1:length(densityplots)){
  ggsave(filename = paste0("figure_images/", "density.", names(outputlist)[i], ".jpg"), 
         plot = densityplots[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}


#############################################################################
# Models with interactions with stratum types
table(droplevels(q$stratum))
# There are only 2 points in the tailwater zone and 16 in the impounded open water zone, let's remove these for the purposes of this analysis. 
q <- droplevels(q[!q$stratum %in% c("TWZ", "IMP-O"),])
table(q$stratum)
# These sample sizes seem much more reasonable

source("CWD_distribution/interact.univ.logit.regression.script.R")
outputlist.interact <- vector("list", length(vars))

for(i in 1:length(vars)){
  a <- makelogit.interact(df_source = q, 
                          var_of_interest = vars[[i]], 
                          cwdfactor = "snagyn", 
                          cwd = "snag", 
                          varname = vars[i],
                          stratum = "stratum")
  outputlist.interact[[i]] <- a
}

#name the elements of the output list
names(outputlist.interact) <- vars

#Let's get these outputs sorted by type
#initialize lists for the output objects
densityplots.interact <- vector("list", length(vars))
modelsummaries.interact <- vector("list", length(vars))
predictions.interact <- vector("list", length(vars))
logitplots.interact <- vector("list", length(vars))

#fill the lists
for(i in 1:length(vars)){
  densityplots.interact[[i]] <- 
    outputlist.interact[[i]]$densityplot_stratum
  modelsummaries.interact[[i]] <- outputlist.interact[[i]]$model_summary
  predictions.interact[[i]] <- outputlist.interact[[i]]$predictions.interact
  logitplots.interact[[i]] <- outputlist.interact[[i]]$logit_plot
}


#Save the logit plots
for(i in 1:length(logitplots.interact)){
  ggsave(filename = paste0("figure_images/", "logitinteract.", names(outputlist.interact)[i], ".jpg"), 
         plot = logitplots.interact[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}
#Save the density plots
for(i in 1:length(densityplots.interact)){
  ggsave(filename = paste0("figure_images/", "density_stratum.", names(outputlist.interact)[i], ".jpg"), 
         plot = densityplots.interact[[i]], 
         width = 7, 
         height = 5, 
         units = "in", 
         dpi = 600)
}

#What if I want to make the lines only go as far as they should based on the standard error clouds?


##########
#########
#########
# Ok, let's do this systematically. 
load("data/sites_aa_5m.Rda")
source("ownfunctions.R")
source("color_schemes.R")
source("libraries.R")
#exclude variables that we really just don't care about for analysis purposes
names(sites_aa_5m)
excl_1 <- c("FID", "Join_Count", "TARGET_FID", "Field1", "lcode", "sdate", "utm_e", "utm_n", "OBJECTID", "aa_num", "AQUA_DESC", "bath_pct", "area_gt50", "area_gt100", "area_gt200", "area_gt300", "min_rm", "max_rm", "len_met", "len_prm_lotic", "len_prm_lentic", "len_terr", "len_wetf", "len_wd", "scour_wd", "psco_wd", "len_revln", "area_tpi1", "pct_tpi1", "area_tpi2", "pct_tpi2", "area_tpi3", "pct_tpi3", "area_tpi4", "pct_tpi4", "year_phot", "area_le50", "area_le100", "area_le200", "area_le300", "gear.p", "sitetype.p")
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-excl_1)
names(sites_aa_5m)
# exclude sd_depth because it doesn't seem very relevant
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-c(sd_depth))
names(sites_aa_5m)
# exclude avg_fetch, sill, num_rev, and num_wd because they aren't biologically logical
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-c(avg_fetch, sill, num_rev, num_wd))
names(sites_aa_5m)
# Molly says to get rid of all terrestrial categories below _31, as well as the _HEIGHT category.
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-c(NEAR_TERR_CLASS_15.p, NEAR_TERR_CLASS_15_N.p, NEAR_TERR_CLASS_7.p, NEAR_TERR_CLASS_7_N.p, NEAR_TERR_HEIGHT_N.p, NEAR_FOREST_CLASS_15.p, NEAR_FOREST_CLASS_15_N.p, NEAR_FOREST_CLASS_7_N.p, NEAR_FOREST_CLASS_7.p, NEAR_FOREST_HEIGHT_N.p))
names(sites_aa_5m)
# as per JC's recommendation and discussion with KathiJo and Molly, exclude revetment categories. Also exclude wdl_p_m2 because it has a lot of NA's and we have a binary `wingdyke` index to use instead that has far fewer NA's. 
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-c(pct_prm_rev, pct_terr_shore_rev, rev_p_m2, wdl_p_m2))
names(sites_aa_5m)
# Exclude pct_area_le50, pct_area_le200, and pct_area_le300 because they're redundant with pct_area_100
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-c(pct_area_le50, pct_area_le200, pct_area_le300))
names(sites_aa_5m)
# Exclude NEAR_TERR_FID.p and NEAR_FOREST_FID.p columns because we've decided we're not going to try to include two levels of random variables (as per discussion with Barb on 7/12/18)
sites_aa_5m <- sites_aa_5m %>% dplyr::select(-c(NEAR_TERR_FID.p, NEAR_FOREST_FID.p))
names(sites_aa_5m)
# Exclude pct_opwat because it's the inverse of pct_aqveg, and exclude pct_aq because it's the inverse of pct_terr
sites_aa_5m <- sites_aa_5m %>% select(-c(pct_opwat, pct_aq))
names(sites_aa_5m)
#separate variables into quantitative predictors, categorical predictors, and other informational/index variables
quant.preds <- sites_aa_5m %>% select(barcode, Area, Perimeter, max_depth, avg_depth, tot_vol, shoreline_density_index, econ, pct_prm_lotic, num_lotic_outl, pct_prm_lentic, num_lentic_outl, pct_aqveg, pct_terr, pct_prm_wetf, pct_terr_shore_wetf, sinuosity, NEAR_TERR_DIST.p, NEAR_FOREST_DIST.p, depth.p, current.p, stageht.p, pct_area_le100)
str(quant.preds) #check that everything is numeric or integer. We're good. 

cat.preds <- sites_aa_5m %>% select(barcode, stratum, AQUA_CODE, pool, NEAR_TERR_CLASS_31.p, NEAR_FOREST_CLASS_31.p, substrt.p, wingdyke.p, riprap.p, trib.p)
str(cat.preds) #check that everything is a factor
#sitetype isn't a factor. Need to go back and change that. 

info.vars <- sites_aa_5m %>% select(barcode, snag, snagyn, uniq_id, NEAR_TERR_CLASS_31_N.p, NEAR_FOREST_CLASS_31_N.p, year.p, stratum_name)
str(info.vars) #examine structure

# Further narrow down the quantitative variables
locate.nas(quant.preds)
  #there are a lot of NA's here. Let's compute correlations only on variables that have fewer than 1000 NA values (arbitrary but ok)
  quant.preds.subset <- quant.preds %>% select(-c(econ, 
                                                  pct_prm_lotic, 
                                                  num_lotic_outl, 
                                                  pct_prm_lentic, 
                                                  sinuosity, 
                                                  current.p, 
                                                  stageht.p))
      locate.nas(quant.preds.subset)
    a <- cor(quant.preds.subset[,2:ncol(quant.preds.subset)], use = "complete.obs")
    cp <- corrplot(a, type = "lower", method =  "color", diag = F)
# High correlations:
  # tot_vol and Area
  # avg_depth and pct_area_le100
# Exclude tot_vol because it's more complicated to calculate than Area
# Keep avg_depth and pct_area_le100 for now, because they give different information and they may both be important. Not sure which I would get rid of.
sites_aa_5m <- sites_aa_5m %>% select(-tot_vol)
quant.preds <- quant.preds %>% select(-tot_vol)
quant.preds.subset <- quant.preds.subset %>% select(-tot_vol)
    a2 <- cor(quant.preds.subset[,2:ncol(quant.preds.subset)], use = "complete.obs")
    cp2 <- corrplot(a2, type = "lower", method =  "color", diag = F)

# This looks a lot better. Let's try running PCA on these quantitative variables and see if we can reduce the dimensionality at all. 
# Scale data and remove NA's
rownames(quant.preds.subset) <- quant.preds.subset$barcode
qps <- quant.preds.subset %>% scale() %>% na.omit() %>% as.data.frame()
pca <- qps %>% princomp()
pca$loadings
  # these are actually relatively interpretable!!!
# PC1: "shallowness"
  #   - max_depth
  #   - avg_depth
  #   + pct_aqveg
  #   + pct_area_le100
# PC2: "offshoreness"
  #   - pct_terr
  #   - pct_prm_wetf
  #   - pct_terr_shore_wetf
  #   + NEAR_FOREST_DIST.p
  #   (+ NEAR_TERR_DIST.p)
# PC3: "bigness"
  #   + Perimeter
  #   + shoreline_density_index
  #   + num_lentic_outl
  #   + Area

scores <- pca$scores %>% as.data.frame() %>% mutate(barcode = as.numeric(rownames(pca$scores)))
scores_plus <- left_join(scores, cat.preds, by = "barcode") %>% left_join(y = info.vars, by = "barcode")

scores_plus %>% ggplot(aes(x = Comp.1, y = Comp.2, col = AQUA_CODE))+
  geom_point()+
  scale_color_manual(name = "Aquatic Habitat Type", values = AQUA_2010_colors)+
  xlab("\"Shallowness\" (PC1)")+
  ylab("\"Offshoreness\" (PC2)")

library(ggfortify)
autoplot(prcomp(qps), data = scores_plus, 
         colour = "AQUA_CODE",
         size = 0.8,
         loadings = T, 
         loadings.colour = "black",
         loadings.label = T,
         loadings.label.size = 3,
         loadings.label.colour = "black")+
  scale_color_manual(name = "Aquatic Habitat Type", values = AQUA_2010_colors)+
  ggtitle("PCA for continuous predictor variables")+
  xlab("PC1 (23.44%): 'Shallowness'")+
  ylab("PC2 (16.32%): 'Offshoreness'")+
  guides(pch = guide_legend(override.aes = list(size=12))) #legend size

# Gower distance: distance matrix that accepts any kind of data that you give it. Categorical, continuous. 
# NMDS instead of PCA (harder to interpret that axes)
# PCoA will accept Gower. (parametric)