###############
# boosted regression trees
# REEMtree

# Molly explore Oct 2018

#########################################
#library(dismo)
#library(REEMtree)
library(gbm)

setwd("//Igsarfebfslacr3/Users/mvanappledorn/My Documents/Manuscripts/REU_Manuscripts_Summer2018/CWD_SpaceTime")

# Source libraries
source("Coarse-Woody-Debris-master/libraries.R")

# Load data
load("data/all_reduced_clean.Rda")
load("data/all2_reduced_clean.Rda")

# remove the river mile column from all_reduced_clean
all_reduced_clean$river_mile <- NULL

arc <- rbind(all_reduced_clean, all2_reduced_clean) # bind the two data sets together
arc$pool <- factor(arc$pool, levels(arc$pool)[c(2, 3, 1, 4, 5, 6)]) # convert `pool` to factor and relevel
locate.nas(arc) # NA's have already been removed. The column `near_forest_class` has also been removed. 

#exclude BWC-O
arc <- arc[which(arc$stratum %in% c("MCB-U","BWC-S","SCB","MCB-W","IMP-S")),]
levels(arc$stratum) <- c("BWC-O","BWC","CBW-P","CTR","IMP","IMP","MCB-U","MCB-W","SCB","TWZ","UXO","TRI")


################################################


brt1 <- gbm(snag~stratum+pool+perimeter+max_depth+avg_depth+tot_vol+
            shoreline_density_index+pct_terr+pct_prm_wetf+near_terr_dist+
            near_terr_name+near_forest_dist+year+wingdyke+riprap, 
            data=arc,  
            distribution="bernoulli",
            n.trees=100)
            #tree.complexity=5,
            #  learning.rate=0.01,bag.fraction=0.5)


