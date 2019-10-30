
#MVA: Set working directory
setwd("//Igsarfebfslacr3/Users/mvanappledorn/My Documents/Manuscripts/REU_Manuscripts_Summer2018/CWD_SpaceTime")
############################
#New reprojected electrofishing and fyke net dataset from Molly.
source("Coarse-Woody-Debris-master/ownfunctions.R")
source("Coarse-Woody-Debris-master/libraries.R")
#fish_data_EF <- read.csv("data/DataSets_7_7/ltrm_fish_data_EF.csv")
#save(fish_data_EF, file = "data/fish_data_EF.Rda")
load("data/fish_data_EF.Rda")
fish_data_EF$year <- str_extract(as.character(fish_data_EF$sdate), pattern = "[:digit:]+$")

aqa_2010_lvl3 <- read.csv("data/DataSets_7_7/AttributeTables/aqa_2010_lvl3_011918.txt")
lc_2010 <- read.csv("data/DataSets_7_7/AttributeTables/lc_2010.txt")
sites_aa <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas.txt")
sites_aa_5m <- read.csv("data/DataSets_7_7/AttributeTables/sites_aquaareas5m.txt")
sites_forest <- read.csv("data/DataSets_7_7/AttributeTables/sites_forest.txt")
sites_p4p8p13 <- read.csv("data/DataSets_7_7/AttributeTables/sites_p4p8p13.txt")
sites_terrestrial <- read.csv("data/DataSets_7_7/AttributeTables/sites_terrestrial.txt")
terrestrial_forests <- read.csv("data/DataSets_7_7/AttributeTables/Terrestrial_Forests.txt")
rivmi <- read.csv("data/p4p8p13_rivmile.txt")

# Join barcodes and pool # from `fish_data_EF` to `sites_p4p8p13` (the reprojected data)
rows <- sites_p4p8p13$Field1
barcodes <- fish_data_EF$barcode[rows]
pools <- fish_data_EF$pool[rows]
sites_p4p8p13$barcode <- barcodes
sites_p4p8p13$pool <- pools
#check that we only have pools 4, 8, and 13
table(sites_p4p8p13$pool)
#good. 
# Join the river mile data
sites_p4p8p13 <- dplyr::left_join(sites_p4p8p13, rivmi[,c("RIVER_MILE", "TARGET_FID")], by = c("FID" = "TARGET_FID"))

# Also join barcodes for sites_aa and sites_aa_5m
sites_aa$barcode <- barcodes
sites_aa$pool <- pools
sites_aa_5m$barcode <- barcodes
sites_aa_5m$pool <- pools
#reorder the columns so that barcode is first
sites_aa <- sites_aa[,c(1:4, 71, 5:70)]
sites_aa_5m <- sites_aa_5m[,c(1:4, 71, 5:70)]
# join river miles
sites_aa <- dplyr::left_join(sites_aa, sites_p4p8p13[,c("barcode", "RIVER_MILE")], by = "barcode")
sites_aa_5m <- dplyr::left_join(sites_aa_5m, sites_p4p8p13[,c("barcode", "RIVER_MILE")], by = "barcode")

# "Observations with value of 0 in all the columns from aqa_2010_lvl3_011918.shp do not intersect with the aquatic areas layer"
# I'd like these to have values of NA, not 0. 
# Figure out which variables come from the aqa_2010_lvl2 file
badrows_0 <- sites_aa %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
badrows_5 <- sites_aa_5m  %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
dim(badrows_0)
dim(badrows_5)
#nice. we see a reduction in the number of bad rows once points are buffered by 5m, just like we expected. 

# Remove the bad rows
sites_aa <- sites_aa %>% filter(Field1 %notin% badrows_0$Field1)
sites_aa_5m <- sites_aa_5m %>% filter(Field1 %notin% badrows_5$Field1)
dim(sites_aa)
dim(sites_aa_5m)

#there are a concerning number of NA's in the `pool` column that shouldn't be there. Luckily, the `uniq_id` column tells us which pool these are from. 
addpools <- function(sites_aa){
  pools <- as.numeric(substr(x = as.character(sites_aa$uniq_id), 
                             start = 2, 
                             stop = 3))
  sites_aa$pool[is.na(sites_aa$pool)] <- pools[is.na(sites_aa$pool)]
  return(sites_aa)
}
sites_aa <- addpools(sites_aa)
sites_aa_5m <- addpools(sites_aa_5m)

# Add columns for distance to terrestrial areas
rows_0 <- sites_aa$Field1
rows_5 <- sites_aa_5m$Field1
sites_aa$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$Field1 %in% rows_0]
sites_aa$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$Field1 %in% rows_0]

sites_aa_5m$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$Field1 %in% rows_5]
sites_aa_5m$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$Field1 %in% rows_5]

# We're going to pull terrestrial areas information columns from `lc_2010`, not from `terrestrial`, because the FID's don't match up in `terrestrial`.
# Which columns do we want to pull in? Need info on what these columns mean.
columns_terr <- lc_2010[, c("FID", "CLASS_31", "CLASS_15_C", "CLASS_7_C","CLASS_31_N","CLASS_15_N", "CLASS_7_N", "HEIGHT_N")]
sites_aa <- left_join(sites_aa, columns_terr, by = c("NEAR_TERR_FID" = "FID"))
sites_aa_5m <- left_join(sites_aa_5m, columns_terr, by = c("NEAR_TERR_FID" = "FID"))
#change names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
sites_aa <- sites_aa %>% dplyr::rename(NEAR_TERR_CLASS_31 = CLASS_31,
                    NEAR_TERR_CLASS_15 = CLASS_15_C,
                    NEAR_TERR_CLASS_7 = CLASS_7_C,
                    NEAR_TERR_CLASS_31_N = CLASS_31_N,
                    NEAR_TERR_CLASS_15_N = CLASS_15_N,
                    NEAR_TERR_CLASS_7_N = CLASS_7_N,
                    NEAR_TERR_HEIGHT_N = HEIGHT_N)

sites_aa_5m <- sites_aa_5m %>% dplyr::rename(NEAR_TERR_CLASS_31 = CLASS_31,
                                       NEAR_TERR_CLASS_15 = CLASS_15_C,
                                       NEAR_TERR_CLASS_7 = CLASS_7_C,
                                       NEAR_TERR_CLASS_31_N = CLASS_31_N,
                                       NEAR_TERR_CLASS_15_N = CLASS_15_N,
                                       NEAR_TERR_CLASS_7_N = CLASS_7_N,
                                       NEAR_TERR_HEIGHT_N = HEIGHT_N)

# Add columns for distance to nearest forested area
sites_aa$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$Field1 %in% rows_0]
sites_aa$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$Field1 %in% rows_0]

sites_aa_5m$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$Field1 %in% rows_5]
sites_aa_5m$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$Field1 %in% rows_5]

#join the attribute columns for the nearest *forested* area
columns_forest <- terrestrial_forests[, c("FID", "CLASS_31", "CLASS_15_C", "CLASS_7_C", "CLASS_31_N", "CLASS_15_N", "CLASS_7_N", "HEIGHT_N")]
sites_aa <- left_join(sites_aa, columns_forest, by = c("NEAR_FOREST_FID" = "FID"))
sites_aa_5m <- left_join(sites_aa_5m, columns_forest, by = c("NEAR_FOREST_FID" = "FID"))
#change names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
sites_aa <- sites_aa %>% dplyr::rename(NEAR_FOREST_CLASS_31 = CLASS_31,
                                       NEAR_FOREST_CLASS_15 = CLASS_15_C,
                                       NEAR_FOREST_CLASS_7 = CLASS_7_C,
                                       NEAR_FOREST_CLASS_31_N = CLASS_31_N,
                                       NEAR_FOREST_CLASS_15_N = CLASS_15_N,
                                       NEAR_FOREST_CLASS_7_N = CLASS_7_N,
                                       NEAR_FOREST_HEIGHT_N = HEIGHT_N)
sites_aa_5m <- sites_aa_5m %>% dplyr::rename(NEAR_FOREST_CLASS_31 = CLASS_31,
                                       NEAR_FOREST_CLASS_15 = CLASS_15_C,
                                       NEAR_FOREST_CLASS_7 = CLASS_7_C,
                                       NEAR_FOREST_CLASS_31_N = CLASS_31_N,
                                       NEAR_FOREST_CLASS_15_N = CLASS_15_N,
                                       NEAR_FOREST_CLASS_7_N = CLASS_7_N,
                                       NEAR_FOREST_HEIGHT_N = HEIGHT_N)

# Any NA's?
locate.nas(sites_aa)
locate.nas(sites_aa_5m)
#looks good except for a few NA's for river mile, which we're going to ignore for now.  

# What are the levels of terrestrial habitat types?
table(droplevels(sites_aa$NEAR_TERR_CLASS_31_N)) #looks good, no water
table(droplevels(sites_aa_5m$NEAR_TERR_CLASS_31_N)) #likewise. 

# Append point-level data to each data frame
tojoin_0 <- fish_data_EF %>% dplyr::select(year, sitetype, depth, current, gear, stageht, substrt, wingdyke, riprap, trib, barcode) %>%
  unique() %>% filter(barcode %in% sites_aa$barcode)
tojoin_5 <- fish_data_EF %>% dplyr::select(year, sitetype, depth, current, gear, stageht, substrt, wingdyke, riprap, trib, barcode) %>%
  unique() %>% filter(barcode %in% sites_aa_5m$barcode)
sites_aa <- left_join(sites_aa, tojoin_0, by = "barcode")
sites_aa_5m <- left_join(sites_aa_5m, tojoin_5, by = "barcode")

pointcols <- c("year", "sitetype","depth", "current", "gear", "stageht", "substrt", "wingdyke", "riprap", "trib", "NEAR_TERR_DIST", "NEAR_TERR_FID", "NEAR_TERR_CLASS_31", "NEAR_TERR_CLASS_15", "NEAR_TERR_CLASS_7", "NEAR_TERR_CLASS_31_N", "NEAR_TERR_CLASS_15_N", "NEAR_TERR_CLASS_7_N", "NEAR_TERR_HEIGHT_N", "NEAR_FOREST_DIST", "NEAR_FOREST_FID", "NEAR_FOREST_CLASS_31", "NEAR_FOREST_CLASS_15", "NEAR_FOREST_CLASS_7", "NEAR_FOREST_CLASS_31_N", "NEAR_FOREST_CLASS_15_N", "NEAR_FOREST_CLASS_7_N", "NEAR_FOREST_HEIGHT_N")
for(i in 1:length(pointcols)){
  colnames(sites_aa)[colnames(sites_aa) == pointcols[i]] <- paste0(pointcols[i], ".p")
  colnames(sites_aa_5m)[colnames(sites_aa_5m) == pointcols[i]] <- paste0(pointcols[i], ".p")
}

# Exclude values with a sitetype of 2 (fixed sites) and exclude fyke net sites
sites_aa <- sites_aa %>% filter(sitetype.p != 2, gear.p == "D")
sites_aa_5m <- sites_aa_5m %>% filter(sitetype.p != 2)
sites_aa_5m <- sites_aa_5m %>% filter(gear.p == "D")

#make the rest of the cleaning into a function so it can be applied to sites_aa_5m as well as sites_aa. 
furthercleaning <- function(sites_aa){
#create a "snagyn" column
sites_aa$snagyn <- ifelse(sites_aa$snag == 0, "no", "yes") # new column based on `snag`

#change some of the column names
names(sites_aa)[names(sites_aa) == "sdi"] <- "shoreline_density_index" # sdi
names(sites_aa)[names(sites_aa) == "pct1wetf"] <- "pct_prm_wetf" # pct1wetf
names(sites_aa)[names(sites_aa) == "pct2wetf"] <- "pct_terr_shore_wetf" # pct2wetf
names(sites_aa)[names(sites_aa) == "len_outl"] <- "len_prm_lotic" # len_outl
names(sites_aa)[names(sites_aa) == "pct_outl"] <- "pct_prm_lotic" # pct_outl
names(sites_aa)[names(sites_aa) == "num_outl"] <- "num_lotic_outl" # num_outl
names(sites_aa)[names(sites_aa) == "len_oute"] <- "len_prm_lentic" # len_oute
names(sites_aa)[names(sites_aa) == "pct_oute"] <- "pct_prm_lentic" # pct_oute
names(sites_aa)[names(sites_aa) == "num_oute"] <- "num_lentic_outl" # num_oute
names(sites_aa)[names(sites_aa) == "pct_chan"] <- "pct_aq" # pct_chan
names(sites_aa)[names(sites_aa) == "sco_wd"] <- "scour_wd" # sco_wd
names(sites_aa)[names(sites_aa) == "pct_rev"] <- "pct_terr_shore_rev" # pct_rev
names(sites_aa)[names(sites_aa) == "pct_rev2"] <- "pct_prm_rev" # pct_rev2

#create reverse of area_gt* columns
#how much of the polygon is less than or equal to a certain depth? (cm)
sites_aa$area_le50 <- sites_aa$Area - sites_aa$area_gt50
sites_aa$area_le100 <- sites_aa$Area - sites_aa$area_gt100
sites_aa$area_le200 <- sites_aa$Area - sites_aa$area_gt200
sites_aa$area_le300 <- sites_aa$Area - sites_aa$area_gt300
sites_aa$pct_area_le100 <- sites_aa$area_le100/sites_aa$Area
sites_aa$pct_area_le50 <- sites_aa$area_le50/sites_aa$Area
sites_aa$pct_area_le200 <- sites_aa$area_le200/sites_aa$Area
sites_aa$pct_area_le300 <- sites_aa$area_le300/sites_aa$Area

#create column with stratum names
sites_aa$stratum_name[sites_aa$stratum == "SCB"] <- "Side Channel Border"
sites_aa$stratum_name[sites_aa$stratum == "MCB-U"] <- "Main Channel Border--Unstructured"
sites_aa$stratum_name[sites_aa$stratum == "MCB-W"] <- "Main Channel Border--Wing Dam Area"
sites_aa$stratum_name[sites_aa$stratum == "TWZ"] <- "Tailwater Zone"
sites_aa$stratum_name[sites_aa$stratum == "BWC-S"] <- "Backwater, Contiguous Shoreline"
sites_aa$stratum_name[sites_aa$stratum == "IMP-O"] <- "Impounded--Offshore"
sites_aa$stratum_name[sites_aa$stratum == "IMP-S"] <- "Impounded--Shoreline"
sites_aa$stratum_name <- factor(sites_aa$stratum_name)

#remove weird slightly negative values of sinuosity
sites_aa$sinuosity[sites_aa$sinuosity == -9999] <- NA
sites_aa$sinuosity[sites_aa$sinuosity<0 & sites_aa$sinuosity > -9999] <- 0

#Make any -99's into NA's. Start at 4 to avoid the barcodes.
for(i in 18:ncol(sites_aa)){
  sites_aa[,i][sites_aa[,i] < -5000] <- NA
}
return(sites_aa)
}
sites_aa <- furthercleaning(sites_aa)
sites_aa_5m <- furthercleaning(sites_aa_5m)
#end function

#pct_prm_rev and pct_terr_shore_rev should not have any values greater than 100
#
fixrevetment <- function(sites_aa){
  sites_aa$pct_prm_rev[sites_aa$pct_prm_rev > 100] <- 100
  sites_aa$pct_terr_shore_rev[sites_aa$pct_terr_shore_rev > 100] <- 100
  #set NA's to 0
  sites_aa$pct_prm_rev[is.na(sites_aa$pct_prm_rev)] <- 0
  sites_aa$pct_terr_shore_rev[is.na(sites_aa$pct_terr_shore_rev)] <- 0
  return(sites_aa)
}
sites_aa <- fixrevetment(sites_aa)
sites_aa_5m <- fixrevetment(sites_aa_5m)

#remove acres and hectares columns
sites_aa$Acres <- NULL
sites_aa$Hectares <- NULL
sites_aa_5m$Acres <- NULL
sites_aa_5m$Hectares <- NULL

# Remove one point that has NEAR_TERR_CLASS.p equal to agriculture
sites_aa_5m <- sites_aa_5m %>% filter(NEAR_TERR_CLASS_7.p != "Ag")

## Make sure all columns are in the right format and correct them if they aren't
  sites_aa$pool <- factor(as.character(sites_aa$pool))
  sites_aa$year.p <- as.numeric(sites_aa$year.p)
  sites_aa$substrt.p <- factor(as.character(sites_aa$substrt.p))
  sites_aa$wingdyke.p <- factor(as.character(sites_aa$wingdyke.p))
  sites_aa$riprap.p <- factor(as.character(sites_aa$riprap.p))
  sites_aa$trib.p <- factor(as.character(sites_aa$trib.p))
  sites_aa$snagyn <- factor(sites_aa$snagyn)
  sites_aa$sitetype.p <- factor(as.character(sites_aa$sitetype))
  
  sites_aa_5m$pool <- factor(as.character(sites_aa_5m$pool))
  sites_aa_5m$year.p <- as.numeric(sites_aa_5m$year.p)
  sites_aa_5m$substrt.p <- factor(as.character(sites_aa_5m$substrt.p))
  sites_aa_5m$wingdyke.p <- factor(as.character(sites_aa_5m$wingdyke.p))
  sites_aa_5m$riprap.p <- factor(as.character(sites_aa_5m$riprap.p))
  sites_aa_5m$trib.p <- factor(as.character(sites_aa_5m$trib.p))
  sites_aa_5m$snagyn <- factor(sites_aa_5m$snagyn)
  sites_aa_5m$sitetype.p <- factor(as.character(sites_aa_5m$sitetype))

save(sites_aa, file = "data/sites_aa.Rda")
save(sites_aa_5m, file = "data/sites_aa_5m.Rda")

# Save subsets by pool
    # Pool 4
    p4_0 <- sites_aa %>% filter(pool == 4)
    p4_5 <- sites_aa_5m %>% filter(pool == 4)
    # Pool 8
    p8_0 <- sites_aa %>% filter(pool == 8)
    p8_5 <- sites_aa_5m %>% filter(pool == 8)
    # Pool 13
    p13_0 <- sites_aa %>% filter(pool == 13)
    p13_5 <- sites_aa_5m %>% filter(pool == 13)
subsets <- list(p4_0, p4_5, p8_0, p8_5, p13_0, p13_5)
names(subsets) <- c("p4_0", "p4_5", "p8_0", "p8_5", "p13_0", "p13_5")

for(i in 1:length(subsets)){
  df <- subsets[[i]]
  write.csv(df, file = paste("data/", names(subsets)[i], ".csv", sep = ""))
}

#############
#############
#############
# Reducing the number of variables
# Ok, let's do this systematically. 
load("data/sites_aa_5m.Rda")
source("ownfunctions.R")
source("libraries.R")
#exclude variables that we really just don't care about for analysis purposes
names(sites_aa_5m)
excl_1 <- c("FID", "Join_Count", "TARGET_FID", "Field1", "lcode", "sdate", "utm_e", "utm_n", "OBJECTID", "aa_num", "AQUA_DESC", "bath_pct", "area_gt50", "area_gt100", "area_gt200", "area_gt300", "min_rm", "max_rm", "len_met", "len_prm_lotic", "len_prm_lentic", "len_terr", "len_wetf", "len_wd", "scour_wd", "psco_wd", "len_revln", "area_tpi1", "pct_tpi1", "area_tpi2", "pct_tpi2", "area_tpi3", "pct_tpi3", "area_tpi4", "pct_tpi4", "year_phot", "area_le50", "area_le100", "area_le200", "area_le300", "gear.p", "sitetype.p")
all_reduced1 <- sites_aa_5m %>% dplyr::select(-excl_1)
names(all_reduced1)
# exclude sd_depth because it doesn't seem very relevant
all_reduced1 <- all_reduced1 %>% dplyr::select(-c(sd_depth))
names(all_reduced1)
# exclude avg_fetch, sill, num_rev, and num_wd because they aren't biologically logical
all_reduced1 <- all_reduced1 %>% dplyr::select(-c(avg_fetch, sill, num_rev, num_wd))
names(all_reduced1)
# Molly says to get rid of all terrestrial categories below _31, as well as the _HEIGHT category.

all_reduced <- all_reduced1 %>% dplyr::select(-c(NEAR_TERR_CLASS_15.p, NEAR_TERR_CLASS_15_N.p, NEAR_TERR_CLASS_7.p, NEAR_TERR_CLASS_7_N.p, NEAR_TERR_HEIGHT_N.p, NEAR_FOREST_CLASS_15.p, NEAR_FOREST_CLASS_15_N.p, NEAR_FOREST_CLASS_7_N.p, NEAR_FOREST_CLASS_7.p, NEAR_FOREST_HEIGHT_N.p))
names(all_reduced)

# After `glmer` rejected the NEAR_TERR_CLASS_31_N.p because it had too few points per category, we're going to change this and keep the 7-level one instead.
all_reduced_7 <- all_reduced1 %>% dplyr::select(-c(NEAR_TERR_CLASS_31.p, NEAR_TERR_CLASS_31_N.p, NEAR_TERR_CLASS_15.p, NEAR_TERR_CLASS_15_N.p, NEAR_TERR_HEIGHT_N.p, NEAR_FOREST_CLASS_31.p, NEAR_FOREST_CLASS_31_N.p, NEAR_FOREST_CLASS_15_N.p, NEAR_FOREST_CLASS_15.p, NEAR_FOREST_HEIGHT_N.p))
names(all_reduced_7)


# as per JC's recommendation and discussion with KathiJo and Molly, exclude revetment categories. Also exclude wdl_p_m2 because it has a lot of NA's and we have a binary `wingdyke` index to use instead that has far fewer NA's. 
all_reduced <- all_reduced %>% dplyr::select(-c(pct_prm_rev, pct_terr_shore_rev, rev_p_m2, wdl_p_m2))
all_reduced_7 <- all_reduced_7 %>% dplyr::select(-c(pct_prm_rev, pct_terr_shore_rev, rev_p_m2, wdl_p_m2))
names(all_reduced)
names(all_reduced_7)
# Exclude pct_area_le50, pct_area_le200, and pct_area_le300 because they're redundant with pct_area_100
all_reduced <- all_reduced %>% dplyr::select(-c(pct_area_le50, pct_area_le200, pct_area_le300))
all_reduced_7 <- all_reduced_7 %>% dplyr::select(-c(pct_area_le50, pct_area_le200, pct_area_le300))
names(all_reduced)
names(all_reduced_7)
# Exclude NEAR_TERR_FID.p and NEAR_FOREST_FID.p columns because we've decided we're not going to try to include two levels of random variables (as per discussion with Barb on 7/12/18)
all_reduced <- all_reduced %>% dplyr::select(-c(NEAR_TERR_FID.p, NEAR_FOREST_FID.p))
names(all_reduced)
all_reduced_7 <- all_reduced_7 %>% dplyr::select(-c(NEAR_TERR_FID.p, NEAR_FOREST_FID.p))
names(all_reduced_7)
# Exclude pct_opwat because it's the inverse of pct_aqveg, and exclude pct_aq because it's the inverse of pct_terr
all_reduced <- all_reduced %>% dplyr::select(-c(pct_opwat, pct_aq))
names(all_reduced)
all_reduced_7 <- all_reduced_7 %>% dplyr::select(-c(pct_opwat, pct_aq))
names(all_reduced_7)
#separate variables into quantitative predictors, categorical predictors, and other informational/index variables
quant.preds <- all_reduced %>% dplyr::select(barcode, Area, Perimeter, max_depth, avg_depth, tot_vol, shoreline_density_index, econ, pct_prm_lotic, num_lotic_outl, pct_prm_lentic, num_lentic_outl, pct_aqveg, pct_terr, pct_prm_wetf, pct_terr_shore_wetf, sinuosity, NEAR_TERR_DIST.p, NEAR_FOREST_DIST.p, depth.p, current.p, stageht.p, pct_area_le100)
str(quant.preds) #check that everything is numeric or integer. We're good. 

cat.preds <- all_reduced %>% dplyr::select(barcode, stratum, AQUA_CODE, pool, NEAR_TERR_CLASS_31.p, NEAR_FOREST_CLASS_31.p, substrt.p, wingdyke.p, riprap.p, trib.p)
str(cat.preds) #check that everything is a factor
#sitetype isn't a factor. Need to go back and change that. 

info.vars <- all_reduced %>% dplyr::select(barcode, snag, snagyn, uniq_id, NEAR_TERR_CLASS_31_N.p, NEAR_FOREST_CLASS_31_N.p, year.p, stratum_name)
str(info.vars) #examine structure

save(quant.preds, file = "data/sites_quantpreds.Rda")
save(cat.preds, file = "data/sites_catpreds.Rda")
save(info.vars, file = "data/sites_infovars.Rda")
save(all_reduced, file = "data/all_reduced.Rda")
save(all_reduced_7, file = "data/all_reduced_7.Rda")
chisq.test(all_reduced$stratum, all_reduced$AQUA_CODE)$expected

#===================================================
# Polygon-level data
#===================================================
load("data/all_reduced.Rda")
source("libraries.R")
source("ownfunctions.R")
source("color_schemes.R")
# Summarize by polygon
# There are multiple strata per polygon so we'll exclude "stratum" as a column for that
strata <- table(all_reduced$uniq_id, all_reduced$stratum) %>% prop.table(margin = 1) %>% as.data.frame.matrix()
strata$uniq_id <- row.names(strata)
row.names(strata) <- NULL

strata[, "max.prop"] <- apply(strata[, 1:11], 1, max)
nrow(strata[strata$max.prop < 0.75,])/nrow(strata)
#continue to exclude stratum for now, but we need to run this by USGS people.

# Same thing with NEAR_TERR_CLASS_31.p and NEAR_FOREST_CLASS_31.p and substrt

poly <- all_reduced %>% group_by(uniq_id) %>% 
  filter(!is.na(snag)) %>%
  summarize(propsnag = sum(snag)/n(),
            n = n(),
            AQUA_CODE = firstel(AQUA_CODE),
            pool = firstel(pool),
            Area = firstel(Area),
            Perimeter = firstel(Perimeter),
            max_depth = firstel(max_depth),
            avg_depth = firstel(avg_depth),
            tot_vol = firstel(tot_vol),
            shoreline_density_index = firstel(shoreline_density_index),
            pct_aqveg = firstel(pct_aqveg),
            pct_terr = firstel(pct_terr),
            pct_prm_wetf = firstel(pct_prm_wetf),
            pct_terr_shore_wetf = firstel(pct_terr_shore_wetf),
            medianNEAR_TERR_DIST.p = median(NEAR_TERR_DIST.p, na.rm = T),
            medianNEAR_FOREST_DIST.p = median(NEAR_FOREST_DIST.p, na.rm = T),
            #median_depth.p = median(depth.p, na.rm = T), #it's kind of redundant to include median point depth when we already have several measures of depth on the polygon level.
            median_current.p = median(current.p, na.rm = T),
            propwingdyke = sum(as.numeric(as.character(wingdyke.p)))/n(),
            propriprap = sum(as.numeric(as.character(riprap.p)))/n(),
            proptrib = sum(as.numeric(as.character(trib.p)))/n(),
            pct_area_le100 = firstel(pct_area_le100)) %>%
  as.data.frame()
head(poly)
poly$propwingdyke <- ifelse(poly$propwingdyke > 0, "1", "0")
poly$propriprap <- ifelse(poly$propriprap > 0, "1", "0")
poly$proptrib <- ifelse(poly$proptrib > 0, "1", "0")
poly <- poly %>% rename(wingdyke = propwingdyke, 
                        riprap = propriprap,
                        trib = proptrib) %>% 
  mutate_if(is.character, as.factor)

# Are some of these variables highly correlated?
locate.nas(poly)
poly <- na.omit(poly) #exclude rows that have NA's, since imputing them tends to mess things up.
c <- cor(poly[,c("Area", "Perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_aqveg", "pct_terr", "pct_prm_wetf", "pct_terr_shore_wetf", "medianNEAR_TERR_DIST.p", "medianNEAR_FOREST_DIST.p", "median_current.p", "pct_area_le100")])
corrplot(c, method = "circle", 
         type = "lower", 
         diag = F)
#see high correlations: absolute value >0.6
chigh <- c
chigh[abs(chigh) <= 0.6] <- NA
corrplot(chigh, method = "circle",
         type = "lower",
         diag = F)
#let's get rid of pct_area_le100, because it's highly correlated with both avg_depth and pct_aqveg. Also get rid of Area because it's highly correlated with tot_vol and Perimeter. pct_terr_shore_wetf is confusing. Also get rid of `trib` because it has low importance/often is excluded from the model due to lack of information.
poly <- poly %>% dplyr::select(-c(Area, pct_area_le100, pct_terr_shore_wetf, trib))


save(poly, file = "data/poly.Rda")



