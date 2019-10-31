
# Edited version of "datacleaning.clean.R" by Kaija Gahm, 11 Aug 2018 (p 4, 8, 13)
# Edited version of "datacleaning.26ilror.clean.R" by Kaija Gahm, 20 Aug 2018 (p 26, ilr, or)
# output: two .Rda files of cleaned data: 1) "all_reduced_clean.Rda" (p4,8,13) and 2) "all2_reduced_clean.Rda" (p26, ilr, or)

## Load functions and libraries
source("Analysis/R_scripts/ownfunctions.R")
source("Analysis/R_scripts/libraries.R")

## Load data
load("Analysis/data/fish_data_EF.Rda") # Fish sampling data
sites_p4p8p13 <- read.csv("Analysis/data/DataSets_7_7/AttributeTables/sites_p4p8p13.txt") # reprojected data
rivmi <- read.table("Analysis/data/p4p8p13_rivmile.txt", sep = ",", header = T) # river mile data
sites_aa_5m <- read.csv("Analysis/data/DataSets_7_7/AttributeTables/sites_aquaareas5m.txt") # merged aquatic areas data for points buffered by 5 meters.
sites_terrestrial <- read.csv("Analysis/data/DataSets_7_7/AttributeTables/sites_terrestrial.txt") # nearest terrestrial area info for each point
terrestrial_forests <- read.csv("Analysis/data/DataSets_7_7/AttributeTables/Terrestrial_Forests.txt") # info about terrestrial areas/forests
sites_forest <- read.csv("Analysis/data/DataSets_7_7/AttributeTables/sites_forest.txt") # nearest forest info for each point
lc_2010 <- read.csv("Analysis/data/DataSets_7_7/AttributeTables/lc_2010.txt") # 2010 landcover info

## Clean data
### sites_aa_5m is going to be the main dataset that we're working with. We need to add site barcodes, pools, and river miles.
### To add river mile data, we have to go via sites_p4p8p13.

### Join river miles to sites_p4p8p13.
sites_p4p8p13 %<>% rename(siteid = FID)
rivmi %<>% rename(siteid = TARGET_FID) 
rivmi_reduced <- rivmi %>% select(siteid, RIVER_MILE)
sites <- left_join(sites_p4p8p13, rivmi_reduced, by = "siteid")

### Join barcodes to sites_p4p8p13. This will allow for a join with sites_aa_5m.
sites %<>% rename(fishdata_rownum = Field1) # the Field1 column in this dataset corresponds to the row number in fish_data_EF.
rows <- sites$fishdata_rownum # get vector of fish data row numbers in the order they appear in the sites data.
sites$barcode <- fish_data_EF$barcode[rows] # add barcodes

### Join barcodes and pools for sites_aa_5m
sites_aa_5m$barcode <- fish_data_EF$barcode[rows]

### use barcode matching to join river miles into sites_aa_5m. 
sites_aa_5m <- left_join(sites_aa_5m, sites[,c("barcode", "RIVER_MILE")], by = "barcode")
### Ok, now we're done with the sites data frame.

### Add pool information
sites_aa_5m$pool <- fish_data_EF$pool[rows]

#reorder the columns so that barcode is first                                      
sites_aa_5m <- sites_aa_5m[,c(1:4, 71, 5:70)]   

# "Observations with value of 0 in all the columns from aqa_2010_lvl3_011918.shp do not intersect with the aquatic areas layer". I'd like these to have values of NA, not 0. 
badrows_5 <- sites_aa_5m  %>% filter(Perimeter == 0, Area == 0, avg_fetch == 0)
dim(badrows_5)

# Remove the bad rows
sites_aa_5m <- sites_aa_5m %>% filter(Field1 %notin% badrows_5$Field1)
dim(sites_aa_5m)

# Where do we have missing values?
locate.nas(sites_aa_5m)

# there are a concerning number of NA's in the `pool` column that shouldn't be there. Luckily, the `uniq_id` column tells us which pool these are from. 
addpools <- function(df){
  pools <- as.numeric(substr(x = as.character(df$uniq_id), 
                             start = 2, 
                             stop = 3))
  df$pool[is.na(df$pool)] <- pools[is.na(df$pool)]
  return(df)
}
sites_aa_5m <- addpools(sites_aa_5m)
locate.nas(sites_aa_5m)

# Add columns for *distance* to terrestrial areas
rows_5 <- sites_aa_5m$Field1 # `Field 1` in this dataset corresponds to the FID in the terrestrial areas dataset. 
sites_aa_5m$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$Field1 %in% rows_5] # which terrestrial region is the closest?
sites_aa_5m$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$Field1 %in% rows_5] # how far is that terrestrial region?

# Add information about nearest terrestrial areas
# We're going to pull terrestrial areas information columns from `lc_2010`, not from `terrestrial`, because the FID's don't match up in `terrestrial`.
columns_terr <- lc_2010[, c("FID", "CLASS_7_C", "CLASS_7_N")] # define which columns we want (class 7 names and codes)
sites_aa_5m <- left_join(sites_aa_5m, columns_terr, by = c("NEAR_TERR_FID" = "FID")) # join the columns

#change column names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
sites_aa_5m <- sites_aa_5m %>% dplyr::rename(NEAR_TERR_CLASS_7 = CLASS_7_C,
                                             NEAR_TERR_CLASS_7_N = CLASS_7_N)

# Add columns for *distance* to nearest forested area
sites_aa_5m$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$Field1 %in% rows_5] # which forested region is the closest?
sites_aa_5m$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$Field1 %in% rows_5] # how far is that forested region?

# Add information about nearest forested areas: 
# As long as we're only using the 7-class forest divisions, we don't need to add information, since they all have the same type.
table(terrestrial_forests$CLASS_7_N)

# Check for NA's
locate.nas(sites_aa_5m)
# we have a bunch of NA's for snag, and 7 for river mile. 

# Join relevant point-level fish survey data to the data frame.
tojoin_5 <- fish_data_EF %>% dplyr::select(year, sitetype, gear, wingdyke, riprap, trib, barcode) %>% unique() %>% filter(barcode %in% sites_aa_5m$barcode)
sites_aa_5m <- left_join(sites_aa_5m, tojoin_5, by = "barcode")

# Exclude values with a sitetype of 2 (fixed sites) and exclude fyke net sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(sitetype != 2) # remove fixed sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(gear == "D") # remove all but daytime electrofishing data
dim(sites_aa_5m)
# exclude MCB-W, but keep wingdyke column (wingdykes could be in other strata)
sites_aa_5m <- sites_aa_5m %>% filter(stratum != "MCB-W")

# Remove one point that has NEAR_TERR_CLASS.p equal to agriculture
table(sites_aa_5m$NEAR_TERR_CLASS_7_N)
sites_aa_5m <- sites_aa_5m %>% filter(NEAR_TERR_CLASS_7 != "Ag")

# Check that we've excluded any data before 1993
table(sites_aa_5m$year, exclude = NULL) # looks good


# Reducing the variables

# Exclude variables that we aren't going to use
names(sites_aa_5m)
excl1 <- c("OBJECTID", "aa_num", "AQUA_CODE", "AQUA_DESC", "Area", "Acres", "Hectares", "bath_pct", "sd_depth", "area_gt50", "area_gt100", "area_gt200", "area_gt300", "avg_fetch", "econ", "sill", "min_rm", "max_rm", "len_met", "len_outl", "pct_outl", "num_outl", "len_oute", "pct_oute", "num_oute", "pct_aqveg", "pct_opwat", "len_terr", "pct_chan", "len_wetf", "len_wd", "wdl_p_m2", "num_wd", "sco_wd", "psco_wd", "len_revln", "rev_p_m2", "num_rev", "pct_rev", "pct_rev2", "area_tpi1", "pct_tpi1", "area_tpi2", "pct_tpi2", "area_tpi3", "pct_tpi3", "area_tpi4", "pct_tpi4", "sinuosity", "year_phot", "NEAR_TERR_FID", "NEAR_FOREST_FID", "sitetype", "gear", "Join_Count", "FID", "TARGET_FID", "Field1", "pct2wetf", "trib")
all_reduced <- sites_aa_5m %>% dplyr::select(-excl1)
names(all_reduced)

# Change some names
all_reduced <- all_reduced %>% rename(perimeter = Perimeter,
                                      shoreline_density_index = sdi,
                                      pct_prm_wetf = pct1wetf,
                                      river_mile = RIVER_MILE,
                                      near_terr_dist = NEAR_TERR_DIST,
                                      near_terr_class_7 = NEAR_TERR_CLASS_7,
                                      near_terr_name = NEAR_TERR_CLASS_7_N,
                                      near_forest_dist = NEAR_FOREST_DIST)

# Make any -99's into NA's. Start at the second column to avoid the `barcode` column.
for(i in 2:ncol(all_reduced)){
  all_reduced[,i][all_reduced[,i] < -5000] <- NA
}

locate.nas(all_reduced)
arc <- as.data.table(all_reduced)
arc2 <- na.omit(object = arc, cols = c("snag", "max_depth", "avg_depth", "tot_vol", "wingdyke", "riprap"))
arc3 <- as.data.frame(arc2)

# Rename as all_reduced_clean
all_reduced_clean <- arc3
locate.nas(all_reduced_clean) # find the NA's: we still have a few for river mile, but they can stay. 
dim(all_reduced_clean) # check that dimensions match the consort diagram: should have 5439 observations at this point.

# Change data types of columns as needed
all_reduced_clean$pool <- factor(as.character(all_reduced_clean$pool)) 
all_reduced_clean$year <- as.numeric(all_reduced_clean$year)

# Exclude UXO
all_reduced_clean <- all_reduced_clean %>% filter(stratum != "UXO")
dim(all_reduced_clean)

# Export dataset
save(all_reduced_clean, file = "data/all_reduced_clean.Rda")

# Part II: pools 26, La grange, and Open River
# 

# Load files
load("data/DataSets_8_17/ltrm_fish_data_new.Rda") # Fish sampling data - made by MVA 10/31/18 by importing csv "ltrm_fish_data.csv" and exporting as Rda
#ltrm_fish_data <- read.csv("data/DataSets_8_17/ltrm_fish_data.csv",sep=",",header=TRUE)
 #save(ltrm_fish_data,file="data/DataSets_8_17/ltrm_fish_data_new.Rda")
sites_p26ilror <- read.csv("data/DataSets_8_17/AttributeTables/sites_p26ilror.txt") # reprojected data
sites_aa_5m <- read.csv("data/DataSets_8_17/AttributeTables/sites_aquaareas5m2.txt") # merged aquatic areas data for points buffered by 5 meters.
sites_terrestrial <- read.csv("data/DataSets_8_17/AttributeTables/sites_terrestrial2.txt") # nearest terrestrial area info for each point
terrestrial_forests <- read.csv("data/DataSets_8_17/AttributeTables/Terrestrial_Forests2.txt") # info about terrestrial areas/forests
sites_forest <- read.csv("data/DataSets_8_17/AttributeTables/sites_forest2.txt") # nearest forest info for each point
lc_2010 <- read.csv("data/DataSets_8_17/AttributeTables/lc_20102.txt") # landcover info

# Add a `year` column to ltrm_fish_data
ltrm_fish_data$year <- str_extract(as.character(ltrm_fish_data$sdate), pattern = "[:digit:]+$")

# Join barcodes from `ltrm_fish_data` to `sites_p26ilror` (the reprojected data)
rows <- sites_p26ilror$RowID_ # `RowID_` in this dataset corresonds to the row number in ltrm_fish_data.
sites_p26ilror$barcode <- ltrm_fish_data$barcode[rows] # Select the barcodes we want and add them as a column to sites_p26ilror
# This will allow for a join with sites_aquaareas5m2. 

# Join barcodes and pools for sites_aa_5m
sites_aa_5m$barcode <- ltrm_fish_data$barcode[rows]
sites_aa_5m$pool <- ltrm_fish_data$pool[rows]

#reorder the columns so that barcode is first                                      
sites_aa_5m <- sites_aa_5m[,c(1:4, 72, 5:71)]   

# "Observations with value of 0 in all the columns from aqa_2010_lvl3_011918.shp do not intersect with the aquatic areas layer". I'd like these to have values of NA, not 0. 
badrows_5 <- sites_aa_5m  %>% filter(Perimeter == 0, Area == 0, avg_fetch ==0)
dim(badrows_5)

# Remove the bad rows
sites_aa_5m <- sites_aa_5m %>% filter(RowID_ %notin% badrows_5$RowID_)
dim(sites_aa_5m)

# Where do we have missing values?
locate.nas(sites_aa_5m) # a few missing values for snag, but otherwise we're golden!

# Skip the part for fixing missing values of `pool` because we don't have any 

# Add columns for *distance* to terrestrial areas
rows_5 <- sites_aa_5m$RowID_ # `RowID_` in this dataset corresponds to the FID in the terrestrial areas dataset. 
sites_aa_5m$NEAR_TERR_FID <- sites_terrestrial$NEAR_FID[sites_terrestrial$RowID_ %in% rows_5] # which terrestrial region is the closest?
sites_aa_5m$NEAR_TERR_DIST <- sites_terrestrial$NEAR_DIST[sites_terrestrial$RowID_ %in% rows_5] # how far is that terrestrial region?

# Add information about nearest terrestrial areas
# We're going to pull terrestrial areas information columns from `lc_2010`, not from `terrestrial`, because the FID's don't match up in `terrestrial`.
columns_terr <- lc_2010[, c("FID", "CLASS_7_C", "CLASS_7_N")] # define which columns we want (class 7 names and codes)
sites_aa_5m <- left_join(sites_aa_5m, columns_terr, by = c("NEAR_TERR_FID" = "FID")) # join the columns

#change column names to indicate that these columns refer to the landcover type of the nearest terrestrial area.
sites_aa_5m <- sites_aa_5m %>% dplyr::rename(NEAR_TERR_CLASS_7 = CLASS_7_C,
                                             NEAR_TERR_CLASS_7_N = CLASS_7_N)

# Add columns for *distance* to nearest forested area
sites_aa_5m$NEAR_FOREST_FID <- sites_forest$NEAR_FID[sites_forest$RowID_ %in% rows_5] # which forested region is the closest?
sites_aa_5m$NEAR_FOREST_DIST <- sites_forest$NEAR_DIST[sites_forest$RowID_ %in% rows_5] # how far is that forested region?

# Add information about nearest forested areas: 
# As long as we're only using the 7-class forest divisions, we don't need to add information, since they all have the same type.
table(terrestrial_forests$CLASS_7_N)

# Check for NA's
locate.nas(sites_aa_5m)
# still, our only NA's are in the `snag` column. Excellent.

# Join relevant point-level fish survey data to the data frame.
tojoin_5 <- ltrm_fish_data %>% dplyr::select(year, sitetype, gear, wingdyke, riprap, trib, barcode) %>% unique() %>% filter(barcode %in% sites_aa_5m$barcode)
sites_aa_5m <- left_join(sites_aa_5m, tojoin_5, by = "barcode")


# Exclude values with a sitetype of 2 (fixed sites) and exclude fyke net sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(sitetype != 2) # remove fixed sites
dim(sites_aa_5m)
sites_aa_5m <- sites_aa_5m %>% filter(gear == "D") # remove all but daytime electrofishing data
dim(sites_aa_5m)
# exclude MCB-W, but keep wingdyke column (wingdykes could be in other strata)
sites_aa_5m <- sites_aa_5m %>% filter(stratum != "MCB-W")


# What distribution of NEAR_TERR_CLASS_7 do we have?
table(sites_aa_5m$NEAR_TERR_CLASS_7_N)

# Check that we've excluded any data before 1993
table(sites_aa_5m$year, exclude = NULL) # looks good


# Reducing the variables
# Exclude variables that we aren't going to use
names(sites_aa_5m)
excl1 <- c("OBJECTID", "aa_num", "AQUA_CODE", "AQUA_DESC", "Area", "Acres", "Hectares", "bath_pct", "sd_depth", "area_gt50", "area_gt100", "area_gt200", "area_gt300", "avg_fetch", "econ", "sill", "min_rm", "max_rm", "len_met", "len_outl", "pct_outl", "num_outl", "len_oute", "pct_oute", "num_oute", "pct_aqveg", "pct_opwat", "len_terr", "pct_chan", "len_wetf", "len_wd", "wdl_p_m2", "num_wd", "sco_wd", "psco_wd", "len_revln", "rev_p_m2", "num_rev", "pct_rev", "pct_rev2", "area_tpi1", "pct_tpi1", "area_tpi2", "pct_tpi2", "area_tpi3", "pct_tpi3", "area_tpi4", "pct_tpi4", "sinuosity", "year_phot", "NEAR_TERR_FID", "NEAR_FOREST_FID", "sitetype", "gear", "Join_Count", "FID", "TARGET_FID", "RowID_", "pct2wetf", "trib")
all2_reduced <- sites_aa_5m %>% dplyr::select(-excl1)
names(all2_reduced)

# Change some names
all2_reduced <- all2_reduced %>% rename(perimeter = Perimeter,
                                        shoreline_density_index = sdi,
                                        pct_prm_wetf = pct1wetf,
                                        near_terr_dist = NEAR_TERR_DIST,
                                        near_terr_class_7 = NEAR_TERR_CLASS_7,
                                        near_terr_name = NEAR_TERR_CLASS_7_N,
                                        near_forest_dist = NEAR_FOREST_DIST)

# Make any -99's into NA's. Start at the second column to avoid the `barcode` column.
for(i in 2:ncol(all2_reduced)){
  all2_reduced[,i][all2_reduced[,i] < -5000] <- NA
}

locate.nas(all2_reduced)
a2r <- as.data.table(all2_reduced)
a2r2 <- na.omit(object = a2r, cols = c("snag", "max_depth", "avg_depth", "tot_vol", "wingdyke", "riprap"))
a2r3 <- as.data.frame(a2r2)
dim(a2r3)

# Rename as all_reduced_clean
all2_reduced_clean <- a2r3
locate.nas(all2_reduced_clean) # find the NA's
dim(all2_reduced_clean) # check that dimensions match the consort diagram

# Change data types of columns as needed
all2_reduced_clean$pool <- factor(as.character(all2_reduced_clean$pool)) 
all2_reduced_clean$year <- as.numeric(all2_reduced_clean$year)

# Remove the OBJECTID_1 column that's still kicking around
all2_reduced_clean$OBJECTID_1 <- NULL

# Export dataset
save(all2_reduced_clean, file = "data/all2_reduced_clean.Rda")