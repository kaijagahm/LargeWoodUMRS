# CART
# Classification Tree with rpart
load("data/p8_5.Rda")
source("ownfunctions.R")
source("libraries.R")

#not going to use stratum because that's outdated: it's from the 1989 levels
#using riprap instead of the revetment values. 
p8_5 <- p8_5 %>% filter(!is.na(snag))
fittree <- rpart(snag ~ AQUA_CODE + Area + avg_depth + pct_terr + pct_prm_wetf + pct_terr_shore_wetf + sinuosity + NEAR_TERR_DIST + NEAR_TERR_CLASS_15_N + NEAR_TERR_HEIGHT_N + NEAR_FOREST_DIST + NEAR_FOREST_CLASS_31 + NEAR_FOREST_HEIGHT_N + depth.p + current.p + riprap.p + substrt.p + wingdyke.p + pct_area_le100,
                 method = "class", 
                 data = p8_5, cp = 0) #cp default is zero, setting it higher just cuts off the tree earlier. 
# we don't want any prior probabilities because we have no reason to believe that there are prior probabilities of CWD occurring or not
# we don't need a loss matrix because we don't particularly care which way our error goes. One type of misclassification error isn't better or worse than the other. 
#I don't know what the parameter "split" is.

printcp(fittree)
plotcp(fittree, col = "red", las = 2)

#the red line represents one standard error above the minimum. Can think of bringing anything below the line up to the line, so ignore anything below it. 
#from this plot, it looks like a good value to use is cp = 0.005. 
fittree2 <- prune(fittree, cp = 0.0051)
printcp(fittree2)

p8_5$CARTpreds <- predict(fittree2, type = "class")
a <- with(p8_5, table(CARTpreds, snag)) %>% prop.table()
wrongprop <- a[1,2] + a[2,1]
  
  
####################
rpart.plot(x = fittree2, type = 5, cex = 0.5)

###
# What if we fit an rpart model in the same way with the entire data set?
sites_aa_5m <- sites_aa_5m %>% filter(!is.na(snag))
fittree_all <- rpart(snag ~ pool + AQUA_CODE + Area + Perimeter + avg_depth + max_depth+ shoreline_density_index + pct_prm_lotic + pct_prm_lentic + wdl_p_m2 + pct_terr + pct_prm_wetf + sinuosity + NEAR_TERR_DIST + NEAR_TERR_CLASS_15_N + NEAR_TERR_CLASS_7_N + NEAR_TERR_HEIGHT_N + NEAR_FOREST_DIST + NEAR_FOREST_CLASS_31 + NEAR_FOREST_HEIGHT_N + depth.p + current.p + riprap.p + substrt.p + wingdyke.p + pct_area_le100,
                 method = "class", 
                 data = sites_aa_5m, cp = 0)

printcp(fittree_all)
plotcp(fittree_all, col = "red", las = 2)

#the red line represents one standard error above the minimum. Can think of bringing anything below the line up to the line, so ignore anything below it. 
#from this plot, it looks like a good value to use is cp = 0.005. 
fittree_all_2 <- prune(fittree_all, cp = 0.008)
printcp(fittree_all_2)

sites_aa_5m$CARTpreds <- predict(fittree_all_2, type = "class")
a <- with(sites_aa_5m, table(CARTpreds, snag)) %>% prop.table()
wrongprop <- a[1,2] + a[2,1]
wrongprop

####################
rpart.plot(x = fittree_all_2, type = 5, cex = 0.5)

#If we cut it off at 0.008, we only have 31% misclassification error and it seems like we could maybe make a model. 
#how do I go from a CART model to a glm??
#portion the data into training and test data
training.ind <- sample(nrow(sites_aa_5m), size = round(nrow(sites_aa_5m)/2), replace = F)
training <- sites_aa_5m[training.ind,]
test <- sites_aa_5m[-training.ind,]

model <- glm(snag ~ AQUA_CODE + depth.p + avg_depth + NEAR_FOREST_DIST + pct_prm_wetf, data = training, family = "binomial")
pred.prob <- predict.glm(model, newdata = test, type = "response")
test$pred <- ifelse(pred.prob > 0.5, 1, 0)
b <- with(test, table(pred, snag)) %>% prop.table()
wrongprop <- b[1,2] + b[2,1]
wrongprop
