#========================================
# cforest tree on point level, excluding `stratum` as a predictor
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

# Remove some variables that we aren't going to use
combined <- combined %>% dplyr::select(-c(stratum, barcode, lcode, sdate, utm_e, utm_n, uniq_id, year, near_terr_name))

#=========================
# intialize functions
#=========================

# initialize function to make nicer variable importance plot
plotnice <- function(varimps, color, title){
  ggplot(data = varimps, aes(x = predictorvars, y = imps))+
    geom_col(fill = color)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x=element_blank())+
    ylab("Rel. importance")+
    ggtitle(title)+
    theme(text = element_text(size=20))
}

# initialize function for splitting data into cross-validation sets
splitup <- function(df, k){
  set.seed(1)
  inds <- sample(rep(1:k, length = nrow(df)))
  df$grp <- inds
  return(df)
}

#=================================
# Points, all pools 
#=================================

# split the full data set into 5 groups
combined <- splitup(combined, 5)

# define predictor variables for the full data set
predictorvars_all_nostratum <- c("perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_terr", "pct_prm_wetf", "near_terr_dist", "near_terr_class_7", "near_forest_dist", "wingdyke", "riprap")

# initialize an empty data frame to contain variable importance values
importances_all_nostratum <- data.frame(
  predictorvars_all_nostratum
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- combined %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable
  test <- combined %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp``
  
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag ~.-weight, 
                            data = train,
                            weights = weight,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632))
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights come from the model.
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_all_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_all_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_all_nostratum <- rowSums(importances_all_nostratum)/5 # calculate average variable importances
varimps_all_nostratum <- cbind(predictorvars_all_nostratum, imps_all_nostratum) %>% as.data.frame %>% mutate(imps_all_nostratum = as.numeric(as.character(imps_all_nostratum))) %>% mutate(predictorvars_all_nostratum = reorder(predictorvars_all_nostratum, -imps_all_nostratum))
colnames(varimps_all_nostratum) <- c("predictorvars", "imps")# get the data into the right format for ggplot
save(varimps_all_nostratum, file = "data/varimps_all_nostratum.Rda") #save the variable importances

p <- plotnice(varimps_all_nostratum, color = "olivedrab4", title = "Point RF var. importance (5fold), all pools (exc. stratum)")
ggsave(plot = p, "rf_avg_varimp_point_all_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5) #save the plot
save(cv_error_all_nostratum_landclass7, file = "data/cv_error_all_nostratum_landclass7.Rda") #save the error

#=============================
# Points, individual pools 
#=============================

predictorvars_pools <- c("perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_terr", "pct_prm_wetf", "near_terr_dist", "near_terr_class_7", "near_forest_dist", "wingdyke", "riprap")

#=============================
# pool 4

points_4 <- splitup(combined[combined$pool == 4,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool4_nostratum <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_4 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_4 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool4_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool4_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool4_nostratum <- rowSums(importances_pool4_nostratum)/5 # calculate average variable importances
varimps_pool4_nostratum <- cbind(predictorvars_pools, imps_pool4_nostratum) %>% as.data.frame %>% mutate(imps_pool4_nostratum = as.numeric(as.character(imps_pool4_nostratum))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool4_nostratum)) # get the data into the right format for ggplot
colnames(varimps_pool4_nostratum) <- c("predictorvars", "imps")
save(varimps_pool4_nostratum, file = "data/varimps_pool4_nostratum.Rda") #save the variable importances

p4 <- plotnice(varimps_pool4_nostratum, color = "cadetblue4", title = "Point RF var. importance (5fold), pool 4")
# save as rf_avg_varimp_point_pool4_nostratum_5foldcv.png
ggsave(plot = p4, 
       "rf_avg_varimp_point_pool4_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool4_nostratum_landclass7, file = "data/cv_error_pool4_nostratum_landclass7.Rda")

#=============================
# pool 8

points_8 <- splitup(combined[combined$pool == 8,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool8_nostratum <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_8 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_8 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool8_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool8_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool8_nostratum <- rowSums(importances_pool8_nostratum)/5 # calculate average variable importances
varimps_pool8_nostratum <- cbind(predictorvars_pools, imps_pool8_nostratum) %>% as.data.frame %>% mutate(imps_pool8_nostratum = as.numeric(as.character(imps_pool8_nostratum))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool8_nostratum)) # get the data into the right format for ggplot
colnames(varimps_pool8_nostratum) <- c("predictorvars", "imps")
save(varimps_pool8_nostratum, file = "data/varimps_pool8_nostratum.Rda") #save the variable importances

p8 <- plotnice(varimps_pool8_nostratum, color = "brown", title = "Point RF var. importance (5fold), pool 8")
# save as rf_avg_varimp_point_pool8_nostratum_5foldcv.png
ggsave(plot = p8, 
       "rf_avg_varimp_point_pool8_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool8_nostratum_landclass7, file = "data/cv_error_pool8_nostratum_landclass7.Rda")

#=============================
# pool 13

points_13 <- splitup(combined[combined$pool == 13,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool13_nostratum <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_13 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_13 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            strata = snag,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool13_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool13_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool13_nostratum <- rowSums(importances_pool13_nostratum)/5 # calculate average variable importances
varimps_pool13_nostratum <- cbind(predictorvars_pools, imps_pool13_nostratum) %>% as.data.frame %>% mutate(imps_pool13_nostratum = as.numeric(as.character(imps_pool13_nostratum))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool13_nostratum)) # get the data into the right format for ggplot
colnames(varimps_pool13_nostratum) <- c("predictorvars", "imps")
save(varimps_pool13_nostratum, file = "data/varimps_pool13_nostratum.Rda") #save the variable importances

p13 <- plotnice(varimps_pool13_nostratum, color = "darkblue", title = "Point RF var. importance (5fold), pool 13")
# save as rf_avg_varimp_point_pool13_nostratum_5foldcv.png
ggsave(plot = p13, 
       "rf_avg_varimp_point_pool13_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool13_nostratum_landclass7, file = "data/cv_error_pool13_nostratum_landclass7.Rda")

#=============================
# pool 26

points_26 <- splitup(combined[combined$pool == 26,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool26_nostratum <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_26 %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_26 %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            strata = snag,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_pool26_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool26_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool26_nostratum <- rowSums(importances_pool26_nostratum)/5 # calculate average variable importances
varimps_pool26_nostratum <- cbind(predictorvars_pools, imps_pool26_nostratum) %>% as.data.frame %>% mutate(imps_pool26_nostratum = as.numeric(as.character(imps_pool26_nostratum))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool26_nostratum)) # get the data into the right format for ggplot
colnames(varimps_pool26_nostratum) <- c("predictorvars", "imps")
save(varimps_pool26_nostratum, file = "data/varimps_pool26_nostratum.Rda") #save the variable importances

p26 <- plotnice(varimps_pool26_nostratum, color = "goldenrod2", title = "Point RF var. importance (5fold), pool 26")
p26 
# save as rf_avg_varimp_point_pool26_nostratum_5foldcv.png
ggsave(plot = p26, 
       "rf_avg_varimp_point_pool26_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool26_nostratum_landclass7, file = "data/cv_error_pool26_nostratum_landclass7.Rda")

#=============================
# LG
points_LG <- splitup(combined[combined$pool == "LG",], 5) # split data into 5 groups
table(points_LG$wingdyke) # no wingdyke points, so we have to exclude this as a predictor.

predictorvars_LG <- c("perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_terr", "pct_prm_wetf", "near_terr_dist", "near_terr_class_7", "near_forest_dist", "riprap")

# initialize an empty data frame to contain variable importance values
importances_poolLG_nostratum <- data.frame(
  predictorvars_LG
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_LG %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_LG %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            strata = snag,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_poolLG_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolLG_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolLG_nostratum <- rowSums(importances_poolLG_nostratum)/5 # calculate average variable importances
varimps_poolLG_nostratum <- cbind(predictorvars_LG, imps_poolLG_nostratum) %>% as.data.frame %>% mutate(imps_poolLG_nostratum = as.numeric(as.character(imps_poolLG_nostratum))) %>% mutate(predictorvars_LG = reorder(predictorvars_LG, -imps_poolLG_nostratum)) # get the data into the right format for ggplot
colnames(varimps_poolLG_nostratum) <- c("predictorvars", "imps")
save(varimps_poolLG_nostratum, file = "data/varimps_poolLG_nostratum.Rda") #save the variable importances

pLG <- plotnice(varimps_poolLG_nostratum, color = "mediumorchid3", title = "Point RF var. importance (5fold), pool LG")
pLG
# save as rf_avg_varimp_point_poolLG_nostratum_5foldcv.png
ggsave(plot = pLG, 
       "rf_avg_varimp_point_poolLG_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolLG_nostratum_landclass7, file = "data/cv_error_poolLG_nostratum_landclass7.Rda")

#=============================
# UPPER RIVER POOLS
points_UPPER <- splitup(combined[combined$pool %in% c("4", "8", "13"),], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_poolUPPER_nostratum <- data.frame(
  predictorvars_pools
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_UPPER %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_UPPER %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            strata = snag,
                            ntree = 25, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_poolUPPER_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolUPPER_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolUPPER_nostratum <- rowSums(importances_poolUPPER_nostratum)/5 # calculate average variable importances
varimps_poolUPPER_nostratum <- cbind(predictorvars_pools, imps_poolUPPER_nostratum) %>% as.data.frame %>% mutate(imps_poolUPPER_nostratum = as.numeric(as.character(imps_poolUPPER_nostratum))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_poolUPPER_nostratum)) # get the data into the right format for ggplot
colnames(varimps_poolUPPER_nostratum) <- c("predictorvars", "imps")
save(varimps_poolUPPER_nostratum, file = "data/varimps_poolUPPER_nostratum.Rda") #save the variable importances

pUPPER <- plotnice(varimps_poolUPPER_nostratum, color = "gray40", title = "Point RF var. importance (5fold), pool UPPER")
# save as rf_avg_varimp_point_poolUPPER_nostratum_5foldcv.png
ggsave(plot = pUPPER, 
       "rf_avg_varimp_point_poolUPPER_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolUPPER_nostratum_landclass7, file = "data/cv_error_poolUPPER_nostratum_landclass7.Rda")

#=============================
# LOWER RIVER SECTIONS: Pool 26, LG, and OR
points_LOWER <- splitup(combined[combined$pool %in% c("26", "LG", "OR"),], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_poolLOWER_nostratum <- data.frame(
  predictorvars_LG # using the predictors for LG, which excludes wingdyke
)

# intialize an empty vector to contain error rates
ers <- rep(NA, 5)

for(i in 1:5){
  print(paste("BEGINNING RUN", i))
  train <- points_LOWER %>% dplyr::filter(grp != i) %>% dplyr::select(-c(grp, pool, wingdyke)) # train on 4/5 of the data, remove `grp` because it's not a predictor variable.
  test <- points_LOWER %>% dplyr::filter(grp == i) %>% dplyr::select(-c(grp, pool, wingdyke)) # test on the remaining 1/5 of the data. Remove `grp`.
  print("calculating weights")
  w1 <- length(train$snag[train$snag == 0])/length(train$snag)
  w0 <- length(train$snag[train$snag == 1])/length(train$snag)
  train$weight <- ifelse(train$snag == 1, w1, w0)
  
  # build a tree
  print("growing forest")
  tree <- partykit::cforest(snag~.-weight, 
                            data = train,
                            weights = weight,
                            strata = snag,
                            ntree = 500, trace = T,
                            perturb = list(replace = F,
                                           fraction = 0.632)) # weights?
  print("making predictions")
  pred_resp <- predict.cforest(tree, newdata = test, type = "response") # response predictions. weights?
  print("constructing confusion matrix")
  cm <- table(true = test$snag, pred = predict(tree, newdata = test)) #confusion matrix
  print("calculating error rate")
  e <- (cm[1,2] + cm[2,1])/sum(cm) # calculate the error rate
  ers[i] <- e # assign to the i'th element of ers vector
  
  # calculate variable importances
  print("calculating variable importances")
  vi.part <- varimp(tree)
  importances_poolLOWER_nostratum[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolLOWER_nostratum_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolLOWER_nostratum <- rowSums(importances_poolLOWER_nostratum)/5 # calculate average variable importances
varimps_poolLOWER_nostratum <- cbind(predictorvars_LG, imps_poolLOWER_nostratum) %>% as.data.frame %>% mutate(imps_poolLOWER_nostratum = as.numeric(as.character(imps_poolLOWER_nostratum))) %>% mutate(predictorvars_LG = reorder(predictorvars_LG, -imps_poolLOWER_nostratum)) # get the data into the right format for ggplot
colnames(varimps_poolLOWER_nostratum) <- c("predictorvars", "imps")
save(varimps_poolLOWER_nostratum, file = "data/varimps_poolLOWER_nostratum.Rda") #save the variable importances

pLOWER <- plotnice(varimps_poolLOWER_nostratum, color = "gray40", title = "Point RF var. importance (5fold), pool LOWER")
pLOWER
# save as rf_avg_varimp_point_poolLOWER_nostratum_5foldcv.png
ggsave(plot = pLOWER, 
       "rf_avg_varimp_point_poolLOWER_nostratum_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolLOWER_nostratum_landclass7, file = "data/cv_error_poolLOWER_nostratum_landclass7.Rda")
#==================
# Load variable importances
load("data/varimps_all_nostratum.Rda")
varimps_all$pool <- "all"
load("data/varimps_pool4_nostratum.Rda")
varimps_pool4_nostratum$pool <- "4"
load("data/varimps_pool8_nostratum.Rda")
varimps_pool8_nostratum$pool <- "8"
load("data/varimps_pool13_nostratum.Rda")
varimps_pool13_nostratum$pool <- "13"
load("data/varimps_pool26_nostratum.Rda")
varimps_pool26_nostratum$pool <- "26"
load("data/varimps_poolLG_nostratum.Rda")
varimps_poolLG_nostratum$pool <- "LG"
load("data/varimps_poolOR_nostratum.Rda")
varimps_poolOR_nostratum$pool <- "OR"
load("data/varimps_poolUPPER_nostratum.Rda")
varimps_poolUPPER_nostratum$pool <- "UPPER"
load("data/varimps_poolLOWER_nostratum.Rda")
varimps_poolLOWER_nostratum$pool <- "LOWER"

# Bind together and reorder the factors
vi <- rbind(varimps_pool4_nostratum, varimps_pool8_nostratum, varimps_pool13_nostratum, varimps_pool26_nostratum, varimps_poolLG_nostratum, varimps_poolOR_nostratum, varimps_poolUPPER_nostratum, varimps_poolLOWER_nostratum)
vi$predictorvars <- factor(vi$predictorvars, levels(vi$predictorvars)[c(1, 9, 11, 4, 10, 12, 5, 2, 8, 13, 3, 7, 6)])
vi$pool <- as.factor(vi$pool)
vi$pool <- factor(vi$pool, levels(vi$pool)[c(3, 4, 1, 2, 5, 7, 8, 6)])

# Make plot
d <- ggplot(data = vi, aes(x = predictorvars, y = imps, fill = pool))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(name = "Pool", values = c("darkturquoise", "firebrick2", "royalblue4", "goldenrod2", "mediumorchid3", "black", "gray40", "gray70"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x=element_blank())+
  ylab("Permutation Importance (MDA)")+
  xlab("Variable")+
  ggtitle("Random forest variable importances by pool")+
  theme(text = element_text(size=20))
d

# Save plot
ggsave(plot = d, 
       "varimps_landclass7.png",
       width = 7.75, height = 5.5)

# plot importances for combined together
k <- ggplot(data = varimps_all, aes(x = predictorvars, y = imps))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x=element_blank())+
  ylab("Permutation Importance (MDA)")+
  xlab("Variable")+
  ggtitle("Random forest variable importances, combined pools")+
  theme(text = element_text(size=20))

# Save plot
ggsave(plot = k, 
       "varimps_landclass7_all.png",
       width = 7.75, height = 5.5)
