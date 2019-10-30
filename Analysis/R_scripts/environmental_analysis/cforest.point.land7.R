
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

# Remove some variables that we aren't going to use
combined <- combined %>% dplyr::select(-c(barcode, lcode, sdate, utm_e, utm_n, uniq_id, year, near_terr_name))

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
predictorvars_all <- c("stratum", "perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_terr", "pct_prm_wetf", "near_terr_dist", "near_terr_class_7", "near_forest_dist", "wingdyke", "riprap")

# initialize an empty data frame to contain variable importance values
importances_all <- data.frame(
  predictorvars_all
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
  importances_all[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_all_landclass7 <- mean(ers) # cross-validated mean error rate

imps_all <- rowSums(importances_all)/5 # calculate average variable importances
varimps_all <- cbind(predictorvars_all, imps_all) %>% as.data.frame %>% mutate(imps_all = as.numeric(as.character(imps_all))) %>% mutate(predictorvars_all = reorder(predictorvars_all, -imps_all))
colnames(varimps_all) <- c("predictorvars", "imps")# get the data into the right format for ggplot
save(varimps_all, file = "data/varimps_all.Rda") #save the variable importances

p <- plotnice(varimps_all, color = "olivedrab4", title = "Point RF var. importance (5fold), all pools")
ggsave(plot = p, "rf_avg_varimp_point_all_5foldcv_landclass7.png",
       width = 7, height = 4.5) #save the plot
save(cv_error_all_landclass7, file = "data/cv_error_all_landclass7.Rda") #save the error

#=============================
# Points, individual pools 
#=============================

predictorvars_pools <- c("stratum", "perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_terr", "pct_prm_wetf", "near_terr_dist", "near_terr_class_7", "near_forest_dist", "wingdyke", "riprap")

#=============================
# pool 4

points_4 <- splitup(combined[combined$pool == 4,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool4 <- data.frame(
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
  importances_pool4[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool4_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool4 <- rowSums(importances_pool4)/5 # calculate average variable importances
varimps_pool4 <- cbind(predictorvars_pools, imps_pool4) %>% as.data.frame %>% mutate(imps_pool4 = as.numeric(as.character(imps_pool4))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool4)) # get the data into the right format for ggplot
colnames(varimps_pool4) <- c("predictorvars", "imps")
save(varimps_pool4, file = "data/varimps_pool4.Rda") #save the variable importances

p4 <- plotnice(varimps_pool4, color = "cadetblue4", title = "Point RF var. importance (5fold), pool 4")
# save as rf_avg_varimp_point_pool4_5foldcv.png
ggsave(plot = p4, 
       "rf_avg_varimp_point_pool4_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool4_landclass7, file = "data/cv_error_pool4_landclass7.Rda")

#=============================
# pool 8

points_8 <- splitup(combined[combined$pool == 8,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool8 <- data.frame(
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
  importances_pool8[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool8_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool8 <- rowSums(importances_pool8)/5 # calculate average variable importances
varimps_pool8 <- cbind(predictorvars_pools, imps_pool8) %>% as.data.frame %>% mutate(imps_pool8 = as.numeric(as.character(imps_pool8))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool8)) # get the data into the right format for ggplot
colnames(varimps_pool8) <- c("predictorvars", "imps")
save(varimps_pool8, file = "data/varimps_pool8.Rda") #save the variable importances

p8 <- plotnice(varimps_pool8, color = "brown", title = "Point RF var. importance (5fold), pool 8")
# save as rf_avg_varimp_point_pool8_5foldcv.png
ggsave(plot = p8, 
       "rf_avg_varimp_point_pool8_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool8_landclass7, file = "data/cv_error_pool8_landclass7.Rda")

#=============================
# pool 13

points_13 <- splitup(combined[combined$pool == 13,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool13 <- data.frame(
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
  importances_pool13[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool13_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool13 <- rowSums(importances_pool13)/5 # calculate average variable importances
varimps_pool13 <- cbind(predictorvars_pools, imps_pool13) %>% as.data.frame %>% mutate(imps_pool13 = as.numeric(as.character(imps_pool13))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool13)) # get the data into the right format for ggplot
colnames(varimps_pool13) <- c("predictorvars", "imps")
save(varimps_pool13, file = "data/varimps_pool13.Rda") #save the variable importances

p13 <- plotnice(varimps_pool13, color = "darkblue", title = "Point RF var. importance (5fold), pool 13")
# save as rf_avg_varimp_point_pool13_5foldcv.png
ggsave(plot = p13, 
       "rf_avg_varimp_point_pool13_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool13_landclass7, file = "data/cv_error_pool13_landclass7.Rda")

#=============================
# pool 26

points_26 <- splitup(combined[combined$pool == 26,], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_pool26 <- data.frame(
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
  importances_pool26[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_pool26_landclass7 <- mean(ers) # cross-validated mean error rate

imps_pool26 <- rowSums(importances_pool26)/5 # calculate average variable importances
varimps_pool26 <- cbind(predictorvars_pools, imps_pool26) %>% as.data.frame %>% mutate(imps_pool26 = as.numeric(as.character(imps_pool26))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_pool26)) # get the data into the right format for ggplot
colnames(varimps_pool26) <- c("predictorvars", "imps")
save(varimps_pool26, file = "data/varimps_pool26.Rda") #save the variable importances

p26 <- plotnice(varimps_pool26, color = "goldenrod2", title = "Point RF var. importance (5fold), pool 26")
p26 
# save as rf_avg_varimp_point_pool26_5foldcv.png
ggsave(plot = p26, 
       "rf_avg_varimp_point_pool26_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_pool26_landclass7, file = "data/cv_error_pool26_landclass7.Rda")

#=============================
# LG
points_LG <- splitup(combined[combined$pool == "LG",], 5) # split data into 5 groups
table(points_LG$wingdyke) # no wingdyke points, so we have to exclude this as a predictor.

predictorvars_LG <- c("stratum", "perimeter", "max_depth", "avg_depth", "tot_vol", "shoreline_density_index", "pct_terr", "pct_prm_wetf", "near_terr_dist", "near_terr_class_7", "near_forest_dist", "riprap")

# initialize an empty data frame to contain variable importance values
importances_poolLG <- data.frame(
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
  importances_poolLG[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolLG_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolLG <- rowSums(importances_poolLG)/5 # calculate average variable importances
varimps_poolLG <- cbind(predictorvars_LG, imps_poolLG) %>% as.data.frame %>% mutate(imps_poolLG = as.numeric(as.character(imps_poolLG))) %>% mutate(predictorvars_LG = reorder(predictorvars_LG, -imps_poolLG)) # get the data into the right format for ggplot
colnames(varimps_poolLG) <- c("predictorvars", "imps")
save(varimps_poolLG, file = "data/varimps_poolLG.Rda") #save the variable importances

pLG <- plotnice(varimps_poolLG, color = "mediumorchid3", title = "Point RF var. importance (5fold), pool LG")
pLG
# save as rf_avg_varimp_point_poolLG_5foldcv.png
ggsave(plot = pLG, 
       "rf_avg_varimp_point_poolLG_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolLG_landclass7, file = "data/cv_error_poolLG_landclass7.Rda")

#=============================
# UPPER
points_UPPER <- splitup(combined[combined$pool %in% c("4", "8", "13"),], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_poolUPPER <- data.frame(
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
  importances_poolUPPER[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolUPPER_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolUPPER <- rowSums(importances_poolUPPER)/5 # calculate average variable importances
varimps_poolUPPER <- cbind(predictorvars_pools, imps_poolUPPER) %>% as.data.frame %>% mutate(imps_poolUPPER = as.numeric(as.character(imps_poolUPPER))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_poolUPPER)) # get the data into the right format for ggplot
colnames(varimps_poolUPPER) <- c("predictorvars", "imps")
save(varimps_poolUPPER, file = "data/varimps_poolUPPER.Rda") #save the variable importances

pUPPER <- plotnice(varimps_poolUPPER, color = "black", title = "Point RF var. importance (5fold), pool UPPER")
# save as rf_avg_varimp_point_poolUPPER_5foldcv.png
ggsave(plot = pUPPER, 
       "rf_avg_varimp_point_poolUPPER_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolUPPER_landclass7, file = "data/cv_error_poolUPPER_landclass7.Rda")

#=============================
# UPPER RIVER POOLS: 4, 8, AND 13
points_UPPER <- splitup(combined[combined$pool %in% c(4, 8, 13),], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_poolUPPER <- data.frame(
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
  importances_poolUPPER[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolUPPER_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolUPPER <- rowSums(importances_poolUPPER)/5 # calculate average variable importances
varimps_poolUPPER <- cbind(predictorvars_pools, imps_poolUPPER) %>% as.data.frame %>% mutate(imps_poolUPPER = as.numeric(as.character(imps_poolUPPER))) %>% mutate(predictorvars_pools = reorder(predictorvars_pools, -imps_poolUPPER)) # get the data into the right format for ggplot
colnames(varimps_poolUPPER) <- c("predictorvars", "imps")
save(varimps_poolUPPER, file = "data/varimps_poolUPPER.Rda") #save the variable importances

pUPPER <- plotnice(varimps_poolUPPER, color = "gray40", title = "Point RF var. importance (5fold), pool UPPER")
pUPPER
# save as rf_avg_varimp_point_poolUPPER_5foldcv.png
ggsave(plot = pUPPER, 
       "rf_avg_varimp_point_poolUPPER_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolUPPER_landclass7, file = "data/cv_error_poolUPPER_landclass7.Rda")

#=============================
# LOWER RIVER SECTIONS: Pool 26, LG, and OR
points_LOWER <- splitup(combined[combined$pool %in% c("26", "LG", "OR"),], 5) # split data into 5 groups

# initialize an empty data frame to contain variable importance values
importances_poolLOWER <- data.frame(
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
  importances_poolLOWER[,i] <- vi.part
  print(paste("Run", i, "complete"))
}
cv_error_poolLOWER_landclass7 <- mean(ers) # cross-validated mean error rate

imps_poolLOWER <- rowSums(importances_poolLOWER)/5 # calculate average variable importances
varimps_poolLOWER <- cbind(predictorvars_LG, imps_poolLOWER) %>% as.data.frame %>% mutate(imps_poolLOWER = as.numeric(as.character(imps_poolLOWER))) %>% mutate(predictorvars_LG = reorder(predictorvars_LG, -imps_poolLOWER)) # get the data into the right format for ggplot
colnames(varimps_poolLOWER) <- c("predictorvars", "imps")
save(varimps_poolLOWER, file = "data/varimps_poolLOWER.Rda") #save the variable importances

pLOWER <- plotnice(varimps_poolLOWER, color = "gray40", title = "Point RF var. importance (5fold), pool LOWER")
pLOWER
# save as rf_avg_varimp_point_poolLOWER_5foldcv.png
ggsave(plot = pLOWER, 
       "rf_avg_varimp_point_poolLOWER_5foldcv_landclass7.png",
       width = 7, height = 4.5)
save(cv_error_poolLOWER_landclass7, file = "data/cv_error_poolLOWER_landclass7.Rda")
#==================
# Load variable importances
load("data/varimps_all.Rda")
varimps_all$pool <- "all"
load("data/varimps_pool4.Rda")
varimps_pool4$pool <- "4"
load("data/varimps_pool8.Rda")
varimps_pool8$pool <- "8"
load("data/varimps_pool13.Rda")
varimps_pool13$pool <- "13"
load("data/varimps_pool26.Rda")
varimps_pool26$pool <- "26"
load("data/varimps_poolLG.Rda")
varimps_poolLG$pool <- "LG"
load("data/varimps_poolOR.Rda")
varimps_poolOR$pool <- "OR"
load("data/varimps_poolUPPER.Rda")
varimps_poolUPPER$pool <- "UPPER"
load("data/varimps_poolLOWER.Rda")
varimps_poolLOWER$pool <- "LOWER"

# Bind together and reorder the factors
vi <- rbind(varimps_pool4, varimps_pool8, varimps_pool13, varimps_pool26, varimps_poolLG, varimps_poolOR, varimps_poolUPPER, varimps_poolLOWER)
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
