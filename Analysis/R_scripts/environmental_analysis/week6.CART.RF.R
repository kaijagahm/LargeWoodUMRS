#CART modeling
##########################################################
# CART
# Classification Tree with rpart
load("data/all_reduced.Rda")
source("ownfunctions.R")
source("libraries.R")

# Exclude points where snag is NA and fixed sites. 
# Pool 4
p4_5 <- all_reduced %>% filter(pool == 4, !is.na(snag))
# Pool 8
p8_5 <- all_reduced %>% filter(pool == 8, !is.na(snag))
# Pool 13
p13_5 <- all_reduced %>% filter(pool == 13, !is.na(snag))
# All
all_reduced <- all_reduced %>% filter(!is.na(snag))
#using stratum for predictive purposes
#using riprap instead of the revetment values.
formula <- snag ~ year.p + stratum + pool + AQUA_CODE + Area + avg_depth + shoreline_density_index + pct_prm_lotic + pct_prm_lentic + pct_terr + pct_prm_wetf + sinuosity + NEAR_TERR_DIST.p + NEAR_TERR_CLASS_31.p + NEAR_FOREST_DIST.p + NEAR_FOREST_CLASS_31.p + depth.p + current.p + riprap.p + substrt.p + trib.p + wingdyke.p + pct_area_le100 #+ year.p

tree.all <- rpart(formula,
                  method = "class", 
                  data = all_reduced, cp = 0)

printcp(tree.all)
plotcp(tree.all, col = "red", las = 2)

#the red line represents one standard error above the minimum. Can think of bringing anything below the line up to the line, so ignore anything below it. 
tree.all.2 <- prune(tree.all, cp = 0.002)
printcp(tree.all.2)

all_reduced$CARTpreds.all <- predict(tree.all.2, type = "class")
cm.trall.teall <- with(all_reduced, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.teall <- cm.trall.teall[1,2] + cm.trall.teall[2,1]
er.trall.teall
#test on pool 4
p4_5$CARTpreds.all <- predict(tree.all.2, newdata = p4_5, type = "class")
cm.trall.te4 <- with(p4_5, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.te4 <- cm.trall.te4[1,2] + cm.trall.te4[2,1]
er.trall.te4
#test on pool 8
p8_5$CARTpreds.all <- predict(tree.all.2, newdata = p8_5, type = "class")
cm.trall.te8 <- with(p8_5, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.te8 <- cm.trall.te8[1,2] + cm.trall.te8[2,1]
er.trall.te8
#test on pool 13
p13_5$CARTpreds.all <- predict(tree.all.2, newdata = p13_5, type = "class")
cm.trall.te13 <- with(p13_5, table(CARTpreds.all, snag)) %>% 
  prop.table()
er.trall.te13 <- cm.trall.te13[1,2] + cm.trall.te13[2,1]
er.trall.te13

#rpart.plot(x = tree.all.2, type = 3, cex = 0.2, main = "Tree for Pools 4, 8, and 13")

### pool4 only 
tree.p4 <- rpart(formula,
                 method = "class", 
                 data = p4_5, cp = 0)

printcp(tree.p4)
plotcp(tree.p4, col = "red", las = 2)
tree.p4.2 <- prune(tree.p4, cp = 0.01)
printcp(tree.p4.2)

#test on pool 4
p4_5$CARTpreds.p4 <- predict(tree.p4.2, type = "class")
cm.tr4.te4 <- with(p4_5, table(CARTpreds.p4, snag)) %>% 
  prop.table()
er.tr4.te4 <- cm.tr4.te4[1,2] + cm.tr4.te4[2,1]
er.tr4.te4
#test on pool 8
p8_5$CARTpreds.p4 <- predict(tree.p4.2, newdata = p8_5, type = "class")
cm.tr4.te8 <- with(p8_5, table(CARTpreds.p4, snag)) %>% 
  prop.table()
er.tr4.te8 <- cm.tr4.te8[1,2] + cm.tr4.te8[2,1]
er.tr4.te8
#test on pool 13
p13_5$CARTpreds.p4 <- predict(tree.p4.2, newdata = p13_5, type = "class")
cm.tr4.te13 <- with(p13_5, table(CARTpreds.p4, snag)) %>% 
  prop.table()
er.tr4.te13 <- cm.tr4.te13[1,2] + cm.tr4.te13[2,1]
er.tr4.te13

#rpart.plot(x = tree.p4.2, type = 5, cex = 0.7, main = "Tree for Pool 4, including Year")

### pool8 only 
tree.p8 <- rpart(formula,
                 method = "class", 
                 data = p8_5, cp = 0)

printcp(tree.p8)
plotcp(tree.p8, col = "red", las = 2)
tree.p8.2 <- prune(tree.p8, cp = 0.006)
printcp(tree.p8.2)

#test on pool 8
p8_5$CARTpreds.p8 <- predict(tree.p8.2, type = "class")
cm.tr8.te8 <- with(p8_5, table(CARTpreds.p8, snag)) %>% 
  prop.table()
er.tr8.te8 <- cm.tr8.te8[1,2] + cm.tr8.te8[2,1]
er.tr8.te8
#test on pool 4
p4_5$CARTpreds.p8 <- predict(tree.p8.2, newdata = p4_5, type = "class")
cm.tr8.te4 <- with(p4_5, table(CARTpreds.p8, snag)) %>% 
  prop.table()
er.tr8.te4 <- cm.tr8.te4[1,2] + cm.tr8.te4[2,1]
er.tr8.te4
#test on pool 13
p13_5$CARTpreds.p8 <- predict(tree.p8.2, newdata = p13_5, type = "class")
cm.tr8.te13 <- with(p13_5, table(CARTpreds.p8, snag)) %>% 
  prop.table()
er.tr8.te13 <- cm.tr8.te13[1,2] + cm.tr8.te13[2,1]
er.tr8.te13

#rpart.plot(x = tree.p8.2, type = 5, cex = 0.7, main = "Tree for Pool 8, including Year")

### pool13 only 
tree.p13 <- rpart(formula,
                  method = "class", 
                  data = p13_5, cp = 0)

printcp(tree.p13)
plotcp(tree.p13, col = "red", las = 2)
tree.p13.2 <- prune(tree.p13, cp = 0.007)
printcp(tree.p13.2)

#test on pool 13
p13_5$CARTpreds.p13 <- predict(tree.p13.2, type = "class")
cm.tr13.te13 <- with(p13_5, table(CARTpreds.p13, snag)) %>% 
  prop.table()
er.tr13.te13 <- cm.tr13.te13[1,2] + cm.tr13.te13[2,1]
er.tr13.te13
#test on pool 4
p4_5$CARTpreds.p13 <- predict(tree.p13.2, newdata = p4_5, type = "class")
cm.tr13.te4 <- with(p4_5, table(CARTpreds.p13, snag)) %>% 
  prop.table()
er.tr13.te4 <- cm.tr13.te4[1,2] + cm.tr13.te4[2,1]
er.tr13.te4
#test on pool 8
p8_5$CARTpreds.p13 <- predict(tree.p13.2, newdata = p8_5, type = "class")
cm.tr13.te8 <- with(p8_5, table(CARTpreds.p13, snag)) %>% 
  prop.table()
er.tr13.te8 <- cm.tr13.te8[1,2] + cm.tr13.te8[2,1]
er.tr13.te8

#rpart.plot(x = tree.p13.2, type = 5, cex = 0.7, main = "Tree for Pool 13, including year")

###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
# Random Forest Modeling
source("libraries.R")
source("ownfunctions.R")
load("data/all_reduced.Rda") # 5m buffer, reduced variables as per cwd.datacleaning.R
all_reduced$snag <- factor(as.character(all_reduced$snag))
all_reduced <- all_reduced %>% filter(!is.na(snag))
head(all_reduced, 3)
dim(all_reduced)
all_reduced <- droplevels(all_reduced)
locate.nas(all_reduced)
  # Missing values have to be either imputed or removed, or filled in with something logical. Let's look at variables with lots of NA's and see if we can do anything about them. 
# max_depth, avg_depth, and tot_vol each have 16 NA's. I'm not too worried about those; they can be imputed.
# econ has 3104 NA's, which is a big problem. Why is this computed only for lentic areas? What's preventing us from computing it for lotic areas as well? Which would be better, computing it for lotic areas or removing it from the model
# pct_prm_lotic has 1578 NA's. It's computed only for lentic areas.  
# num_lotic_outl has 1578 NA's. It's computed only for lentic areas.
# sinuosity has 2352 NA's. It's computed only for lotic areas. (Why can't we compute it for lentic areas too?)
# depth.p has real missing values. Fine to impute this.
# current.p has real missing values. Fine to impute this.
# stageht.p has a lot of missing values and it's somewhat redundant with depth.p as well. Might mess things up to impute this. Better to just exclude it.
# substrt.p has real missing values. Fine to impute this.
# stratum name has missing values because we didn't define names for a few of the strata in Pool 8 or Pool 13. This doesn't matter because it's not going to be used in modeling. 


#Let's remove econ, pct_prm_lotic, num_lotic_outl, sinuosity, and stageht.p.
sites <- all_reduced %>% dplyr::select(-c(econ, pct_prm_lotic, num_lotic_outl, sinuosity, stageht.p))
#remove the informational variables
sites <- sites %>% dplyr::select(-c(barcode, uniq_id, NEAR_TERR_CLASS_31.p, NEAR_FOREST_CLASS_31.p, year.p, snagyn, stratum_name))
locate.nas(sites)

# separate into training and test data, taking 1/3 of the data to be test data (as per the powerpoint slides and the paper)
# Note that Dittman et al. (2015) found that having a slightly unbalanced sample didn't cause significant problems for RF. They used 35/65 vs. 50/50, and although 50/50 was better, it wasn't significantly so. My data is approx. 42/57, so I won't worry about it too much. 
nrow_Train <- 2*round(nrow(sites)/3)
intrain <- sample(1:nrow(sites), nrow_Train, replace = F)
train <- sites[intrain,]
test <- sites[-intrain,]

#=====================
# random forests
#=====================
#impute missing values for training and test data
train <- rfImpute(x = train[,-2], y = train$snag)
names(train)[1] <- "snag"
test <- rfImpute(x = test[,-2], y = test$snag)
names(test)[1] <- "snag"

# to make sure the imputation was successful, check for NA's
locate.nas(train)
locate.nas(test)

# Make a tree
rf.fit <- randomForest(snag~.,
                       data = train,
                       cutoff = c(0.43, 0.57),
                       importance = T)
# predict on the training data
rf.pred <- predict(rf.fit, newdata = train, type = "class")
table(rf.pred, train$snag)
(error_rate <- mean(rf.pred!=train$snag, na.rm = T))

# predict on the test data
rf.pred <- predict(rf.fit, newdata = test, type = "class")
table(rf.pred, test$snag)
(error_rate <- mean(rf.pred!=test$snag, na.rm = T))

# out-of bag error rate
rf.fit
  #Error rate plot over number of trees
  err.rate <- rf.fit$err.rate %>% as.data.frame()
  err.rate$ntrees <- 1:nrow(err.rate)
  err.rate <- melt(err.rate, id.vars = "ntrees")
  names(err.rate)[names(err.rate) == "variable"] <- "type"
      #make the plot
      err.rate %>% ggplot(aes(x = ntrees, 
                              y = value, 
                              color = type))+
        geom_line()+
        scale_color_manual(name = "Type", values = c("blue", "black", "red"))+
        ylab("Error Rate")+
        xlab("Number of Trees in Forest")+
        ggtitle("Error Rates for Random Forest Classification")+
        theme_bw()
      
  #these error rates are quite different, and it makes us wonder if we should balance the data. 

# variable importance
varImpPlot(rf.fit)
# explanation of variable importance plots
#https://www.quora.com/How-do-you-explain-%E2%80%98mean-decrease-accuracy%E2%80%99-and-%E2%80%98mean-decrease-gini%E2%80%99-in-layman%E2%80%99s-terms
  # Note that all the caveats apply here: mixture of continuous and categorical variables, vastly different numbers of categories in the categorical variables, and comparing numeric variables with vastly different scales. 
  # The conclusion from most sources seems to be that MDA is more useful with respect to the whole forest than Gini
  # Follow up on the top few variables 

# MDS plot of the proximities
# https://stats.stackexchange.com/questions/137358/what-is-meant-by-proximity-in-random-forests
rf.fit <- randomForest(snag~., data = train,
                       na.action = na.roughfix,
                       proximity = TRUE)
MDSplot(rf.fit, train$snag)

rf.fit$confusion # confusion matrix
hist(rf.fit$oob.times)#how many times was each point allowed to be out of the bag?


#===========================
# random forest with data by polygon (presence/absence of snag)
#===========================
load("data/poly.Rda")
source("libraries.R")
source("ownfunctions.R")
locate.nas(poly)
#omit NA's
poly <- na.omit(poly)

# split propsnag into snag presence/absence
poly$pa <- NULL
poly$pa[poly$propsnag == 0] <- "absence"
poly$pa[poly$propsnag > 0] <- "presence"
poly$pa <- as.factor(poly$pa)
  # now we have very unbalanced classes.
poly <- poly %>% dplyr::select(-c("propsnag", "uniq_id", "n"))

# separate into training and test data, taking 1/3 of the data to be test data (as per the powerpoint slides and the paper)
# We're going to deal with data imbalance using the BRF (balanced random forest) technique, also known as downsampling. See http://appliedpredictivemodeling.com/blog/2013/12/8/28rmc2lv96h8fw8700zm4nl50busep and http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf for guidelines on this.  
nrow_Train <- 2*round(nrow(poly)/3)
intrain <- sample(1:nrow(poly), nrow_Train, replace = F)
train <- poly[intrain,]
test <- poly[-intrain,]

# how many samples are in the minority class?
nmin <- nrow(train[train$pa == "absence",])
nmin
nmax <- nrow(train[train$pa == "presence",])
nmax

# Make a balanced tree
set.seed(4)
tree_balanced <- randomForest(pa ~., 
                       data = train,
                       strata = pa,
                       sampsize = nmax,
                       ntree = 1000,
                       importance = TRUE,
                       cutoff = c(.3, .7))

# predict on the test data
bal.pred <- predict(tree_balanced, newdata = test, type = "class")
table(bal.pred, test$pa)
(error_rate <- mean(bal.pred!=test$pa, na.rm = T))

# plot the error rate over number of trees
#with ggplot
err.rate <- tree_balanced$err.rate %>% as.data.frame()
err.rate$ntrees <- 1:nrow(err.rate)
err.rate <- melt(err.rate, id.vars = "ntrees")
names(err.rate)[names(err.rate) == "variable"] <- "type"
#make the plot
err.rate %>% ggplot(aes(x = ntrees, 
                        y = value, 
                        color = type))+
  geom_line()+
  scale_color_manual(name = "Type", values = c("blue", "black", "red"))+
  ylab("Error Rate")+
  xlab("Number of Trees in Forest")+
  ggtitle("Error Rates for Random Forest Classification (4)")+
  theme_bw()


# Make an unbalanced tree for comparison
tree_ub <- randomForest(pa ~., 
                        data = train,
                        ntree = 1000,
                        importance = TRUE)
# predict on the test data
bal.pred <- predict(tree_ub, newdata = test, type = "class")
table(bal.pred, test$pa)
(error_rate <- mean(bal.pred!=test$pa, na.rm = T))

# plot the error rate over number of trees
#with ggplot
err.rate <- tree_ub$err.rate %>% as.data.frame()
err.rate$ntrees <- 1:nrow(err.rate)
err.rate <- melt(err.rate, id.vars = "ntrees")
names(err.rate)[names(err.rate) == "variable"] <- "type"
#make the plot
err.rate %>% ggplot(aes(x = ntrees, 
                        y = value, 
                        color = type))+
  geom_line()+
  scale_color_manual(name = "Type", values = c("blue", "black", "red"))+
  ylab("Error Rate")+
  xlab("Number of Trees in Forest")+
  ggtitle("Error Rates for Random Forest Classification")+
  theme_bw()

#===========================
# Random forest *regression* tree to predict `propsnag`, not binary categories
#===========================
load("data/poly.Rda")
source("libraries.R")
source("ownfunctions.R")
locate.nas(poly)
poly <- na.omit(poly) #for now, we'll just drop the rows that have NA's in them. But I don't understand why the depth information is missing, and this may significantly affect the models. Can we fill in the missing data?

poly <- poly %>% dplyr::select(-c(uniq_id, n))

# separate into training and test data, taking 1/3 of the data to be test data (as per the powerpoint slides and the paper)
# We're going to deal with data imbalance using the BRF (balanced random forest) technique, also known as downsampling. See http://appliedpredictivemodeling.com/blog/2013/12/8/28rmc2lv96h8fw8700zm4nl50busep and http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf for guidelines on this.  
nrow_Train <- 2*round(nrow(poly)/3)
intrain <- sample(1:nrow(poly), nrow_Train, replace = F)
train <- poly[intrain, ]
test <- poly[-intrain,]

# Make a regression tree
tree_reg <- randomForest(propsnag ~., 
                         data = train, 
                         ntree = 10000, 
                         importance = TRUE)

# Write a function to transform variable importance values and plot them in a pretty way. 
plotimportance <- function(tree_reg, color){
(importance <- importance(tree_reg, type = 1) %>% as.data.frame) #have to specify type = 1 to get mean decrease in accuracy; the default type = 2 is mean decrease in node impurity, which is unreliable and subject to bias. 
importance$var <- c("Habitat", "Pool", "Perimeter", "Max. Depth", "Avg. Depth", "Volume", "Shoreline Density Index", "% Aquatic Veg.", "% Terrestrial Border", "% Forest Border", "Med. Distance to Land", "Med. Distance to Forest", "Med. Current", "Wingdam", "Revetment", "Tributary Mouth")
names(importance)[names(importance) == "%IncMSE"] <- "Pct_Inc_MSE"
importance <- importance %>% arrange(desc(Pct_Inc_MSE)) %>% 
  mutate(var = factor(var)) %>% mutate(var = reorder(var, -Pct_Inc_MSE))
importance$var

  #nicer variable importance plot
ggplot(data = importance, aes(x = var, y = Pct_Inc_MSE))+
  geom_col(fill = color)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x=element_blank())+
  ylab("Relative Importance")+
  ggtitle(paste0("Random Forest Variable Importance for ", quote(tree_reg)))+
  theme(text = element_text(size=20))
}
plotimportance(tree_reg, color = "olivedrab4")

# if we run this a couple times, we see that it's not completely robust, but the same variables are jumping to the top every time. 
# predict on the training data
pred <- predict(tree_reg, newdata = train, type = "response")
train$pred <- pred
MSE <- mean((train$pred - train$propsnag)^2)
MSE

# predict on the test data
pred1 <- predict(tree_reg, newdata = test, type = "response")
test$pred <- pred1
MSE1 <- mean((test$pred - test$propsnag)^2)
MSE1

# Plot the (OOB?) MSE over number of trees.
df <- cbind(1:1000, tree_reg$mse) %>% as.data.frame() %>% rename(ntree = V1, OOB_MSE = V2)
  
ggplot(data = df, aes(x = ntree, y = OOB_MSE))+
  geom_line()+
  theme_bw() + ylab("Out-of-bag MSE rate") + xlab("Number of Trees in Forest") + ggtitle("Error Rate by Number of Trees")

#==============================
# How transferrable is this between pools?
#==============================
poly_4 <- poly[poly$pool == 4,]
  #separate into training and test data
  nrow_Train4 <- 2*round(nrow(poly_4)/3)
  intrain4 <- sample(1:nrow(poly_4), nrow_Train4, replace = F)
  train4 <- poly_4[intrain4,]
  test4 <- poly_4[-intrain4,]

poly_8 <- poly[poly$pool == 8,]
  #separate into training and test data
  nrow_Train8 <- 2*round(nrow(poly_8)/3)
  intrain8 <- sample(1:nrow(poly_8), nrow_Train8, replace = F)
  train8 <- poly_8[intrain8,]
  test8 <- poly_8[-intrain8,]
  
poly_13 <- poly[poly$pool == 13,]
  #separate into training and test data
  nrow_Train13 <- 2*round(nrow(poly_13)/3)
  intrain13 <- sample(1:nrow(poly_13), nrow_Train13, replace = F)
  train13 <- poly_13[intrain13,]
  test13 <- poly_13[-intrain13,]

# Make a regression tree for pool 8
tree8 <- randomForest(propsnag ~., 
                      data = train8, 
                      ntree = 10000, 
                      importance = T)
plotimportance(tree8, color = "brown")

# predict on the other pools
pred8_4 <- predict(tree8, newdata = test4, type = "response")
(MSE1 <- mean((pred8_4 - test4$propsnag)^2))
pred8_13 <- predict(tree8, newdata = test13, type = "response")
(MSE1 <- mean((pred8_13 - test13$propsnag)^2))

# Make a regression tree for pool 4
tree4 <- randomForest(propsnag ~., 
                      data = train4, 
                      ntree = 10000, 
                      importance = T)
plotimportance(tree4, color = "cadetblue4")

# predict on the other pools
pred4_8 <- predict(tree4, newdata = test8, type = "response")
(MSE1 <- mean((pred4_8 - test8$propsnag)^2))
pred4_13 <- predict(tree4, newdata = test13, type = "response")
(MSE1 <- mean((pred4_13 - test13$propsnag)^2))

# Make a regression tree for pool 13
tree13 <- randomForest(propsnag ~., 
                       data = train13, 
                       ntree = 10000, 
                       importance = T)
plotimportance(tree13, color = "darkblue")

# predict on the other pools
pred13_4 <- predict(tree13, newdata = test4, type = "response")
(MSE1 <- mean((pred13_4 - test4$propsnag)^2))
pred13_8 <- predict(tree13, newdata = test8, type = "response")
(MSE1 <- mean((pred13_8 - test8$propsnag)^2))