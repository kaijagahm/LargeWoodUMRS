# Useful functions

#First Element: takes the first element if there's only one unique value in the column, returns NA otherwise. 
firstel <- function(x){
  if(length(unique(x)) > 1){
    return(NA)
  }
  else{
    return(x[1])
  }
}


#Count NA's: produces a data frame with column names and number of na's
locate.nas <- function(df){
  na.df <- data.frame(
    colname = names(df),
    nas = NA
  )
  
  for(i in 1:ncol(df)){
    na.df$nas[i] <- sum(is.na(df[,i]))
  }
  return(na.df)
}


#Notin: define %notin% operator
`%notin%` <- Negate(`%in%`) 

# splitup
# initialize function for splitting data into cross-validation sets
splitup <- function(df, k){
  inds <- sample(rep(1:k, length = nrow(df)))
  df$grp <- inds
  return(df)
}

# GLMER error rates
# glmerAUCtest
library(pROC)
glmerAUCtest <- function(model, dataframe, k){
  allsplit <- splitup(allscaled, k)
  
  rocs_test <- vector(mode = "list", length = k)
  rocs_train <- vector(mode = "list", length = k)
  aucs_test <- rep(NA, k)
  aucs_train <- rep(NA, k)
  
  for(i in 1:k){
    print(paste("beginning round", i))
    print("splitting data")
    train <- allsplit[allsplit$grp != i, -17]
    test <- allsplit[allsplit$grp == i, -17]
    print("updating model")
    mod <- update(mfinal_all_nostratum, data = train)
    print("predicting")
    test$pred <- predict(mod, newdata = test, allow.new.levels = T, type = "response")
    train$pred <- predict(mod, newdata = train, allow.new.levels = T, type = "response")
    
    print("calculating roc/auc for training data")
    roc_train <- roc(response = train$snag, predictor = train$pred, auc = T)
    rocs_train[[i]] <- roc_train
    aucs_train[i] <- roc_train$auc 
    
    print("calculating roc/auc for test data")
    roc_test <- roc(response = test$snag, predictor = test$pred, auc = T)
    rocs_test[[i]] <- roc_test
    aucs_test[i] <- roc_test$auc 
    print(paste("completed round", i))
  }
  
  #calculate mean auc for training and test data
  mean_auc_test <- mean(aucs_test)
  mean_auc_train <- mean(aucs_train)
  
  #list the results together
  l <- list("rocs_test" = rocs_test, "rocs_train" = rocs_train, "aucs_test" = aucs_test, "aucs_train" = aucs_train,"mean_auc_test" = mean_auc_test, "mean_auc_train" = mean_auc_train)
  
  return(l)
}

# Plotnice
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
