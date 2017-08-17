

# Model -------------------------------------------------------------------
library(lightgbm)
library(caret)

params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.05,
               num_leaves = 256,
               max_depth = 12,
               tree_learner = "serial",
               feature_fraction = 0.6,
               bagging_freq = 3,
               bagging_fraction = 0.9,
               min_sum_hessian_in_leaf = 20,
               verbose = 1)


#model1
set.seed(72)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)
  
set.seed(71)
model1_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 500, early_stopping_rounds=30)

#tree_imp <- lgb.importance(model1_1, percentage = TRUE)
#lgb.plot.importance(tree_imp, top_n = 91, measure = "Frequency")

rm(X1, evalX1)
gc()



# Apply model -------------------------------------------------------------
set.seed(71)
out1_1 <- predict(model1_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
test$reordered <- out1_1 

