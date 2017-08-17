

# Model -------------------------------------------------------------------
library(lightgbm)

params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.03,
               num_leaves = 256,
               max_depth = -12,
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
                      nrounds = 5000, early_stopping_rounds=30)

tree_imp <- lgb.importance(model1_1, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 91, measure = "Frequency")

rm(X1, evalX1)
gc()




#model2
set.seed(71)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model2_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)

rm(X1, evalX1)
gc()


#model3
set.seed(70)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model3_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)
rm(X1, evalX1)
gc()


params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.03,
               max_depth = -12,
               num_leaves = 512,
               tree_learner = "serial",
               feature_fraction = 0.6,
               bagging_freq = 3,
               bagging_fraction = 0.9,
               min_sum_hessian_in_leaf = 20,
               verbose = 1)



#model4
set.seed(42)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model4_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)
rm(X1, evalX1)
gc()


#model5
set.seed(37)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model5_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)
rm(X1, evalX1)
gc()


#model6
set.seed(96)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model6_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)
rm(X1, evalX1)
gc()


params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.01,
               max_depth = -12,
               num_leaves = 128,
               tree_learner = "serial",
               feature_fraction = 0.9,
               bagging_freq = 1,
               bagging_fraction = 0.7,
               min_data_in_leaf = 50,
               min_sum_hessian_in_leaf = 5.0)



#model7
set.seed(123)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model7_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)
rm(X1, evalX1)
gc()


#model8
set.seed(114514)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model8_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)

rm(X1, evalX1)
gc()

#model9
set.seed(1145141919)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model9_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)

rm(X1, evalX1)
gc()

# Apply model -------------------------------------------------------------
set.seed(71)
out1_1 <- predict(model1_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out2_1 <- predict(model2_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out3_1 <- predict(model3_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out4_1 <- predict(model4_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out5_1 <- predict(model5_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out6_1 <- predict(model6_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out7_1 <- predict(model6_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out8_1 <- predict(model6_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))
out9_1 <- predict(model6_1, data = as.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))

test$reordered <- (out1_1 + out2_1 + out3_1 + out4_1 + out5_1 + out6_1 + out7_1 + out8_1 + out9_1)/9

