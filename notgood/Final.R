library(data.table)
library(dplyr)
library(tidyr)
library(caret)


data <- fread("data2.csv")

ordert <- fread("order_products__train.csv")
orders <- fread("orders.csv")
ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(ordert, orders)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$user_id <- NULL
train$aisle <- NULL
train$organic <- NULL


train$user_median_orders <- NULL
train$user_sd_orders <- NULL
train$reordered_median_dow_ratio <- NULL
train$prod_median_orders <- NULL
train$user_days_since_prior_order14 <- NULL
train$user_days_since_prior_order30 <- NULL
train$n <- NULL
train$prod_sd_days_since_prior_order30 <- NULL
train$user_mean_add_to_cart_relative <- NULL
train$prod_mean_add_to_cart_inverted <- NULL

train$reordered[is.na(train$reordered)] <- 0


test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$aisle <- NULL
test$organic <- NULL


test$user_median_orders <- NULL
test$user_sd_orders <- NULL
test$reordered_median_dow_ratio <- NULL
test$prod_median_orders <- NULL
test$user_days_since_prior_order14 <- NULL
test$user_days_since_prior_order30 <- NULL
test$n <- NULL
test$prod_sd_days_since_prior_order30 <- NULL
test$user_mean_add_to_cart_relative <- NULL
test$prod_mean_add_to_cart_inverted <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(lightgbm)

params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.03,
               num_leaves = 256,
               max_depth = 12,
               tree_learner = "serial",
               feature_fraction = 0.6,
               bagging_freq = 3,
               bagging_fraction = 0.9,
               min_sum_hessian_in_leaf = 20,
               verbose = 1)

#model1
set.seed(114514)
train_part <- createDataPartition(train$reordered, p=0.7, list=FALSE)

X1 <- lgb.Dataset(data.matrix(train[train_part,] %>% select(-reordered)), 
                  label = as.numeric(train[train_part,]$reordered))
gc()
evalX1 <-  lgb.Dataset(data.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = as.numeric(train[-train_part,]$reordered))

set.seed(71)
model1_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)

rm(X1, evalX1)
gc()

params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.03,
               num_leaves = 512,
               max_depth = -12,
               tree_learner = "serial",
               feature_fraction = 0.6,
               bagging_freq = 3,
               bagging_fraction = 0.9,
               min_sum_hessian_in_leaf = 20,
               verbose = 1,
               lambda_l2 = 0.001)


#model2
set.seed(71)
train_part <- createDataPartition(train$reordered, p=0.7, list=FALSE)

X1 <- lgb.Dataset(data.matrix(train[train_part,] %>% select(-reordered)), 
                  label = as.numeric(train[train_part,]$reordered))
gc()
evalX1 <-  lgb.Dataset(data.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = as.numeric(train[-train_part,]$reordered))

set.seed(71)
model1_2 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 8000, early_stopping_rounds=30)



rm(X1, evalX1)
gc()

params <- list(objective = "binary", 
               boosting_type = "gbdt",
               metric="binary_logloss,auc",
               learning_rate = 0.03,
               num_leaves = 1024,
               max_depth = -12,
               tree_learner = "serial",
               feature_fraction = 0.6,
               bagging_freq = 3,
               bagging_fraction = 0.9,
               min_sum_hessian_in_leaf = 20,
               verbose = 1,
               lambda_l1 = 0.001,
               lambda_l2 = 0.001)

#model3
set.seed(42)
train_part <- createDataPartition(train$reordered, p=0.7, list=FALSE)

X1 <- lgb.Dataset(data.matrix(train[train_part,] %>% select(-reordered)), 
                  label = as.numeric(train[train_part,]$reordered))
gc()
evalX1 <-  lgb.Dataset(data.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = as.numeric(train[-train_part,]$reordered))

set.seed(71)
model1_3 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 8000, early_stopping_rounds=30)



rm(X1, evalX1, train)
gc()

# Apply model -------------------------------------------------------------
set.seed(71)
out1_1 <- predict(model1_1, 
                  data = data.matrix(test %>% select(-order_id, -product_id)), 
                  n = model1_1$best_iter)
out1_2 <- predict(model1_2, 
                  data = data.matrix(test %>% select(-order_id, -product_id)), 
                  n = model1_2$best_iter)
out1_3 <- predict(model1_3, 
                  data = data.matrix(test %>% select(-order_id, -product_id)), 
                  n = model1_3$best_iter)


test$reordered <- (out1_1 + out1_2 + out1_3)/3