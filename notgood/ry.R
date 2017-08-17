library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(makedummies)
library(text2vec)
library(magrittr)
library(textmineR)
library(Rtsne)
library(readr)

train <- fread("train.csv")
test <- fread("test.csv")

train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$user_id <- NULL
train$aisle <- NULL
train$organic <- NULL
train$periods <- NULL
train$aisle_id <- NULL
train$department_id <- NULL

train$reordered[is.na(train$reordered)] <- 0


test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$aisle <- NULL
test$organic <- NULL
test$periods <- NULL
test$aisle_id <- NULL
test$department_id <- NULL


rm(data)
gc()


#model1
set.seed(72)
train_part <- createDataPartition(train$reordered, p=0.5, list=FALSE)

X1 <- lgb.Dataset(data.matrix(train[train_part,] %>% select(-reordered)), 
                  label = as.numeric(train[train_part,]$reordered))
gc()
evalX1 <-  lgb.Dataset(data.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = as.numeric(train[-train_part,]$reordered))

set.seed(71)
model1_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=30)

tree_imp <- lgb.importance(model1_1, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 91, measure = "Frequency")

rm(X1, evalX1)
gc()

# Apply model -------------------------------------------------------------
set.seed(71)
out1_1 <- predict(model1_1, data = data.matrix(test %>% select(-order_id, -product_id), n = bst.cv$best_iter))

test$reordered <- (out1_1)/9

