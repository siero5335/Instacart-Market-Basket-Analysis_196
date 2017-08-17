library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(makedummies)

# Load Data ---------------------------------------------------------------


aisles <- fread("aisles.csv")
departments <- fread("departments.csv")
orderp <- fread("order_products__prior.csv")
ordert <- fread("order_products__train.csv")
orders <- fread("orders.csv")
products <- fread("products.csv")



# Reshape data ------------------------------------------------------------


aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))




products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) 

temp <- makedummies(data.frame(products$organic), numeric = "Result")

products$organic <- as.numeric(temp$res)

rm(aisles, departments, temp)

product_data <- products[, c(1, 3:5)]

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")


rm(orderp, products)
gc()


# Products ----------------------------------------------------------------
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_cart = sum(add_to_cart_order),
    prod_reorders = sum(reordered),
    prod_order_num = mean(order_number),
    prod_order = max(order_number),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2),
    prod_multi_orders = sum(product_time > 2)
  )


prd <- prd %>%
  inner_join(product_data, by = "product_id")


prd$prod_first_probability <- prd$prod_first_orders / prd$prod_orders
prd$prod_second_probability <- prd$prod_second_orders / prd$prod_orders
prd$prod_multiorder_probability <- prd$prod_multi_orders/ prd$prod_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders
prd$range_second_order <- prd$prod_first_orders - prd$prod_second_orders
prd$range_multi_order <- prd$prod_first_orders - prd$prod_multi_orders
prd$cart_reorder_ratio <-  prd$prod_reorders / prd$prod_cart

prd <- prd %>% select(-prod_reorders, -prod_first_orders, 
                      -prod_second_orders, -prod_multi_orders)

gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_mean_orders = mean(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_max_dow = max(order_dow),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )



us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id),
    user_mean_cart = mean(add_to_cart_order),
    user_mean_hour = mean(order_hour_of_day),
    user_weekend = sum(order_dow == c(0, 1)),
    user_week = sum(order_dow == c(2:6))
  )



users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders


us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

rm(us)
gc()





# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_mean_order = mean(order_number),
    up_sd_order = sd(order_number))

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")



data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
  
  

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

data$up_sd_order[is.na(data$up_sd_order)] <- 0


data[, c(3:35, 38:41)] <- scale(data[, c(3:35, 38:41)])

rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL


rm(data)
gc()



# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.60,
  "subsample"           = 0.75,
  "colsample_bytree"    = 0.9,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

#feature mix
set.seed(71)
subtrain1 <- train %>% sample_frac(0.1)

subtrain_order_feature <- subtrain1[, c(1:6, 30)]
subtrain_multiorde_feature <- subtrain1[, c(7:9, 30)]
subtrain_reorder_feature <- subtrain1[, c(10:14, 30)]
subtrain_userorder_feature <- subtrain1[, c(15:17,  19:21, 25 ,30)]
subtrain_usertime_feature <- subtrain1[, c(18, 22:24, 30)]
subtrain_uporder_feature <- subtrain1[, c(26:29, 30)]


X1 <- xgb.DMatrix(as.matrix(subtrain_order_feature %>% select(-reordered)), label = subtrain_order_feature$reordered)
X2 <- xgb.DMatrix(as.matrix(subtrain_multiorde_feature %>% select(-reordered)), label = subtrain_multiorde_feature$reordered)
X3 <- xgb.DMatrix(as.matrix(subtrain_reorder_feature %>% select(-reordered)), label = subtrain_reorder_feature$reordered)
X4 <- xgb.DMatrix(as.matrix(subtrain_userorder_feature %>% select(-reordered)), label = subtrain_userorder_feature$reordered)
X5 <- xgb.DMatrix(as.matrix(subtrain_usertime_feature %>% select(-reordered)), label = subtrain_usertime_feature$reordered)
X6 <- xgb.DMatrix(as.matrix(subtrain_uporder_feature %>% select(-reordered)), label = subtrain_uporder_feature$reordered)

###########################################################train_feature

#### X1
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model1_1 <- xgboost(data = X1, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(42)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(42)
model1_2 <- xgboost(data = X1, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(57)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(57)
model1_3 <- xgboost(data = X1, params = params, nrounds = nround)


cv.nround <- 1000 #search
set.seed(123)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(123)
model1_4 <- xgboost(data = X1, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(24)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(24)
model1_5 <- xgboost(data = X1, params = params, nrounds = nround)


rm(X1)
gc()

#### X2
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X2, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model2_1 <- xgboost(data = X2, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(42)
bst.cv <- xgb.cv(params = params, data = X2, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(42)
model2_2 <- xgboost(data = X2, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(57)
bst.cv <- xgb.cv(params = params, data = X2, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(57)
model2_3 <- xgboost(data = X2, params = params, nrounds = nround)


cv.nround <- 1000 #search
set.seed(123)
bst.cv <- xgb.cv(params = params, data = X2, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(123)
model2_4 <- xgboost(data = X2, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(24)
bst.cv <- xgb.cv(params = params, data = X2, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(24)
model2_5 <- xgboost(data = X2, params = params, nrounds = nround)


rm(X2)
gc()

#### X3
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model3_1 <- xgboost(data = X3, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(42)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(42)
model3_2 <- xgboost(data = X3, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(57)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(57)
model3_3 <- xgboost(data = X3, params = params, nrounds = nround)


cv.nround <- 1000 #search
set.seed(123)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(123)
model3_4 <- xgboost(data = X3, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(24)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(24)
model3_5 <- xgboost(data = X3, params = params, nrounds = nround)


rm(X3)
gc()

#### X4
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X4, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model4_1 <- xgboost(data = X4, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(42)
bst.cv <- xgb.cv(params = params, data = X4, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(42)
model4_2 <- xgboost(data = X4, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(57)
bst.cv <- xgb.cv(params = params, data = X4, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(57)
model4_3 <- xgboost(data = X4, params = params, nrounds = nround)


cv.nround <- 1000 #search
set.seed(123)
bst.cv <- xgb.cv(params = params, data = X4, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(123)
model4_4 <- xgboost(data = X4, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(24)
bst.cv <- xgb.cv(params = params, data = X4, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(24)
model4_5 <- xgboost(data = X4, params = params, nrounds = nround)


rm(X4)
gc()

#### X5
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X5, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model5_1 <- xgboost(data = X5, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(42)
bst.cv <- xgb.cv(params = params, data = X5, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(42)
model5_2 <- xgboost(data = X5, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(57)
bst.cv <- xgb.cv(params = params, data = X5, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(57)
model5_3 <- xgboost(data = X5, params = params, nrounds = nround)


cv.nround <- 1000 #search
set.seed(123)
bst.cv <- xgb.cv(params = params, data = X5, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(123)
model5_4 <- xgboost(data = X5, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(24)
bst.cv <- xgb.cv(params = params, data = X5, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(24)
model5_5 <- xgboost(data = X5, params = params, nrounds = nround)


rm(X5)
gc()

#### X6
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X6, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model6_1 <- xgboost(data = X6, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(42)
bst.cv <- xgb.cv(params = params, data = X6, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(42)
model6_2 <- xgboost(data = X6, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(57)
bst.cv <- xgb.cv(params = params, data = X6, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(57)
model6_3 <- xgboost(data = X6, params = params, nrounds = nround)


cv.nround <- 1000 #search
set.seed(123)
bst.cv <- xgb.cv(params = params, data = X6, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(123)
model6_4 <- xgboost(data = X6, params = params, nrounds = nround)

cv.nround <- 1000 #search
set.seed(24)
bst.cv <- xgb.cv(params = params, data = X6, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(24)
model6_5 <- xgboost(data = X6, params = params, nrounds = nround)


rm(X6)
gc()



X <- xgb.DMatrix(as.matrix(train_features_2 %>% select(-subtrain1.reordered)), label = train_features_2$subtrain1.reordered)

###########################################################train_feature

#### X1
cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model <- xgboost(data = X, params = params, nrounds = nround)



gc()
######################################

test_order_feature <- test[, c(1:7,27)]
test_feature <- test[, c(1, 8:10, 27)]
test_reorder_feature <- test[, c(1, 11:15, 27)]
test_userorder_feature <- test[, c(1, 1:18,  20:22 ,27)]
test_usertime_feature <- test[, c(1, 19, 23:25, 27)]
test_uporder_feature <- test[, c(1, 27:31)]

X1 <- xgb.DMatrix(as.matrix(test_order_feature %>% select(-order_id, -product_id)))
X2 <- xgb.DMatrix(as.matrix(test_feature %>% select(-order_id, -product_id)))
X3 <- xgb.DMatrix(as.matrix(test_reorder_feature %>% select(-order_id, -product_id)))
X4 <- xgb.DMatrix(as.matrix(test_userorder_feature %>% select(-order_id, -product_id)))
X5 <- xgb.DMatrix(as.matrix(test_usertime_feature %>% select(-order_id, -product_id)))
X6 <- xgb.DMatrix(as.matrix(test_uporder_feature %>% select(-order_id, -product_id)))


out1_1 <- predict(model1_1, X1)
out1_2 <- predict(model1_2, X1)
out1_3 <- predict(model1_3, X1)
out1_4 <- predict(model1_4, X1)
out1_5 <- predict(model1_5, X1)
out1_6 <- (out1_1 + out1_2 + out1_3 + out1_4 + out1_5)/5


out2_1 <- predict(model2_1, X1)
out2_2 <- predict(model2_2, X1)
out2_3 <- predict(model2_3, X1)
out2_4 <- predict(model2_4, X1)
out2_5 <- predict(model2_5, X1)
out2_6 <- (out2_1 + out2_2 + out2_3 + out2_4 + out2_5)/5

out3_1 <- predict(model3_1, X1)
out3_2 <- predict(model3_2, X1)
out3_3 <- predict(model3_3, X1)
out3_4 <- predict(model3_4, X1)
out3_5 <- predict(model3_5, X1)
out3_6 <- (out3_1 + out3_2 + out3_3 + out3_4 + out3_5)/5

out4_1 <- predict(model4_1, X1)
out4_2 <- predict(model4_2, X1)
out4_3 <- predict(model4_3, X1)
out4_4 <- predict(model4_4, X1)
out4_5 <- predict(model4_5, X1)
out4_6 <- (out4_1 + out4_2 + out4_3 + out4_4 + out4_5)/5

out5_1 <- predict(model5_1, X1)
out5_2 <- predict(model5_2, X1)
out5_3 <- predict(model5_3, X1)
out5_4 <- predict(model5_4, X1)
out5_5 <- predict(model5_5, X1)
out5_6 <- (out5_1 + out5_2 + out5_3 + out5_4 + out5_5)/5

out6_1 <- predict(model6_1, X1)
out6_2 <- predict(model6_2, X1)
out6_3 <- predict(model6_3, X1)
out6_4 <- predict(model6_4, X1)
out6_5 <- predict(model6_5, X1)
out6_6 <- (out6_1 + out6_2 + out6_3 + out6_4 + out6_5)/5

test$reordered <- (out1_6*10 + out2_6 + out3_6 + out4_6 + out5_6 + out6_6*3)/17
test$reordered <- (test$reordered > 0.21) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "kernel_feature9_mod_xgboost_0.21.csv", row.names = F)


