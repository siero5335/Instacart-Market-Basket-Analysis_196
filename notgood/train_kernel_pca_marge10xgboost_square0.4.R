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
products$organic <- as.factor(products$organic)


departments$reo_prob_dep <- c(1,0,1,1,1,0,1,1,1,1,0,1,0,1,1, 1, 0, 1,1,1,0) 


products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)

products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), 
         organic= as.factor(organic))

temp <- makedummies(data.frame(products$organic), numeric = "Result")

products$organic <- as.numeric(temp$res)


products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) 

rm(aisles, departments, temp)


ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")


rm(orderp)
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
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2),
    prod_multi_orders = sum(product_time > 2),
    prod_days_since_prior_order = sum(days_since_prior_order == 30, na.rm = T)
  )

prd <- prd %>%
  inner_join(products[, c(1, 3, 6, 7)], by = "product_id")

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders
prd$prod_multiorder_probability <- prd$prod_multi_orders/ prd$prod_orders
prd$range_multi_order <- prd$prod_first_orders - prd$prod_multi_orders
prd$cart_reorder_ratio <-  prd$prod_reorders / prd$prod_cart
  


prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders,
                      -prod_multi_orders)


gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_mean_orders = mean(order_number, na.rm = T),
    user_sd_orders = sd(order_number, na.rm = T),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
    user_sd_days_since_prior = sd(days_since_prior_order, na.rm = T)
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_mean_cart = mean(add_to_cart_order),
    user_sd_cart = sd(add_to_cart_order),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id),
    user_mean_hour = mean(order_hour_of_day),
    user_sd_hour = sd(order_hour_of_day),
    user_weekend = sum(order_dow == c(0, 1),
    user_weekhead = sum(order_dow == 2)
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
    up_average_cart_position = mean(add_to_cart_order))

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
data$up_order_rate2 <- (data$up_order_rate)^2
data$up_orders_since_last_order2 <- (data$up_orders_since_last_order)^2
data$up_order_rate_since_first_order2 <- (data$up_order_rate_since_first_order)^2
data$orderfirstlast <- scale(data$up_order_rate_since_first_order * data$up_orders_since_last_order)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))


data$departents <- NULL
data[, -c(1, 2, 32, 33 ,42)] <- scale(data[, -c(1, 2, 32, 33 ,42)])

pca_res <- prcomp(data[, -c(1, 2, 32, 33 ,42)])
data[, 43:45] <- pca_res$x[,1:3]


library(Rtsne)
set.seed(71)
tsne_res <- Rtsne(as.matrix(data[, -c(1, 2, 32, 33 ,42)]), 
                  verbose=TRUE, dim = 3, initial_dims = 30, theta = 1,
                  pca = TRUE, max_iter = 30)


rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$user_distinct_products <- NULL
train$reordered[is.na(train$reordered)] <- 0


test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_distinct_products <- NULL
rm(data)
gc()

write.csv(train, "train.csv")

# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

#model1
set.seed(71)
subtrain1 <- train %>% sample_frac(0.3)
X1 <- xgb.DMatrix(as.matrix(subtrain1 %>% select(-reordered)), label = subtrain1$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model1 <- xgboost(data = X1, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X1), model = model1)
xgb.ggplot.importance(importance)

rm(X1, importance)
gc()

#model2
set.seed(57)
subtrain2 <- train %>% sample_frac(0.3)
X2 <- xgb.DMatrix(as.matrix(subtrain2 %>% select(-reordered)), label = subtrain2$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X2, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model2 <- xgboost(data = X2, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X2), model = model2)
xgb.ggplot.importance(importance)

rm(X2, importance)
gc()


#model3
set.seed(42)
subtrain3 <- train %>% sample_frac(0.3)
X3 <- xgb.DMatrix(as.matrix(subtrain3 %>% select(-reordered)), label = subtrain3$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model3 <- xgboost(data = X3, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X3), model = model3)
xgb.ggplot.importance(importance)

rm(X3, importance)
gc()

#model4
set.seed(123)
subtrain4 <- train %>% sample_frac(0.3)
X4 <- xgb.DMatrix(as.matrix(subtrain4 %>% select(-reordered)), label = subtrain4$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X4, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model4 <- xgboost(data = X4, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X4), model = model4)
xgb.ggplot.importance(importance)

rm(X4, importance)
gc()

#model5
set.seed(24)
subtrain5 <- train %>% sample_frac(0.3)
X5 <- xgb.DMatrix(as.matrix(subtrain5 %>% select(-reordered)), label = subtrain5$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X5, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model5 <- xgboost(data = X5, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X5), model = model5)
xgb.ggplot.importance(importance)

rm(X5, importance)
gc()

#model6
set.seed(35)
subtrain6 <- train %>% sample_frac(0.3)
X6 <- xgb.DMatrix(as.matrix(subtrain3 %>% select(-reordered)), label = subtrain3$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X6, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model6 <- xgboost(data = X6, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X6), model = model6)
xgb.ggplot.importance(importance)

rm(X6, importance)
gc()

#model7
set.seed(57)
subtrain7 <- train %>% sample_frac(0.3)
X7 <- xgb.DMatrix(as.matrix(subtrain7 %>% select(-reordered)), label = subtrain7$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X7, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model7 <- xgboost(data = X7, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X7), model = model7)
xgb.ggplot.importance(importance)

rm(X7, importance)
gc()

#model8
set.seed(65)
subtrain8 <- train %>% sample_frac(0.3)
X8 <- xgb.DMatrix(as.matrix(subtrain8 %>% select(-reordered)), label = subtrain8$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X8, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model8 <- xgboost(data = X8, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X8), model = model8)
xgb.ggplot.importance(importance)

rm(X3, importance)
gc()


#model9
set.seed(92)
subtrain9 <- train %>% sample_frac(0.3)
X9 <- xgb.DMatrix(as.matrix(subtrain9 %>% select(-reordered)), label = subtrain9$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X9, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model9 <- xgboost(data = X9, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X9), model = model9)
xgb.ggplot.importance(importance)

rm(X9, importance)
gc()



# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
out1 <- predict(model1, X)
out2 <- predict(model2, X)
out3 <- predict(model3, X)
out4 <- predict(model4, X)
out5 <- predict(model5, X)
out6 <- predict(model6, X)
out7 <- predict(model7, X)
out8 <- predict(model8, X)
out9 <- predict(model9, X)

test$reordered <- ((out1 + out2 + out3+ out4 + out5 + out6 + out7 + out8 + out9)/9)

test$reordered <- (test$reordered > 0.210) * 1

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
write.csv(submission, file = "submit_kernel_PCAxgboost_0.21_sq0.3.csv", row.names = F)