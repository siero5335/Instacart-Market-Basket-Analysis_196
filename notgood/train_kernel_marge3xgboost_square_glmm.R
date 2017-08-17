library(data.table)
library(dplyr)
library(tidyr)
library(stringr)



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


products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")
orders_products <- mutate(orders_products, organic = )


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
    prod_second_orders = sum(product_time == 2)
  )



prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)


gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
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




data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

library(arm)
stan_res <- bayesglm(up_order_rate ~ (1|user_id), data = data)
data$glmmm <- predict(stan_res, data)


data[, c(3:17, 20:23, 25:28)] <- scale(data[, c(3:17, 20:23, 25:28)])

rm(ordert, prd, users)
gc()

# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0
val_users <- sample(unique(train$user_id), size = 10000, replace = FALSE)


test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$reordered <- NULL
val_users_test <- sample(unique(test$user_id), size = 10000, replace = FALSE)


rm(data)
gc()



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
subtrain1 <- train %>% sample_frac(0.1)
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
subtrain2 <- train %>% sample_frac(0.1)
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
subtrain3 <- train %>% sample_frac(0.1)
X3 <- xgb.DMatrix(as.matrix(subtrain3 %>% select(-reordered)), label = subtrain3$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X3, nfold = 5, nrounds=cv.nround, early_stopping_rounds=5)
nround <- bst.cv$best_iteration

set.seed(71)
model3 <- xgboost(data = X3, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X3), model = model)
xgb.ggplot.importance(importance)

rm(X3, importance)
gc()





# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
out1 <- predict(model1, X)
out2 <- predict(model2, X)
out3 <- predict(model3, X)

test$reordered <- ((out1 + out2 + out3)/3)

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
write.csv(submission, file = "submit_kernel_marge3xgboost_0.21_sq.csv", row.names = F)