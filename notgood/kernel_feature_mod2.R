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

rm(aisles, departments)

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
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_dow = mean(order_dow),
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


data$organic  <- NULL
data$department <- NULL
data$org_ave_reorder_probability <- NULL
data$department_id <- NULL
data$aisle_id <- NULL
data$dep_ave_reorder_probability <- NULL
data$user_total_products <- NULL
data$prod_cart <- NULL

data$up_sd_order[is.na(data$up_sd_order)] <- 0


data[, c(3:27, 30:33)] <- scale(data[, c(3:27, 30:33)])

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



# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))

test$reordered1 <- predict(model1, X)
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
write.csv(submission, file = "kernel_feature5_mod_xgboost_0.21.csv", row.names = F)


