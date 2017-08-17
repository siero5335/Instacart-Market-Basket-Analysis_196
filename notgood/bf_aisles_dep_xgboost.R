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


# Load Data ---------------------------------------------------------------


aisles <- fread("aisles.csv")
departments <- fread("departments.csv")
orderp <- fread("order_products__prior.csv")
ordert <- fread("order_products__train.csv")
orders <- fread("orders.csv")
products <- fread("products.csv")
bf <- fread("bf.csv")
order_streaks <- fread("order_streaks.csv")

bf <- bf[, 2:7]

prep_fun = tolower
tok_fun = word_tokenizer

##aisles
set.seed(71)
it_train_aisle = itoken(aisles$aisle, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  progressbar = FALSE)
set.seed(71)
vocab = create_vocabulary(it_train_aisle, ngram = c(1L, 3L))

vectorizer = vocab_vectorizer(vocab)

set.seed(71)
dtm = create_dtm(it_train_aisle, vectorizer)
test <- as.matrix(dtm)
test <- as.data.frame(test)

set.seed(71)
test1 <- Rtsne(test, dims = 3, initial_dims = 50, perplexity = 30,
               theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
               verbose = TRUE)

aisles <- cbind(aisles, test1$Y)
colnames(aisles) <- c("aisle_id", "aisle", "ais_tsne1", "ais_tsne2", "ais_tsne3") 
rm(test, test1, dtm, vectorizer, it_train_aisle, vocab)

##dep
set.seed(71)
it_train_dep = itoken(departments$department,
                        preprocessor = prep_fun, 
                        tokenizer = tok_fun, 
                        progressbar = FALSE)
set.seed(71)
vocab = create_vocabulary(it_train_dep, ngram = c(1L, 3L))

vectorizer = vocab_vectorizer(vocab)

set.seed(71)
dtm = create_dtm(it_train_dep, vectorizer)
test <- as.matrix(dtm)
test <- as.data.frame(test)

set.seed(71)
test1 <- Rtsne(test, dims = 3, initial_dims = 50, perplexity = 5,
               theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
               verbose = TRUE)

departments <- cbind(departments, test1$Y)
colnames(departments) <- c("department_id", "departments", "departments_tsne1", "departments_tsne2", "departments_tsne3") 
rm(test, test1, dtm, vectorizer, it_train_dep, vocab)



# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)
products$organic <- as.factor(products$organic)



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


ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")


# aisle
tmp <- orders_products %>%
  left_join(products,by="product_id") %>%
  left_join(aisles,by="aisle_id") %>%
  left_join(departments,by="department_id") 

tmp <- tmp %>%
  group_by(aisle.x, department.x) %>%
  tally(sort=TRUE) %>%
  mutate(perc = round(100*n/nrow(orders_products),2)) %>%
  ungroup()

colnames(tmp) <- c("aisle","department","n","pern")

c("aisle_id", "aisle", "ais_tsne1", "ais_tsne2", "ais_tsne3") 

products <- products %>% 
  inner_join(tmp) %>% 
  select(-aisle, -department)


rm(aisles, departments, temp, tmp)
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
    prod_cart = sum(add_to_cart_order),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2),
    prod_multi_orders = sum(product_time > 2),
    prod_mean_orders = mean(product_time, na.rm = T),
    prod_sd_orders = sd(product_time, na.rm = T),
    prod_mean_days_since_prior_order = mean(days_since_prior_order, na.rm = T),
    prod_sd_days_since_prior_order = sd(days_since_prior_order, na.rm = T),
    prod_mean_days_since_prior_order7 = mean(days_since_prior_order == 7, na.rm = T),
    prod_sd_days_since_prior_order7 = sd(days_since_prior_order == 7, na.rm = T),
    prod_mean_days_since_prior_order14 = mean(days_since_prior_order == 14, na.rm = T),
    prod_sd_days_since_prior_order14 = sd(days_since_prior_order == 14, na.rm = T),
    prod_mean_days_since_prior_order30 = mean(days_since_prior_order == 30, na.rm = T),
    prod_sd_days_since_prior_order30 = sd(days_since_prior_order == 30, na.rm = T)
  )



ord2 <- orders_products %>%
  group_by(product_id, user_id) %>%
  summarise(
    prod_mean_order_dow = mean(order_dow),
    prod_sd_order_dow = sd(order_dow),
    prod_mean_order_hour = mean(order_hour_of_day),
    prod_sd_order_hour = sd(order_hour_of_day)
  )



prd <- prd %>%
  inner_join(products[, c(1, 3:6, 8:14)], by = "product_id")


prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders
prd$prod_multiorder_probability <- prd$prod_multi_orders/ prd$prod_orders
prd$range_multi_order <- prd$prod_first_orders - prd$prod_multi_orders
prd$cart_reorder_ratio <-  prd$prod_reorders / prd$prod_cart
prd$cart_meanorder_ratio <-  prd$prod_mean_orders / prd$prod_cart  
prd$cart_sdorder_ratio <-  prd$prod_sd_orders / prd$prod_cart 

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders,
                      -prod_multi_orders)

rm(products)



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
    user_sd_days_since_prior = sd(days_since_prior_order, na.rm = T),
    user_days_since_prior_order7 = sum(days_since_prior_order == 7, na.rm = T),
    user_days_since_prior_order14 = sum(days_since_prior_order == 14, na.rm = T),
    user_days_since_prior_order30 = sum(days_since_prior_order == 30, na.rm = T)
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id),
    user_mean_hour = mean(order_hour_of_day),
    user_sd_hour = sd(order_hour_of_day),
    user_morning_hour = sum(order_hour_of_day >=5 & order_hour_of_day <=8)
  )

users <- users %>% inner_join(us)

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

ord <- orders_products %>%
  group_by(order_id, user_id) %>%
  summarise(
    user_cart = max(add_to_cart_order)
  )


temp <- ord %>%
  group_by(user_id) %>%
  summarise(
    user_mean_cart = mean(user_cart),
    user_sd_cart = sd(user_cart)
  )

users <- users %>% inner_join(temp)


users$user_mean_cart_order <- users$user_mean_orders / users$user_mean_cart
users$user_sd_cart_order <- users$user_sd_orders / users$user_sd_cart
users$user_diff_ordermean <- users$user_mean_days_since_prior - users$time_since_last_order
users$user_diff_ordersd <- users$user_diff_ordermean / users$user_sd_days_since_prior

rm(us, ord, temp)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_mean_order = mean(order_number, na.rm = T),
    up_sd_order = sd(order_number, na.rm = T),
    up_average_cart_position = mean(add_to_cart_order),
    up_sd_cart_position = sd(add_to_cart_order))

rm(orders_products)
gc()

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_order_sd_rate <- data$up_orders / data$user_sd_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
data$orderfirstlast <- scale(data$up_order_rate_since_first_order * data$up_orders_since_last_order)
data$order_cart_ratio <-  data$prod_orders / data$prod_cart
data$cartpos_order_rate <- data$up_average_cart_position / data$user_orders

gc()


data <- data %>%
  left_join(ord2,  by = c("user_id", "product_id"))

data$orderfirstlast <- data$orderfirstlast[,1]
data$mean_prodorder_userordererate <- data$prod_mean_orders / data$user_mean_orders
data$sd_prodorder_userordererate <- data$prod_sd_orders / data$user_sd_orders

data <- data %>% 
  left_join(bf)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))



rm(ordert, prd, users, ord2, products, bf, orders)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$user_id <- NULL
train$aisle <- NULL
train$organic <- NULL

train$reordered[is.na(train$reordered)] <- 0


test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$aisle <- NULL
test$organic <- NULL
rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "eta"                 = 0.1,
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.75,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

#model1
set.seed(72)
subtrain1 <- train %>% sample_frac(0.3)
X1 <- xgb.DMatrix(as.matrix(subtrain1 %>% select(-reordered)), label = subtrain1$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 4, nrounds=cv.nround, early_stopping_rounds= 3)
nround <- bst.cv$best_iteration

set.seed(71)
model1 <- xgboost(data = X1, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X1), model = model1)
xgb.ggplot.importance(importance)

rm(X1, importance)
gc()

#model2
set.seed(71)
subtrain1 <- train %>% sample_frac(0.3)
X1 <- xgb.DMatrix(as.matrix(subtrain1 %>% select(-reordered)), label = subtrain1$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 4, nrounds=cv.nround, early_stopping_rounds= 3)
nround <- bst.cv$best_iteration

set.seed(71)
model2 <- xgboost(data = X1, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X1), model = model1)
xgb.ggplot.importance(importance)

rm(X1, importance)
gc()

#model3
set.seed(70)
subtrain1 <- train %>% sample_frac(0.3)
X1 <- xgb.DMatrix(as.matrix(subtrain1 %>% select(-reordered)), label = subtrain1$reordered)

cv.nround <- 1000 #search
set.seed(71)
bst.cv <- xgb.cv(params = params, data = X1, nfold = 4, nrounds=cv.nround, early_stopping_rounds= 3)
nround <- bst.cv$best_iteration

set.seed(71)
model3 <- xgboost(data = X1, params = params, nrounds = nround)

importance <- xgb.importance(colnames(X1), model = model1)
xgb.ggplot.importance(importance)

rm(X1, importance)
gc()

# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
out1 <- predict(model1, X)
out2 <- predict(model2, X)
out3 <- predict(model3, X)

test$reordered <- (out1 + out2 + out3)/3
