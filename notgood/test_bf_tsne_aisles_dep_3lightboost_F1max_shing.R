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


# Load Data ---------------------------------------------------------------


aisles <- fread("aisles.csv")
departments <- fread("departments.csv")
orderp <- fread("order_products__prior.csv")
ordert <- fread("order_products__train.csv")
orders <- fread("orders.csv")
products <- fread("products.csv")
order_streaks <- fread("order_streaks.csv")
products_tsnes <- read_csv("~/Desktop/Instacart Market Basket Analysis/product_embeddings.csv")


?fread()


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
test1 <- Rtsne(test, dims = 3, initial_dims = 30, perplexity = 30,
               theta = 0.1, check_duplicates = TRUE, pca = TRUE, max_iter = 1000,
               verbose = TRUE)

aisles <- cbind(aisles, test1$Y)
colnames(aisles) <- c("aisle_id", "aisle", "ais_tsne1", "ais_tsne2", "ais_tsne3") 
rm(test, test1, dtm, vectorizer, it_train_aisle, vocab)



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



set.seed(71)
res_tsne_prod <- Rtsne(as.matrix(products_tsnes[, 5:36]), dims = 2, initial_dims = 30, perplexity = 5,
                       theta = 0.7, check_duplicates = FALSE, pca = TRUE, max_iter = 500,
                       verbose = TRUE)


prod_vec_tsne <- data.frame(products_tsnes[,1:4], res_tsne_prod$Y[, 1:2])


products <- products %>% 
  left_join(prod_vec_tsne)

rm(products_tsnes, prod_vec_tsne)
gc()


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
    prod_median_orders = median(product_time, na.rm = T),
    prod_sd_orders = sd(product_time, na.rm = T),
    prod_mean_days_since_prior_order = mean(days_since_prior_order, na.rm = T),
    prod_median_days_since_prior_order = median(days_since_prior_order, na.rm = T),
    prod_sd_days_since_prior_order = sd(days_since_prior_order, na.rm = T),
    prod_mean_days_since_prior_order7 = mean(days_since_prior_order == 7, na.rm = T),
    prod_median_days_since_prior_order7 = median(days_since_prior_order == 7, na.rm = T),
    prod_sd_days_since_prior_order7 = sd(days_since_prior_order == 7, na.rm = T),
    prod_mean_days_since_prior_order14 = mean(days_since_prior_order == 14, na.rm = T),
    prod_median_days_since_prior_order14 = median(days_since_prior_order == 14, na.rm = T),
    prod_sd_days_since_prior_order14 = sd(days_since_prior_order == 14, na.rm = T),
    prod_mean_days_since_prior_order30 = mean(days_since_prior_order == 30, na.rm = T),
    prod_median_days_since_prior_order30 = median(days_since_prior_order == 30, na.rm = T),
    prod_sd_days_since_prior_order30 = sd(days_since_prior_order == 30, na.rm = T)
  )



ord2 <- orders_products %>%
  group_by(product_id, user_id) %>%
  summarise(
    prod_mean_order_dow = mean(order_dow),
    prod_median_order_dow = median(order_dow),
    prod_sd_order_dow = sd(order_dow),
    prod_mean_order_hour = mean(order_hour_of_day),
    prod_median_order_hour = mean(order_hour_of_day),
    prod_sd_order_hour = sd(order_hour_of_day)
  )

orderdow <- orders_products %>%
  group_by(product_id, user_id, order_dow) %>%
  summarise(
    dowcount = sum(reordered),
    dowsize = length(reordered)
  )

orderdow$reordered_dow_ratio <- orderdow$dowcount / orderdow$dowsize

prd <- prd %>%
  inner_join(products[, c(1, 3:12)], by = "product_id")


prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders
prd$prod_multiorder_probability <- prd$prod_multi_orders/ prd$prod_orders
prd$range_multi_order <- prd$prod_first_orders - prd$prod_multi_orders
prd$cart_reorder_ratio <-  prd$prod_reorders / prd$prod_cart
prd$cart_meanorder_ratio <-  prd$prod_mean_orders / prd$prod_cart  
prd$cart_sdorder_ratio <-  prd$prod_sd_orders / prd$prod_cart 
prd$add_to_cart_order_inverted <- prd$prod_orders - prd$prod_cart



prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders,
                      -prod_multi_orders)

rm(products)

set.seed(71)
test1 <- Rtsne(as.matrix(prd[, -c(2, 6:18,25,26, 36)]), dims = 2, initial_dims = 50, perplexity = 30,
               theta = 0.5, check_duplicates = FALSE, pca = TRUE, max_iter = 300,
               verbose = TRUE)


prd$prod_tsne1 <- test1$Y[,1]
prd$prod_tsne2 <- test1$Y[,2]

rm(test1)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_min_orders = min(order_number),
    user_mean_orders = mean(order_number, na.rm = T),
    user_median_orders = median(order_number, na.rm = T),
    user_sd_orders = sd(order_number, na.rm = T),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
    user_median_days_since_prior = median(days_since_prior_order, na.rm = T),
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
    user_mean_hour = median(order_hour_of_day),
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
    user_median_cart = median(user_cart),
    user_sd_cart = sd(user_cart)
  )

users <- users %>% inner_join(temp)


users$user_mean_cart_order <- users$user_mean_orders / users$user_mean_cart
users$user_sd_cart_order <- users$user_sd_orders / users$user_sd_cart
users$user_diff_ordermean <- users$user_mean_days_since_prior - users$time_since_last_order
users$user_diff_ordersd <- users$user_diff_ordermean / users$user_sd_days_since_prior

rm(us, ord, temp)
gc()

set.seed(71)
test1 <- Rtsne(as.matrix(users[, -c(21, 27, 29)]), dims = 2, initial_dims = 30, perplexity = 30,
               theta = 0.5, check_duplicates = FALSE, pca = TRUE, max_iter = 500,
               verbose = TRUE)


users$user_tsne1 <- test1$Y[,1]
users$user_tsne2 <- test1$Y[,2]

rm(test1)
gc()

# Database ----------------------------------------------------------------
orders_products <- orders_products %>% 
  inner_join(orderdow, by = c("user_id", "product_id", "order_dow"))

data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    sum_dowcount = sum(dowcount),
    sum_dowsize = sum(dowsize),
    reordered_mean_dow_ratio = mean(reordered_dow_ratio),
    reordered_median_dow_ratio = median(reordered_dow_ratio),
    reordered_sd_dow_ratio = sd(reordered_dow_ratio),
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_mean_order = mean(order_number, na.rm = T),
    up_sd_order = sd(order_number, na.rm = T),
    up_average_cart_position = mean(add_to_cart_order),
    up_sd_cart_position = sd(add_to_cart_order)
  )


data <- data %>%
  left_join(order_streaks,  by = c("user_id", "product_id"))


rm(orders_products, order_streaks)
gc()



data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_order_sd_rate <- data$up_orders / data$user_sd_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)
data$orderfirstlast <- data$up_order_rate_since_first_order * data$up_orders_since_last_order
data$order_cart_ratio <-  data$prod_orders / data$prod_cart
data$cartpos_order_rate <- data$up_average_cart_position / data$user_orders

data$tsne_mix1 <- data$user_tsne1 * data$prod_tsne1
data$tsne_mix2 <- data$user_tsne1 * data$prod_tsne2
data$tsne_mix3 <- data$user_tsne2 * data$prod_tsne1
data$tsne_mix4 <- data$user_tsne2 * data$prod_tsne2

gc()


data <- data %>%
  left_join(ord2,  by = c("user_id", "product_id"))

data$mean_prodorder_userordererate <- data$prod_mean_orders / data$user_mean_orders
data$sd_prodorder_userordererate <- data$prod_sd_orders / data$user_sd_orders

rm(prd, users, ord2, products, orders)

bf <- fread("bf.csv")
bf <- bf[, 2:7]


data <- data %>% 
  left_join(bf)

rm(bf)
gc()


data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))
rm(ordert, orderdow)
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
                      nrounds = 5000, early_stopping_rounds=20)

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
                      nrounds = 5000, early_stopping_rounds=20)

rm(X1, evalX1)
gc()


#model3
set.seed(70)
train_part <- createDataPartition(train$reordered, p=0.4, list=FALSE)

X1 <- lgb.Dataset(as.matrix(train[train_part,] %>% select(-reordered)), 
                  label = train[train_part,]$reordered)

evalX1 <-  lgb.Dataset(as.matrix(train[-train_part,] %>% select(-reordered)), 
                       label = train[-train_part,]$reordered)

set.seed(71)
model3_1 <- lgb.train(data = X1, valids = list(eval = evalX1), params = params, 
                      nrounds = 5000, early_stopping_rounds=20)
rm(X1, evalX1)
gc()



# Apply model -------------------------------------------------------------
set.seed(71)
X <- as.matrix(test %>% select(-order_id, -product_id))

out1_1 <- predict(model1_1, data = X, n = bst.cv$best_iter)
out2_1 <- predict(model2_1, data = X, n = bst.cv$best_iter)
out3_1 <- predict(model3_1, data = X, n = bst.cv$best_iter)

test$reordered <- (out1_1 + out2_1 + out3_1)/3

