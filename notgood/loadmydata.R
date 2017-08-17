library(data.table)
library(dplyr)
library(tidyr)
library(caret)

data <- fread("my_data.csv")

# Train / Test datasets ---------------------------------------------------
train2 <- as.data.frame(data[data$eval_set == "train",])
train2$eval_set <- NULL
train2$product_id <- NULL
train2$order_id <- NULL
train2$user_id <- NULL
train2$aisle <- NULL
train2$organic <- NULL
train2$V1 <- NULL

train2$reordered[is.na(train2$reordered)] <- 0


test2 <- as.data.frame(data[data$eval_set == "test",])
test2$eval_set <- NULL
test2$user_id <- NULL
test2$aisle <- NULL
test2$organic <- NULL
test2$V1 <- NULL
rm(data)
gc()

# Load Train / Test datasets ---------------------------------------------------

train <- fread("train_py.csv", header=T)
test <- fread("test_py.csv", header=T)

train$V1 <- NULL
test$V1 <- NULL
gc()

train$tsne_mix1 <- train2$tsne_mix1
train$tsne_mix2 <- train2$tsne_mix2
train$tsne_mix3 <- train2$tsne_mix3
train$tsne_mix4 <- train2$tsne_mix4
train$tsne_word1 <- train2$X1
train$tsne_word2 <- train2$X2
train$user_sd_orders<- train2$user_sd_orders
train$recount_c <- train2$recount_c
train$cumulative <- train2$cumulative
train$orderfirstlast <- train2$orderfirstlast
train$bf <- train2$bf
train$order_streak <- train2$order_streak
train$reordered <- train2$reordered
train$product_id <- NULL



test$tsne_mix1 <- test2$tsne_mix1
test$tsne_mix2 <- test2$tsne_mix2
test$tsne_mix3 <- test2$tsne_mix3
test$tsne_mix4 <- test2$tsne_mix4
test$tsne_word1 <- test2$X1
test$tsne_word2 <- test2$X2
test$user_sd_orders<- test2$user_sd_orders
test$recount_c <- test2$recount_c
test$cumulative <- test2$cumulative
test$orderfirstlast <- test2$orderfirstlast
test$bf <- test2$bf
test$order_streak <- test2$order_streak
test$product_id <- test2$product_id
test$order_id <- test2$order_id
rm(train2, test2)
gc()




#train
colnames(train) <- c("user_product_reordered_ratio" ,   "reordered_sum"     ,              "add_to_cart_order_inverted_mean" ,"add_to_cart_order_relative_mean", "reorder_prob"      ,              "last" ,                          
"prev1"    ,                       "prev2"         ,                  "median"      ,                    "mean"    ,                        "dep_reordered_ratio"   ,         "aisle_reordered_ratio"    ,      
"aisle_products"       ,           "aisle_reordered"         ,        "dep_products"         ,           "dep_reordered"           ,        "prod_users_unq"             ,     "prod_users_unq_reordered"    ,   
"order_number"      ,              "prod_add_to_card_mean"      ,     "days_since_prior_order"  ,        "order_dow"    ,                   "order_hour_of_day"  ,             "reorder_ration"  ,               
"user_orders"    ,                 "user_order_starts_at"  ,          "user_mean_days_since_prior"  ,    "user_average_basket"   ,          "user_distinct_products"  ,        "user_reorder_ratio"  ,           
"user_total_products"       ,      "prod_orders"           ,          "prod_reorders"   ,                "up_order_rate"           ,        "up_orders_since_last_order"    ,  "up_order_rate_since_first_order",
"up_orders"              ,         "up_first_order"        ,          "up_last_order"    ,               "up_mean_cart_position"    ,       "days_since_prior_order_mean"   ,  "order_dow_mean"   ,              
"order_hour_of_day_mean"     ,     "0a"                        ,       "1a"              ,                 "2a"          ,                     "3a"        ,                       "4a"    ,                          
"5a"                        ,       "6a"     ,                          "7a"      ,                         "8a"    ,                           "9a"    ,                           "10a"   ,                          
"11a"                         ,     "12a"                    ,          "13a"                      ,       "14a"            ,                  "15a"        ,                      "16a"    ,                         
"17a"                         ,     "18a"    ,                          "19a"      ,                        "20a"   ,                           "21a"     ,                         "22a" ,                            
"23a"                     ,         "24a"                  ,            "25a"                     ,         "26a"            ,                  "27a"           ,                   "28a"  ,                           
"29a"                     ,         "30a"    ,                          "31a"     ,                         "0"     ,                          "1"     ,                          "2"  ,                            
"3"                            ,   "4"                      ,         "5"               ,                "6"              ,                 "7"        ,                       "8"     ,                         
"9"                           ,    "10"        ,                      "11"   ,                           "12"   ,                           "13"   ,                           "14"   ,                          
"15"                        ,      "16"              ,                "17"         ,                     "18"      ,                        "19"            ,                  "20"  ,                           
"21"                        ,      "22"       ,                       "23"       ,                       "24"     ,                         "25"   ,                           "26"  ,                           
"27"                ,              "28"    ,                          "29"         ,                     "30"   ,                           "31"     ,                         "aisle_id" ,                      
"department_id"     ,              "tsne_mix1"      ,                 "tsne_mix2"         ,              "tsne_mix3"     ,                  "tsne_mix4"  ,                     "tsne_word1"   ,                  
"tsne_word2"        ,              "user_sd_orders"   ,               "recount_c"     ,                  "cumulative"   ,                   "orderfirstlast"    ,              "bf"    ,                         
"order_streak"      ,              "reordered"     )

#test
colnames(test) <- c("user_product_reordered_ratio"   , "reordered_sum"      ,             "add_to_cart_order_inverted_mean" ,"add_to_cart_order_relative_mean", "reorder_prob"     ,               "last"   ,                        
"prev1"            ,               "prev2"            ,               "median"                  ,        "mean"          ,                  "dep_reordered_ratio" ,            "aisle_reordered_ratio"  ,        
"aisle_products"    ,              "aisle_reordered"   ,              "dep_products"             ,       "dep_reordered"  ,                 "prod_users_unq"      ,            "prod_users_unq_reordered"  ,     
"order_number"       ,             "prod_add_to_card_mean"  ,         "days_since_prior_order"    ,      "order_dow"       ,                "order_hour_of_day"    ,           "reorder_ration"         ,        
"user_orders"         ,            "user_order_starts_at"    ,        "user_mean_days_since_prior" ,     "user_average_basket" ,            "user_distinct_products"  ,        "user_reorder_ratio"     ,        
"user_total_products"  ,           "prod_orders"              ,       "prod_reorders"               ,    "up_order_rate"        ,           "up_orders_since_last_order" ,     "up_order_rate_since_first_order",
"up_orders"             ,          "up_first_order"            ,      "up_last_order"                ,   "up_mean_cart_position" ,          "days_since_prior_order_mean" ,    "order_dow_mean"        ,         
"order_hour_of_day_mean" ,         "0a"       ,                        "1a"       ,                        "2a"     ,                          "3a"        ,                       "4a"       ,                       
"5a"        ,                       "6a"        ,                       "7a"        ,                       "8a"      ,                         "9a"         ,                      "10a"       ,                      
"11a"        ,                      "12a"        ,                      "13a"        ,                      "14a"      ,                        "15a"         ,                     "16a"        ,                     
"17a"         ,                     "18a"         ,                     "19a"         ,                     "20a"       ,                       "21a"          ,                    "22a"         ,                    
"23a"          ,                    "24a"          ,                    "25a"          ,                    "26a"        ,                      "27a"           ,                   "28a"          ,                   
"29a"           ,                   "30a"           ,                   "31a"           ,                   "0"          ,                     "1"             ,                  "2"            ,                  
"3"             ,                  "4"             ,                  "5"             ,                  "6"           ,                    "7"              ,                 "8"             ,                 
"9"              ,                 "10"             ,                 "11"             ,                 "12"           ,                   "13"              ,                "14"             ,                
"15"              ,                "16"              ,                "17"              ,                "18"            ,                  "19"               ,               "20"              ,               
"21"               ,               "22"               ,               "23"               ,               "24"             ,                 "25"                ,              "26"               ,              
"27"                ,              "28"                ,              "29"                ,              "30"              ,                "31"                 ,             "product_id"        ,             
"aisle_id"           ,             "department_id"      ,             "tsne_mix1"          ,             "tsne_mix2"        ,               "tsne_mix3"           ,            "tsne_mix4"          ,            
"tsne_word1"          ,            "tsne_word2"          ,            "user_sd_orders"      ,            "recount_c"        ,              "cumulative"           ,           "orderfirstlast"       ,         
"bf"                   ,           "order_streak"         ,           "order_id" )

write.csv(train, "train.csv")
write.csv(test, "test.csv")