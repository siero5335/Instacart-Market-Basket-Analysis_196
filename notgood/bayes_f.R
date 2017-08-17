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
orderp <- fread("order_products__prior.csv")
orders <- fread("orders.csv")

users2 <- orders %>%
  filter(eval_set == "test") %>%
  group_by(user_id)

prior <- orderp %>%
  group_by(product_id) %>%
    summarise(
    sum_of_reorders = sum(reordered, na.rm = T),
    number_of_orders =n ()
    )

prior$prior_p <- (prior$sum_of_reorders + 1) / (prior$number_of_orders +2)


