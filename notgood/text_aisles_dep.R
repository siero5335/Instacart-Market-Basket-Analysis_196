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

library(text2vec)
library(magrittr)
library(tsne)

SET_WORD2VEC_PARAM <- list(
  VECTORS = 100, WINDOW = 10,
  THREADS = 1
)


word2vec_aisle_model <- wordVectors::train_word2vec(
  train_file = aisles$aisle, output_file = aisles$aisle_vec,
  vectors = SET_WORD2VEC_PARAM$VECTORS, window = SET_WORD2VEC_PARAM$WINDOW,
  threads = SET_WORD2VEC_PARAM$THREADS
)


