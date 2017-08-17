bf <- fread("bf.csv")

library(data.table)
predicted_trainset <- fread("predicted_trainset.csv")

library(ggplot2)
library(plotly)
library(dplyr)

train_plot <- predicted_trainset %>%
  group_by(user_id, aisle_id, reordered) %>%
  summarise(
    mean_reordered = mean(reordered2),
    sd_reordered = sd(reordered2)
  )

g <- ggplot(
  train_plot,
  aes (
    x = as.factor(aisle_id),
    y = mean_reordered,            # xで数値データを指定
    colour = as.factor(reordered)   # カテゴリーごとで色分け
  )
)

g <- g + geom_boxplot()　 

plot(g)

ggplotly(g)



reordered_set <- predicted_trainset %>%
                     filter(predicted_trainset$reordered ==1, 
                            predicted_trainset$aisle_id ==134)


mean(reordered_set$reordered2)

head(bf)
