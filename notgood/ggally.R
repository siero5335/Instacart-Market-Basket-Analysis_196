library(GGally)
library(caret)

train_sub<-sample_frac(train, 0.5)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test< -train[-sid,]

train_sub <- train
ggpairs(na.omit(train), lower=list(continuous="smooth"), colour="reordered")
