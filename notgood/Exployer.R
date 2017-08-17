library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

order_products_train <- read_csv("~/Desktop/Instacart Market Basket Analysis/order_products__train.csv")
orders <- read_csv("~/Desktop/Instacart Market Basket Analysis/orders.csv")
products <- read_csv("~/Desktop/Instacart Market Basket Analysis/products.csv")

orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="red")

orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="red")

summary(orders$days_since_prior_order)

orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="red")
#定期購入みたいなのがあるのかな。それならそこは特徴量を作ったほうが良さそう。NAもなるべく埋めたい。

orders %>% filter(eval_set=="prior") %>% count(order_number) %>% ggplot(aes(order_number,n)) + geom_line(color="red", size=1)+geom_point(size=2, color="red")
summary(orders$order_number)


order_products_train %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))
summary(order_products_train$add_to_cart_order)


order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="red") + 
  geom_rug() + 
  coord_cartesian(xlim=c(0,80))
summary(order_products_prior$add_to_cart_order)

tmp1 <- order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(50, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
kable(tmp1)

tmp2 <- order_products_prior %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(50, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
kable(tmp2)

tmp3 <- bind_cols(tmp1, tmp2)
kable(tmp3)

des
tmp <- order_products_train %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(tmp)

tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity")

tmp <-order_products_train %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n>40) %>% 
  top_n(50,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")

kable(tmp)


tmp %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))


tmp <- order_products_train %>% 
  group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)

kable(tmp)

order_products_train %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="red")


order_products_train %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  ggplot(aes(x=n,y=proportion_reordered))+
  geom_point()+
  geom_smooth(color="red")+
  coord_cartesian(xlim=c(0,2000))

products <- products %>% mutate(organic=ifelse(str_detect(products$product_name,'Organic'),"organic","not organic"), organic= as.factor(organic))

tmp <- order_products_train %>% 
  left_join(products, by="product_id") %>% 
  group_by(organic) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count/sum(count))
kable(tmp)

tmp %>% 
  ggplot(aes(x=organic,y=count, fill=organic))+
  geom_bar(stat="identity")

tmp <- order_products_train %>% left_join(products,by="product_id") %>% group_by(organic) %>% summarize(mean_reordered = mean(reordered))
kable(tmp)

tmp %>% 
  ggplot(aes(x=organic,fill=organic,y=mean_reordered))+geom_bar(stat="identity")


tmp <- order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(m = mean(reordered),n=n()) %>% 
  right_join(filter(orders,order_number>2), by="order_id")

tmp2 <- tmp %>% 
  filter(eval_set =="prior") %>% 
  group_by(user_id) %>% 
  summarize(n_equal = sum(m==1,na.rm=T), percent_equal = n_equal/n()) %>% 
  filter(percent_equal == 1) %>% 
  arrange(desc(n_equal))