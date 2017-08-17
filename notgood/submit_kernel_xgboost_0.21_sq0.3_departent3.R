predicted_testset <- fread("predicted_testset.csv")

predicted_testset$reordered <- 
  (predicted_testset$reordered > 0.2524822| predicted_testset$department_id == 1 &
   predicted_testset$reordered > 0.2193894| predicted_testset$department_id == 2 &
   predicted_testset$reordered > 0.2947325| predicted_testset$department_id == 3 &
   predicted_testset$reordered > 0.3064661| predicted_testset$department_id == 4 &
   predicted_testset$reordered > 0.2910752| predicted_testset$department_id == 5 &
   predicted_testset$reordered > 0.1723321| predicted_testset$department_id == 6 &
   predicted_testset$reordered > 0.3605012| predicted_testset$department_id == 7 &
   predicted_testset$reordered > 0.2825073| predicted_testset$department_id == 8 &
   predicted_testset$reordered > 0.1736425| predicted_testset$department_id == 9 &
   predicted_testset$reordered > 0.2900956| predicted_testset$department_id == 10 &
   predicted_testset$reordered > 0.1395246| predicted_testset$department_id == 11 &
   predicted_testset$reordered > 0.2368062| predicted_testset$department_id == 12 &
   predicted_testset$reordered > 0.1389745| predicted_testset$department_id == 13 &
   predicted_testset$reordered > 0.2396637| predicted_testset$department_id == 14 &
   predicted_testset$reordered > 0.1761429| predicted_testset$department_id == 15 &
   predicted_testset$reordered > 0.330685| predicted_testset$department_id == 16 &
   predicted_testset$reordered > 0.1635336| predicted_testset$department_id == 17 &
   predicted_testset$reordered > 0.2402551| predicted_testset$department_id == 18 &
   predicted_testset$reordered > 0.2651497| predicted_testset$department_id == 19 &
   predicted_testset$reordered > 0.2810854| predicted_testset$department_id == 20 &
   predicted_testset$reordered > 0.2810571| predicted_testset$department_id == 21) * 1





submission <- predicted_testset %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(predicted_testset$order_id[!predicted_testset$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit_kernel_xgboost_0.21_sq0.3_departent3.csv", row.names = F)
