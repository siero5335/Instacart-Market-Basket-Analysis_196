
library(data.table)
test <- fread("predicted_testset.csv")





test$reordered <- (test$reordered > 0.211 | test$aisle_id ==1 &
                   test$reordered > 0.17 | test$aisle_id ==2 &
                   test$reordered > 0.211 | test$aisle_id ==3 &
                   test$reordered > 0.17 | test$aisle_id ==4 &
                   test$reordered > 0.1 | test$aisle_id ==5 &
                   test$reordered > 0.1 | test$aisle_id ==6 &
                   test$reordered > 0.17 | test$aisle_id ==7 &
                     test$reordered > 0.211 | test$aisle_id ==8 &
                     test$reordered > 0.17 | test$aisle_id ==9 &
                     test$reordered > 0.17 | test$aisle_id ==10 &
                     test$reordered > 0.1 | test$aisle_id ==11 &
                     test$reordered > 0.17 | test$aisle_id ==12 &
                     test$reordered > 0.211 | test$aisle_id ==13 &
                     test$reordered > 0.2 | test$aisle_id ==14 &
                     test$reordered > 0.17 | test$aisle_id ==15 &
                     test$reordered > 0.1 | test$aisle_id ==16 &
                     test$reordered > 0.1 | test$aisle_id ==17 &
                     test$reordered > 0.25 | test$aisle_id ==18 &
                     test$reordered > 0.1| test$aisle_id ==19 &
                     test$reordered > 0.1 | test$aisle_id ==20 &
                     test$reordered > 0.17 | test$aisle_id ==21 &
                     test$reordered > 0.1 | test$aisle_id ==22 &
                     test$reordered > 0.211 | test$aisle_id ==23 &
                     test$reordered > 0.25 | test$aisle_id ==24 &
                     test$reordered > 0.1 | test$aisle_id ==25 &
                     test$reordered > 0.211 | test$aisle_id ==26 &
                     test$reordered > 0.211 | test$aisle_id ==27 &
                     test$reordered > 0.211 | test$aisle_id ==28 &
                     test$reordered > 0.1 | test$aisle_id ==29 &
                     test$reordered > 0.1 | test$aisle_id ==30 &
                     test$reordered > 0.25 | test$aisle_id ==31 &
                     test$reordered > 0.25 | test$aisle_id ==32 &
                     test$reordered > 0.17 | test$aisle_id ==33 &
                     test$reordered > 0.17 | test$aisle_id ==34 &
                     test$reordered > 0.17 | test$aisle_id ==35 &
                     test$reordered > 0.1 | test$aisle_id ==36 &
                     test$reordered > 0.17 | test$aisle_id ==37 &
                     test$reordered > 0.211 | test$aisle_id ==38 &
                     test$reordered > 0.17 | test$aisle_id ==39 &
                     test$reordered > 0.211 | test$aisle_id ==40 &
                     test$reordered > 0.211 | test$aisle_id ==41 &
                     test$reordered > 0.211 | test$aisle_id ==42 &
                     test$reordered > 0.211 | test$aisle_id ==43 &
                     test$reordered > 0.1 | test$aisle_id ==44 &
                     test$reordered > 0.211 | test$aisle_id ==45 &
                     test$reordered > 0.211 | test$aisle_id ==46 &
                     test$reordered > 0.1 | test$aisle_id ==47 &
                     test$reordered > 0.211 | test$aisle_id ==48 &
                     test$reordered > 0.17 | test$aisle_id ==49 &
                     test$reordered > 0.211 | test$aisle_id ==50 &
                     test$reordered > 0.1 | test$aisle_id ==51 &
                     test$reordered > 0.211 | test$aisle_id ==52 &
                     test$reordered > 0.211 | test$aisle_id ==53 &
                     test$reordered > 0.1 | test$aisle_id ==54 &
                     test$reordered > 0.1 | test$aisle_id ==55 &
                     test$reordered > 0.17 | test$aisle_id ==56 &
                     test$reordered > 0.2 | test$aisle_id ==57 &
                     test$reordered > 0.211 | test$aisle_id ==58 &
                     test$reordered > 0.1 | test$aisle_id ==59 &
                     test$reordered > 0.1 | test$aisle_id ==60 &
                     test$reordered > 0.2 | test$aisle_id ==61 &
                     test$reordered > 0.25 | test$aisle_id ==62 &
                     test$reordered > 0.098 | test$aisle_id ==63 &
                     test$reordered > 0.25 | test$aisle_id ==64 &
                     test$reordered > 0.211 | test$aisle_id ==65 &
                     test$reordered > 0.17 | test$aisle_id ==66 &
                     test$reordered > 0.2 | test$aisle_id ==67 &
                     test$reordered > 0.17 | test$aisle_id ==68 &
                     test$reordered > 0.1 | test$aisle_id ==69 &
                     test$reordered > 0.17 | test$aisle_id ==70 &
                     test$reordered > 0.211 | test$aisle_id ==71 &
                     test$reordered > 0.1 | test$aisle_id ==72 &
                     test$reordered > 0.1 | test$aisle_id ==73 &
                     test$reordered > 0.1 | test$aisle_id ==74 &
                     test$reordered > 0.1 | test$aisle_id ==75 &
                     test$reordered > 0.1 | test$aisle_id ==76 &
                     test$reordered > 0.25 | test$aisle_id ==77 &
                     test$reordered > 0.17 | test$aisle_id ==78 &
                     test$reordered > 0.2 | test$aisle_id ==79 &
                     test$reordered > 0.1 | test$aisle_id ==80 &
                     test$reordered > 0.1 | test$aisle_id ==81 &
                     test$reordered > 0.1 | test$aisle_id ==82 &
                     test$reordered > 0.17 | test$aisle_id ==83 &
                     test$reordered > 0.25 | test$aisle_id ==84 &
                     test$reordered > 0.1 | test$aisle_id ==85 &
                     test$reordered > 0.211 | test$aisle_id ==86 &
                     test$reordered > 0.1 | test$aisle_id ==87 &
                     test$reordered > 0.1 | test$aisle_id ==88 &
                     test$reordered > 0.1 | test$aisle_id ==89 &
                     test$reordered > 0.17 | test$aisle_id ==90 &
                     test$reordered > 0.25 | test$aisle_id ==91 &
                     test$reordered > 0.211 | test$aisle_id ==92 &
                     test$reordered > 0.211 | test$aisle_id ==93 &
                     test$reordered > 0.211 | test$aisle_id ==94 &
                     test$reordered > 0.17 | test$aisle_id ==95 &
                     test$reordered > 0.211 | test$aisle_id ==96 &
                     test$reordered > 0.1 | test$aisle_id ==97 &
                     test$reordered > 0.211 | test$aisle_id ==98 &
                     test$reordered > 0.17 | test$aisle_id ==99 &
                     test$reordered > 0.1 | test$aisle_id ==100 &
                     test$reordered > 0.1 | test$aisle_id ==101 &
                     test$reordered > 0.1 | test$aisle_id ==102 &
                     test$reordered > 0.11 | test$aisle_id ==103 &
                     test$reordered > 0.1 | test$aisle_id ==104 &
                     test$reordered > 0.1 | test$aisle_id ==105 &
                     test$reordered > 0.17 | test$aisle_id ==106 &
                     test$reordered > 0.17 | test$aisle_id ==107 &
                     test$reordered > 0.17 | test$aisle_id ==108 &
                     test$reordered > 0.1 | test$aisle_id ==109 &
                     test$reordered > 0.11 | test$aisle_id ==110 &
                     test$reordered > 0.17 | test$aisle_id ==111 &
                     test$reordered > 0.211 | test$aisle_id ==112 &
                     test$reordered > 0.211 | test$aisle_id ==113 &
                     test$reordered > 0.1 | test$aisle_id ==114 &
                     test$reordered > 0.25 | test$aisle_id ==115 &
                     test$reordered > 0.17 | test$aisle_id ==116 &
                     test$reordered > 0.17 | test$aisle_id ==117 &
                     test$reordered > 0.1 | test$aisle_id ==118 &
                     test$reordered > 0.17 | test$aisle_id ==119 &
                     test$reordered > 0.25 | test$aisle_id ==120 &
                     test$reordered > 0.17 | test$aisle_id ==121 &
                     test$reordered > 0.17 | test$aisle_id ==122 &
                     test$reordered > 0.211 | test$aisle_id ==123 &
                     test$reordered > 0.25 | test$aisle_id ==124 &
                     test$reordered > 0.25 | test$aisle_id ==125 &
                     test$reordered > 0.1 | test$aisle_id ==126 &
                     test$reordered > 0.1 | test$aisle_id ==127 &
                     test$reordered > 0.17 | test$aisle_id ==128 &
                     test$reordered > 0.17 | test$aisle_id ==129 &
                     test$reordered > 0.17 | test$aisle_id ==130 &
                     test$reordered > 0.1 | test$aisle_id ==131 &
                     test$reordered > 0.1 | test$aisle_id ==132 &
                     test$reordered > 0.17 | test$aisle_id ==133 &
                     test$reordered > 0.211 | test$aisle_id ==134 ) * 1


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
write.csv(submission, file = "threthold_test.csv", row.names = F)