library(dplyr)
library(corrplot)

db_transactions <- readRDS('data/processed/transaction_db.RDS')


category_features2 <-  db_transactions %>% 
  filter(type == 'expense', !(extra_fields_category_0 %in% c("Transfer", "Payment"))) %>% 
  select(user_id, extra_fields_category_0, amount, date) %>% 
  filter(!is.na(extra_fields_category_0)) %>% 
  mutate(
    date = as.Date(date),
  ) %>% 
  group_by(user_id, date, extra_fields_category_0) %>% 
  summarise(
    n_transactions = n(),
    amount = sum(abs(amount))
  ) %>% 
  group_by(user_id, extra_fields_category_0) %>% 
  summarise(
    n_transactions = sum(n_transactions),
    amount = sum(amount)
  ) %>% 
  ungroup() %>% 
  mutate(amount_mean = amount/n_transactions) %>% 
  pivot_wider(names_from = extra_fields_category_0, values_from = c(n_transactions, amount, amount_mean)) %>% 
  janitor::clean_names() %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), 0, .))


cor_db <- category_features2 %>% 
  select_at(vars(starts_with('amount_mean'))) %>% 
  cor()

corrplot(cor_db,method = 'number', type="upper", order="hclust", hclust.method = 'average')  
  