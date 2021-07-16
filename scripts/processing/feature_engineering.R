library(dplyr)
library(tidyr)

source('scripts/processing/utils.R')
#---- reading_data ----

db_users <- readRDS('data/processed/user_db.RDS') %>% 
  mutate(
    id = toupper(id), #it's necessary to join with transational data
    age_at = lubridate::time_length(difftime(created_at, date_of_birth), "years") #extracting age
  )

db_transactions <- readRDS('data/processed/transaction_db.RDS')

#---- group rfm features ----

rfm_type_features <- group_rfm_variables(db_transactions)

#---- category_features ----


category_features1 <- db_transactions %>% 
  select(user_id, extra_fields_category_0, extra_fields_category_1, amount, date) %>% 
  filter(!is.na(extra_fields_category_0)) %>% 
  mutate(
    date = as.Date(date),
    new_category = paste(extra_fields_category_0, extra_fields_category_1, sep = '_')
  ) %>% 
  group_by(user_id, date, new_category) %>% 
  summarise(
    n_transactions = n(),
    amount = sum(abs(amount))
  ) %>% 
  group_by(user_id, new_category) %>% 
  summarise(
    n_transactions = sum(n_transactions),
    amount = sum(amount)
  ) %>% 
  ungroup() %>% 
  mutate(amount_mean = amount/n_transactions) %>% 
  pivot_wider(names_from = new_category, values_from = c(n_transactions, amount, amount_mean)) %>% 
  janitor::clean_names() %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), 0, .)) 


category_features2 <- db_transactions %>% 
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


#---- payment features ----

payment_channel_features <- db_transactions %>% 
  select(user_id, extra_fields_payment_channel) %>% 
  group_by(user_id) %>% 
  count(extra_fields_payment_channel) %>% 
  mutate(
    extra_fields_payment_channel = ifelse(is.na(extra_fields_payment_channel), 'no_channel', extra_fields_payment_channel),
    pc_by_channel = n/sum(n)
  ) %>% 
  select(-n) %>% 
  pivot_wider(names_from = extra_fields_payment_channel, values_from = pc_by_channel) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), 0, .)) %>% 
  janitor::clean_names()
  
#---- dataset_to_train ----

db_train <- db_users %>% 
  select(id, state, age_at) %>% 
  inner_join(
    rfm_type_features,
    by = c("id")
  ) %>% 
  left_join(
    category_features2,
    by = c("id" = 'user_id'),
    suffix = c("", "cat")
  ) %>% 
  left_join(
    payment_channel_features,
    by = c("id" = 'user_id')
  ) %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), 0, .)) %>% 
  mutate(
    age_at = if_else(age_at == 0, NA_real_, age_at)
  )

saveRDS(object = db_train, file = 'data/processed/db_train.RDS')

