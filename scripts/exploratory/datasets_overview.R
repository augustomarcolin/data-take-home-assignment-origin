library(dplyr)
library(tidyr)
library(ggplot2)
library(skimr)
library(forcats)
#---- reading data ----

db_users <- readRDS('data/processed/user_db.RDS') %>% 
  mutate(
    age_at = lubridate::time_length(difftime(created_at, date_of_birth), "years") #extracting age
  )

db_transactions <- readRDS('data/processed/transaction_db.RDS')


#---- descriptive ----

#---- * Users ----
skim(db_users)

# gender is usefull because has just 6% of complete cases
# age has a huge outlier, just remove to analize

#---- ** age ----

db_users %>% 
  filter(age_at <= 100) %>% 
  ggplot(aes(x = age_at)) +
  geom_density()

#---- ** state ----

library(usmap)

db_users %>% 
  count(state) %>% 
  mutate(
    perc = round(n / sum(n) * 100, digits = 2)
  ) %>% 
  arrange(desc(n)) %>% 
  plot_usmap(data = ., values = "perc", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Users %", label = scales::comma
  ) + theme(legend.position = "right")

# more than half of the sample is from CA or has no available state
# maybe it's important group some states to analize and if possible some cities

#---- ** city ----

db_users %>% 
  count(city) %>% 
  mutate(
    perc = round(n / sum(n) * 100, digits = 2)
  ) %>% 
  arrange(desc(n)) 


#---- * transactional ----

glimpse(db_transactions)

# database overview

skim(db_transactions)

# just 84 users has transaction data

#---- ** number count by id ----

db_transactions %>% 
  select(user_id, account_id) %>% 
  distinct() %>% 
  count(user_id, sort = T) %>% 
  count(n)


#---- ** amount ----


db_transactions %>% 
  ggplot(aes(x = amount)) +
  geom_histogram()

# has some outliers, we can exclude for a quickly analysis or interpret and try to deal with it

#---- ** type ----

db_transactions %>% 
  count(type) %>% 
  mutate(
    perc = n / sum(n) 
  ) %>% 
  arrange(desc(n))  %>% 
  ggplot(aes(x = type, y = perc, label = scales::percent(perc))) +
  geom_bar(stat = 'identity', fill = '#ed2f5b') +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = 'Transaction Type', y = "Percent") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  theme_bw()

# I need to divide in group types and get RFM features for each group


#----- *** extra_fields_category ----

db_transactions %>% 
  select(starts_with("extra_fields_category")) %>% 
  group_by_all() %>% 
  count(sort = T)  


db_transactions %>% 
  count(extra_fields_category_id, sort = T)


db_transactions %>% 
  select(extra_fields_category_0, extra_fields_category_1) %>% 
  group_by_all() %>% 
  count(sort = T)  

#we have just 12 different categories id, also in the more granular category (2) we have just two fields
# maybe I can group by by category 0 and 1 and we keep 10 different groups

#----- *** extra_field merchant name ----

db_transactions %>% 
  count(extra_fields_merchant_name, sort = T) %>% 
  mutate(
    perc = round(n / sum(n) , digits = 2),
    extra_fields_merchant_name = fct_reorder(as_factor(extra_fields_merchant_name), perc, .desc = T), 
  ) %>% 
  arrange(desc(n))  %>% 
  ggplot(aes(x = extra_fields_merchant_name, y = perc, label = scales::percent(perc))) +
  geom_bar(stat = 'identity', fill = '#ed2f5b') +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = 'Transaction Type', y = "Percent") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  theme_bw()



db_transactions %>% 
  select(extra_fields_category_0, extra_fields_category_1, extra_fields_merchant_name) %>% 
  group_by_all() %>% 
  count(sort = T)  

#almost half database has missing merchant name, it make sense, because 35% percent of all transactions are transfer and income and doesn't have merchant category

# ----- ** extra_fields_payment_channel ----


db_transactions %>% 
  count(extra_fields_payment_channel, sort = T) %>% 
  mutate(
    perc = round(n / sum(n) , digits = 2),
    extra_fields_payment_channel = fct_reorder(as_factor(extra_fields_payment_channel), perc, .desc = T)
  ) %>% 
  arrange(desc(n))  %>% 
  ggplot(aes(x = extra_fields_payment_channel, y = perc, label = scales::percent(perc))) +
  geom_bar(stat = 'identity', fill = '#ed2f5b') +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = 'Transaction Type', y = "Percent") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  theme_bw()

# almost half of transactions are in store and 25% of NA


#----- ** extra_fields_payment_meta_payment_method ----


db_transactions %>% 
  count(extra_fields_payment_meta_payment_method, sort = T) %>% 
  mutate(
    perc = round(n / sum(n) , digits = 2)
  ) %>% 
  arrange(desc(n))  %>% 
  ggplot(aes(x = extra_fields_payment_meta_payment_method, y = perc, label = scales::percent(perc))) +
  geom_bar(stat = 'identity', fill = '#ed2f5b') +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = 'Transaction Type', y = "Percent") +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  theme_bw()

# just 4% of transactions with this data, it's almost useless

