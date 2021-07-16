library(dplyr)
library(tidyr)
library(stringr)
library(testthat)
op <- options(digits.secs = 3)
#---- reading data ----

users_db <- read.csv('data/raw/users.csv') %>% 
  as_tibble()

transaction_db <- read.csv('data/raw/transactions.csv') %>% 
  as_tibble()

#---- cleaning user db ----

usa_states <- state.abb

users_db_clean <- users_db %>% 
  janitor::clean_names() %>% 
  mutate_all(list(~str_replace(., "\\<img.*", ''))) %>% #replace img css by empty space
  mutate_all(list(~na_if(., ""))) %>% #replace empty space by NA
  mutate(
    state = ifelse(!(state %in% usa_states), NA_character_, state), #replace non compatible us states by NA 
    date_of_birth = as.Date(date_of_birth),
    created_at = strptime(created_at, '%Y-%m-%d %H:%M:%S')
    ) 

# unit test with has unique id's

max_unique_id <- users_db_clean %>% count(id) %>% pull(n) %>% max()

expect_equal(max_unique_id, 1)

saveRDS(object = users_db_clean, file = 'data/processed/user_db.RDS')

#---- cleaning transaction db ----

transaction_db_clean <- transaction_db %>% 
  janitor::clean_names() %>% 
  mutate(
    created_at = lubridate::as_datetime(created_at),
    date = lubridate::as_datetime(date)
  )  %>% 
  mutate_if(is.character, list(~na_if(., ""))) %>%  #replace empty space by NA
  mutate(extra_fields_category_id = as.character(extra_fields_category_id))

saveRDS(object = transaction_db_clean, file = 'data/processed/transaction_db.RDS')
