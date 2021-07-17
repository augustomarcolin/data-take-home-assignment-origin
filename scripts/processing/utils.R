library(rfm)
library(purrr)

group_rfm_variables <- function(db_transactions){
  
  transactions_by_type <- db_transactions %>% 
    select(id = user_id, amount, date, type) %>% 
    mutate(
      date = as.Date(date)
    ) %>% 
    group_by(id, date, type) %>% 
    summarise(
      count_transactions = n(),
      amount = sum(abs(amount))
    ) %>% 
    ungroup() %>% 
    split(.$type, x = .) #list of dataframes by type
  
  fm_tables_type <- transactions_by_type %>% 
    map(
      ~(.x %>% group_by(id) %>% summarise(
        count_transactions = sum(count_transactions),
        amount = sum(amount)
      ) %>% 
        ungroup() %>% 
        mutate(amount_mean = amount/count_transactions) %>% 
        janitor::clean_names() %>% 
        mutate_if(is.integer, as.numeric) %>% 
        mutate_if(is.numeric, ~if_else(is.na(.), 0, .)))
    ) %>% 
    purrr::map2(.x = ., .y = names(.), ~.x %>% rename_at(vars(-id),function(x) paste0(x,"_", .y))) #adding suffix type
  
  
  # rfm_tables_type <- transactions_by_type %>% 
  #   purrr::map(~rfm_table_order(.x, customer_id = user_id, order_date = date, revenue = amount, analysis_date = Sys.Date())) %>% 
  #   purrr::map(., ~.x$rfm) %>% #getting rfm database
  #   purrr::map(., ~(.x %>%
  #                     select(id = customer_id, recency_days, transaction_count, amount) %>% 
  #                     mutate(amount_mean = amount/transaction_count) #creating feature amount mean
  #   )
  #   )  %>% 
  #   purrr::map2(.x = ., .y = names(.), ~.x %>% rename_at(vars(-id),function(x) paste0(x,"_", .y))) #adding suffix type
  
  
  rfm_vars <- fm_tables_type %>% 
    reduce(full_join, by = "id") %>% 
    mutate_if(is.numeric, ~if_else(is.na(.), 0, .)) 
  
  
  return(rfm_vars)
  
}




