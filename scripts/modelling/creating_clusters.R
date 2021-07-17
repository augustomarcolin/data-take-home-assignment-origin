library(factoextra)
library(dplyr)
library(purrr)

db_train <- readRDS('data/processed/db_train.RDS')

db_train_pad <- db_train %>% 
  mutate_if(is.numeric, ~((. - mean(., na.rm = T))/sd(., na.rm = T))) 
#---- kmeans ----

#---- expense + income ----

#---- * number clusters ----

n_cluster_expense_income <- map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_train_pad %>% select(amount_mean_income, amount_mean_expense),
                         kmeans,
                         .x)
  )

#---- * clusters ----
cluster_expense_income <- db_train_pad %>% select(amount_mean_income, amount_mean_expense) %>% 
  kmeans(centers = 4, nstart = 25)

#---- * descriptive ----
db_expense_income <- db_train %>% 
  select(amount_mean_expense, amount_mean_income) %>% 
  mutate(
    cluster = as.factor(cluster_expense_income$cluster)
  )

db_expense_income %>% 
  ggplot(aes(x = amount_mean_expense, y = amount_mean_income)) + 
  geom_point(aes(color = cluster, shape = cluster), alpha = 0.7, size = 3) +
  theme_bw()

#---- expense + income + age----

#---- * number clusters ----

n_cluster_age_expense_income <- map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_train_pad %>% 
                           select(age_at, amount_mean_income, amount_mean_expense) %>% 
                           na.omit(),
                         kmeans,
                         .x)
  )

#---- * clusters ----
cluster_age_expense_income <- db_train_pad %>% 
  select(age_at, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  kmeans(centers = 3, nstart = 25)


#---- * descriptive ----
db_age_expense_income <- db_train %>% 
  select(age_at, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate(
    cluster = as.factor(cluster_age_expense_income$cluster)
  )

db_age_expense_income %>% 
  pivot_longer(cols = c(amount_mean_income, amount_mean_expense), names_to = 'amount', values_to = "value") %>% 
  ggplot(aes(x = age_at, y = value)) + 
  geom_point(aes(color = cluster), alpha = 1, size = 3) +
  facet_wrap(~amount) +
  theme_bw()

#the age give no information for the cluster at this moment


#---- expense + income + number_expense----

#---- * number clusters ----

n_cluster_expense_income2 <-   map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_train_pad %>% 
                           select(count_transactions_expense, amount_mean_income, amount_mean_expense) %>% 
                           na.omit(),
                         kmeans,
                         .x)
  )

#---- * clusters ----
cluster_expense_income2 <- db_train %>% 
  select(count_transactions_expense, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  kmeans(centers = 4, nstart = 25)


#---- * descriptive ----
db_expense_income2 <- db_train %>% 
  select(count_transactions_expense, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate(
    cluster = as.factor(cluster_expense_income2$cluster)
  )

db_expense_income2 %>% 
  pivot_longer(cols = c(amount_mean_income, amount_mean_expense), names_to = 'amount', values_to = "value") %>% 
  ggplot(aes(x = count_transactions_expense, y = value)) + 
  geom_point(aes(color = cluster, shape = cluster), alpha = 1, size = 3) +
  facet_wrap(~amount, scales = "free") +
  theme_bw()


#---- types_expense amount ----

#---- * number clusters ----

customer_with_expense <- db_train %>% 
  select(id, count_transactions_expense) %>% 
  filter(count_transactions_expense > 0) %>% 
  ungroup() %>% 
  select(id)


db_cluster_types_expense <- db_train %>% 
  inner_join(customer_with_expense, by = 'id') %>% 
  select(amount_expense, starts_with("transactions_amount")) %>% 
  mutate_at(vars(-amount_expense), ~./amount_expense) 
  #mutate_if(is.numeric, ~((. - mean(., na.rm = T))/sd(., na.rm = T))) 

n_cluster_types <- 
  map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_cluster_types_expense,
                         kmeans,
                         .x)
  )

#---- * clusters ----

cluster_types <- db_cluster_types_expense %>% 
  na.omit() %>% 
  kmeans(centers = 3, nstart = 25)


#---- * descriptive ----
db_expense_types <- db_train %>% 
  inner_join(customer_with_expense, by = 'id') %>% 
  select(amount_expense, starts_with("transactions_amount")) %>% 
  mutate_at(vars(-amount_expense), ~./amount_expense) %>% 
  select(-amount_expense) %>% 
  mutate(
    cluster = as.factor(cluster_types$cluster)
  )

db_expense_types %>%
  group_by(cluster) %>% 
  summarise_all(
    list(mean = mean)
  ) %>% View()


#---- types_expense freq ----

#---- * number clusters ----

customer_with_expense <- db_train %>% 
  select(id, count_transactions_expense) %>% 
  filter(count_transactions_expense > 0) %>% 
  ungroup() %>% 
  select(id)


db_cluster_types_expense_fre <- db_train %>% 
  inner_join(customer_with_expense, by = 'id') %>% 
  select(count_transactions_expense, starts_with("n_transactions")) %>% 
  mutate_at(vars(-count_transactions_expense), ~./count_transactions_expense) 
#mutate_if(is.numeric, ~((. - mean(., na.rm = T))/sd(., na.rm = T))) 

n_cluster_types_freq <- 
  map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_cluster_types_expense_fre,
                         kmeans,
                         .x)
  )

n_cluster_types_freq[[3]]

#---- * clusters ----

cluster_types <- db_cluster_types_expense_fre %>% 
  na.omit() %>% 
  kmeans(centers = 3, nstart = 25)


#---- * descriptive ----
db_expense_types_freq <- db_train %>% 
  inner_join(customer_with_expense, by = 'id') %>% 
  select(count_transactions_expense, starts_with("n_transactions")) %>% 
  mutate_at(vars(-count_transactions_expense), ~./count_transactions_expense) %>% 
  select(-count_transactions_expense) %>% 
  mutate(
    cluster = as.factor(cluster_types$cluster)
  )

db_expense_types_freq %>%
  group_by(cluster) %>% 
  summarise_all(
    list(mean = mean)
  ) %>% View()
