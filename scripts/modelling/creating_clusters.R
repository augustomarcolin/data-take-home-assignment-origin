library(factoextra)
library(dplyr)


#---- kmeans ----

#---- expense + income ----

#---- * number clusters ----

n_cluster_expense_income <- db_train %>% 
  select(amount_mean_income, amount_mean_expense) %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_to_cluster, kmeans, .x)
  )

#---- * clusters ----
cluster_expense_income <- db_train %>% 
  select(amount_mean_expense, amount_mean_income) %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  kmeans(centers = 5, nstart = 25)

#---- * descriptive ----
db_expense_income <- db_train %>% 
  select(amount_mean_expense, amount_mean_income) %>% 
  mutate(
    cluster = as.factor(cluster_expense_income$cluster)
  )

db_expense_income %>% 
  ggplot(aes(x = amount_mean_expense, y = amount_mean_income)) + 
  geom_point(aes(color = cluster), alpha = 1) +
  theme_bw()

#---- expense + income + age----

#---- * number clusters ----

n_cluster_age_expense_income <- db_train %>% 
  select(age_at, amount_mean_income, amount_mean_expense) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_to_cluster, kmeans, .x)
  )

#---- * clusters ----
cluster_age_expense_income <- db_train %>% 
  select(age_at, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
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

n_cluster_expense_income2 <- db_train %>% 
  select(transaction_count_expense, amount_mean_income, amount_mean_expense) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_to_cluster, kmeans, .x)
  )

#---- * clusters ----
cluster_expense_income2 <- db_train %>% 
  select(transaction_count_expense, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  kmeans(centers = 4, nstart = 25)


#---- * descriptive ----
db_expense_income2 <- db_train %>% 
  select(transaction_count_expense, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate(
    cluster = as.factor(cluster_expense_income2$cluster)
  )

db_expense_income2 %>% 
  pivot_longer(cols = c(amount_mean_income, amount_mean_expense), names_to = 'amount', values_to = "value") %>% 
  ggplot(aes(x = transaction_count_expense, y = value)) + 
  geom_point(aes(color = cluster, shape = cluster), alpha = 1, size = 3) +
  facet_wrap(~amount, scales = "free") +
  theme_bw()


#---- expense + income + types_expense ----

#---- * number clusters ----

n_cluster_expense_income2 <- db_train %>% 
  select(transaction_count_expense, amount_mean_income, amount_mean_expense) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  map(.x = c("silhouette", "wss", "gap_stat"),
      .f = ~fviz_nbclust(db_to_cluster, kmeans, .x)
  )

#---- * clusters ----
cluster_expense_income2 <- db_train %>% 
  select(transaction_count_expense, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate_if(is.numeric, ~((. - mean(.))/sd(.))) %>% 
  kmeans(centers = 4, nstart = 25)


#---- * descriptive ----
db_expense_income2 <- db_train %>% 
  select(transaction_count_expense, amount_mean_expense, amount_mean_income) %>% 
  na.omit() %>% 
  mutate(
    cluster = as.factor(cluster_expense_income2$cluster)
  )

db_expense_income2 %>% 
  pivot_longer(cols = c(amount_mean_income, amount_mean_expense), names_to = 'amount', values_to = "value") %>% 
  ggplot(aes(x = transaction_count_expense, y = value)) + 
  geom_point(aes(color = cluster, shape = cluster), alpha = 1, size = 3) +
  facet_wrap(~amount) +
  theme_bw()

