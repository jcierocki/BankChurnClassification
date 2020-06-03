#### random forest tunning

library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)
library(doFuture)
library(knitr)

rm(list = ls())

all_cores <- parallel::detectCores(logical = FALSE) - 1

registerDoFuture()
cl <- makeCluster(all_cores)
plan(future::cluster, workers = cl)

df <- read_rds("data/split_raw.RDS")
df_train <- df %>% training()
df_test <- df %>% testing()

cv_splits <- vfold_cv(df$data, 3, 8, Exited)

rand_forest_model <- rand_forest("classification", tune(), 1000, 4) %>% 
  set_engine("ranger", num.threads = 1, replace = T, sample.fraction = 0.9)

rand_forest_params <- rand_forest_model %>% 
  parameters() %>% 
  update(mtry = mtry(c(2L,3L)))

rand_forest_wf <- workflow() %>% 
  add_model(rand_forest_model) %>% 
  add_formula(Exited ~ .)

search_res2 <- rand_forest_wf %>% 
  tune_grid(
    resamples = cv_splits,
    param_info = rand_forest_params,
    metrics = metric_set(accuracy, mn_log_loss, roc_auc)
  )

search_res %>% autoplot(metric = "accuracy")
search_res %>% autoplot(metric = "roc_auc")
show_best(search_res, metric = "roc_auc")

search_res2 %>% autoplot()
search_res2 %>% autoplot(metric = "roc_auc")
show_best(search_res, metric = "roc_auc")

search_res2 %>% collect_metrics()

#### mtry = 3 najlepsze

###### tune sample fraction

rf_model1 <- rand_forest("classification", 3, 1000, 4) %>% 
  set_engine("ranger", num.threads = 8, replace = T, sample.fraction = 0.8) %>% 
  fit(Exited ~ ., data = df_train)

df_with_res <- bind_cols(
  df_test,
  rf_model1 %>% predict(df_test, type = "class"),
  rf_model1 %>% predict(df_test, type = "prob")
)

df_with_res %>% 
  metrics(Exited, .pred_class, .pred_No) %>% 
  kable("markdown")

df_metrics <- df_with_res %>% 
  metrics(Exited, .pred_class, .pred_No)

df_metrics %>% pull(.estimate) %>% set_names(df_metrics$.metric) %>% as_tibble_row()

######

sf_results2 <- seq(0.7,0.95, by = 0.025) %>%  map_dfr(function(sf) {
  rf_model1 <- rand_forest("classification", 3, 1000, 4) %>% 
    set_engine("ranger", num.threads = 8, replace = T, sample.fraction = sf) %>% 
    fit(Exited ~ ., data = df_train)
  
  df_with_res <- bind_cols(
    df_test,
    rf_model1 %>% predict(df_test, type = "class"),
    rf_model1 %>% predict(df_test, type = "prob")
  )
  
  df_metrics <- df_with_res %>% 
    metrics(Exited, .pred_class, .pred_No)
  
  df_metrics %>% 
    pull(.estimate) %>% 
    set_names(df_metrics$.metric) %>% 
    as_tibble_row() %>% 
    return
})
