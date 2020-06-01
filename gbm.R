##### XGBoost GBM model

library(tidyverse)
library(stringr)
library(tidymodels)
library(xgboost)
library(vip)
library(knitr)
library(kableExtra)

rm(list = ls())

source("funs_valid.R")
# source("dataset_prep.R")

dataset_split <- readRDS("data/split.RDS")
dataset_split$data <-  dataset_split$data %>%
  mutate_if(~ length(levels(.x)) > 3, as.integer) %>%
  mutate_at(vars(Balance), as.integer)

# dataset_split <- readRDS("data/split_raw.RDS")

df_train <- dataset_split %>% training()
df_test <- dataset_split %>% testing()

gbm_model_1 <- boost_tree(mode = "classification",
                          mtry = 3,
                          trees = 500,
                          min_n = 5,
                          # tree_depth = 5,
                          learn_rate = .1,
                          loss_reduction = 0,
                          sample_size = 0.7) %>% 
  set_engine("xgboost", objective = "binary:logistic") %>% 
  fit(Exited ~ ., data = df_train)

df_pred <- gbm_model_1 %>%
  predict(df_test) %>%
  bind_cols(df_test)

df_pred %>% metrics(Exited, .pred_class)

############################

df_pred <- gbm_model_1 %>% 
  predict(df_test, type = "prob") %>% 
  bind_cols(df_pred)

df_pred %>% 
  metrics(Exited, .pred_class, .pred_No) %>% 
  kable("html") %>% 
  save_kable("figures/metrics_gbm.png")

df_pred %>% 
  exportable_conf_matrix() %>% 
  kable("html") %>% 
  save_kable("figures/conf_matrix_gbm.png")

############################

gbm_model_1 %>% write_rds("data/fitted_xgboost.RDS", compress = "bz2")
df_pred %>% write_rds("data/predictions_xgboost.RDS", compress = "bz2")

############################

df_pred_probs <- gbm_model_1 %>% 
  predict(df_test, type = "prob") %>% 
  bind_cols(df_test)

# df_pred_probs %>% roc_auc(Exited, .pred_No)
df_pred_probs %>% roc_curve(Exited, .pred_No) %>% autoplot()
ggsave("figures/roc_gbm.png")

vi(gbm_model_1)
vip(gbm_model_1)
# vip(gbm_model_1, method = "permute", train = df_train, target = "Exited", metric = "accuracy", pred_wrapper = predict)

ggsave("figures/vip_gbm.png")
