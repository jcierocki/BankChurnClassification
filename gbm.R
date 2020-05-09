##### XGBoost GBM model

library(tidyverse)
library(stringr)
library(tidymodels)
library(xgboost)
library(vip)

rm(list = ls())

# source("dataset_prep.R")

dataset_split <- readRDS("data/split.RDS")
dataset_split$data <-  dataset_split$data %>% 
  mutate_if(~ length(levels(.x)) > 3, as.integer) %>% 
  mutate_at(vars(Balance), as.integer)

df_train <- dataset_split %>% training()
df_test <- dataset_split %>% testing()

gbm_model_1 <- boost_tree(mode = "classification",
                          mtry = 2,
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

df_pred_probs <- gbm_model_1 %>% 
  predict(df_test, type = "prob") %>% 
  bind_cols(df_test)

df_pred_probs %>% roc_auc(Exited, .pred_No)
df_pred_probs %>% roc_curve(Exited, .pred_No) %>% autoplot()

vi(gbm_model_1)
vip(gbm_model_1)
