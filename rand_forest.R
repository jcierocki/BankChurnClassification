##### ranger random forest model
  
library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)
library(vip)

rm(list = ls())
  
# source("dataset_prep.R")

dataset_split1 <- readRDS("data/split.RDS")
dataset_split2 <- readRDS("data/split_raw.RDS")

df_train1 <- dataset_split1 %>% training()
df_test1 <- dataset_split1 %>% testing()
df_train2 <- dataset_split2 %>% training()
df_test2 <- dataset_split2 %>% testing()
  
ranger_model_specs <- rand_forest("classification", 2, 1000, 5) %>% 
  # set_engine("ranger", num.threads = 8, replace = F, sample.fraction = 0.8, importance = "impurity") %>%
  set_engine("ranger", num.threads = 8, replace = F, sample.fraction = 0.8, importance = "permutation", local.importance = T)

ranger_model_1 <- ranger_model_specs %>% fit(Exited ~ ., data = df_train1)

ranger_model_2 <- ranger_model_specs %>% fit(Exited ~ ., data = df_train2)
  
df_pred1 <- ranger_model_1 %>% 
  predict(df_test1) %>% 
  bind_cols(df_test1)

df_pred2 <- ranger_model_2 %>% 
  predict(df_test2) %>% 
  bind_cols(df_test2)
  
df_pred1 %>% metrics(Exited, .pred_class)
df_pred2 %>% metrics(Exited, .pred_class)
  
df_pred_probs1 <- ranger_model_1 %>% 
  predict(df_test1, type = "prob") %>% 
  bind_cols(df_test1)

df_pred_probs2 <- ranger_model_2 %>% 
  predict(df_test2, type = "prob") %>% 
  bind_cols(df_test2)

df_pred_probs1 %>% roc_auc(Exited, .pred_No)
df_pred_probs2 %>% roc_auc(Exited, .pred_No)

df_pred_probs1 %>% roc_curve(Exited, .pred_No) %>% autoplot()
df_pred_probs2 %>% roc_curve(Exited, .pred_No) %>% autoplot()

vi(ranger_model_1)
vi(ranger_model_2)

vip(ranger_model_1)
vip(ranger_model_2)



