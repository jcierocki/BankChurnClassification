##### work file 2 - tidymodels + ranger

library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)

source("main.R")
rm(list = ls()[ls() != "data2"])

dataset_split <- initial_split(data2, prop = 0.75)
df_train <- dataset_split %>% training()
df_test <- dataset_split %>% testing()

ranger_model_1 <- rand_forest("classification", 2, 500, 5) %>% 
  set_engine("ranger", num.threads = 8, replace = F, sample.fraction = 0.8) %>%
  # set_engine("ranger", num.threads = 8) %>%
  fit(Exited ~ ., data = df_train)

df_pred <- ranger_model_1 %>% 
  predict(df_test) %>% 
  bind_cols(df_test)

df_pred %>% metrics(Exited, .pred_class)

df_pred_probs <- ranger_model_1 %>% 
  predict(df_test, type = "prob") %>% 
  bind_cols(df_test)

df_pred_probs %>% roc_curve(Exited, .pred_No) %>% autoplot()

