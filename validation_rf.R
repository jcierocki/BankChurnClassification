#### walidation

library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)
library(vip)
library(knitr)
library(kableExtra)

rm(list = ls())

fitted_models <- read_rds("data/fitted_models.RDS")
pred_dfs <- read_rds("data/predictions.RDS")

source("funs_valid.R")

all_metrics <- get_all_metrics(pred_dfs)

pred_dfs[[1]][[1]] %>% exportable_conf_matrix %>% kable(format = "markdown")

# df_pred_probs1 %>% roc_curve(Exited, .pred_No) %>% autoplot()
# df_pred_probs2 %>% roc_curve(Exited, .pred_No) %>% autoplot()
# 
# vi(ranger_model_1)
# vi(ranger_model_2)
# 
# vip(ranger_model_1)
# vip(ranger_model_2)