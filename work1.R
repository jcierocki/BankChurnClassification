#### work file 1

library(tidyverse)
library(stringr)
library(scorecard)
library(tidymodels)

rm(list = ls())

source("funs_valid.R")
source("funs_preproc.R")

# df <- read_rds("data/split.RDS")$data
# plot_multi_freq(df, c("Age", "Balance", "NumOfProducts"))
# 
# plot_freq(df, "Age")

tree_model_full <- read_rds("data/tree_model1.rds")
tree_model_simple <- read_rds("data/tree_model2.rds")
ranger_models <- read_rds("data/fitted_models.RDS") %>% as.list()
xgboost_model <- read_rds("data/fitted_xgboost.RDS")
df_1 <- read_rds("data/split.RDS")
df_2 <- read_rds("data/split_raw.RDS")

models <- list(tree = tree_model_full, forest1 = ranger_models$model_1[[1]], 
            forest2 = ranger_models$model_2[[1]], gbm = xgboost_model)

test_dfs <- list(tree = df_2, forest1 = df_1, forest2 = df_2, gbm = df_1) %>% map(~ testing(.x))

test_dfs$gbm <- test_dfs$gbm %>%
  mutate_if(~ length(levels(.x)) > 2, as.integer) %>%
  mutate_at(vars(Balance), as.integer)

spec_names <- c("Drzewo", "Las (uproszczone)", "Las (surowe)", "XGBoost")

predict_dfs <- predict_and_bind(models, test_dfs, spec_names)

roc_list <- predict_dfs %>% map2(names(predict_dfs), ~ roc_curve(.x, Exited, .pred_No) %>% 
                                   mutate(.level = .y))

rlang::exec(bind_rows, !!!roc_list) %>% 
  mutate(.level = as.factor(.level)) %>% 
  autoplot()
