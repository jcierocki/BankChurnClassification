##### ranger random forest model
  
library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)
library(vip)

rm(list = ls())
  
source("funs_valid.R")

dataset_splits <- list(
  read_rds("data/split.RDS"),
  read_rds("data/split_raw.RDS")
  )

testing_sets <- dataset_splits %>% map(~ .x %>% testing())

models_specs <- list(
  rand_forest("classification", 3, 1000, 5) %>% 
    # set_engine("ranger", num.threads = 8, replace = F, sample.fraction = 0.8, importance = "impurity") %>%
    set_engine("ranger", num.threads = 8, replace = T, sample.fraction = 0.7, importance = "permutation", local.importance = T)
  )

spec_names <- str_c("model_", 1:length(dataset_splits))
fitted_models <- dataset_splits %>% 
  map(~ .x %>% training()) %>% 
  map2_dfc(spec_names, function(df, col_name) {
    tibble(!!col_name := models_specs %>% map(~ .x %>%  fit(Exited ~ ., data = df)))
  })
  
pred_dfs <- predict_and_bind(fitted_models, testing_sets, spec_names)

# fitted_models %>% write_rds("data/fitted_models.RDS", compress = "bz2")
# pred_dfs %>% write_rds("data/predictions.RDS", compress = "bz2")
