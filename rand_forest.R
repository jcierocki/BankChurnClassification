##### ranger random forest model
  
library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)
library(vip)

rm(list = ls())
  
# source("dataset_prep.R")

dataset_splits <- list(
  read_rds("data/split.RDS"),
  read_rds("data/split_raw.RDS")
  )

testing_sets <- dataset_splits %>% map(~ .x %>% testing())

models_specs <- list(
  rand_forest("classification", 2, 1000, 5) %>% 
    # set_engine("ranger", num.threads = 8, replace = F, sample.fraction = 0.8, importance = "impurity") %>%
    set_engine("ranger", num.threads = 8, replace = F, sample.fraction = 0.8, importance = "permutation", local.importance = T)
  )

spec_names <- str_c("model_", 1:length(dataset_splits))
fitted_models <- dataset_splits %>% 
  map(~ .x %>% training()) %>% 
  map2_dfc(spec_names, function(df, col_name) {
    tibble(!!col_name := models_specs %>% map(~ .x %>%  fit(Exited ~ ., data = df)))
  })
  
pred_dfs <- list(fitted_models, testing_sets, spec_names) %>% pmap_dfc(function(models_by_spec, df, spec_name) {
  tibble(!!spec_name := 
           models_by_spec %>% map(function(model) {
             df %>% bind_cols(
               model %>% predict(df),
               model %>% predict(df, type = "prob")
             )
           }))
})

fitted_models %>% write_rds("data/fitted_models.RDS", compress = "bz2")
pred_dfs %>% write_rds("data/predictions.RDS", compress = "bz2")
