#### data load and preprocessing

library(tidyverse)
library(tidymodels)
library(stringr)
library(scorecard)
library(recipes)

rm(list = ls())

source("funs_preproc.R")

data_raw <- read_csv("data/dataset1.csv")
data1 <- data_raw %>% 
  mutate(Geography = factor(Geography), Gender = factor(Gender), 
         Exited = factor(Exited) %>% `levels<-`(c("No", "Yes")), 
         IsActiveMember = factor(IsActiveMember) %>% `levels<-`(c("No", "Yes")),
         HasCrCard = factor(HasCrCard) %>% `levels<-`(c("No", "Yes"))) %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname)

data2 <- recipe(Exited ~ ., data = data1) %>% 
  step_dummy(Geography) %>% 
  prep %>% bake(new_data = data1)

changed_cols_idx <- data2 %>% colnames %>% str_split("_") %>% map_lgl(~ .x[1] == "Geography")
changed_cols <- colnames(data2)[changed_cols_idx]
data2 <- data2 %>% 
  mutate_at(changed_cols, ~ as.factor(.x) %>% `levels<-`(c("No", "Yes"))) %>% 
  rename_at(changed_cols, ~ str_remove(.x, "_"))

data2 %>% filter_vars_by_iv(significance_thres = 0.01) %>%
  initial_split(prop = 0.75) %>%
  write_rds("data/split_raw.RDS", compress = "gz2")

data3 <- data2 %>% 
  factorize(bin_methods = "tree") %>% 
  as_tibble() %>% 
  filter_vars_by_iv(significance_thres = 0.01)

dataset_split <- data3 %>% initial_split(prop = 0.75) %>% write_rds("data/split.RDS", compress = "gz2")

rm(list = ls())
