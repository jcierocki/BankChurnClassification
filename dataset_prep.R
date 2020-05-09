#### data load and preprocessing

library(tidyverse)
library(tidymodels)
library(stringr)
library(scorecard)

rm(list = ls())

source("funs_preproc.R")

data_raw <- read_csv("data/dataset1.csv")
data1 <- data_raw %>% 
  mutate(Geography = factor(Geography), Gender = factor(Gender), 
         Exited = factor(Exited) %>% `levels<-`(c("No", "Yes")), 
         IsActiveMember = factor(IsActiveMember) %>% `levels<-`(c("No", "Yes")),
         HasCrCard = factor(HasCrCard) %>% `levels<-`(c("No", "Yes"))) %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname)

data1 %>% filter_vars_by_iv(significance_thres = 0.02) %>% 
  initial_split(prop = 0.75) %>%
  saveRDS("data/split_raw.RDS")

data2 <- data1 %>% 
  factorize(bin_methods = "tree") %>% 
  as_tibble() %>% 
  filter_vars_by_iv(significance_thres = 0.02)

dataset_split <- initial_split(data2, prop = 0.75) %>% saveRDS("data/split.RDS")

rm(list = ls())
