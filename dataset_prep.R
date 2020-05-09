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

data2 <- data1 %>% factorize() %>% as_tibble() %>% filter_vars_by_iv()

dataset_split <- initial_split(data2, prop = 0.75) %>% saveRDS("data/split.RDS")

rm(list = ls())


