##### XGBoost GBM model

library(tidyverse)
library(stringr)
library(tidymodels)
library(xgboost)
library(vip)

rm(list = ls())

# source("dataset_prep.R")

dataset_split <- readRDS("data/split.RDS")
df_train <- dataset_split %>% training()
df_test <- dataset_split %>% testing()

