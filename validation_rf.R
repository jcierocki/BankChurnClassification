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
all_metrics %>%
  dplyr::select(-2) %>% 
  kable(format = "html") %>% 
  save_kable("figures/metrics.png")

pred_dfs[[1]][[1]] %>% 
  exportable_conf_matrix %>% 
  kable(format = "html") %>% 
  save_kable("figures/conf_matrix1.png")

pred_dfs[[2]][[1]] %>% 
  exportable_conf_matrix %>% 
  kable(format = "html") %>% 
  save_kable("figures/conf_matrix2.png")

roc_1 <- pred_dfs[[1]][[1]] %>% 
  roc_curve(Exited, .pred_No) %>% 
  autoplot()

roc_2 <- pred_dfs[[2]][[1]] %>% 
  roc_curve(Exited, .pred_No) %>% 
  autoplot()

ggsave("figures/roc_1.png", roc_1)
ggsave("figures/roc_2.png", roc_2)

vip_1 <- vip(fitted_models[[1]][[1]])
vip_2 <- vip(fitted_models[[2]][[1]])

ggsave("figures/vip_1.png", vip_1)
ggsave("figures/vip_2.png", vip_2)
