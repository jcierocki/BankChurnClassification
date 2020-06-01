#### validation

library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)
library(vip)
library(knitr)
library(kableExtra)
library(scorecard)

rm(list = ls())

source("funs_valid.R")
source("funs_preproc.R")

fitted_models <- read_rds("data/fitted_models.RDS")
pred_dfs <- read_rds("data/predictions.RDS")
df_1 <- read_rds("data/split.RDS")$data
df_2 <- read_rds("data/split_raw.RDS")$data

##########

## testy

plot_freq(df_1, "Age") +
  xlab("Wiek")

vip(fitted_models[[1]][[1]], aesthetics = list(fill = "#56B4E9", color = "grey30"))

##########

all_iv <- get_all_iv(df_1, df_2)
all_iv %>% kable("markdown")

# all_iv %>% kable("html") %>% save_kable("figures/ivs.png")

all_metrics <- get_all_metrics(pred_dfs)
all_metrics %>% kable("markdown")

# all_metrics %>% kable("html") %>% save_kable("figures/metrics.png")

##########

## tu filtrujemy wybraną specyfikację

preds <- pred_dfs %>% map(~ .x[[1]])

##########

conf_matrix_1 <- preds[[1]] %>% exportable_conf_matrix 
conf_matrix_1 %>% kable("markdown")
  
# conf_matrix_1 %>% kable("html") %>% save_kable("figures/conf_matrix1.png")

conf_matrix_2 <- preds[[2]] %>% exportable_conf_matrix 
conf_matrix_2 %>% kable("markdown")

# conf_matrix_2 %>% kable("html") %>% save_kable("figures/conf_matrix2.png")

roc_1 <- preds[[1]] %>% 
  roc_curve(Exited, .pred_No) %>% 
  autoplot()

roc_2 <- preds[[1]] %>% 
  roc_curve(Exited, .pred_No) %>% 
  autoplot()

roc_1
roc_2

# ggsave("figures/roc_1.png", roc_1)
# ggsave("figures/roc_2.png", roc_2)

vip_1 <- vip(fitted_models[[1]][[1]])
vip_2 <- vip(fitted_models[[2]][[1]])

vip_1
vip_2

# ggsave("figures/vip_1.png", vip_1)
# ggsave("figures/vip_2.png", vip_2)
