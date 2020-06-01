#### work file 1

library(tidyverse)
library(stringr)
library(scorecard)

rm(list = ls())

df <- read_rds("data/split.RDS")$data

source("funs_valid.R")
source("funs_preproc.R")

plot_multi_freq(df, c("Age", "Balance", "NumOfProducts"))

plot_freq(df, "Age")
