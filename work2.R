##### work file 2 - tidymodels + ranger

library(tidyverse)
library(stringr)
library(tidymodels)
library(ranger)

source("main.R")
rm(list = ls()[ls() != "data2"])
