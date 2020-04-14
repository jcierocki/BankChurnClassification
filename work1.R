#### work file 1

library(tidyverse)
library(stringr)

rm(list = ls())

data1 <- read_csv("data/dataset1.csv")
data1 <- data1 %>% 
  mutate(Geography = factor(Geography), Gender = factor(Gender)) %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname)
