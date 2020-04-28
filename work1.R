#### work file 1

library(tidyverse)
library(stringr)
library(ggplot2)
library(GGally)
library(scorecard)
library(caret)
library(ranger)

rm(list = ls())

source("funs.R")

data_raw <- read_csv("data/dataset1.csv")
data1 <- data_raw %>% 
  mutate(Geography = factor(Geography), Gender = factor(Gender), 
         Exited = factor(Exited) %>% `levels<-`(c("No", "Yes")), 
         IsActiveMember = factor(IsActiveMember) %>% `levels<-`(c("No", "Yes")),
         HasCrCard = factor(HasCrCard) %>% `levels<-`(c("No", "Yes"))) %>% 
  dplyr::select(-RowNumber, -CustomerId, -Surname)

ggplot(data1, aes(x = Age)) + 
  geom_histogram(bins = 50, color = "black", fill = "grey")

ggplot(data1, aes(x = CreditScore, y = Age)) +
  geom_point() +
  facet_grid(rows = vars(Exited))

ggpairs(data1[,c("Age", "Exited")])

ggplot(data1, aes(x = Exited, y = Age)) + geom_violin()

plot_freq(data1, "Geography")
data1 <- data1 %>% mutate(NotSpain = as.factor(map_chr(Geography, ~ ifelse(.x == "Spain", "No", "Yes"))))
plot_freq(data1, "NotSpain")

iv(data1, "Exited", "Geography", positive = "No")
iv(data1, "Exited", "NotSpain", positive = "No")

opt_bin <- woebin(data1, "Exited", "Age", positive = "No")#[[1]]$breaks
opt_bin2 <- woebin(data1,"Exited","Balance",positive = "No")
opt_bin3 <- woebin(data1,"Exited","CreditScore",positive = "No")
opt_bin4 <- woebin(data1,"Exited","NumOfProducts",positive = "No")
opt_bin5 <- woebin(data1,"Exited","EstimatedSalary",positive = "No")
opt_bin6 <- woebin(data1,"Exited","Tenure",positive = "No")
data2 <- data1 %>% woebin_ply(opt_bin, to = "bin") %>% mutate(Age_bin =as.factor(Age_bin)) 
data2 <- data2 %>% woebin_ply(opt_bin2, to = "bin") %>% mutate(Balance_bin = as.factor(Balance_bin)) 
data2 <- data2 %>% woebin_ply(opt_bin3, to = "bin") %>% mutate(CreditScore_bin = as.factor(CreditScore_bin)) 
data2 <- data2 %>% woebin_ply(opt_bin4, to = "bin") %>% mutate(NumOfProducts_bin = as.factor(NumOfProducts_bin)) 
data2 <- data2 %>% woebin_ply(opt_bin5, to = "bin") %>% mutate(EstimatedSalary_bin = as.factor(EstimatedSalary_bin)) 
data2 <- data2 %>% woebin_ply(opt_bin6, to = "bin") %>% mutate(Tenure_bin =as.factor(Tenure_bin))