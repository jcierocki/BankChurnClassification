#### work file 1

library(tidyverse)
library(stringr)
library(ggplot2)
library(GGally)
library(scorecard)
library(funModeling)
# library(caret)
# library(ranger)

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

data2 <- data1 %>% mutate(Age_bin2 = discretize_rgr(Age, Exited, max_n_bins = 6))
data2 <- data2 %>% woebin_ply(opt_bin, to = "bin") %>% mutate(Age_bin = as.factor(Age_bin))

# opt_bin2 <- discretize_get_bins(data1, 4, input = "Age")

head(data2$Age_bin2)
plot(data2$Age_bin)
plot(data2$Age_bin2)

iv(data2, y = "Exited", positive = "No")

#### łączenie zmiennych w celu uzyskania zbliżonej liczby kategorii (nieobciążone oceny ważności parametrów i IV)

# dodałem funkcję w pliku funs.R
