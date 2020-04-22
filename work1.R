/#### work file 1

library(tidyverse)
library(stringr)
library(ggplot2)
library(GGally)
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
#Kod smbinning - sprawdzone iv
install.packages("smbinning")
library("smbinning")
data1_dataframe <- as.data.frame(data1)
data1_dataframe$Exited<-as.numeric(data1$Exited)
data1_dataframe$Exited<-as.integer(data1_dataframe$Exited)-1
result <- smbinning(df=data1_dataframe,y="Exited",x="Age",p=0.05)
result #wynik dla age
result2 <- smbinning(df=data1_dataframe,y="Exited",x="CreditScore",p=0.05)
result2 # wynik dla creditscore
 