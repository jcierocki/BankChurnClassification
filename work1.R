#### work file 1

library(tidyverse)
library(stringr)
library(ggplot2)
library(GGally)
library(scorecard)
library(caret)
library(ranger)


# ggplot(data1, aes(x = Age)) + 
#   geom_histogram(bins = 50, color = "black", fill = "grey")
# 
# ggplot(data1, aes(x = CreditScore, y = Age)) +
#   geom_point() +
#   facet_grid(rows = vars(Exited))
# 
# ggpairs(data1[,c("Age", "Exited")])
# 
# ggplot(data1, aes(x = Exited, y = Age)) + geom_violin()
# 
# plot_freq(data1, "Geography")
# data1 <- data1 %>% mutate(NotSpain = as.factor(map_chr(Geography, ~ ifelse(.x == "Spain", "No", "Yes"))))
# plot_freq(data1, "NotSpain")
# 
# iv(data1, "Exited", "Geography", positive = "No")
# iv(data1, "Exited", "NotSpain", positive = "No")


