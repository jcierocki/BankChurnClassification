#### single tree

library(tidyverse)
library(tidymodels)
library(stringr)
library(rpart)
library(rpart.plot)
library(knitr)
library(tune)
library(doFuture)

rm(list = ls())

dataset_splits <- list(
  read_rds("data/split.RDS"),
  read_rds("data/split_raw.RDS")
)

df_train <- dataset_splits[[2]] %>% training()
df_test <- dataset_splits[[2]] %>% testing()

tree_model1 <- decision_tree("classification", cost_complexity = 0.00005, min_n = 5, tree_depth = 8) %>% 
  set_engine("rpart") %>% 
  fit(Exited ~ ., data = df_train)

tree_model2 <- decision_tree("classification", cost_complexity = 0.0005, min_n = 5, tree_depth = 5) %>% 
  set_engine("rpart") %>% 
  fit(Exited ~ ., data = df_train)

rpart.plot(tree_model1$fit, roundint = F)

rpart.plot(tree_model2$fit, roundint = F)

tree_model1 %>% write_rds("data/tree_model1.rds", compress = "gz")

tree_model2 %>% write_rds("data/tree_model2.rds", compress = "gz")

df_with_res <- bind_cols(
  df_test,
  tree_model1 %>% predict(df_test, type = "class"),
  tree_model1 %>% predict(df_test, type = "prob")
)

df_with_res %>% 
  metrics(Exited, .pred_class, .pred_No) %>% 
  kable("markdown")

df_with_res %>% 
  roc_curve(Exited, .pred_No) %>% 
  autoplot()

sum((as.integer(df_with_res$Exited) - as.integer(df_with_res$.pred_class))^2)

#### set all cores for grid tunning

all_cores <- parallel::detectCores(logical = FALSE) - 1

registerDoFuture()
cl <- makeCluster(all_cores)
plan(future::cluster, workers = cl)

#### tunning

cv_list <- dataset_splits[[2]]$data %>% vfold_cv(10, 5, Exited)

tree_model <- decision_tree("classification", cost_complexity = tune(), min_n = 5, tree_depth = tune()) %>% 
  set_engine("rpart")

# tree_param <- tree_model %>% 
#   parameters() %>% 
#   update(cost_complexity = cost_complexity(c(0.0005, 0.0015), NULL))

search_results <- tree_model %>% tune_grid(
  preprocessor = Exited ~ .,
  resamples = cv_list,
  grid = grid_regular(cost_complexity(c(0.00005, 0.0005), NULL),
                      tree_depth(c(5L, 10L)),
                      levels = 5),
  metrics = metric_set(accuracy, mn_log_loss, roc_auc)
  )

autoplot(search_results)

collect_metrics(search_results) %>% view

show_best(search_results, metric = "roc_auc", n = 25) %>% filter(tree_depth == 8)

select_best(search_results, metric = "roc_auc")
select_best(search_results, metric = "accuracy")

#### optymalne rozwiązanie 0.01
