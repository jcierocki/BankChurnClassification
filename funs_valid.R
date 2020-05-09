#### validation and visualisation automating functions

get_all_metrics <- function(pred_dfs, spec = 1L) {
  f <- function(df) metrics(df, Exited, .pred_class, .pred_No)
  model1_metrics <- f(pred_dfs[[1]][[spec]])
  
  all_metrics <- do.call(
    function(...) bind_cols(model1_metrics, ...), 
    pred_dfs %>% select(-1) %>% map(~ f(.x[[spec]])$.estimate)) %>%
    rename(model_1 = .estimate) %>% 
    return
}

exportable_conf_matrix <- function(df) {
  conf_mat(df, Exited, .pred_class)$table
}