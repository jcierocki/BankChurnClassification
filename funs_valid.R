#### validation and visualisation automating functions

get_all_iv <- function(..., y_name = "Exited", pos = "No") {
  ivs_list <- list(...) %>% map(~ iv(.x, y = y_name, positive = pos) %>% as_tibble)
  
  ivs_list %>% 
    reduce(~ full_join(.x, .y, by = ("variable"))) %>% 
    `colnames<-`(c("Klasyfikator", str_c("Model ", 1:length(ivs_list)))) %>% 
    return
}

get_all_metrics <- function(pred_dfs, spec = 1L, y_name = "Exited") {
  metrics_list <- pred_dfs %>% 
    map(~ .x[[1]]) %>%
    map(~ metrics(.x, y_name, .pred_class, .pred_No) %>% select(-2))
  
  metrics_list %>% 
    reduce(~ full_join(.x, .y, by = (".metric"))) %>% 
    `colnames<-`(c("Metryka", str_c("Model ", 1:length(metrics_list)))) %>% 
    return
}

exportable_conf_matrix <- function(df) {
  conf_mat(df, Exited, .pred_class)$table
}