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

predict_and_bind <- function(fitted_models, testing_sets, spec_names) {
  UseMethod("predict_and_bind", fitted_models)
}

predict_and_bind.tbl_df <- function(fitted_models, testing_sets, spec_names) {
  list(fitted_models, testing_sets, spec_names) %>% pmap_dfc(function(models_by_spec, df, spec_name) {
    tibble(!!spec_name := 
             models_by_spec %>% map(function(model) {
               df %>% bind_cols(
                 model %>% predict(df),
                 model %>% predict(df, type = "prob")
               )
             }))
  })
}

predict_and_bind.list <- function(fitted_models, testing_sets, spec_names) {
  map2(fitted_models, testing_sets, function(model, df) {
    df %>% bind_cols(
      model %>% predict(df),
      model %>% predict(df, type = "prob")
    )
  }) %>% set_names(spec_names)
}
