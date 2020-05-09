#### auxilary functions concerning data preprocessing linked visualization

plot_freq <- function(df, fac_var, target = "Exited") {
  target_vec <- df %>% pull(target)
  
  count_frame <- as.data.frame(table(df[,fac_var])) %>% as_tibble() %>%
    rename(TotalFreq = Freq) %>% 
    add_column(ExitedFreq = as.integer(table(df[target_vec == "Yes", fac_var]))) %>% 
    mutate_at(vars(TotalFreq, ExitedFreq), ~ .x / 100)  %>% 
    column_to_rownames(var = "Var1")
  
  print(count_frame)
  multi <- max(count_frame$TotalFreq)/max(count_frame$ExitedFreq)
  
  ggplot(count_frame) + 
    geom_bar(aes(x = rownames(count_frame), y = TotalFreq), stat = "identity", color = "black", fill = "grey") +
    geom_line(aes(x = 1:nrow(count_frame), y = ExitedFreq * multi), color = "red", lwd = 2) + 
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / multi)) +
    xlab("Country")
}

merge_factor_vars <- function(var1, ...) {
  UseMethod("merge_factor_vars", var1)
}

merge_factor_vars.factor <- function(var1, ...) {
  factor(str_c(var1, ...))
}

merge_factor_vars.tbl <- function(var1, ...) {
  do.call(function(...) factor(str_c(...)), as.list(var1))
}

choose_best_binning <- function(binnings_df) {
  binnings_df %>% pmap(function(...) {
    opts <- list(...)
    best_iv_idx <- opts %>% map_dbl(~ .x$total_iv[1]) %>% which.max()

    opts[[best_iv_idx]]
  }) %>% return
}

factorize <- function(df, y_name = "Exited", y_pos = "No", bin_limit = 6, bin_methods = c("tree", "chimerge")) {
  fct_cols <- colnames(df)[(df %>% map_lgl(~ !is.factor(.x))) & colnames(df) != y_name]
  binnings <- bin_methods %>% 
    map(~ df %>% woebin(y = y_name, x = fct_cols, positive = y_pos, bin_num_limit = bin_limit, method = .x)) %>% 
    `names<-`(bin_methods) %>% 
    as_tibble()
  
  bins_best <- choose_best_binning(binnings)
  
  df %>% woebin_ply(bins = bins_best, to = "bin") %>% 
    mutate_if(~ !is.factor(.x), as.factor) %>% 
    rename_all(function(x) str_split(x, "_") %>% map_chr(~ .x[1])) %>% 
    return
}

filter_vars_by_iv <- function(df, significance_thres = 0.02, y_name = "Exited", y_pos = "No") {
  non_significant_vars <- df %>% 
    iv(y_name, positive = y_pos) %>% 
    filter(info_value < significance_thres) %>% 
    pull(variable)
  
  df %>% dplyr::select(-all_of(non_significant_vars)) %>% return
}