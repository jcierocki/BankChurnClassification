#### auxilary functions concerning data preprocessing linked visualization

calc_count_frame <- function(df, fac_var, target) {
  target_vec <- df %>% pull(target)
  
  as.data.frame(table(df[,fac_var])) %>% as_tibble() %>%
    rename(TotalFreq = Freq, Cathegory = Var1) %>% 
    add_column(ExitedFreq = as.integer(table(df[target_vec == "Yes", fac_var]))) %>% 
    mutate_at(vars(TotalFreq, ExitedFreq), ~ .x / 100)
}

plot_freq <- function(df, fac_var, target = "Exited", axis2_lab = "Odsetek churnu na kategorię (%)") {
  count_frame <- calc_count_frame(df, fac_var, target)
  multi <- max(count_frame$TotalFreq)/max(count_frame$ExitedFreq)
  
  ggplot(count_frame) + 
    geom_bar(aes(x = Cathegory, y = TotalFreq), stat = "identity", color = "grey30", fill = "#56B4E9") +
    geom_line(aes(x = 1:nrow(count_frame), y = ExitedFreq * multi), color = "firebrick2", lwd = 2) + 
    scale_y_continuous(sec.axis = sec_axis(name = axis2_lab, trans = ~ . / multi)) +
    labs(x = fac_var, y = "Odsetek instancji o danej kategorii (%)")
}

plot_multi_freq <- function(df, var_vec, target = "Exited", axis2_lab = "Odsetek churnu na kategorię (%)") {
  df_count_list <- var_vec %>% 
    map(~ calc_count_frame(df, .x, target) %>% mutate(Zmienna = .x))
  
  multi <- df_count_list %>% 
    map_dbl(~ max(.x$TotalFreq)/max(.x$ExitedFreq)) %>% 
    max
  
  df_full <- do.call(bind_rows, df_count_list)
  
  df_full %>% 
    ggplot(aes(x = levels(Cathegory), y = TotalFreq, group = 1)) +
      geom_col(color = "grey30", fill = "#56B4E9") +
      geom_line(aes(y = ExitedFreq * multi), color = "firebrick2", lwd = 1.3) +
      scale_y_continuous(sec.axis = sec_axis(~ . / multi, axis2_lab)) +
      facet_wrap(~ Zmienna, nrow = 1, scales = "free_x") + 
      labs(x = "Kategorie", y = "Odsetek instancji o danej kategorii (%)") +
      theme(axis.text=element_text(size=6.5))
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