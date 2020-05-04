#### auxilary functions

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