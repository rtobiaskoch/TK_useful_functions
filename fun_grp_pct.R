fun_pct = function(df, var1, var2) {
  df %>%
    group_by({{var1}}, {{var2}}) %>%
    count() %>%
    group_by(result) %>%
    mutate(n2 = sum(n)) %>%
    mutate(pct = n/n2) %>%
    ungroup()
  
}