
sum_stats = function(df){
  df  %>%
    select_if(function(x) is.numeric(x) & !is.integer(x)) %>%
    gather(factor_key = T) %>%
    group_by(key) %>%
    summarise(mean= mean(value, na.rm = T), 
              sd= sd(value, na.rm = T), 
              max = max(value, na.rm = T), 
              min = min(value, na.rm = T)) %>%
    mutate_if(is.numeric, ~round(., 3))
  
}


