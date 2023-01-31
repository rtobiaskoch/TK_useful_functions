#function cleans data and iteratively adds data to a compiled dataframe for plotting

mod_clean = function(x, mod) {
  #x = the previously existing compiled model data
  #mod = the model you want to add
  x_nm = deparse(substitute(x)) #get object as a string
  mod_nm = deparse(substitute(mod))
  formula = paste(mod$call$formula[2],mod$call$formula[1], mod$call$formula[3])
  
  if(exists(x_nm)) {
    t = mod %>%
      
      broom::tidy(., conf.int = T, exponentiate = T) %>%
      mutate(sig = if_else(p.value < 0.05, 1, 0)) %>%
      mutate_if(is.numeric, round, 3) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%
      mutate(family = case_when(
        str_detect(model, "modp|mod_p|pois") ~ "pois",
        str_detect(model, "modnb|modnb|negbin") ~ "neg_bin",
        str_detect(model, "modlm|mod_lm") ~ "lm",
        TRUE ~ "unknown")
      ) %>%
      rbind(x) %>% #append to existing dataframe
      distinct_all(.) #remove any models you tried to run twice
    
  }else{
    mod %>%
      broom::tidy(., conf.int = T, exponentiate = T) %>%
      mutate(sig = if_else(p.value < 0.05, 1, 0)) %>%
      mutate_if(is.numeric, round, 3) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%      
      mutate(family = case_when(
        str_detect(model, "modp|mod_p|pois") ~ "pois",
        str_detect(model, "modnb|modnb|negbin") ~ "neg_bin",
        str_detect(model, "modlm|mod_lm") ~ "lm",
        TRUE ~ "unknown")
      )
    
  } 
  
}