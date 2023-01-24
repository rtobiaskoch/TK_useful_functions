#this function will iteratively add the estimates and statistics of terms
#from a model to a dataframe that you name

mod_clean_compile = function(x, mod) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(dplyr, broom)
  
  x_nm = deparse(substitute(x)) #get object as a string
  mod_nm = deparse(substitute(mod))
  
  if(exists(x_nm)) {
    mod %>%
      broom::tidy(., conf.int = T, exponentiate = T) %>%
      # mutate_at(vars(estimate, std.error, conf.low, conf.high), ~ exp(.)) %>%
      mutate(sig = if_else(p.value < 0.05, 1, 0)) %>%
      mutate_if(is.numeric, round, 3) %>%
      mutate(model = mod_nm) %>%
      rbind(x) %>% #append to existing dataframe
      distinct_all(.) #remove any models you tried to run twice
  }else{
    mod %>%
      broom::tidy(., conf.int = T, exponentiate = T) %>%
      # mutate_at(vars(estimate, std.error, conf.low, conf.high), ~ exp(.)) %>%
      mutate(sig = if_else(p.value < 0.05, 1, 0)) %>%
      mutate_if(is.numeric, round, 3) %>%
      mutate(model = mod_nm)
  }
  
}