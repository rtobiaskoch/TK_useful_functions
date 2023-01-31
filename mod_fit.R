mod_fit = function(x, mod) {
  #x = the previously existing compiled model data
  #mod the model you want to add
  x_nm = deparse(substitute(x)) #get object as a string
  mod_nm = deparse(substitute(mod))
  formula = paste(mod$call$formula[2],mod$call$formula[1], mod$call$formula[3])
  
  #if the x object to write to doesn't exist create one
  if(exists(x_nm)){
    glance(mod) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%
      rbind(x) %>%
      arrange(AIC) %>%
      distinct_all(.) %>% #incase a model is run and added twice it will remove it
      dplyr::select(model,formula, everything())
  } else {
    glance(mod) %>%
      mutate(model = mod_nm) %>%
      mutate(formula = formula) %>%
      dplyr::select(model,formula, everything())
  } 
  
}