#this function will iteratively add goodness of fit statistics 
#from a model to a dataframe that you name

mod_fit = function(x, mod) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(dplyr, broom)
  
  x_nm = deparse(substitute(x)) #get object as a string
  mod_nm = deparse(substitute(mod))
  
  #if the x object to write to doesn't exist create one
  if(exists(x_nm)){
    glance(mod) %>%
      mutate(model = mod_nm) %>%
      rbind(x) %>%
      arrange(AIC) %>%
      distinct_all(.) #incase a model is run and added twice it will remove it
  } else {
    glance(mod) %>%
      mutate(model = mod_nm)
  } 
  
}