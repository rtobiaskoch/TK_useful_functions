mod_check = function(mod) {
  
  #extract the name of the model
  mod_nm = deparse(substitute(mod))
  
  t = list(
    "check_model" = check_model(mod),
    "collinearity" = check_collinearity(mod),
    "distribution" = check_distribution(mod),
    "dispersion" = check_overdispersion(mod),
    "zeroinflation" = check_zeroinflation(mod),
    #"heteroskedasticity" = check_heteroskedasticity(mod),
    "aurocorrelation" = check_autocorrelation(mod)
    
  )
  assign(paste0(mod_nm, "_checks"), t, envir = globalenv())
  
}