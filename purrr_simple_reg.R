#this function compares a iterated simple regression across all variables to 
#note: as of 23-01-31 you need to change the model type if not doing neg binom

pacman::p_load(MASS, tidyverse, broom, AER)

purrr_lm = function(y, df) {
  
  uni = df %>% 
    dplyr::select(-y) %>%  # exclude outcome, leave only predictors 
    map(~glm.nb(df$y ~ .x, data = Affairs))
  
  t = uni %>%
    map_df(~tidy(.x, conf.int = T, exponentiate = T))
  
  t2 = uni %>%
    map("coefficients")
  
  t2 =  as.data.frame(unlist(t2)) %>%
    transmute(term = rownames(.),
              estimate = exp(`unlist(t2)`))
  
  rownames(t2) = NULL
  
  #merge by estimate
  uni = left_join(t,t2, by = "estimate") %>%
    filter(term.x != "(Intercept)") %>%
    mutate(term.x = term.y) %>%
    dplyr::select(-term.y) %>%
    mutate(term.x = str_remove(term.x, "\\.\\.x")) %>%
    rename(term = term.x)
  
  rm(t,t2)
  
  p_uni = ggplot(uni, aes(x = term, y = estimate)) +
    geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    theme_classic() +
    ggtitle("Univariate")
  
  #compare to multinomial
  full = glm.nb(df$y ~ ., data = df)
  
  full = tidy(full, conf.int = T, exponentiate = T) %>%
    filter(term != "(Intercept)")
  
  p_full = ggplot(full, aes(x = term, y = estimate)) +
    geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
    theme_classic() +
    ggtitle("Multivariate")
  
  data = list("uni" = uni, 
       "multivariate" = full,
        "plot" = ggpubr::ggarrange(p_uni, p_full))
  return(data)
  
}