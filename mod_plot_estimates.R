#plots model estimate faceted by model

plot_estimates = function(mod){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(ggplot)
  
  if(is.data.frame(mod)) {
    p_mod_est = ggplot(mod, aes(x = term, y = estimate, color = term)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin=conf.low,ymax=conf.high,width=0.2)) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      theme_classic()+
      theme(axis.text.x = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, size = 7),
            legend.position = "none")+
      facet_grid(.~model)
  } else {
    print("mod must be a dataframe compiled using mod_clean function")
  }
  
}