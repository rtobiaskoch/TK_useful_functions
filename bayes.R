function (data = c(), prior_prop = c(1, 1), n_draws = 10000, 
          show_plot = TRUE) 
{
  data <- as.logical(data)
  proportion_success <- c(0, seq(0, 1, length.out = 100), 1)
  data_indices <- round(seq(0, length(data), length.out = min(length(data) + 
                                                                1, 20)))
  post_curves <- map_dfr(data_indices, function(i) {
    value <- ifelse(i == 0, "Prior", ifelse(data[i], "Success", 
                                            "Failure"))
    label <- paste0("n=", i)
    probability <- dbeta(proportion_success, prior_prop[1] + 
                           sum(data[seq_len(i)]), prior_prop[2] + sum(!data[seq_len(i)]))
    probability <- probability/max(probability)
    data_frame(value, label, proportion_success, probability)
  })
  post_curves$label <- fct_rev(factor(post_curves$label, levels = paste0("n=", 
                                                                         data_indices)))
  post_curves$value <- factor(post_curves$value, levels = c("Prior", 
                                                            "Success", "Failure"))
  p <- ggplot(post_curves, aes(x = proportion_success, y = label, 
                               height = probability, fill = value)) + geom_joy(stat = "identity", 
                                                                               color = "white", alpha = 0.8, panel_scaling = TRUE, size = 1) + 
    scale_y_discrete("", expand = c(0.01, 0)) + scale_x_continuous("Underlying proportion of success") + 
    scale_fill_manual(values = hcl(120 * 2:0 + 15, 100, 65), 
                      name = "", drop = FALSE, labels = c("Prior   ", "Success   ", 
                                                          "Failure   ")) + theme_light(base_size = 18) + 
    theme(legend.position = "top")
  if (show_plot) {
    print(p)
  }
  invisible(rbeta(n_draws, prior_prop[1] + sum(data), prior_prop[2] + 
                    sum(!data)))
}