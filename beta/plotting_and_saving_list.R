df_list = df%>%
  group_by(id) %>%
  group_split()


plot_list = list()

for(x in seq_along(id_list)){
  
plot_list[[x]] <-
  ggplot(df_list[[x]], aes(x = new_date, y = n, fill = vaccinated, color = vaccinated)) +
    geom_col(alpha=0.8) +
    facet_grid(rows = vars(age_group),
               cols = vars(location)) +
    theme_minimal() +
    ggtitle(cluster_nm[[x]]) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
}

for(x in seq_along(cluster_df)) {
  
  ggsave(plot= cluster_list[[x]], file=paste0(dir, cluster_nm[[x]], "_","cluster_traits_", date, ".pdf"), width=11, height=8, units="in")
}
