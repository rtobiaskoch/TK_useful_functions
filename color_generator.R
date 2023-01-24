#Color Generator

color_generator = function(names) {
  
}
n <- length(names)

#generate colors
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = sample(col_vector,n)
pie(rep(1,n), col=sample(col_vector, n), labels = unique(c_cmplt_mdata_ds$Country))

#combine color df to match nextstrain format
data.frame("name" = names,
            "col" = col_vector)