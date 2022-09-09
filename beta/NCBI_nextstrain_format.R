#NEXTSTRAIN FORMATTING FUNCTION FOR TEST
#converts NCBI data to match desired Nextstrain formatting

ns_format_fun = function(data, col = NULL){
  if(is.null(col)) {
    col = c("Accession", "Collection_Date", "Country",
            "Latitude", "Longitude")       #selecting only columns that will go in the build
  }
  
  set.seed(1)
  test = 
    data %>%
    select(all_of(col)) %>%
    rename(accession = Accession,
           country = Country,
           date = Collection_Date) %>%
    mutate(strain = accession,
           state = paste0(country,"/unknown"),
           date = as.character(date),
           date = case_when(
                            str_count(date) == 4 ~ paste0(date, "-XX-XX"), #when year add XX-XX
                            str_count(date) == 7 ~ paste0(date, "-XX"),    #when month add -XX
                            str_count(date) == 10 ~ date                   #when day do nothing
                                      ),
           date = as.Date(date)
           ) 
  
  #%>% of colors are wanted for the output
  # bind_cols(head(colors, n))
  
  
  t.s = test %>% 
    select(-country) %>%
    mutate(type = "state")
  
  t.c = test %>%
    select(-state) %>%
    mutate(type = "country")
  
  test2 = bind_rows(t.c, t.s) %>%
    mutate(country = if_else(is.na(country),
                             state,
                             country)) %>%
    distinct(country, .keep_all = T) %>%
    select(-state)
  
  geo = test2 %>%
    select(type, country, Latitude, Longitude)
  
  #colr = test2 %>%
  #  select(country, colors, type)
  
  test_fasta = filter_fasta(c_fasta, test$accession)
  
  #final select for metadata
  test_final = test %>%
    select(strain, accession, date, country, state)
  
  #output
  write_tsv(test_final, "test/metadata.tsv")
  
  #write_tsv(colr, "test/colors.tsv", col_names = F)
  
  write_tsv(geo, "test/lat_longs.tsv", col_names = F)
  
  
  write.fasta(sequences = test_fasta, 
              names = names(test_fasta),
              file.out = "test/sequences.fasta")
  
  rm(t.s, t.c)
  
}



