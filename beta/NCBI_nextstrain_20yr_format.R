#NEXTSTRAIN FORMATTING FUNCTION
#converts NCBI data to match desired Nextstrain formatting

ns_format20_fun = function(data, col = NULL){
  if(is.null(col)) {
    col = c("Accession", "Collection_Date", "Country",       #selecting only columns that will go in the build
               "USA", "City", "Host", "lineage")
  }
  
  
  data %>%
    mutate(City = "unknown",                                  #adding city column to match the 20 yr WNV format
           lineage = if_else(str_count(Genotype) == 0,        #replacing blanks in genotype with fake WN00 so not blank
                             "WN00",
                             Genotype
           ),
           USA = if_else(str_count(USA) == 0,                #replaces blank with unknown to match the 20 yr WNV format
                         "unknown",
                         USA),
           Collection_Date = as.character(Collection_Date),
           Collection_Date = case_when(
             str_count(Collection_Date) == 4 ~ paste0(Collection_Date, "-XX-XX"), #when year add XX-XX
             str_count(Collection_Date) == 7 ~ paste0(Collection_Date, "-XX"),    #when month add -XX
             str_count(Collection_Date) == 10 ~ Collection_Date                   #when day do nothing
                                      ),
           Collection_Date = as.Date(Collection_Date)
    ) %>%
    select(all_of(col))
}



