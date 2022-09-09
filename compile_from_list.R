#PURPOSE OF SCRIPT
#compiles data by list of file name patterns and file type
#creates column for tsv and csv files with the file name to allow for data identification
#NOTE THAT IT WILL CONVERT ALL DATA TYPES INTO CHARACTERS FOR EASY MERGING

compile_by_list = function(path = getwd(), list, type) {
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #load in dependencies
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !require(pacman) 
  if (!require("pacman")) install.packages("pacman") #installs packman if you don't have it. pacman is a package manager
  pacman::p_load(purrr, dplyr, readr)

  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # GET PATHS
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  pattern = paste0(list, ".+", type) # combines list with file type to only look for files that match pattern and have matching file type ending
 
  #get path to files that match pattern of names in your list 
  fn_list = pattern %>% 
    map(~ list.files(path = path, #path will be current working directory if left blank
                     pattern = ., # "." is a place holder for file_list
                     recursive = T, #this means it will work in all subdirectories
                     full.names = T)) #this mean it will return the filename that matched the pattern as well as the relative path to get there
  
  if(any(fn_list == "character(0)")) {
    print("some files from list were not found in the path provided")
  }
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #TSV FILE READ
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  if(type == "tsv") {
    #read in file paths as list of dataframes
    df_list = fn_list %>%
      map(~ read_tsv(.) %>%
            mutate_all(as.character) #mutating everything as character ensures that rbind will work
      )                              #if you don't want this behavior modify this line
    #take list of data frames and combine into one 
    df = map2(df_list, list, #map2 is runs through two lists to perform loop. .x is the first argument and .y is the second
         ~mutate(.x, id = .y)) %>%
      map_dfr(rbind)
    
    return(df)
  }

  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #CSV FILE READ
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  if(type == "csv") {
    #read in files as list of dataframes
    df_list = fn_list %>%
      map(~ read_csv(.) %>%
            mutate_all(as.character) #mutating everything as character ensures that rbind will work
      )
    #new df
    df = map2(df_list, list, #map2 is runs through two lists to perform loop. .x is the first argument and .y is the second
         ~mutate(.x, id = .y)) %>%
      map_dfr(rbind)
    
    return(df)
  }
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #FASTA FILE READ
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  if(type == "fasta") {
    #read in files as list of dataframes
    fasta_list = fn_list %>%
      map(~ read_fasta(.))
    
   return(c(fasta_list))

  }
  

}
