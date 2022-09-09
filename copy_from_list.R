#COPY FILES FROM LIST

copy_by_list = function(path = getwd(), list, type) {
  
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
  
 fn_list %>% map(~ file.copy(from = ., to = getwd())
                 )

}
  
  
  
  