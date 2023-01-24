tk_clean_names = function(x) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(stringr)
  
  if(!is.character(x)) {
    "x must be a character"
  } else {
    x = str_to_lower(x)
    x = str_remove(x, "[[:punct:]]")
    x = str_trim(x)
    x = str_replace(x, " ", "_")
    return(x)
  }
  
}