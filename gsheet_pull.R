gsheet_pull = function(key, sheet, email) {
  require(googlesheets4)
  require(dplyr)
  library(googlesheets4)
  library(dplyr)
  googlesheets4::gs4_auth(email)
  googlesheets4::read_sheet(ss = key, sheet = sheet) %>%
    dplyr::mutate_if(is.list, as.character)
  
}