#this function 

gsheet_pull = function(key, sheet) {
  require(googlesheets4)
  library(googlesheets4)
  
  d = read_sheet(key, sheet = sheet)
  
  #authorize token response
  1
  
  mdata = apply(d, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
  write.csv(mdata, "gsheet_data.csv")
  
}