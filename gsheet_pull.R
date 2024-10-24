

gsheet_pull = function(key, sheet, out_fn, update = T) {
  
  require(googlesheets4)
  library(googlesheets4)
  
  if(update == T) { #if update is T read in new 
    googlesheets4::gs4_auth()
    mdata = read_sheet(key, sheet = sheet)
    #mdata = apply(mdata, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
    write.csv(mdata, out_fn, row.names = F, na = "")
    read.csv(out_fn)
  } else { # if you don't want to pull it off google drive load the out_fn locally
    if(file.exists(out_fn)) {read.csv(out_fn)} 
    else {
      print(paste(out_fn, "file doesn't exist, please set update = T or download desired gsheet file"))
    }
    
  }
  
}   