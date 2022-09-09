
#this function does the following:
#1 moves files in current folder to desired directory
#2 renames file to have desired date as prefix
#3 removes old file

#WARNING FILE SHOULD BE OUTPUT EASILY RECREATED INCASE ERROR IN COPYING OCCURS AND FILE IS DELETED

file_clean = function(fn, dir, date){
  
  #name to replace with once file is moved this keeps the name but adds date where you manually added date to be replaced
  fn2 = paste0(date, "_", fn)
  
  #if file exists move it to dir
  if(file.exists(fn)) {
    file.copy(from = fn, 
              to = paste0(dir, "/", fn))
    #rename file with fn2
    file.rename(from = paste0(dir, "/", fn),
                to = paste0(dir, "/", fn2))
    file.remove(fn)
  }       
}