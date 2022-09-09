#script to update files to newest version in the parent functions folder where the editing is done


#update_dir contains the files that you would like to push to to update
#master_dir contains the files that you would like to pull from

update_file_fun = function(update_dir, master_dir) {
  #list file names in directory that you want to update
  u_files = list.files(update_dir)
  #list files in directory that you want to pull from
  m_files = list.files(master_dir)
  
  #creates vector with files positions in master directory that match your update directory files
  #omits na so that you dont have filepath/NA files
  m = na.omit(match(u_files, m_files))
  
  #creates full file path to be copied
  c_files = c(paste0(master_dir, m_files[m]))
  
  #overwrite needs to be T so that it will actually overwrite old files
  file.copy(c_files, update_dir, overwrite = T)
}

update_file_fun("functions","../../functions_master/")
