
fasta_name_switch = function(fasta, new_names){
  
  require(stringr)
  
  t = fasta
  
  for(i in 1:length(fasta)) {
    n = which(str_detect(new_names, names(fasta)[i]))
    names(t)[i] = new_names[n]
  }
  t
}