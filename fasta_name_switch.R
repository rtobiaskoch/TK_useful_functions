fasta_name_switch = function(f, x, y){
  
  t = f
  
  for(i in 1:length(f)) {
    n = which(names(f)[i] == x)
    names(t)[i] = y[n]
  }
  t
}