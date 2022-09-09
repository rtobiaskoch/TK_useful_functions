#filter fasta file by list of ids
filter_fasta = function(fa, filter_id) {
  id = c(filter_id)
  fa %>%
    keep(names(.) %in% id)
}