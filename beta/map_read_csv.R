#MAP READ CSV

map_read_fun = function (subdir, p) {
  fns = list.files(subdir, pattern = p)
  map_df(fns, ~read.csv(file.path(subdir, .x)))
}
  
