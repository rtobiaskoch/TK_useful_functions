get_mode <- function(x){
  tab <- table(x)
  candidates <- names(tab)[tab == max(tab)]
  return (as.numeric(max(candidates)))
}