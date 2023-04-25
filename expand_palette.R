
pacman::p_load(scales, RColorBrewer, wesanderson)

#myvals = list of unique vales from data to assign to colors
#palette you want to expand. example wes_palette(name = 'Darjeeling1')

pal_expand_fun = function(myvals, palette) {
  l = length(myvals)
  mycolors <- colorRampPalette(palette)(l)
  setNames(mycolors, myvals)
   
}
