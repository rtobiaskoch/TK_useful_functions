#INSTALL PACKAGES
if (!require("pacman")) install.packages("pacman") #pacman is a useful package mgmt tool to ensure other users have the same packages
pacman::p_load(tidyverse, ggpubr) # you can list any packages that are required in the script and pacman will install tham and load them into the library

#pick grouping variable that you want to use to make your list
x = iris %>%
  group_by(Species) %>%
  group_split()


#create empty list for the for loop
x_list = list()
y = c(2,5,10) # example of how you could mnanipulate something specific for each dataframe

#operate some function on each data.frame in the list
for(i in seq_along(x)) {
  x_list[[i]] = x[[i]] %>%
    mutate(x = Sepal.Width* y[[i]]) #this could be anything often some function like estimate_R that you want to run over numerous different groups
}


#EXTRA IF YOU WANT TO PLOT YOUR RESULTS

#good idea to get names for plots and files
names = unique(iris$Species)


#create plots after applying functions and will maintain x and y axis specific to each of the 
plot_list = list()

for(i in seq_along(x)) {
  plot_list[[i]] = ggplot(x_list[[i]], aes(x = Sepal.Length, y =x)) +
    geom_point() +
    theme_classic() +
    ggtitle(names[i])
}

#quick way to plot all on 
ggarrange(plot_list[[1]], 
          plot_list[[2]], 
          plot_list[[3]],
          nrow = length(plot_list))
